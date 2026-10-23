# =====================================================================
# 02_etl.R  -  Extracción, Transformación y Carga (landing -> warehouse)
# - Conforma claves (documento/correo/carnet -> id_estudiante_sk)
# - Deduplica (golden record), aplica reglas de calidad y registra rechazos
# - Segrega PII (dim_estudiante_pii) y seudonimiza (documento_hash SHA-256)
# - Registra trazabilidad (etl_log, dq_issues, commit de git)
# - Carga a CSV (data/warehouse) y, si hay DB_HOST, a PostgreSQL
# Uso: Rscript R/02_etl.R
# =====================================================================
source("R/00_config.R")
suppressPackageStartupMessages(library(digest))

run_id <- format(Sys.time(), "%Y%m%d%H%M%S")
commit <- tryCatch(suppressWarnings(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE)),
                   error = function(e) character(0))
commit <- if (length(commit) == 1) commit else "sin-git"
SALT <- Sys.getenv("PII_SALT", "nova-demo-salt")   # en producción: gestor de secretos

etl_log <- list(); dq <- list()
add_log <- function(paso, entrada, salida, rechazadas = 0, nota = "")
  etl_log[[length(etl_log) + 1]] <<- tibble(run_id, commit_git = commit, paso, filas_entrada = entrada,
                                            filas_salida = salida, rechazadas, nota, ts = format(Sys.time()))
add_dq <- function(fuente, regla, filas, accion)
  dq[[length(dq) + 1]] <<- tibble(run_id, fuente, regla, filas_afectadas = filas, accion)
rd <- function(f) read_csv(file.path(DIR_RAW, f), col_types = cols(.default = "c"), progress = FALSE, show_col_types = FALSE)

# ============================ EXTRACT ================================
acad <- rd("academico_estudiantes.csv"); notas <- rd("academico_notas.csv")
lms <- rd("lms_actividad.csv"); fin <- rd("financiero_pagos.csv"); bib <- rd("biblioteca_prestamos.csv")
bien <- rd("bienestar_atenciones.csv"); emp <- rd("rrhh_empleados.csv")
progs <- rd("catalogo_programas.csv"); cursos <- rd("catalogo_cursos.csv")
add_log("extract", NA, nrow(acad) + nrow(notas) + nrow(lms) + nrow(fin) + nrow(bib) + nrow(bien) + nrow(emp),
        nota = "9 archivos en data/raw")

# ============================ TRANSFORM ==============================
# 1. Estudiantes: normalizar y deduplicar (golden record)
acad <- acad |> mutate(doc_norm = normalizar_doc(num_doc),
                       nombre_norm = normalizar_nombre(nombre_completo),
                       email_l = tolower(email))
n_ini <- nrow(acad)
dorados <- acad |>
  arrange(doc_norm, desc(nchar(nombre_norm)), id_academico) |>
  group_by(doc_norm) |> slice(1) |> ungroup() |>
  mutate(id_estudiante_sk = row_number())
add_dq("academico_estudiantes", "Duplicado por documento normalizado", n_ini - nrow(dorados), "Fusionado en registro dorado")
add_log("dedup_estudiantes", n_ini, nrow(dorados), n_ini - nrow(dorados), "clave: documento normalizado")

xw_doc  <- dorados |> select(doc_norm, id_estudiante_sk)
xw_id   <- acad |> inner_join(xw_doc, by = "doc_norm") |> distinct(id_academico, id_estudiante_sk)
xw_mail <- acad |> inner_join(xw_doc, by = "doc_norm") |> distinct(email_l, id_estudiante_sk)

dim_estudiante_pii <- dorados |>
  transmute(id_estudiante_sk, documento = doc_norm, nombre_completo = nombre_norm, email = email_l, fecha_nacimiento)
dim_estudiante <- dorados |>
  mutate(documento_hash = vapply(doc_norm, function(d) digest(paste0(SALT, d), "sha256", serialize = FALSE), ""),
         edad = as.integer(floor(as.numeric(as.Date("2025-12-31") - as.Date(fecha_nacimiento)) / 365.25)),
         rango_edad = as.character(cut(edad, c(0, 19, 22, 25, 200), labels = c("<=19", "20-22", "23-25", "26+")))) |>
  transmute(id_estudiante_sk, documento_hash, genero, rango_edad, estrato = as.integer(estrato), region,
            cod_programa, anio_ingreso = as.integer(anio_ingreso), estado)

# 2. Dimensiones de apoyo
dim_programa <- progs
dim_curso <- cursos |> transmute(cod_curso, nombre_curso, creditos = as.integer(creditos), cod_programa, nivel = as.integer(nivel))
dim_docente <- emp |> filter(tipo == "Docente") |> arrange(id_rrhh) |>
  transmute(id_docente_sk = row_number(), id_rrhh, facultad, categoria, dedicacion, anio_ingreso = as.integer(anio_ingreso))
add_dq("rrhh_empleados", "Minimización: documento, nombre y salario NO se cargan al DW", nrow(emp), "Excluido de dim_docente")
dim_tiempo <- tibble(periodo = PERIODOS, anio = as.integer(substr(PERIODOS, 1, 4)),
                     semestre = as.integer(substr(PERIODOS, 6, 6))) |>
  mutate(fecha_inicio = as.Date(ifelse(semestre == 1, paste0(anio, "-03-01"), paste0(anio, "-08-15"))),
         fecha_fin    = as.Date(ifelse(semestre == 1, paste0(anio, "-07-15"), paste0(anio, "-12-15"))))

# 3. Notas -> fact_matricula
n_n <- nrow(notas)
notas <- notas |>
  mutate(nota_num = suppressWarnings(as.numeric(str_replace(nota, ",", "."))),
         asist = suppressWarnings(as.numeric(asistencia_pct)))
add_dq("academico_notas", "Decimal con coma", sum(str_detect(notas$nota, ","), na.rm = TRUE), "Corregido a punto")
add_dq("academico_notas", "Nota nula", sum(is.na(notas$nota)), "Rechazado")
add_dq("academico_notas", "Nota fuera de rango [0,20]", sum(!is.na(notas$nota_num) & (notas$nota_num < 0 | notas$nota_num > 20)), "Rechazado")
notas_ok <- notas |> filter(!is.na(nota_num), nota_num >= 0, nota_num <= 20) |>
  inner_join(xw_id, by = "id_academico")
fact_matricula <- notas_ok |>
  left_join(dim_docente |> select(id_rrhh, id_docente_sk), by = "id_rrhh") |>
  left_join(dim_curso |> select(cod_curso, creditos), by = "cod_curso") |>
  transmute(id_estudiante_sk, cod_curso, periodo, id_docente_sk, nota_final = nota_num,
            asistencia_pct = pmin(100, pmax(0, asist)), aprobado = as.integer(nota_num >= NOTA_MIN_APROBAR), creditos) |>
  distinct(id_estudiante_sk, cod_curso, periodo, .keep_all = TRUE)
add_log("fact_matricula", n_n, nrow(fact_matricula), n_n - nrow(fact_matricula), "nota válida + clave conformada")

# 4. LMS
lms_f <- lms |> mutate(email_l = tolower(usuario_lms)) |>
  inner_join(xw_mail, by = "email_l") |>
  mutate(across(c(logins, horas_semana, entregas_pct), as.numeric)) |>
  group_by(id_estudiante_sk, periodo) |>
  summarise(logins = sum(logins, na.rm = TRUE), horas_plataforma_sem = mean(horas_semana, na.rm = TRUE),
            entregas_pct = mean(entregas_pct, na.rm = TRUE), .groups = "drop")
add_dq("lms_actividad", "Correo en mayúsculas (normalizado)", sum(lms$usuario_lms != tolower(lms$usuario_lms)), "Corregido")
add_dq("lms_actividad", "Horas nulas", sum(is.na(lms$horas_semana)), "Imputado con mediana global")
add_log("lms", nrow(lms), nrow(lms_f), nrow(lms) - nrow(lms_f))

# 5. Finanzas
fin <- fin |> mutate(doc_norm = normalizar_doc(doc_pagador),
                     across(c(valor_facturado, valor_pagado, dias_mora), as.numeric))
add_dq("financiero_pagos", "Pagador sin ficha académica (huérfano)", sum(!fin$doc_norm %in% xw_doc$doc_norm), "Rechazado")
add_dq("financiero_pagos", "Valor pagado negativo", sum(fin$valor_pagado < 0, na.rm = TRUE), "Corregido a valor absoluto")
fin_f <- fin |> inner_join(xw_doc, by = "doc_norm") |>
  mutate(valor_pagado = abs(valor_pagado)) |>
  group_by(id_estudiante_sk, periodo) |>
  summarise(facturado = sum(valor_facturado), pagado = sum(valor_pagado), dias_mora = max(dias_mora), .groups = "drop")
add_log("finanzas", nrow(fin), nrow(fin_f), nrow(fin) - nrow(fin_f))

# 6. Biblioteca y bienestar
bib_f <- bib |> inner_join(xw_id, by = c("carnet" = "id_academico")) |>
  group_by(id_estudiante_sk, periodo) |> summarise(prestamos_biblioteca = sum(as.integer(prestamos)), .groups = "drop")
fact_bienestar <- bien |> mutate(doc_norm = normalizar_doc(documento)) |>
  inner_join(xw_doc, by = "doc_norm") |>
  transmute(id_estudiante_sk, periodo, atenciones = as.integer(atenciones),
            categoria_principal = motivo_principal, nivel_riesgo)       # SENSIBLE: acceso restringido
add_log("bienestar", nrow(bien), nrow(fact_bienestar), nrow(bien) - nrow(fact_bienestar))

# 7. fact_desempeno + mart analítico
mediana <- function(x) median(x, na.rm = TRUE)
fact_desempeno <- fact_matricula |>
  group_by(id_estudiante_sk, periodo) |>
  summarise(promedio_ponderado = round(weighted.mean(nota_final, creditos), 2),
            creditos_inscritos = sum(creditos), asistencia_prom = round(mean(asistencia_pct), 1),
            tasa_aprobacion = round(mean(aprobado), 3), .groups = "drop") |>
  left_join(lms_f, by = c("id_estudiante_sk", "periodo")) |>
  left_join(fin_f, by = c("id_estudiante_sk", "periodo")) |>
  left_join(bib_f, by = c("id_estudiante_sk", "periodo")) |>
  left_join(fact_bienestar |> group_by(id_estudiante_sk, periodo) |>
              summarise(atenciones_bienestar = sum(atenciones), .groups = "drop"),
            by = c("id_estudiante_sk", "periodo")) |>
  mutate(across(c(prestamos_biblioteca, atenciones_bienestar, dias_mora, facturado, pagado), ~ tidyr::replace_na(.x, 0)),
         across(c(logins, horas_plataforma_sem, entregas_pct), ~ ifelse(is.na(.x) | is.nan(.x), mediana(.x), .x)),
         horas_plataforma_sem = round(horas_plataforma_sem, 1), entregas_pct = round(entregas_pct, 1),
         bajo_rendimiento = as.integer(promedio_ponderado < NOTA_MIN_APROBAR)) |>
  group_by(id_estudiante_sk) |> arrange(periodo, .by_group = TRUE) |>
  mutate(semestre = row_number()) |> ungroup() |>
  select(id_estudiante_sk, periodo, semestre, everything())

mart_desempeno <- fact_desempeno |>
  left_join(dim_estudiante |> select(id_estudiante_sk, genero, rango_edad, estrato, region, cod_programa), by = "id_estudiante_sk") |>
  left_join(dim_programa, by = "cod_programa")
add_log("mart_desempeno", nrow(fact_desempeno), nrow(mart_desempeno), 0, "sin PII; listo para ML y dashboard")

# ============================ LOAD ===================================
tablas <- list(dim_tiempo = dim_tiempo, dim_programa = dim_programa, dim_estudiante = dim_estudiante,
               dim_estudiante_pii = dim_estudiante_pii, dim_curso = dim_curso, dim_docente = dim_docente,
               fact_matricula = fact_matricula, fact_desempeno = fact_desempeno, fact_bienestar = fact_bienestar,
               mart_desempeno = mart_desempeno)
for (nm in names(tablas)) write_csv(tablas[[nm]], file.path(DIR_DW, paste0(nm, ".csv")))
add_log("load_csv", NA, sum(sapply(tablas, nrow)), 0, paste(length(tablas), "tablas en", DIR_DW))
etl_log_df <- bind_rows(etl_log); dq_df <- bind_rows(dq)
write_csv(etl_log_df, file.path(DIR_DW, "etl_log.csv")); write_csv(dq_df, file.path(DIR_DW, "dq_issues.csv"))

# Carga opcional a PostgreSQL (docker compose up -d ; export DB_HOST=localhost)
if (nzchar(Sys.getenv("DB_HOST")) && requireNamespace("RPostgres", quietly = TRUE)) {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("DB_HOST"), port = as.integer(Sys.getenv("DB_PORT", "5432")),
                        dbname = Sys.getenv("DB_NAME", "nova_dw"), user = Sys.getenv("DB_USER", "etl_user"),
                        password = Sys.getenv("DB_PASSWORD", "etl_pass"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  orden <- c("dim_tiempo","dim_programa","dim_estudiante","dim_estudiante_pii","dim_curso","dim_docente",
             "fact_matricula","fact_desempeno","fact_bienestar")
  DBI::dbExecute(con, paste0("TRUNCATE ", paste0("dw.", orden, collapse = ", "), " CASCADE"))
  for (nm in orden) DBI::dbAppendTable(con, DBI::Id(schema = "dw", table = nm), as.data.frame(tablas[[nm]]))
  DBI::dbAppendTable(con, DBI::Id(schema = "gov", table = "etl_log"), as.data.frame(etl_log_df))
  DBI::dbAppendTable(con, DBI::Id(schema = "gov", table = "dq_issues"), as.data.frame(dq_df))
  message("Carga a PostgreSQL completada.")
}
cat("\n== Resumen ETL (run", run_id, "| commit", commit, ") ==\n"); print(etl_log_df |> select(paso, filas_entrada, filas_salida, rechazadas), n = 30)
cat("\n== Calidad de datos ==\n"); print(dq_df |> select(fuente, regla, filas_afectadas, accion), n = 30, width = 140)
