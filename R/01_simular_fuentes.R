# =====================================================================
# 01_simular_fuentes.R
# Simula los sistemas fuente de la Universidad Nova CON problemas reales:
# formatos de documento/nombre distintos, duplicados, notas fuera de rango,
# decimales con coma, valores nulos y registros huérfanos.
# Salida: data/raw/*.csv  (zona "landing")
# Uso: Rscript R/01_simular_fuentes.R
# =====================================================================
source("R/00_config.R")
set.seed(SEED)
quitar_tildes <- function(x) chartr("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN", x)

programas <- tribble(
  ~cod_programa, ~programa,                    ~facultad,
  "ING-SIS", "Ingeniería de Sistemas",         "Ingeniería",
  "ING-IND", "Ingeniería Industrial",          "Ingeniería",
  "ING-CIV", "Ingeniería Civil",               "Ingeniería",
  "ADM-EMP", "Administración de Empresas",     "Ciencias Económicas",
  "CON-PUB", "Contabilidad",                   "Ciencias Económicas",
  "ECO",     "Economía",                       "Ciencias Económicas",
  "MED",     "Medicina",                       "Ciencias de la Salud",
  "ENF",     "Enfermería",                     "Ciencias de la Salud",
  "PSI",     "Psicología",                     "Ciencias de la Salud",
  "DER",     "Derecho",                        "Ciencias Sociales",
  "EDU",     "Educación",                      "Ciencias Sociales",
  "COM",     "Comunicación Social",            "Ciencias Sociales")
cursos <- programas |>
  tidyr::crossing(n = 1:10) |>
  transmute(cod_curso = sprintf("%s-%02d", cod_programa, n),
            nombre_curso = paste(programa, "- Asignatura", sprintf("%02d", n)),
            creditos = sample(3:4, n(), TRUE), cod_programa, nivel = n)

nombres <- c("Juan","María","Carlos","Ana","Luis","Sofía","Jorge","Lucía","Pedro","Camila","Andrés","Valentina","Diego","Daniela","Miguel","Paula","Santiago","Laura","Fernando","Carolina","José","Gabriela","Ricardo","Natalia","Héctor","Alejandra","Manuel","Isabel","Rafael","Mónica")
apellidos <- c("García","Rodríguez","López","Martínez","González","Pérez","Sánchez","Ramírez","Torres","Flores","Rivera","Gómez","Díaz","Cruz","Morales","Reyes","Gutiérrez","Ortiz","Chávez","Ruiz","Mendoza","Vargas","Castillo","Jiménez","Herrera","Medina","Aguilar","Vega","Rojas","Silva")
n_emp <- N_DOC + N_ADM
empleados <- tibble(
  id_rrhh = sprintf("E%04d", 1:n_emp),
  tipo = c(rep("Docente", N_DOC), rep("Administrativo", N_ADM)),
  documento = sprintf("%08d", sample(10000000:99999999, n_emp)),
  nombre = paste(sample(nombres, n_emp, TRUE), sample(apellidos, n_emp, TRUE), sample(apellidos, n_emp, TRUE)),
  facultad = c(sample(unique(programas$facultad), N_DOC, TRUE),
               sample(c("Finanzas","Registro Académico","TI","RRHH","Bienestar","Biblioteca","Jurídica"), N_ADM, TRUE)),
  categoria = c(sample(c("Auxiliar","Asociado","Principal"), N_DOC, TRUE, prob = c(.5,.35,.15)), rep("N/A", N_ADM)),
  dedicacion = sample(c("Tiempo completo","Medio tiempo","Por horas"), n_emp, TRUE, prob = c(.4,.2,.4)),
  anio_ingreso = sample(2005:2025, n_emp, TRUE),
  salario = round(runif(n_emp, 2500, 12000), 2)      # dato SENSIBLE (no va al DW)
)

N <- N_EST
aptitud    <- rnorm(N)
compromiso <- 0.3 * aptitud + sqrt(1 - 0.09) * rnorm(N)
master <- tibble(
  idx = 1:N,
  id_academico = sprintf("A%06d", 1:N),
  doc = sprintf("%08d", sample(10000000:99999999, N)),
  tipo_doc = sample(c("DNI","CE","PAS"), N, TRUE, prob = c(.92,.05,.03)),
  nombre = sample(nombres, N, TRUE), ap1 = sample(apellidos, N, TRUE), ap2 = sample(apellidos, N, TRUE),
  genero = sample(c("F","M"), N, TRUE, prob = c(.54,.46)),
  fecha_nac = as.Date("1995-01-01") + sample(0:3650, N, TRUE),
  estrato = sample(1:6, N, TRUE, prob = c(.15,.28,.30,.15,.08,.04)),
  region = sample(c("Norte","Sur","Centro","Oriente","Costa"), N, TRUE),
  cod_programa = sample(programas$cod_programa, N, TRUE),
  aptitud, compromiso,
  ingreso_idx = sample(1:6, N, TRUE, prob = c(.30,.15,.20,.10,.15,.10))
) |>
  mutate(email = paste0(tolower(quitar_tildes(nombre)), ".", tolower(quitar_tildes(ap1)), idx, "@nova.edu.pe"),
         anio_ingreso = 2022 + ceiling(ingreso_idx / 2))

variante_nombre <- function(n, a1, a2, tipo) {
  dplyr::case_when(
    tipo == 1 ~ toupper(paste0(a1, " ", a2, ", ", n)),
    tipo == 2 ~ paste(n, a1, a2),
    tipo == 3 ~ tolower(paste0(n, " ", a1, " ", substr(a2, 1, 1), ".")),
    TRUE      ~ toupper(paste(n, a1)))
}
variante_doc <- function(d, tipo) {
  dplyr::case_when(
    tipo == 1 ~ d,
    tipo == 2 ~ paste0(substr(d,1,2), ".", substr(d,3,5), ".", substr(d,6,8)),
    tipo == 3 ~ paste("DNI", d),
    TRUE      ~ paste0(" ", d, " "))
}

acad <- master |>
  mutate(nombre_completo = variante_nombre(nombre, ap1, ap2, sample(1:4, N, TRUE)),
         num_doc = variante_doc(doc, sample(1:4, N, TRUE))) |>
  transmute(id_academico, tipo_doc, num_doc, nombre_completo, genero,
            fecha_nacimiento = as.character(fecha_nac), estrato, region, cod_programa,
            anio_ingreso, email, estado = "Activo")
dup_idx <- sample(1:N, round(0.03 * N))                    # 3 % duplicados
dups <- master[dup_idx, ] |>
  mutate(id_academico = sprintf("A%06d", N + row_number()),
         nombre_completo = variante_nombre(nombre, ap1, ap2, sample(1:4, n(), TRUE)),
         num_doc = variante_doc(doc, sample(1:4, n(), TRUE))) |>
  transmute(id_academico, tipo_doc, num_doc, nombre_completo, genero,
            fecha_nacimiento = as.character(fecha_nac), estrato, region, cod_programa,
            anio_ingreso, email, estado = "Activo")
acad_all <- bind_rows(acad, dups) |> slice_sample(prop = 1)
id_por_idx <- split(c(acad$id_academico, dups$id_academico), c(1:N, dup_idx))

dur <- pmin(1 + rgeom(N, 0.06), 7 - master$ingreso_idx)
sp <- master |>
  mutate(dur = dur) |>
  tidyr::uncount(dur, .id = "semestre") |>
  mutate(p_idx = ingreso_idx + semestre - 1, periodo = PERIODOS[p_idx])
n <- nrow(sp)
sp <- sp |> mutate(
  comp_p = compromiso + rnorm(n, 0, 0.4),
  horas  = pmax(0.5, round(8 + 2.5 * comp_p + rnorm(n, 0, 2.5), 1)),
  logins = rpois(n, exp(4 + 0.30 * comp_p)),
  entregas_pct = round(100 * plogis(0.6 + 1.0 * comp_p + 0.3 * aptitud + rnorm(n, 0, .5)), 1),
  asistencia = round(pmin(100, pmax(30, 82 + 8 * comp_p + rnorm(n, 0, 6))), 1),
  mora = rbinom(n, 1, plogis(-1.7 - 0.30 * (estrato - 3) - 0.25 * comp_p)),
  dias_mora = ifelse(mora == 1, round(rexp(n, 1/25)) + 1, 0),
  facturado = round(runif(n, 1500, 3500), 2),
  pagado = round(facturado - ifelse(mora == 1, facturado * runif(n, .1, .6), 0), 2),
  prestamos = rpois(n, exp(0.5 + 0.30 * aptitud)),
  atenciones = rpois(n, 0.3 * exp(-0.3 * comp_p + 0.1 * mora)),
  rend = 0.8 * aptitud + 0.6 * comp_p - 0.012 * dias_mora)

sp$base <- sample(0:9, n, TRUE)
notas <- sp |>
  select(idx, periodo, cod_programa, rend, asistencia, base) |>
  tidyr::uncount(5, .id = "k") |>
  mutate(cod_curso = sprintf("%s-%02d", cod_programa, (base + k) %% 10 + 1),
         nota = pmin(20, pmax(0, round(12.6 + 2.4 * rend + rnorm(n(), 0, 2.2), 1))),
         asist = pmin(100, pmax(0, round(asistencia + rnorm(n(), 0, 4), 1))))
fac_est <- programas$facultad[match(notas$cod_programa, programas$cod_programa)]
docs_fac <- split(empleados$id_rrhh[empleados$tipo == "Docente"], empleados$facultad[empleados$tipo == "Docente"])
notas$id_rrhh <- NA_character_
for (f in unique(fac_est)) {
  i <- which(fac_est == f); notas$id_rrhh[i] <- sample(docs_fac[[f]], length(i), TRUE)
}
notas$id_academico <- vapply(seq_len(nrow(notas)), function(i) {
  ids <- id_por_idx[[as.character(notas$idx[i])]]; if (length(ids) == 1) ids else sample(ids, 1)
}, character(1))
m <- nrow(notas)
notas$nota_txt <- as.character(notas$nota)
i_coma <- sample(m, round(.02 * m)); notas$nota_txt[i_coma] <- sub("\\.", ",", as.character(notas$nota[i_coma]))
notas$nota_txt[sample(m, round(.012 * m))] <- NA
k_out <- round(.003 * m); notas$nota_txt[sample(m, k_out)] <- sample(c("-1","130","99"), k_out, TRUE)
notas_raw <- notas |> transmute(id_academico, cod_curso, periodo, id_rrhh, nota = nota_txt, asistencia_pct = asist)

lms <- sp |> transmute(usuario_lms = ifelse(runif(n) < .05, toupper(email), email), periodo,
                       logins, horas_semana = horas, entregas_pct)
lms$horas_semana[sample(n, round(.02 * n))] <- NA
fin <- sp |> transmute(doc_pagador = variante_doc(doc, sample(1:4, n, TRUE)), periodo,
                       valor_facturado = facturado, valor_pagado = pagado, dias_mora)
neg <- sample(nrow(fin), 30); fin$valor_pagado[neg] <- -abs(fin$valor_pagado[neg])
huerf <- tibble(doc_pagador = sprintf("%08d", sample(10000000:99999999, 40)), periodo = sample(PERIODOS, 40, TRUE),
                valor_facturado = 2000, valor_pagado = 2000, dias_mora = 0)
fin <- bind_rows(fin, huerf)
bib <- sp |> transmute(carnet = vapply(idx, function(i) id_por_idx[[as.character(i)]][1], character(1)),
                       periodo, prestamos)
bien <- sp |> filter(atenciones > 0) |>
  transmute(documento = doc, periodo, atenciones,
            motivo_principal = sample(c("Salud mental","Económico","Académico","Familiar"), n(), TRUE, prob = c(.35,.25,.25,.15)),
            nivel_riesgo = sample(c("Bajo","Medio","Alto"), n(), TRUE, prob = c(.6,.3,.1)))

w <- function(df, f) write_csv(df, file.path(DIR_RAW, f), na = "")
w(acad_all, "academico_estudiantes.csv"); w(notas_raw, "academico_notas.csv")
w(programas, "catalogo_programas.csv");  w(cursos, "catalogo_cursos.csv")
w(lms, "lms_actividad.csv"); w(fin, "financiero_pagos.csv")
w(bib, "biblioteca_prestamos.csv"); w(bien, "bienestar_atenciones.csv")
w(empleados, "rrhh_empleados.csv")
cat(sprintf("Fuentes simuladas en %s | estudiantes: %d (+%d duplicados) | notas: %d | est-periodo: %d\n",
            DIR_RAW, N, length(dup_idx), nrow(notas_raw), n))
