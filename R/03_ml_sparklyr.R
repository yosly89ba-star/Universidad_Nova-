# =====================================================================
# 03_ml_sparklyr.R - Clasificación y agrupación con sparklyr (Spark MLlib)
#   Clasificación: predecir bajo_rendimiento (promedio < 11) con
#                  Regresión logística y Random Forest.
#   Agrupación:    K-Means (k = 4) para perfiles de estudiantes.
# Requisitos: install.packages("sparklyr"); sparklyr::spark_install("3.5.1"); Java 8/11/17
# Uso: Rscript R/03_ml_sparklyr.R
# Salidas: docs/metricas_ml.csv, docs/importancia_variables.csv, docs/perfiles_clusters.csv
# =====================================================================
suppressPackageStartupMessages({ library(sparklyr); library(dplyr); library(readr) })
source("R/00_config.R")
set.seed(SEED)

sc <- spark_connect(master = "local", app_name = "nova_ml")
mart <- spark_read_csv(sc, "mart", file.path(DIR_DW, "mart_desempeno.csv"), infer_schema = TRUE)

# ---- Variables: se excluyen promedio_ponderado y tasa_aprobacion (fuga de información: definen la etiqueta)
num <- c("semestre","asistencia_prom","logins","horas_plataforma_sem","entregas_pct",
         "dias_mora","prestamos_biblioteca","atenciones_bienestar","estrato","creditos_inscritos")
cat_ <- c("genero","region","rango_edad")

datos <- mart |>
  mutate(deuda_pct = (facturado - pagado) / facturado) |>
  select(id_estudiante_sk, bajo_rendimiento, all_of(num), all_of(cat_), deuda_pct) |>
  mutate(label = as.numeric(bajo_rendimiento)) |> na.omit()
num <- c(num, "deuda_pct")

# ---- Partición POR ESTUDIANTE (evita que el mismo alumno esté en train y test)
datos <- datos |> mutate(grupo = id_estudiante_sk %% 5)
train <- datos |> filter(grupo < 4); test <- datos |> filter(grupo == 4)

pipe_base <- ml_pipeline(sc) |>
  ft_string_indexer("genero", "genero_i") |> ft_string_indexer("region", "region_i") |>
  ft_string_indexer("rango_edad", "edad_i") |>
  ft_one_hot_encoder(c("genero_i","region_i","edad_i"), c("genero_v","region_v","edad_v")) |>
  ft_vector_assembler(c(num, "genero_v","region_v","edad_v"), "features")

evaluar <- function(modelo, nombre) {
  pred <- ml_transform(modelo, test)
  auc <- ml_binary_classification_evaluator(pred, label_col = "label", metric_name = "areaUnderROC")
  f1  <- ml_multiclass_classification_evaluator(pred, label_col = "label", metric_name = "f1")
  acc <- ml_multiclass_classification_evaluator(pred, label_col = "label", metric_name = "accuracy")
  tibble(modelo = nombre, AUC = auc, F1 = f1, Accuracy = acc)
}
m_lr <- ml_fit(pipe_base |> ml_logistic_regression(features_col = "features", label_col = "label"), train)
m_rf <- ml_fit(pipe_base |> ml_random_forest_classifier(features_col = "features", label_col = "label",
                                                                    num_trees = 100, max_depth = 8), train)
metricas <- bind_rows(evaluar(m_lr, "Regresión logística"), evaluar(m_rf, "Random Forest"))
print(metricas); write_csv(metricas, file.path(DIR_DOCS, "metricas_ml.csv"))

# Importancia de variables del Random Forest
rf_stage <- ml_stages(m_rf)[[length(ml_stages(m_rf))]]
imp_vec <- as.numeric(invoke(invoke(spark_jobj(rf_stage), "featureImportances"), "toArray"))
nombres <- c(num, paste0("categoria_", seq_len(length(imp_vec) - length(num))))
imp <- data.frame(variable = nombres, importancia = imp_vec)
imp <- imp[order(-imp$importancia), ]
print(head(imp, 10))
write_csv(imp, file.path(DIR_DOCS, "importancia_variables.csv"))

# ---- Agrupación K-Means (perfiles)
cl <- mart |> mutate(deuda_pct = (facturado - pagado) / facturado) |>
  select(all_of(c("asistencia_prom","logins","horas_plataforma_sem","entregas_pct","dias_mora","promedio_ponderado","deuda_pct"))) |> na.omit()
cl_pipe <- ml_pipeline(sc) |>
  ft_vector_assembler(colnames(cl), "raw") |> ft_standard_scaler("raw", "features", with_mean = TRUE)
cl_fit <- ml_fit(cl_pipe |> ml_kmeans(features_col = "features", k = 4, seed = SEED), cl)
cl_out <- ml_transform(cl_fit, cl)
perfiles <- cl_out |> group_by(prediction) |>
  summarise(n = n(), across(all_of(colnames(cl)), mean)) |> collect()
print(perfiles); write_csv(perfiles, file.path(DIR_DOCS, "perfiles_clusters.csv"))
spark_disconnect(sc)
