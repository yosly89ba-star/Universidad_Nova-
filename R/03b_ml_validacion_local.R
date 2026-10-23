# =====================================================================
# 03b_ml_validacion_local.R - Mismo experimento SIN Spark (randomForest + glm + kmeans)
# Sirve para validar la lógica y obtener métricas de referencia en una
# máquina sin Spark. Misma partición por estudiante y mismas variables.
# Uso: Rscript R/03b_ml_validacion_local.R
# =====================================================================
source("R/00_config.R")
suppressPackageStartupMessages(library(randomForest))
set.seed(SEED)
mart <- read_csv(file.path(DIR_DW, "mart_desempeno.csv"), show_col_types = FALSE) |>
  mutate(deuda_pct = (facturado - pagado) / facturado,
         across(c(genero, region, rango_edad), as.factor), y = factor(bajo_rendimiento)) |>
  tidyr::drop_na()
vars <- c("semestre","asistencia_prom","logins","horas_plataforma_sem","entregas_pct","dias_mora",
          "prestamos_biblioteca","atenciones_bienestar","estrato","creditos_inscritos","deuda_pct",
          "genero","region","rango_edad")
train <- mart |> filter(id_estudiante_sk %% 5 < 4); test <- mart |> filter(id_estudiante_sk %% 5 == 4)

auc <- function(score, y) { r <- rank(score); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
metr <- function(p, y, nombre) {
  cls <- as.integer(p >= 0.5); tp <- sum(cls == 1 & y == 1); fp <- sum(cls == 1 & y == 0); fn <- sum(cls == 0 & y == 1)
  prec <- tp / (tp + fp); rec <- tp / (tp + fn)
  tibble(modelo = nombre, AUC = round(auc(p, y), 4), Accuracy = round(mean(cls == y), 4),
         Precision = round(prec, 4), Recall = round(rec, 4), F1 = round(2 * prec * rec / (prec + rec), 4))
}
f <- reformulate(vars, "y")
lr <- glm(f, data = train, family = binomial()); p_lr <- predict(lr, test, type = "response")
rf <- randomForest(f, data = train, ntree = 100, maxnodes = 256, importance = TRUE)
p_rf <- predict(rf, test, type = "prob")[, "1"]
yt <- as.integer(as.character(test$y))
res <- bind_rows(metr(p_lr, yt, "Regresión logística"), metr(p_rf, yt, "Random Forest"))
res <- res |> mutate(prevalencia_test = round(mean(yt), 4), n_train = nrow(train), n_test = nrow(test))
print(as.data.frame(res)); write_csv(res, file.path(DIR_DOCS, "metricas_ml_validacion_local.csv"))
imp <- importance(rf)[, "MeanDecreaseGini"]
imp <- tibble(variable = names(imp), importancia = round(imp, 1)) |> arrange(desc(importancia))
print(as.data.frame(imp)); write_csv(imp, file.path(DIR_DOCS, "importancia_variables_validacion_local.csv"))

# K-means (k=4) sobre variables estandarizadas
cv <- c("asistencia_prom","logins","horas_plataforma_sem","entregas_pct","dias_mora","promedio_ponderado","deuda_pct")
km <- kmeans(scale(mart[, cv]), centers = 4, nstart = 10)
perf <- mart |> mutate(cluster = km$cluster) |> group_by(cluster) |>
  summarise(n = n(), across(all_of(cv), ~ round(mean(.x), 2)), pct_bajo_rend = round(mean(bajo_rendimiento), 3))
print(as.data.frame(perf)); write_csv(perf, file.path(DIR_DOCS, "perfiles_clusters_validacion_local.csv"))
