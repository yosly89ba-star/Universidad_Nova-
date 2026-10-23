# =====================================================================
# Universidad Nova - Proyecto Big Data / Gobierno de Datos
# 00_config.R : parámetros y funciones comunes (ejecutar desde la raíz)
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
})

SEED     <- 2026
DIR_RAW  <- "data/raw"
DIR_DW   <- "data/warehouse"
DIR_DOCS <- "docs"
N_EST <- 15000; N_DOC <- 1200; N_ADM <- 700
PERIODOS <- c("2023-1","2023-2","2024-1","2024-2","2025-1","2025-2")
NOTA_MIN_APROBAR <- 11   # escala vigesimal 0-20
for (d in c(DIR_RAW, DIR_DW, DIR_DOCS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- Normalización de claves (misma regla para TODAS las fuentes) -------
normalizar_doc <- function(x) {
  x |> str_to_upper() |> str_remove_all("[^0-9A-Z]") |> str_remove("^(DNI|CE|PAS)")
}
normalizar_nombre <- function(x) {
  x <- str_squish(x)
  con_coma <- str_detect(x, ",")
  x <- ifelse(con_coma,
              paste(str_squish(str_extract(x, "(?<=,).*")), str_squish(str_extract(x, "^[^,]*"))),
              x)
  str_to_title(str_to_lower(str_squish(x)))
}
