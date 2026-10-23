# global.R - se ejecuta una vez al iniciar la app (carga datos y librerías)
library(shiny); library(shinydashboard); library(dplyr); library(readr)
library(ggplot2); library(plotly); library(DT)

# Ruta al warehouse: funciona al lanzar desde dashboard/ o desde la raíz del proyecto
dir_dw <- if (dir.exists("../data/warehouse")) "../data/warehouse" else "data/warehouse"
leer <- function(f) read_csv(file.path(dir_dw, f), show_col_types = FALSE)

mart   <- leer("mart_desempeno.csv")          # sin PII
dq     <- leer("dq_issues.csv")
etl    <- leer("etl_log.csv")
bien   <- leer("fact_bienestar.csv")           # sensible: solo agregados en el dashboard
mart$estrato <- as.integer(mart$estrato)

periodos   <- sort(unique(mart$periodo))
facultades <- sort(unique(mart$facultad))
colores <- c(azul = "#1F3864", dorado = "#F2B705", rojo = "#C0392B", verde = "#2E8B57", gris = "#7F8C8D")
