# Universidad Nova - Solución Big Data con Gobierno de Datos

Prototipo reproducible (datos simulados) basado en DAMA-DMBOK: inventario y catálogo, actores y accesos,
ETL con calidad y trazabilidad, warehouse dimensional, ML con SparklyR y dashboard Shiny.

## Requisitos
R >= 4.2 y los paquetes: dplyr, tidyr, readr, stringr, digest, shiny, shinydashboard, plotly, DT, ggplot2, randomForest.
Para Spark: `install.packages("sparklyr"); sparklyr::spark_install("3.5.1")` y Java 8/11/17.
Opcional: Docker (PostgreSQL + Spark).

## Ejecución (desde la raíz del proyecto)
```r
install.packages(c("dplyr","tidyr","readr","stringr","digest","shiny","shinydashboard","plotly","DT","ggplot2","randomForest"))
```
```bash
Rscript R/01_simular_fuentes.R      # genera data/raw (9 CSV con defectos intencionales)
Rscript R/02_etl.R                  # ETL -> data/warehouse + etl_log + dq_issues
Rscript R/03_ml_sparklyr.R          # ML con Spark (requiere sparklyr)  -> docs/metricas_ml.csv
Rscript R/03b_ml_validacion_local.R # mismo experimento sin Spark
Rscript -e "shiny::runApp('dashboard')"
```
Opcional PostgreSQL (puerto 5433): `docker compose -f docker/docker-compose.yml up -d`, luego `DB_HOST=localhost DB_PORT=5433 Rscript R/02_etl.R`
(requiere el paquete RPostgres). Los roles de `sql/02_roles_seguridad.sql` se aplican con un usuario administrador.

## Resultados del modelo (SparklyR 3.5.1, partición por estudiante)
| Modelo | AUC | F1 | Accuracy |
|---|---|---|---|
| Regresión logística | 0.884 | 0.809 | 0.812 |
| Random Forest | 0.878 | 0.805 | 0.810 |

Variables más importantes (Random Forest): `entregas_pct` (0.52), `logins` (0.20), `asistencia_prom` (0.13).
K-Means (k=4) identifica 4 perfiles de estudiantes; ver `docs/perfiles_clusters.csv`.

## Estructura
- `R/` scripts · `dashboard/` Shiny (global.R, ui.R, server.R) · `sql/` esquema y roles · `docker/` compose
- `docs/` inventario_datos.csv, actores.csv, informe PDF, métricas y figuras
- `data/` se regenera con la semilla (excluido de Git)

## Seguridad
Los datos son simulados. La sal de seudonimización (`PII_SALT`) y las credenciales de la base deben
definirse como variables de entorno, nunca en el repositorio.
