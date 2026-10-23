# Guion del video (5 min)
**0:00-0:30 Problema.** Universidad Nova: 9 sistemas aislados, duplicados, cifras distintas, sin catálogo ni responsables, datos sensibles.
**0:30-1:30 Gobierno (DAMA-DMBOK).** Mostrar inventario_datos.csv (12 dominios, sensibilidad) y actores.csv; explicar mínimo privilegio y los roles SQL.
**1:30-2:15 Arquitectura.** Figura 1: fuentes, landing, ETL en R, warehouse (dw/pii/gov), Spark, Shiny, Git y Docker. Justificar estrella y segregación de PII.
**2:15-3:15 ETL en vivo.** Ejecutar `Rscript R/02_etl.R`. Señalar: 15.450 a 15.000 estudiantes, notas rechazadas, commit en etl_log. Abrir dq_issues.csv.
**3:15-4:00 Machine Learning.** Variables, exclusión de fuga, partición por estudiante, AUC 0,88 / F1 0,68, variables más importantes, 4 perfiles K-Means.
**4:00-4:45 Dashboard.** Ejecutar la app; usar filtros; mostrar mora vs. bajo rendimiento y la pestaña Gobierno de datos.
**4:45-5:00 Riesgos y cierre.** Tabla de 6 riesgos, repositorio en GitHub, próximos pasos.
