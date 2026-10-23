# ui.R - Diseño de la interfaz (elementos visuales e interactivos)
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Universidad Nova - Analítica"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen",          tabName = "resumen",    icon = icon("gauge")),
      menuItem("Rendimiento",      tabName = "rend",       icon = icon("graduation-cap")),
      menuItem("Riesgo académico", tabName = "riesgo",     icon = icon("triangle-exclamation")),
      menuItem("Gobierno de datos",tabName = "gobierno",   icon = icon("shield-halved"))
    ),
    hr(),
    selectInput("f_periodo", "Periodo", choices = c("Todos", periodos), selected = "Todos"),
    selectInput("f_facultad", "Facultad", choices = c("Todas", facultades), selected = "Todas"),
    uiOutput("ui_programa"),
    selectInput("f_genero", "Género", choices = c("Todos", "F", "M"), selected = "Todos")
  ),
  dashboardBody(
    tabItems(
      tabItem("resumen",
        fluidRow(valueBoxOutput("kpi_est", 3), valueBoxOutput("kpi_prom", 3),
                 valueBoxOutput("kpi_apr", 3), valueBoxOutput("kpi_riesgo", 3)),
        fluidRow(box(title = "Evolución del promedio y del % de bajo rendimiento", width = 8, status = "primary",
                     plotlyOutput("g_tendencia", height = 320)),
                 box(title = "Estudiantes por facultad", width = 4, status = "primary",
                     plotlyOutput("g_facultad", height = 320)))),
      tabItem("rend",
        fluidRow(box(title = "Promedio por programa", width = 7, status = "primary", plotlyOutput("g_programa", height = 420)),
                 box(title = "Distribución del promedio ponderado", width = 5, status = "primary", plotlyOutput("g_hist", height = 420))),
        fluidRow(box(title = "Promedio por estrato y género", width = 12, status = "primary", plotlyOutput("g_estrato", height = 300)))),
      tabItem("riesgo",
        fluidRow(box(title = "Participación en el LMS vs. promedio", width = 7, status = "warning",
                     plotlyOutput("g_scatter", height = 400),
                     helpText("Muestra aleatoria de hasta 3.000 registros del filtro actual.")),
                 box(title = "Bajo rendimiento según días de mora", width = 5, status = "warning", plotlyOutput("g_mora", height = 400))),
        fluidRow(box(title = "Atenciones de bienestar por motivo (agregado, sin identificar personas)", width = 12,
                     status = "warning", plotlyOutput("g_bienestar", height = 280)))),
      tabItem("gobierno",
        fluidRow(box(title = "Problemas de calidad detectados y su tratamiento (gov.dq_issues)", width = 12, status = "success",
                     DTOutput("t_dq"))),
        fluidRow(box(title = "Trazabilidad del ETL (gov.etl_log)", width = 12, status = "success", DTOutput("t_etl"))))
    )
  )
)
