# server.R - Lógica del servidor (reactividad entre inputs y outputs)
server <- function(input, output, session) {

  # Los programas disponibles dependen de la facultad elegida
  output$ui_programa <- renderUI({
    d <- if (input$f_facultad == "Todas") mart else filter(mart, facultad == input$f_facultad)
    selectInput("f_programa", "Programa", choices = c("Todos", sort(unique(d$programa))), selected = "Todos")
  })

  datos <- reactive({
    d <- mart
    if (input$f_periodo  != "Todos") d <- filter(d, periodo == input$f_periodo)
    if (input$f_facultad != "Todas") d <- filter(d, facultad == input$f_facultad)
    if (!is.null(input$f_programa) && input$f_programa != "Todos") d <- filter(d, programa == input$f_programa)
    if (input$f_genero   != "Todos") d <- filter(d, genero == input$f_genero)
    req(nrow(d) > 0)
    d
  })

  # ---- KPIs
  output$kpi_est <- renderValueBox(valueBox(format(n_distinct(datos()$id_estudiante_sk), big.mark = ","),
                                            "Estudiantes", icon = icon("users"), color = "blue"))
  output$kpi_prom <- renderValueBox(valueBox(round(mean(datos()$promedio_ponderado), 2), "Promedio ponderado (0-20)",
                                             icon = icon("chart-line"), color = "aqua"))
  output$kpi_apr <- renderValueBox(valueBox(paste0(round(100 * mean(datos()$tasa_aprobacion), 1), "%"),
                                            "Tasa de aprobación", icon = icon("check"), color = "green"))
  output$kpi_riesgo <- renderValueBox(valueBox(paste0(round(100 * mean(datos()$bajo_rendimiento), 1), "%"),
                                               "En bajo rendimiento", icon = icon("triangle-exclamation"), color = "red"))

  # ---- Resumen
  output$g_tendencia <- renderPlotly({
    t <- datos() |> group_by(periodo) |>
      summarise(promedio = mean(promedio_ponderado), pct_riesgo = 100 * mean(bajo_rendimiento), .groups = "drop")
    p <- ggplot(t, aes(periodo, promedio, group = 1,
                       text = paste0("Periodo: ", periodo, "<br>Promedio: ", round(promedio, 2),
                                     "<br>% bajo rendimiento: ", round(pct_riesgo, 1)))) +
      geom_line(colour = colores["azul"], linewidth = 1.2) + geom_point(colour = colores["azul"], size = 3) +
      labs(x = NULL, y = "Promedio (0-20)") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  output$g_facultad <- renderPlotly({
    t <- datos() |> group_by(facultad) |> summarise(n = n_distinct(id_estudiante_sk), .groups = "drop")
    p <- ggplot(t, aes(reorder(facultad, n), n, text = paste0(facultad, ": ", n))) +
      geom_col(fill = colores["dorado"]) + coord_flip() + labs(x = NULL, y = "Estudiantes") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # ---- Rendimiento
  output$g_programa <- renderPlotly({
    t <- datos() |> group_by(programa) |> summarise(prom = mean(promedio_ponderado), .groups = "drop")
    p <- ggplot(t, aes(reorder(programa, prom), prom, text = paste0(programa, ": ", round(prom, 2)))) +
      geom_col(fill = colores["azul"]) + coord_flip(ylim = c(min(t$prom) - 1, max(t$prom) + 0.5)) +
      labs(x = NULL, y = "Promedio") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  output$g_hist <- renderPlotly({
    p <- ggplot(datos(), aes(promedio_ponderado)) + geom_histogram(binwidth = 1, fill = colores["azul"], colour = "white") +
      geom_vline(xintercept = 11, colour = colores["rojo"], linetype = "dashed") +
      labs(x = "Promedio ponderado (línea roja = mínimo aprobatorio 11)", y = "Registros") + theme_minimal()
    ggplotly(p)
  })
  output$g_estrato <- renderPlotly({
    t <- datos() |> group_by(estrato, genero) |> summarise(prom = mean(promedio_ponderado), .groups = "drop")
    p <- ggplot(t, aes(factor(estrato), prom, fill = genero, text = paste0("Estrato ", estrato, " - ", genero, ": ", round(prom, 2)))) +
      geom_col(position = "dodge") + scale_fill_manual(values = c(F = unname(colores["dorado"]), M = unname(colores["azul"]))) +
      labs(x = "Estrato", y = "Promedio", fill = NULL) + theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # ---- Riesgo
  output$g_scatter <- renderPlotly({
    d <- datos(); d <- d[sample(nrow(d), min(3000, nrow(d))), ]
    d$estado <- ifelse(d$bajo_rendimiento == 1, "Bajo rendimiento", "Aprobatorio")
    p <- ggplot(d, aes(entregas_pct, promedio_ponderado, colour = estado)) + geom_point(alpha = .35, size = 1.2) +
      scale_colour_manual(values = c("Bajo rendimiento" = unname(colores["rojo"]), "Aprobatorio" = unname(colores["azul"]))) +
      labs(x = "% de entregas en el LMS", y = "Promedio ponderado", colour = NULL) + theme_minimal()
    ggplotly(p)
  })
  output$g_mora <- renderPlotly({
    t <- datos() |> mutate(mora = cut(dias_mora, c(-1, 0, 30, 60, Inf), labels = c("Sin mora", "1-30", "31-60", ">60"))) |>
      group_by(mora) |> summarise(pct = 100 * mean(bajo_rendimiento), n = n(), .groups = "drop")
    p <- ggplot(t, aes(mora, pct, text = paste0(mora, ": ", round(pct, 1), "% (n=", n, ")"))) +
      geom_col(fill = colores["rojo"]) + labs(x = "Días de mora", y = "% bajo rendimiento") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  output$g_bienestar <- renderPlotly({
    ids <- unique(datos()$id_estudiante_sk)
    t <- bien |> filter(id_estudiante_sk %in% ids) |>
      count(categoria_principal, nivel_riesgo)
    req(nrow(t) > 0)
    p <- ggplot(t, aes(categoria_principal, n, fill = nivel_riesgo, text = paste0(categoria_principal, " / ", nivel_riesgo, ": ", n))) +
      geom_col() + scale_fill_manual(values = c(Bajo = "#2E8B57", Medio = "#F2B705", Alto = "#C0392B")) +
      labs(x = NULL, y = "Atenciones", fill = "Riesgo") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # ---- Gobierno
  output$t_dq  <- renderDT(datatable(select(dq, fuente, regla, filas_afectadas, accion), rownames = FALSE,
                                     options = list(pageLength = 10, dom = "tp")))
  output$t_etl <- renderDT(datatable(select(etl, paso, filas_entrada, filas_salida, rechazadas, commit_git, ts), rownames = FALSE,
                                     options = list(pageLength = 10, dom = "tp")))
}
