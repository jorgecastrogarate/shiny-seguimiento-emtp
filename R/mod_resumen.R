# UI modulo Resumen

mod_resumen_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Resumen regional",
    icon  = bs_icon("clipboard-data"),
    
    layout_sidebar(
      fillable = TRUE,
      
      sidebar = sidebar(
        id    = ns("sidebar_resumen"),
        width = 250,
        open  = list(desktop = "open", mobile = "closed"),
        
        selectInput(ns("p1_glosa"), "Especialidad",
                    choices  = opciones_glosa,
                    selected = "Atención de Enfermería"
        ),
        selectInput(ns("p1_comuna"), "Comuna",
                    choices  = opciones_comunas,
                    selected = "Todas"
        ),
        selectInput(ns("p1_liceo"), "Establecimiento",
                    choices  = c("Todos", opciones_liceos),
                    selected = "Todos"
        )
      ),
      uiOutput(ns("titulo_filtros")),
      mobile_filter_hint(),  
      
      # col_widths con breakpoints: 4 columnas en desktop (lg),
      # 2 columnas en tablet (sm), 1 columna apilada en mobile (xs)
      layout_columns(
        col_widths = breakpoints(
          xs = c(12, 12, 12, 12),
          sm = c(6, 6, 6, 6),
          lg = c(3, 3, 3, 3)
        ),
        value_box(
          title    = paste("N° de Matrículas de la especialidad en", anio_max_mat),
          value    = textOutput(ns("vb_mat")),
          showcase = bs_icon("person-fill"),
          theme    = "primary"
        ),
        value_box(
          title    = "N° de Establecimientos que la imparten la especialidad",
          value    = textOutput(ns("vb_lic")),
          showcase = bs_icon("building"),
          theme    = "success"
        ),
        value_box(
          title    = paste("N° de Titulados de la especialidad en", anio_max_tit),
          value    = textOutput(ns("vb_tit")),
          showcase = bs_icon("mortarboard-fill"),
          theme    = "info"
        ),
        value_box(
          title    = "N° de Matriculados en convenios vigentes con el CFT Estatal",
          value    = textOutput(ns("vb_conv")),
          showcase = bs_icon("patch-check-fill"),
          theme    = "bg-purple",
          p(textOutput(ns("vb_conv_sub")), style = "font-size:0.8rem;")
        )
      ),
      
      # --- Texto introductorio ---
      card(
        card_body(
          p(
            "Nota: Los datos usados son obtenidos mediante registro administrativo y publicado en los datos abiertos del MINEDUC, por lo
            que podrían diferir de las estadísticas internas de cada establecimiento. Para más información de la fuente, revisar bases de datos de",
            tags$a(
              href = "https://datosabiertos.mineduc.cl/matricula-por-estudiante-2/",
              target = "_blank",
              "matrículas de educación media"
            ), "y ",
            tags$a(
              href = "https://datosabiertos.mineduc.cl/practicantes-y-titulados-tecnico-profesional/",
              target = "_blank",
              "practicantes y titulados Técnico-Profesional."
            )
          )
        )
      ),
      
      # --- Gráficos apilados verticalmente ---
      # full_screen = TRUE permite expandir el gráfico en pantalla completa,
      # muy útil en mobile donde 400px de alto puede quedar apretado
      card(
        full_screen = TRUE,
        height = "400px",
        card_header("Evolución de matriculados/as por año, periodo 2021-2025", class = "card-header-custom"),
        plotOutput(ns("plot_evol_mat"), height = "100%")
      ),
      card(
        full_screen = TRUE,
        height = "400px",
        card_header("Evolución de titulados/as por año, periodo 2021-2025", class = "card-header-custom"),
        plotOutput(ns("plot_evol_tit"), height = "100%")
      ),
      card(
        full_screen = TRUE,
        height = "400px",
        card_header("Distribución de Matrícula por sexo en", anio_max_mat, "(último año)", class = "card-header-custom"),
        plotOutput(ns("plot_sexo"), height = "100%")
      )
    )
  )
}

# Server modulo Resumen
mod_resumen_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      d <- datos_mat
      if (!is.null(input$p1_glosa) && length(input$p1_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p1_glosa)
      if (!is.null(input$p1_comuna) && input$p1_comuna != "Todas")
        d <- d |> filter(comuna == input$p1_comuna)
      liceos_disp <- sort(unique(d$nombre_rbd))
      updateSelectInput(session, "p1_liceo",
                        choices  = c("Todos", liceos_disp),
                        selected = "Todos")
    })

    datos_p1 <- reactive({
      d <- datos_mat
      if (!is.null(input$p1_glosa) && length(input$p1_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p1_glosa)
      if (!is.null(input$p1_comuna) && input$p1_comuna != "Todas")
        d <- d |> filter(comuna == input$p1_comuna)
      if (!is.null(input$p1_liceo) && input$p1_liceo != "Todos")    
        d <- d |> filter(nombre_rbd == input$p1_liceo)               
      d
    })
    
    datos_p1_s <- reactive({
      d <- datos_mat
      if (!is.null(input$p1_glosa) && length(input$p1_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p1_glosa)
      if (!is.null(input$p1_comuna) && input$p1_comuna != "Todas")
        d <- d |> filter(comuna == input$p1_comuna)
      d
    })
    
    datos_p1_anio <- reactive({
      datos_p1_s() |> filter(AGNO == anio_max_mat)
    })
    
    tit_p1 <- reactive({
      d <- datos_tit
      if (!is.null(input$p1_glosa) && length(input$p1_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p1_glosa)
      if (!is.null(input$p1_comuna) && input$p1_comuna != "Todas")
        d <- d |> filter(comuna == input$p1_comuna)
      if (!is.null(input$p1_liceo) && input$p1_liceo != "Todos")     
        d <- d |> filter(nombre_rbd == input$p1_liceo)               
      d
    })
    
    tit_p1_s <- reactive({
      d <- datos_tit
      if (!is.null(input$p1_glosa) && length(input$p1_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p1_glosa)
      if (!is.null(input$p1_comuna) && input$p1_comuna != "Todas")
        d <- d |> filter(comuna == input$p1_comuna)
      d
    })
    
    tit_p1_anio <- reactive({
      tit_p1_s() |> filter(AGNO_TITULACION == anio_max_tit)
    })
    
    output$titulo_filtros <- renderUI({
      
      especialidad <- input$p1_glosa
      comuna       <- if (input$p1_comuna == "Todas") "Tarapacá" else input$p1_comuna
      liceo        <- input$p1_liceo
      
      tags$div(
        class = "px-1 py-2 mb-2",
        tags$h4(
          class = "fw-bold text-primary mb-1",
          ifelse(
            input$p1_liceo == "Todos",
            paste(especialidad, comuna, sep = " - "),
            paste(especialidad, liceo, sep = " - ")
          )
        )
      )
    })
    
    # --- Value boxes ---
    
    output$vb_mat <- renderText({
      n <- n_distinct(datos_p1_anio()$MRUN)
      format(n, big.mark = ".")
    })
    
    output$vb_lic <- renderText({
      n <- n_distinct(datos_p1_anio()$nombre_rbd)
      as.character(n)
    })
    
    output$vb_tit <- renderText({
      n <- n_distinct(tit_p1_anio()$MRUN)
      format(n, big.mark = ".")
    })
    
    output$vb_conv <- renderText({
      d    <- datos_p1_anio()
      conv <- n_distinct(d$MRUN[d$nombre_rbd %in% liceos_con_conv$nombre_rbd])
      if (n_distinct(d$MRUN) == 0) return("–")
      format(conv, big.mark = ".")
    })
    
    output$vb_conv_sub <- renderText({
      d     <- datos_p1_anio()
      tot   <- n_distinct(d$MRUN)
      conv  <- n_distinct(d$MRUN[d$nombre_rbd %in% liceos_con_conv$nombre_rbd])
      n_lic <- n_distinct(d$nombre_rbd[d$nombre_rbd %in% liceos_con_conv$nombre_rbd])
      if (tot == 0) return("")
      paste0(round(conv / tot * 100, 1), "% del total · ", n_lic, " establecimientos")
    })
    
    # --- Gráficos página 1 ---
    
    output$plot_evol_mat <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_evol_mat"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      datos_p1() |>
        group_by(AGNO) |>
        summarise(n = n_distinct(MRUN), .groups = "drop") |>
        ggplot(aes(x = factor(AGNO), y = n)) +
        geom_col(fill = "#3396c6", width = 0.5) +
        geom_text(aes(label = format(n, big.mark = ".")),
                  vjust = -0.4, size = t$texto) +
        scale_y_continuous(labels = label_comma(big.mark = "."),
                           expand = expansion(mult = c(0, 0.12))) +
        labs(x = NULL, y = "N° Matrículas") +
        theme_minimal(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(panel.grid = element_blank())
    }, res = 96)
    
    output$plot_evol_tit <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_evol_tit"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      tit_p1() |>
        group_by(AGNO_TITULACION) |>
        summarise(n = n_distinct(MRUN), .groups = "drop") |>
        ggplot(aes(x = factor(AGNO_TITULACION), y = n)) +
        geom_col(fill = "#22498e", width = 0.5) +
        geom_text(aes(label = format(n, big.mark = ".")),
                  vjust = -0.4, size = t$texto) +
        scale_y_continuous(labels = label_comma(big.mark = "."),
                           expand = expansion(mult = c(0, 0.12))) +
        labs(x = NULL, y = "N° Titulados/as") +
        theme_minimal(base_size = t$base) +
        tema_nota_pie(size = t$pie) +
        theme(panel.grid = element_blank())
    }, res = 96)
    
    output$plot_sexo <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_sexo"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      es_chico <- t$base == 10   # reutiliza el mismo criterio para el label compacto
      
      datos_p1() |>
        filter(AGNO == anio_max_mat) |>
        group_by(sexo) |>
        summarise(n = n_distinct(MRUN), .groups = "drop") |>
        mutate(
          pct   = n / sum(n),
          label = if (es_chico) {
            paste0(sexo, "\n", scales::percent(pct, accuracy = 0.1, decimal.mark = ","))
          } else {
            paste0(sexo, "\n", format(n, big.mark = "."), "\n(",
                   scales::percent(pct, accuracy = 0.1, decimal.mark = ","), ")")
          }
        ) |>
        ggplot(aes(x = 1, y = pct, fill = sexo)) +
        geom_bar(stat = "identity", position = "fill", width = 0.8, 
                 color = "white", linewidth = 0.5) +
        geom_text(aes(label = label),
                  position = position_fill(vjust = 0.5), size = t$texto, color = "white") +
        scale_fill_manual(values = c("#12478D", "#E83D5C")) +
        scale_x_continuous(limits = c(0, 2)) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        theme_void(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(legend.position = "none")
    }, res = 96)
   })
  }