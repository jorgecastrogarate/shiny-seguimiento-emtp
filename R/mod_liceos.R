# UI modulo liceos

mod_liceos_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Establecimientos CRT-2595",
    icon  = bs_icon("building-fill"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        id    = ns("sidebar_liceos"),
        width = 250,
        open  = list(desktop = "open", mobile = "closed"),
        selectInput(ns("p2_glosa"), "Especialidad",
          choices  = opciones_glosa,
          selected = "Atención de Enfermería"
        ),
        selectInput(ns("p2_liceo"), "Establecimiento",
          choices  = c("Todos" = "Todos", opciones_liceos_p2),
          selected = "Todos"
        )
      ),
      uiOutput(ns("titulo_filtros_p2")),
      mobile_filter_hint(),
      # Fila 1: value boxes
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Tasa de Egreso último año",
          value    = textOutput(ns("vb_tasa_egreso")),
          showcase = bs_icon("mortarboard-fill"),
          theme    = "primary"
        ),
        value_box(
          title    = "Tasa de Titulación último año",
          value    = textOutput(ns("vb_tasa_tit")),
          showcase = bs_icon("patch-check-fill"),
          theme    = "success"
        ),
        value_box(
          title    = "Tiempo Promedio de Titulación últimos 5 años",
          value    = textOutput(ns("vb_tiempo_tit")),
          showcase = bs_icon("hourglass-split"),
          theme    = "info"
        )
      ),
      card(
        card_body(
          p(
            "Nota: Estos datos son registros administrativos del MINEDUC, por lo que podrían diferir de las estadísticas internas de cada 
            establecimiento. En esta sección, se realizó un seguimiento por cohortes, combinando tres bases de datos distintas ",
            tags$a(
              href = "https://datosabiertos.mineduc.cl/matricula-por-estudiante-2/",
              target = "_blank",
              "(matrículas de educación media,"
            ),
            tags$a(
              href = "https://datosabiertos.mineduc.cl/notas-y-egresados-de-ensenanza-media/",
              target = "_blank",
              "notas y egresados de enseñanza media"
            ), "y ",
            tags$a(
              href = "https://datosabiertos.mineduc.cl/practicantes-y-titulados-tecnico-profesional/",
              target = "_blank",
              "practicantes y titulados Técnico-Profesional)"
            ), "las cuales poseen en común la variable MRUN y MRUN_IPE. Para más información, revisar las fuentes señaladas.",
            p("Los datos de esta sección sólo corresponden a cohortes de egresados/as de establecimientos del proyecto CRT-2595"),
            p("PD: En el caso del liceo Kronos, no se consideró la enseñanza T-P para adultos, dada la gran cantidad de datos perdidos en la 
              base de datos de egresados.")
          )
        )
      ),

      # Fila 2: % egreso y titulación por cohorte
        card(
          card_header("Evolución de matriculados/as vs egresados/as por cohorte, periodo 2019-2024", class = "card-header-custom"),
          plotOutput(ns("plot_p2_egreso"), height = "400px")
        ),
        card(
          card_header("Evolución de egresados/as vs titulados/as por cohorte, periodo 2019-2024", class = "card-header-custom"),
          plotOutput(ns("plot_p2_tit"), height = "400px")
        ),
      # Fila 3: brecha de género
        card(
          card_header("Evolución de la distribución de egresados/as por sexo y cohorte, periodo 2019-2024", class = "card-header-custom"),
          plotOutput(ns("plot_p2_egreso_sexo"), height = "400px")
        ),
        card(
          card_header("Evolución de la distribución de titulados/as por sexo y cohorte, periodo 2019-2024", class = "card-header-custom"),
          plotOutput(ns("plot_p2_tit_sexo"), height = "400px"),
        ),
       # Fila 4: centros de práctica
         card(
           card_header("Centros de Práctica de estudiantes EMTP, periodo 2019-2024", class = "card-header-custom"),
           card_body(
             class = "p-2",
             layout_columns(
               col_widths = c(7, 5),
              
          # Top 10 histórico
              tags$div(
                 tags$p(
                  class = "fw-semibold mb-2 text-uppercase small text-muted",
                  bs_icon("building"), "10 centros de práctica más concurridos últimos 5 años"
                  ),
                  DTOutput(ns("tbl_practicas_total"), fill = FALSE)
                ),
                
            # Top 5 último año
               tags$div(
                  uiOutput(ns("tbl_practicas_anio_titulo")),
                  DTOutput(ns("tbl_practicas_anio"), fill = FALSE))
             )
           )
         )
       )
    )
  }



# Server modulo liceos
mod_liceos_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observe({
      d <- seguimiento_emtp
      if (!is.null(input$p2_glosa) && length(input$p2_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p2_glosa)
      
      liceos_disp <- d %>%
        distinct(RBD, nombre_rbd) %>%
        arrange(nombre_rbd) %>%
        { setNames(as.character(.$RBD), .$nombre_rbd) }
      
      updateSelectInput(session, "p2_liceo",
                        choices  = c("Todos" = "Todos", liceos_disp),
                        selected = "Todos")
    })
    
    datos_p2 <- reactive({
      d <- seguimiento_emtp
      if (!is.null(input$p2_glosa) && length(input$p2_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p2_glosa)
      if (!is.null(input$p2_liceo) && input$p2_liceo != "Todos")
        d <- d |> filter(RBD == as.numeric(input$p2_liceo))
      d
    })
    
    # Datos de prácticas filtrados 
    practicas_p2 <- reactive({
      d <- practicas
      if (!is.null(input$p2_glosa) && length(input$p2_glosa) > 0)
        d <- d |> filter(glosa_simple %in% input$p2_glosa)
      if (!is.null(input$p2_liceo) && input$p2_liceo != "Todos")
        d <- d |> filter(RBD == as.numeric(input$p2_liceo))
      d
    })
    
    anio_max_practicas <- reactive({
      max(practicas_p2()$AGNO_ESCOLAR, na.rm = TRUE)
    })
    
    output$titulo_filtros_p2 <- renderUI({
      
      especialidad <- input$p2_glosa
      
      if (input$p2_liceo == "Todos") {
        titulo <- especialidad
      } else {
        # Tomar el nombre del primer registro filtrado
        nombre_liceo <- datos_p2() |> 
          distinct(nombre_rbd) |> 
          pull(nombre_rbd) |> 
          first()
        titulo <- paste(especialidad, nombre_liceo, sep = " - ")
      }
      
      tags$div(
        class = "px-1 py-2 mb-2",
        tags$h4(
          class = "fw-bold text-primary mb-1",
          titulo
        )
      )
    })
    
    # --- Value boxes ---
    output$vb_tasa_egreso <- renderText({
      d     <- datos_p2()
      total <- n_distinct(d$MRUN)
      egr   <- d |> filter(egresado == "Sí") |> pull(MRUN) |> n_distinct()
      if (total == 0) return("S/I")
      paste0(round(egr / total * 100, 1), "%")
    })
    
    output$vb_tasa_tit <- renderText({
      d   <- datos_p2() |>  filter(AGNO < 2024)
      egr <- d |> filter(egresado == "Sí") |> pull(MRUN) |> n_distinct()
      tit <- d |> filter(titulado == "Sí") |> pull(MRUN) |> n_distinct()
      if (egr == 0) return("S/I")
      paste0(round(tit / egr * 100, 1), "%")
    })
    
    output$vb_tiempo_tit <- renderText({
      d <- datos_p2() |>
        filter(titulado == "Sí", !is.na(AGNO_TITULACION)) |>
        mutate(tiempo = AGNO_TITULACION - AGNO)
      if (nrow(d) == 0) return("S/I")
      paste0(round(mean(d$tiempo, na.rm = TRUE), 2), " años")
    })
    
    # --- Matriculados vs Egresados por cohorte ---
    output$plot_p2_egreso <- renderPlot({
      datos_p2() |>
        group_by(AGNO) |>
        summarise(
          Matriculados = n_distinct(MRUN),
          Egresados    = n_distinct(MRUN[egresado == "Sí"]),
          .groups = "drop"
        ) |>
        pivot_longer(
          cols      = c(Matriculados, Egresados),
          names_to  = "categoria",
          values_to = "total"
        ) |>
        mutate(categoria = factor(categoria, levels = c("Matriculados", "Egresados"))) |>
        ggplot(aes(x = factor(AGNO), y = total, fill = categoria)) +
        geom_col(position = position_dodge(width = 0.5), width = 0.5) +
        geom_text(aes(label = total),
                  position = position_dodge(width = 0.5),
                  vjust = -0.4, size = 5) +
        scale_fill_manual(values = c("Matriculados" = "#3396c6", "Egresados" = "#f2a541")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = 11) + tema_nota_pie() +
        theme(
          panel.grid   = element_blank(),
          legend.position = "top"
        )
    })
    
    # --- Egresados vs Titulados por cohorte ---
    output$plot_p2_egreso <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p2_egreso"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      datos_p2() |>
        group_by(AGNO) |>
        summarise(
          Matriculados = n_distinct(MRUN),
          Egresados    = n_distinct(MRUN[egresado == "Sí"]),
          .groups = "drop"
        ) |>
        pivot_longer(
          cols      = c(Matriculados, Egresados),
          names_to  = "categoria",
          values_to = "total"
        ) |>
        mutate(categoria = factor(categoria, levels = c("Matriculados", "Egresados"))) |>
        ggplot(aes(x = factor(AGNO), y = total, fill = categoria)) +
        geom_col(position = position_dodge(width = 0.5), width = 0.5) +
        geom_text(aes(label = total),
                  position = position_dodge(width = 0.5),
                  vjust = -0.4, size = t$texto) +
        scale_fill_manual(values = c("Matriculados" = "#3396c6", "Egresados" = "#f2a541")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(
          panel.grid   = element_blank(),
          legend.position = "top"
        )
    }, res = 96)
    
    # --- Egresados vs Titulados por cohorte ---
    output$plot_p2_tit <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p2_tit"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      datos_p2() |>
        group_by(AGNO) |>
        summarise(
          Egresados = n_distinct(MRUN[egresado == "Sí"]),
          Titulados = n_distinct(MRUN[titulado == "Sí"]),
          .groups = "drop"
        ) |>
        pivot_longer(
          cols      = c(Egresados, Titulados),
          names_to  = "categoria",
          values_to = "total"
        ) |>
        mutate(categoria = factor(categoria, levels = c("Egresados", "Titulados"))) |>
        ggplot(aes(x = factor(AGNO), y = total, fill = categoria)) +
        geom_col(position = position_dodge(width = 0.5), width = 0.5) +
        geom_text(aes(label = total),
                  position = position_dodge(width = 0.5),
                  vjust = -0.4, size = t$texto) +
        scale_fill_manual(values = c("Egresados" = "#f2a541", "Titulados" = "#22498e")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(
          panel.grid   = element_blank(),
          legend.position = "top"
        )
    }, res = 96)
    
    # --- Egresados por sexo y cohorte (barras apiladas) ---
    output$plot_p2_egreso_sexo <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p2_egreso_sexo"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      datos_p2() |>
        group_by(AGNO, sexo) |>
        summarise(
          egr = n_distinct(MRUN[egresado == "Sí"]),
          .groups = "drop"
        ) |>
        ggplot(aes(x = factor(AGNO), y = egr, fill = sexo)) +
        geom_col(position = "stack", width = 0.8) +
        geom_text(aes(label = egr),
                  position = position_stack(vjust = 0.5),
                  size = t$texto, color = "white", fontface = "bold") +
        scale_fill_manual(values = c("Hombre" = "#12478D", "Mujer" = "#E83D5C")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        coord_flip() +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(panel.grid = element_blank(),
              legend.position = "top")
    }, res = 96)
    
    # --- Titulados por sexo y cohorte (barras apiladas horizontales) ---
    output$plot_p2_tit_sexo <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p2_tit_sexo"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      datos_p2() |>
        group_by(AGNO, sexo) |>
        summarise(
          tit = n_distinct(MRUN[titulado == "Sí"]),
          .groups = "drop"
        ) |>
        ggplot(aes(x = factor(AGNO), y = tit, fill = sexo)) +
        geom_col(position = "stack", width = 0.8) +
        geom_text(aes(label = tit),
                  position = position_stack(vjust = 0.5),
                  size = t$texto, color = "white", fontface = "bold") +
        scale_fill_manual(values = c("Hombre" = "#12478D", "Mujer" = "#E83D5C")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        coord_flip() +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(panel.grid = element_blank(),
              legend.position = "top")
    }, res = 96)
      
      # --- Top 10 centros históricos ---
      output$tbl_practicas_total <- renderDT({
        d <- practicas_p2()
        
        if (nrow(d) == 0) return(datatable(data.frame(Mensaje = "Sin datos"), rownames = FALSE))
        
        df <- d |>
          group_by(NOMBRE_EMPRESA, GLOSA_RUBRO) |>
          summarise(
            `N° Practicantes (total)` = n_distinct(MRUN),
            .groups = "drop"
          ) |>
          arrange(desc(`N° Practicantes (total)`)) |>
          slice_head(n = 10) |>
          rename(
            `Centro de Práctica` = NOMBRE_EMPRESA,
            `Rubro`              = GLOSA_RUBRO
          )
        
        datatable(
          df,
          options  = list(
            language   = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
            pageLength = 10,
            dom        = "t"
          ),
          rownames = FALSE,
          class    = "table table-striped table-hover table-sm"
        )
      })
      
      # --- Título dinámico último año ---
      output$tbl_practicas_anio_titulo <- renderUI({
        tags$p(
          class = "fw-semibold mb-2 text-uppercase small text-muted",
          bs_icon("calendar-check"), "5 centros de práctica más concurridos, año escolar ", anio_max_practicas(), "(último año con registro)"
        )
      })
      
      # --- Top 5 centros último año ---
      output$tbl_practicas_anio <- renderDT({
        d <- practicas_p2() |>
          filter(AGNO_ESCOLAR == anio_max_practicas())
        
        if (nrow(d) == 0) return(datatable(data.frame(Mensaje = "Sin datos"), rownames = FALSE))
        
        df <- d |>
          group_by(NOMBRE_EMPRESA, GLOSA_RUBRO) |>
          summarise(
            `N° Practicantes` = n_distinct(MRUN),
            .groups = "drop"
          ) |>
          arrange(desc(`N° Practicantes`)) |>
          slice_head(n = 5) |>
          rename(
            `Centro de Práctica` = NOMBRE_EMPRESA,
            `Rubro`              = GLOSA_RUBRO
          )
        
        datatable(
          df,
          options  = list(
            language   = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
            pageLength = 5,
            dom        = "t"
          ),
          rownames = FALSE,
          class    = "table table-striped table-hover table-sm"
        )
      })
    })
}