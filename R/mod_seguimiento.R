# UI modulo seguimiento
mod_seguimiento_ui <- function(id) {
  ns <- NS(id)


nav_panel(
  title = "Continuidad de Estudios",
  icon  = bs_icon("mortarboard"),
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      id    = ns("sidebar_seguimiento"),
      width = 250,
      open  = list(desktop = "open", mobile = "closed"),
      selectInput(ns("p3_glosa"), "Especialidad",
        choices  = opciones_glosa,
        selected = opciones_glosa[1]
      ),
      selectInput(ns("p3_liceo"), "Establecimiento",
        choices  = c("Todos" = "Todos", opciones_liceos_p3),
        selected = "Todos"
      )
    ),
    uiOutput(ns("titulo_filtros_p3")),
    mobile_filter_hint(),
    # Fila 1: value boxes nivel carrera
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "",
        value    = uiOutput(ns("vb_sup")),
        showcase = bs_icon("mortarboard"),
        theme    = "primary"
      ),
      value_box(
        title    = "",
        value    = uiOutput(ns("vb_tec")),
        showcase = bs_icon("wrench-adjustable"),
        theme    = "info"
      ),
      value_box(
        title    = "",
        value    = uiOutput(ns("vb_prof")),
        showcase = bs_icon("briefcase-fill"),
        theme    = "success"
      )
    ),
    card(
      card_body(
        p(
          "Nota: Los datos usados son obtenidos mediante registro administrativo y publicado en los datos abiertos del MINEDUC, por lo
          que podrían diferir de las estadísticas internas de cada establecimiento. En esta sección, se realizó un seguimiento por los cohortes
          construidos en la sección anterior y se combinó con los datos de ",
          tags$a(
            href = "https://datosabiertos.mineduc.cl/matricula-en-educacion-superior/",
            target = "_blank",
            "matrículas de educación superior"
          ), " a través de la variable común MRUN. Para más información, revisar las fuentes señaladas.",
          p("Los datos de esta sección sólo corresponden a cohortes de egresados/as de establecimientos del proyecto CRT-2595"),
        )
      )
    ),
    
    # Fila 2: IES por cohorte + área
      card(
        full_screen = TRUE,
        card_header("Distribución de egresados/as de EMTP en educación superior según afinidad del área de conocimiento, periodo 2020-2024", class = "card-header-custom"),
        plotOutput(ns("plot_p3_area"), height = "260px")
      ),
      card(
        full_screen = TRUE,
        card_header("Evolucion de la institución de educación superior de egresados/as de EMTP por cohorte, periodo 2020-2024", class = "card-header-custom"),
        plotOutput(ns("plot_p3_ies"), height = "260px")
      ),

      card(
        full_screen = TRUE,
        card_header("Forma de ingreso a IES, periodo 2020-2024", class = "card-header-custom"),
        plotOutput(ns("plot_p3_ingreso"), height = "260px")
      ),
      card(
        full_screen = TRUE,
        card_header("Matrículas de egresados/as de EMTP en carreras del CFT Estatal de Tarapacá, periodo 2020-2024", class = "card-header-custom"),
        tableOutput(ns("tabla_p3_cft"))
      )
     )
    )
   }


# Server modulo seguimiento
mod_seguimiento_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
observe({
  d <- seguimiento_sup
  if (!is.null(input$p3_glosa) && length(input$p3_glosa) > 0)
    d <- d |> filter(glosa_simple %in% input$p3_glosa)
  liceos_disp <- sort(unique(d$nombre_rbd))
  updateSelectInput(session, "p3_liceo",
                    choices  = c("Todos" = "Todos", setNames(liceos_disp, liceos_disp)),
                    selected = "Todos")
})

datos_p3 <- reactive({
  d <- seguimiento_sup
  if (!is.null(input$p3_glosa) && length(input$p3_glosa) > 0)
    d <- d |> filter(glosa_simple %in% input$p3_glosa)
  if (!is.null(input$p3_liceo) && input$p3_liceo != "Todos")
    d <- d |> filter(nombre_rbd == input$p3_liceo)
  d
})

# --- Value boxes nivel carrera ---
output$vb_sup <- renderUI({
  d   <- datos_p3() |> filter(AGNO == 2023)
  n   <- sum(!is.na(d$NIVEL_CARRERA_2))
  pct <- round(n / nrow(d) * 100, 1)
  HTML(paste0(
    "<span style='font-size:26px;'>", pct, "%</span>",
    "<span style='font-size:16px;'> continuó estudios superiores (cohorte 2023)</span>"
  ))
})

output$vb_tec <- renderUI({
  d <- datos_p3() |> filter(!is.na(NIVEL_CARRERA_2) & AGNO == 2023)
  n   <- sum(d$NIVEL_CARRERA_2 == "Carreras Técnicas")
  pct <- round(n / nrow(d) * 100, 1)
  HTML(paste0(
    "<span style='font-size:26px;'>", pct, "%</span>",
    "<span style='font-size:16px;'> de quienes continuaron se matriculó en una carrera técnica(cohorte 2023)</span>"
  ))
})

output$vb_prof <- renderUI({
  d <- datos_p3() |> filter(!is.na(NIVEL_CARRERA_2) & AGNO == 2023)
  n   <- sum(d$NIVEL_CARRERA_2 == "Carreras Profesionales")
  pct <- round(n / nrow(d) * 100, 1)
  HTML(paste0(
    "<span style='font-size:26px;'>", pct, "%</span>",
    "<span style='font-size:16px;'>  de quienes continuaron se matriculó en una carrera profesional (cohorte 2023)</span>"
  ))
})

    output$titulo_filtros_p3 <- renderUI({
      
      especialidad <- input$p3_glosa
      
      if (input$p3_liceo == "Todos") {
        titulo <- especialidad
      } else {
        # Tomar el nombre del primer registro filtrado
        nombre_liceo <- datos_p3() |> 
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
    

# --- IES por cohorte (barras apiladas) ---
    output$plot_p3_ies <- renderPlot({
      ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p3_ies"), "_width")]]
      t <- tamanos_responsive(ancho_px)
      
      datos_p3() |>
        filter(!is.na(nomb_inst_agrupado)) |> 
        group_by(AGNO, nomb_inst_agrupado) |>
        summarise(n = n(), .groups = "drop") |> 
        ggplot(aes(x = factor(AGNO), y = n, fill = nomb_inst_agrupado, alpha = nomb_inst_agrupado == "CFT Estatal de Tarapacá" )) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("#149AD8", "#FF4C4C", "#196057", "#046AB2", "#DEB01F")) +
        scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.85), guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = t$base) + 
        tema_nota_pie(size = t$pie) +
        theme(panel.grid = element_blank(),
              legend.position = "top",
              legend.text = element_text(size = t$texto * 1.3))
    }, res = 96)

# --- Área de estudio (barras apiladas 100% horizontal) ---
output$plot_p3_area <- renderPlot({
  ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p3_area"), "_width")]]
  t <- tamanos_responsive(ancho_px)
  
  datos_p3() |>
    filter(!is.na(CAT_PERIODO)) |> 
    group_by(area_agrupada) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(
      pct   = n / sum(n),
      label = paste0(area_agrupada, "\n", scales::percent(pct, accuracy = 0.1)),
      area_agrupada = factor(
        area_agrupada,
        levels = c("Otras áreas", "Administración y Comercio", "Salud")
      )
    ) |>
    ggplot(aes(x = 1, y = pct, fill = area_agrupada)) +
    geom_col(position = "fill", width = 0.8, color = "white") +
    geom_text(aes(label = label),
              position = position_fill(vjust = 0.5),
              size = t$texto - 1, color = "white", fontface = "bold") +
    scale_fill_manual(
      values = c(
        "Administración y Comercio" = "#12478D",
        "Salud"          = "#12478D",
        "Otras áreas"    = "#8A8A8A"
      )
    ) +
    scale_x_continuous(limits = c(0, 2)) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(fill = NULL) +
    theme_void(base_size = t$base) + 
    tema_nota_pie(size = t$pie) +
    theme(legend.position = "none")
}, res = 96)

output$plot_p3_ingreso <- renderPlot({
  ancho_px <- session$clientData[[paste0("output_", session$ns("plot_p3_ingreso"), "_width")]]
  t <- tamanos_responsive(ancho_px)
  
  datos_p3() |>
    filter(!is.na(CAT_PERIODO)) |> 
    group_by(forma_ingreso_ok) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(pct = round(n / sum(n) * 100, 1),
           forma_ingreso_ok = reorder(forma_ingreso_ok, n)) |>
    ggplot(aes(x = n, y = forma_ingreso_ok)) +
    geom_col(fill = "#3396c6") +
    geom_text(aes(label = paste0(n, " (", pct, "%)")),
              hjust = -0.1, size = t$texto) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = t$base) + 
    tema_nota_pie(size = t$pie) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = t$base)
    )
}, res = 96)

# --- Tabla CFT Estatal (sin cambios, no es plot) ---
output$tabla_p3_cft <- renderTable({
  datos_p3() |>
    filter(nomb_inst_simple == "CFT Estatal de Tarapacá") |>
    group_by(nomb_carrera) |>
    summarise(Total = n(), .groups = "drop") |>
    arrange(desc(Total)) |>
    rename("Carrera" = nomb_carrera)
}, striped = TRUE, hover = TRUE, bordered = TRUE)

  })
}
