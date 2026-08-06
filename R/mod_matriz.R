mod_matriz_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Matriz preliminar",
    icon  = bs_icon("intersect"),
    
    div(
      class = "px-3 pt-2",
      radioGroupButtons(ns("area"), NULL,
                        label    = "Selecciona el área:",
                        choices  = c("Enfermería", "Administración"),
                        selected = "Enfermería",
                        status   = "default",
                        size     = "sm"
      )
    ),
    mobile_filter_hint(), 
    layout_columns(
      col_widths = breakpoints(
        xs = c(12, 12, 12),   # mobile: todo apilado en una columna
        md = c(3, 3, 6),      # tablet: proporciones intermedias
        lg = c(2, 2, 8)       # desktop: tu diseño original
      ),
      height = "calc(100vh - 80px)",
      
      # ── Columna 1: Asignaturas TNS ──────────────────────────────────────
      card(
        full_screen = FALSE,
        card_header(
          class = "bg-success text-white fw-semibold",
          uiOutput(ns("tns_card_titulo"))
        ),
        card_body(
          class = "p-2 overflow-auto",
          tags$p(
            class = "fw-semibold mb-2 text-uppercase small text-muted",
            "Asignaturas CFT seleccionadas"
          ),
          uiOutput(ns("tns_asignaturas_btns"))
        )
      ),
      
      # ── Columna 2: Módulos EMTP congruentes ─────────────────────────────
      card(
        full_screen = FALSE,
        card_header(
          class = "bg-primary text-white fw-semibold",
          uiOutput(ns("emtp_card_titulo"))
        ),
        card_body(
          class = "p-2 overflow-auto",
          tags$p(
            class = "fw-semibold mb-2 text-uppercase small text-muted",
            "Módulos EMTP congruentes"
          ),
          uiOutput(ns("emtp_modulos_btns"))
        )
      ),
      
      # ── Columna 3: dividida en dos mitades ──────────────────────────────
      tagList(
        card(
          full_screen = FALSE,
          card_header(
            class = "bg-success text-white fw-semibold",
            uiOutput(ns("tns_detalle_titulo"))
          ),
          card_body(
            class = "p-2",
            uiOutput(ns("tns_meta")),
            DTOutput(ns("tbl_tns_obj"), fill = FALSE)
          )
        ),
        div(style = "height: 1rem;"),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white fw-semibold",
            uiOutput(ns("emtp_detalle_titulo"))
          ),
          card_body(
            class = "p-2",
            uiOutput(ns("emtp_meta")),
            DTOutput(ns("tbl_emtp_apr"), fill = FALSE)
          )
        )
      )
    )
  )
}

mod_matriz_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ── Títulos dinámicos ────────────────────────────────────────────────
    
    output$tns_card_titulo <- renderUI({
      titulo <- if (input$area == "Enfermería") "TNS en Enfermería" else "TNS en Administración de Empresas"
      tagList(bs_icon("book-fill"), tags$span(titulo, style = "font-size:1rem;"))
    })
    
    output$emtp_card_titulo <- renderUI({
      titulo <- if (input$area == "Enfermería") "Atención de Enfermería" else "Administración"
      tagList(bs_icon("mortarboard-fill"), tags$span(titulo, style = "font-size:1rem;"))
    })
    
    output$emtp_detalle_titulo <- renderUI({
      req(input$emtp_mod)
      d <- modulos_emtp |> filter(cod_m == input$emtp_mod)
      req(nrow(d) > 0)
      tagList(bs_icon("mortarboard-fill"), tags$span(d$modulo[1], style = "font-size:1rem;"))
    })
    
    output$tns_detalle_titulo <- renderUI({
      req(input$tns_mod)
      d <- modulos_tns |> filter(cod_m == input$tns_mod)
      req(nrow(d) > 0)
      tagList(bs_icon("mortarboard-fill"), tags$span(d$glosa_modulo[1], style = "font-size:1rem;"))
    })
    # ── Asignaturas TNS con congruencia ──────────────────────────────────
    
    tns_con_congruencia <- reactive({
      codigos_con_cong <- congruencias |>
        filter(area == input$area) |>
        pull(cod_m) |>
        unique()
      
      modulos_tns |>
        filter(area == input$area, cod_m %in% codigos_con_cong) |>
        arrange(semestre, glosa_modulo)
    })
    
    output$tns_asignaturas_btns <- renderUI({
      df <- tns_con_congruencia()
      req(nrow(df) > 0)
      
      radioGroupButtons(
        inputId   = session$ns("tns_mod"),
        label     = NULL,
        choices   = setNames(df$cod_m, df$glosa_modulo),
        selected  = df$cod_m[1],
        direction = "vertical",
        width     = "100%",
        status    = "success",
        size      = "sm"
      )
    })
  
    
    # ── Meta + objetivos TNS ─────────────────────────────────────────────
    
    output$tns_meta <- renderUI({
      req(input$tns_mod)
      d <- modulos_tns |> filter(area == input$area, cod_m == input$tns_mod)
      req(nrow(d) > 0)
      
      n_cong <- congruencias |>
        filter(area == input$area, cod_m == input$tns_mod) |>
        nrow()
      
      tags$div(
        class = "d-flex gap-2 flex-wrap mb-2",
        tags$span(class = "badge bg-primary",   bs_icon("calendar"),   "Sem.", d$semestre[1]),
        tags$span(class = "badge bg-info",      bs_icon("tag"),         d$tipo[1]),
        tags$span(class = "badge bg-secondary", bs_icon("clock"),       d$horas_lectivas[1], " hrs lectivas"),
        tags$span(class = "badge bg-secondary", bs_icon("clock"),       d$horas_practicas[1], " hrs prácticas")
      )
    })
    
    # ── Tabla objetivos TNS ──────────────────────────────────────────────
    
    output$tbl_tns_obj <- renderDT({
      req(input$tns_mod)
      df <- obj_modulos_tns |>
        filter(area == input$area, cod_m == input$tns_mod) |>
        select(`Resultados de aprendizaje` = glosa_objetivo_modulos)
      datatable(df, options = dt_opts, rownames = FALSE,
                class = "table table-striped table-hover table-sm")
    })
    
    # ── Módulos EMTP congruentes ─────────────────────────────────────────
    
    emtp_congruentes <- reactive({
      req(input$tns_mod)
      
      codigos <- congruencias |>
        filter(area == input$area, cod_m == input$tns_mod) |>
        pull(cod_m_esp)
      
      modulos_emtp |>
        filter(area == input$area, cod_m %in% codigos) |>
        arrange(nivel, modulo)
    })
    
    output$emtp_modulos_btns <- renderUI({
      df <- emtp_congruentes()
      
      if (nrow(df) == 0) {
        tags$p(
          class = "text-muted small fst-italic mt-2",
          bs_icon("info-circle"), " Sin módulos EMTP congruentes."
        )
      } else {
        etiquetas <- paste0(df$modulo, " (", df$nivel, ")")
        
        radioGroupButtons(
          inputId   = session$ns("emtp_mod"),
          label     = NULL,
          choices   = setNames(df$cod_m, etiquetas),
          selected  = df$cod_m[1],
          direction = "vertical",
          width     = "100%",
          status    = "primary",
          size      = "sm"
        )
      }
    })
    
    observeEvent(input$tns_mod, {
      df <- emtp_congruentes()
      if (nrow(df) > 0) {
        etiquetas <- paste0(df$modulo, " (", df$nivel, ")")
        updateRadioGroupButtons(session, "emtp_mod",
                                choices  = setNames(df$cod_m, etiquetas),
                                selected = df$cod_m[1]
        )
      }
    })
    
    # ── Meta del módulo EMTP ─────────────────────────────────────────────
    
    output$emtp_meta <- renderUI({
      req(input$emtp_mod)
      d <- modulos_emtp |> filter(area == input$area, cod_m == input$emtp_mod)
      req(nrow(d) > 0)
      tags$div(
        class = "d-flex gap-2 flex-wrap mb-2",
        tags$span(class = "badge bg-primary",   bs_icon("layers"), d$nivel[1]),
        tags$span(class = "badge bg-info",      bs_icon("tag"),    d$mencion[1]),
        tags$span(class = "badge bg-secondary", bs_icon("clock"),  d$horas[1], " hrs")
      )
    })
    
    # ── Tablas EMTP ──────────────────────────────────────────────────────
    
   output$tbl_emtp_apr <- renderDT({
      req(input$emtp_mod)
      df <- apr_esp_emtp |>
        filter(area == input$area, cod_m == input$emtp_mod) |>
        select(`Aprendizajes Esperados` = aprendizaje_esperado)
      datatable(df, options = dt_opts, rownames = FALSE,
                class = "table table-striped table-hover table-sm")
    })
    
  })
}