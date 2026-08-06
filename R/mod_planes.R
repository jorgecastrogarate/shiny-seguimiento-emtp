mod_planes_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Planes EMTP-CFT",
    icon  = bs_icon("arrow-left-right"),
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
        xs = c(12, 12),   # mobile: EMTP y TNS apiladas
        md = c(6, 6)      # tablet y desktop: lado a lado, como tenías
      ),
      height = "calc(100vh - 80px)",

      # ── Columna EMTP ────────────────────────────────────────────────────
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary text-white fw-semibold",
          uiOutput(ns("emtp_card_titulo"))
        ),
        layout_sidebar(
          sidebar = sidebar(
            id    = ns("sidebar_emtp_planes"),
            width = 200,
            uiOutput(ns("emtp_nivel_ui")),
            uiOutput(ns("emtp_modulo_ui")),
            hr(),
            uiOutput(ns("emtp_meta"))
          ),
          navset_card_underline(
            nav_panel(
              title = tagList(bs_icon("check2-square"), " Objetivos de Aprendizaje"),
              DTOutput(ns("cmp_emtp_obj"), fill = TRUE)
            ),
            nav_panel(
              title = tagList(bs_icon("journal-check"), " Aprendizajes Esperados"),
              DTOutput(ns("cmp_emtp_apr"), fill = TRUE)
            )
          )
        )
      ),
      # ── Columna TNS ─────────────────────────────────────────────────────
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-success text-white fw-semibold",
          uiOutput(ns("tns_card_titulo"))
        ),
        layout_sidebar(
          sidebar = sidebar(
            id       = ns("sidebar_tns_planes"),
            width    = 200,
            position = "right",
            uiOutput(ns("tns_semestre_ui")),
            uiOutput(ns("tns_tipo_ui")),
            uiOutput(ns("tns_modulo_ui")),
            hr(),
            uiOutput(ns("tns_meta"))
          ),
          DTOutput(ns("cmp_tns_obj"), fill = TRUE)
        )
      )
    )
  )
}

mod_planes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ── Títulos dinámicos ──────────────────────────────────────────────────
    
    output$emtp_card_titulo <- renderUI({
      titulo <- if (input$area == "Enfermería") {
        "Especialidad Atención de Enfermería"
      } else {
        "Especialidad Administración"
      }
      tagList(bs_icon("mortarboard-fill"), tags$span(titulo, style = "font-size:1.25rem;"))
    })
    
    output$tns_card_titulo <- renderUI({
      titulo <- if (input$area == "Enfermería") {
        "Técnico de Nivel Superior en Enfermería"
      } else {
        "Técnico de Nivel Superior en Administración de Empresas"
      }
      tagList(bs_icon("book-fill"), tags$span(titulo, style = "font-size:1.25rem;"))
    })
    
    # ── Filtros dinámicos EMTP ─────────────────────────────────────────────
    
    output$emtp_nivel_ui <- renderUI({
      niveles <- modulos_emtp |> filter(area == input$area) |> pull(nivel) |> unique() |> sort()
      selectInput(session$ns("emtp_nivel"), "Nivel",    # ✅ session$ns()
                  choices  = c("Todos", niveles),
                  selected = "Todos"
      )
    })
    
    emtp_filtrados <- reactive({
      req(input$emtp_nivel)
      df <- modulos_emtp |> filter(area == input$area)
      if (input$emtp_nivel != "Todos")
        df <- df |> filter(nivel == input$emtp_nivel)
      df
    })
    
    output$emtp_modulo_ui <- renderUI({
      df <- emtp_filtrados()
      selectInput(session$ns("emtp_mod"), "Módulo",     # ✅ session$ns()
                  choices  = setNames(df$cod_m, df$modulo),
                  selected = df$cod_m[1]
      )
    })
    
    output$emtp_meta <- renderUI({
      req(input$emtp_mod)
      d <- modulos_emtp |> filter(area == input$area, cod_m == input$emtp_mod)
      req(nrow(d) > 0)
      tags$div(
        class = "mt-2 p-2 rounded border bg-light",
        tags$p(class = "fw-semibold mb-1 small text-uppercase text-muted", "Módulo seleccionado"),
        tags$p(class = "mb-1 small", tags$strong(d$modulo[1])),
        tags$div(
          class = "d-flex gap-2 flex-wrap",
          tags$span(class = "badge bg-primary",   bs_icon("layers"),  d$nivel[1]),
          tags$span(class = "badge bg-info",      bs_icon("tag"),     d$mencion[1]),
          tags$span(class = "badge bg-secondary", bs_icon("clock"),   d$horas[1], " hrs")
        )
      )
    })
    
    output$cmp_emtp_obj <- renderDT({
      req(input$emtp_mod)
      df <- emtp_full |>
        filter(area == input$area, cod_m == input$emtp_mod) |>
        select(`Objetivo de Aprendizaje` = objetivo_aprendizaje)
      datatable(df, options = dt_opts, rownames = FALSE,
                class = "table table-striped table-hover table-sm")
    })
    
    output$cmp_emtp_apr <- renderDT({
      req(input$emtp_mod)
      df <- apr_esp_emtp |>
        filter(area == input$area, cod_m == input$emtp_mod) |>
        select(`Aprendizaje Esperado` = aprendizaje_esperado)
      datatable(df, options = dt_opts, rownames = FALSE,
                class = "table table-striped table-hover table-sm")
    })
    
    # ── Filtros dinámicos TNS ──────────────────────────────────────────────
    
    output$tns_semestre_ui <- renderUI({
      semestres <- modulos_tns |> filter(area == input$area) |> pull(semestre) |> unique() |> sort()
      selectInput(session$ns("tns_semestre"), "Semestre",  # ✅ session$ns()
                  choices  = c("Todos", semestres),
                  selected = "Todos"
      )
    })
    
    output$tns_tipo_ui <- renderUI({
      tipos <- modulos_tns |> filter(area == input$area) |> pull(tipo) |> unique() |> sort()
      selectInput(session$ns("tns_tipo"), "Tipo de módulo",  # ✅ session$ns()
                  choices  = c("Todos", tipos),
                  selected = "Todos"
      )
    })
    
    tns_filtrados <- reactive({
      req(input$tns_semestre, input$tns_tipo)
      df <- modulos_tns |> filter(area == input$area)
      if (input$tns_semestre != "Todos")
        df <- df |> filter(semestre == input$tns_semestre)
      if (input$tns_tipo != "Todos")
        df <- df |> filter(tipo == input$tns_tipo)
      df
    })
    
    output$tns_modulo_ui <- renderUI({
      df <- tns_filtrados()
      selectInput(session$ns("tns_mod"), "Módulo",     # ✅ session$ns()
                  choices  = setNames(df$cod_m, df$glosa_modulo),
                  selected = df$cod_m[1]
      )
    })
    
    output$tns_meta <- renderUI({
      req(input$tns_mod)
      d <- modulos_tns |> filter(area == input$area, cod_m == input$tns_mod)
      req(nrow(d) > 0)
      badge_convalida <- if (d$convalida[1] == "Sí") "badge bg-success" else "badge bg-danger text-white"
      tags$div(
        class = "mt-2 p-2 rounded border bg-light",
        tags$p(class = "fw-semibold mb-1 small text-uppercase text-muted", "Módulo seleccionado"),
        tags$p(class = "mb-1 small", tags$strong(d$glosa_modulo[1])),
        tags$div(
          class = "d-flex gap-2 flex-wrap",
          tags$span(class = "badge bg-primary",   bs_icon("calendar"),     "Sem.", d$semestre[1]),
          tags$span(class = "badge bg-info",      bs_icon("tag"),           d$tipo[1]),
          tags$span(class = "badge bg-secondary", bs_icon("clock"),         d$horas_lectivas[1],  " hrs lectivas"),
          tags$span(class = "badge bg-secondary", bs_icon("clock"),         d$horas_practicas[1], " hrs prácticas"),
          tags$span(class = badge_convalida,      bs_icon("arrow-repeat"),  d$convalida[1],       " reconoce asignaturas EMTP")
        )
      )
    })
    
    output$cmp_tns_obj <- renderDT({
      req(input$tns_mod)
      df <- obj_modulos_tns |>
        filter(area == input$area, cod_m == input$tns_mod) |>
        select(`Resultados de Aprendizaje` = glosa_objetivo_modulos)
      datatable(df, options = dt_opts, rownames = FALSE,
                class = "table table-striped table-hover table-sm")
    })
  })
}