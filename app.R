# Carga módulos
source("global.R")
source("R/mod_resumen.R")
source("R/mod_liceos.R")
source("R/mod_seguimiento.R")
source("R/mod_sabe.R")
source("R/mod_planes.R")
# source("R/mod_matriz.R") ------------- Módulo omitido para repositorio público

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span("Proyecto CRT 2595", style = "font-size: 24px; font-weight: bold;"),
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Ubuntu"),
    version = 5
  ),
  
  # ── CSS responsivo ────────────────────────────────────────────────────────
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, shrink-to-fit=no"),

    tags$style(HTML("
      html, body {
        min-height: 100%;
        font-size: 14px;
      }
      
      /* ============ NAVBAR ============ */
      .navbar {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        width: 100% !important;
        z-index: 1030 !important;
      }
      body {
        padding-top: 56px !important;
      }
      
      /* ============ SIDEBAR PRINCIPAL - Desktop (>= 768px) ============ */
      @media (min-width: 768px) {
        .bslib-sidebar-layout > .sidebar {
          position: fixed !important;
          top: 56px !important;
          left: 0 !important;
          bottom: 0 !important;
          width: 250px !important;
          height: auto !important;
          z-index: 1020 !important;
          overflow-y: auto !important;
        }
        .bslib-sidebar-layout > .main {
          margin-left: 250px !important;
          padding: 1rem !important;
        }
      }
      
      /* ============ SIDEBAR PRINCIPAL - Mobile (< 768px) ============ */
      @media (max-width: 767px) {
        .bslib-sidebar-layout > .main {
          margin-left: 0 !important;
          padding: 0.5rem !important;
        }
      }
      
      .bslib-page-fill {
        height: calc(100vh - 56px) !important;
      }
      
      /* ============ SIDEBARS INTERNOS (dentro de cards) - reset ============ */
      .card .bslib-sidebar-layout > .sidebar {
        position: relative !important;
        top: auto !important;
        left: auto !important;
        bottom: auto !important;
        width: 200px !important;
        z-index: auto !important;
      }
      .card .bslib-sidebar-layout > .main {
        margin-left: 0 !important;
        padding: 0 !important;
      }
      @media (max-width: 767px) {
        .card .bslib-sidebar-layout {
          flex-direction: column !important;
        }
        .card .bslib-sidebar-layout > .sidebar {
          width: 100% !important;
        }
      }
      
      /* ============ TABLAS (DT) ============ */
      .dataTables_wrapper {
        font-size: 0.85rem;
      }
      .dataTables_scrollBody {
        overflow-x: auto !important;
      }
      
      /* ============ CARD HEADERS ============ */
      .card-header-custom {
        font-size: 1.25rem;
        font-weight: 600;
        background-color: white;
        color: black;
        padding: 0.75rem 1rem;
      }
      @media (max-width: 767px) {
        .card-header-custom {
          font-size: 1.05rem;
          padding: 0.5rem 0.75rem;
        }
      }
      
      /* ============ AVISO DE FILTROS - solo mobile ============ */
      .mobile-filter-hint {
        display: none;
      }
      @media (max-width: 767px) {
        .mobile-filter-hint {
          display: block;
          background-color: #e7f3ff;
          border-left: 4px solid #0d6efd;
          padding: 0.5rem 0.75rem;
          margin-bottom: 1rem;
          font-size: 0.85rem;
          border-radius: 4px;
        }
      }
      /* Agregar al CSS global */
      @media (max-width: 767px) {
        .bslib-grid[style*='calc(100vh'] {
          height: auto !important;
        }
      }
    ")),
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      
      function isMobile() {
        return window.innerWidth < 768;
      }
      
      function collapseAllSidebars() {
        document.querySelectorAll('.bslib-sidebar-layout').forEach(function(layout) {
          if (!layout.classList.contains('sidebar-collapsed')) {
            var toggle = layout.querySelector('.collapse-toggle');
            if (toggle) toggle.click();
          }
        });
      }
      
      if (isMobile()) {
        setTimeout(collapseAllSidebars, 300);
      }
      
      var navbarCollapseEl = document.querySelector('.navbar-collapse');
      if (navbarCollapseEl) {
        var bsCollapse = new bootstrap.Collapse(navbarCollapseEl, { toggle: false });
        document.addEventListener('click', function(e) {
          if (e.target.classList.contains('nav-link')) {
            var toggler = document.querySelector('.navbar-toggler');
            if (toggler && window.getComputedStyle(toggler).display !== 'none') {
              bsCollapse.hide();
              if (isMobile()) {
                setTimeout(collapseAllSidebars, 300);
              }
            }
          }
        });
      }
    });
  "))
  ),
  fillable = c("Comparar Planes"),
  nav_spacer(),
  mod_resumen_ui("resumen"),
  mod_liceos_ui("liceos"),
  mod_seguimiento_ui("seguimiento"),
  mod_planes_ui("planes"),
#  mod_matriz_ui("matriz"), ------------ Módulo omitido para repositorio público   
  mod_sabe_ui("sabe"),
  nav_spacer(),
  nav_item(tags$img(src = "logo.png", height = "50px"))
)



server <- function(input, output, session) {
  mod_resumen_server("resumen")
  mod_liceos_server("liceos")
  mod_seguimiento_server("seguimiento")
  mod_planes_server("planes")
#  mod_matriz_server("matriz") ---------- Módulo omitido para repositorio público
}

shinyApp(ui, server)
