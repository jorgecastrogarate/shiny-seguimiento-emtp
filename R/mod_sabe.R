mod_sabe_ui <- function(id) {
  ns <- NS(id)
  
nav_panel(
  title = "Plataforma SABE-SENCE",
  icon  = bs_icon("bar-chart-line-fill"),
  
  tags$iframe(
    src    = "https://app.powerbi.com/view?r=eyJrIjoiMTI1MDRmMTMtNGJjYi00MzcwLWJiM2UtOTFmZTNmOTRlM2I1IiwidCI6IjJlNDQ0ODNkLTA3ZTEtNGMyYS1iM2I5LTViOTlkMDE5ZjgwZCIsImMiOjR9",
    width  = "100%",
    height = "800px",
    frameborder = "0",
    allowfullscreen = NA,
    style  = "border:none;"
  )
)
}