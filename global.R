library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(data.table)
library(DT)
library(scales)
library(readxl)
library(shinyWidgets)

options(OutDec = ",")

# ------------------------------------------------------------------------------
# CARGA DE DATOS – CRT (CSV)
# ------------------------------------------------------------------------------

datos_mat <- fread("datos_mat.csv")
datos_tit <- fread("datos_tit.csv")
liceos_convenio <- fread("liceos_tp.csv")[, convenio_cft := fifelse(RBD %in% c(12602, 12534, 40422, 161), "si", "no")
][order(nombre_rbd)]
liceos_con_conv    <- liceos_convenio[convenio_cft == "si"]
seguimiento_emtp <- fread("seguimiento_emtp.csv")
seguimiento_sup <- fread("seguimiento_sup.csv")
practicas <- fread("practicas_tp.csv")

# Opciones para los filtros
opciones_glosa   <- sort(unique(datos_mat$glosa_simple))
opciones_comunas <- c("Todas", sort(unique(datos_mat$comuna)))
opciones_liceos <- datos_mat %>%
  distinct(RBD, nombre_rbd) %>%
  arrange(nombre_rbd) %>%
  { setNames(as.character(.$RBD), .$nombre_rbd) }
opciones_liceos_p2 <- seguimiento_emtp %>%
  distinct(RBD, nombre_rbd) %>%
  arrange(nombre_rbd) %>%
  { setNames(as.character(.$RBD), .$nombre_rbd) }
opciones_liceos_p3 <- seguimiento_sup %>%
  distinct(nombre_rbd) %>%
  arrange(nombre_rbd) %>%
  pull(nombre_rbd) %>%
  setNames(., .)
anio_max_mat     <- max(datos_mat$AGNO)
anio_max_tit     <- max(datos_tit$AGNO_TITULACION)

# ------------------------------------------------------------------------------
# CARGA DE DATOS – COMPARAR PLANES (Excel)
# ------------------------------------------------------------------------------

#Enfermería
modulos_emtp_enf              <- read_xlsx("enfermeria.xlsx",     sheet = 2) |> mutate(area = "Enfermería")
obj_apr_emtp_enf              <- read_xlsx("enfermeria.xlsx",     sheet = 3) |> mutate(area = "Enfermería")
apr_esp_emtp_enf              <- read_xlsx("enfermeria.xlsx",     sheet = "aprendizajes") |> mutate(area = "Enfermería")
relacion_modulos_obje_mtp_enf <- read_xlsx("enfermeria.xlsx",     sheet = 5) |> mutate(area = "Enfermería")
modulos_tns_enf               <- read_xlsx("tns enfermeria.xlsx", sheet = 1) |> mutate(area = "Enfermería")
obj_modulos_tns_enf           <- read_xlsx("tns enfermeria.xlsx", sheet = 4) |> mutate(area = "Enfermería")

#Administración
modulos_emtp_adm              <- read_xlsx("administracion.xlsx",     sheet = 2) |> mutate(area = "Administración")
obj_apr_emtp_adm              <- read_xlsx("administracion.xlsx",     sheet = 3) |> mutate(area = "Administración")
apr_esp_emtp_adm              <- read_xlsx("administracion.xlsx",     sheet = "aprendizajes") |> mutate(area = "Administración")
relacion_modulos_obje_mtp_adm <- read_xlsx("administracion.xlsx",     sheet = 5) |> mutate(area = "Administración")
modulos_tns_adm               <- read_xlsx("tns administracion.xlsx", sheet = 1) |> mutate(area = "Administración")
obj_modulos_tns_adm           <- read_xlsx("tns administracion.xlsx", sheet = 4) |> mutate(area = "Administración")

#Joins
modulos_emtp              <- bind_rows(modulos_emtp_enf, modulos_emtp_adm)
obj_apr_emtp              <- bind_rows(obj_apr_emtp_enf, obj_apr_emtp_adm)
apr_esp_emtp              <- bind_rows(apr_esp_emtp_enf, apr_esp_emtp_adm)
relacion_modulos_obje_mtp <- bind_rows(relacion_modulos_obje_mtp_enf, relacion_modulos_obje_mtp_adm)
modulos_tns               <- bind_rows(modulos_tns_enf, modulos_tns_adm)
obj_modulos_tns           <- bind_rows(obj_modulos_tns_enf, obj_modulos_tns_adm)

# --- emtp_full ----------------------------------------------
emtp_full <- relacion_modulos_obje_mtp |>
  left_join(modulos_emtp,  by = "cod_m") |>
  left_join(obj_apr_emtp,  by = "cod_oa")

#  matriz convalidacion

congruencias <- read_xlsx("congruencias.xlsx",     sheet = 1)

# -- Funciones auxiliares ------------------------------------

# Ajuste de gráficos para móviles

tamanos_responsive <- function(ancho_px) {
  es_chico <- !is.null(ancho_px) && ancho_px < 350
  
  list(
    texto = if (es_chico) 3.5 else 5,
    base  = if (es_chico) 10 else 14,
    pie   = if (es_chico) 8  else 12
  )
}

# Nota al pie gráficos

tema_nota_pie <- function(fuente = "Fuente: Elaboración propia en base a datos abiertos MINEDUC.",
                          size = 12) {
  list(
    labs(caption = fuente),
    theme(
      plot.caption = element_text(size = size, color = "grey40", hjust = 1, 
                                  margin = margin(t = 10))
    )
  )
}

# Funcion para filtros en móviles

mobile_filter_hint <- function() {
  div(
    class = "mobile-filter-hint",
    bs_icon("info-circle"),
    "Esta aplicación está diseñada para visualizar en notebook o PC de escritorio."
  )
}


dt_opts <- list(
  language       = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
  pageLength     = 200,
  dom            = "t",
  scrollY        = "calc(100vh - 340px)",
  scrollCollapse = TRUE,
  autoWidth      = FALSE
)
