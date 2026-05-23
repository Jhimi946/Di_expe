# ============================================================
# Incidencia y severidad de fitoenfermedades del haba

# ------------------------------------------------------------
# 00. CONFIGURACIÓN GENERAL
# ------------------------------------------------------------

entrada_datos <- "google_sheet" 

url_sheet <- "https://docs.google.com/spreadsheets/d/1zlK6huNyAi67h-WxXF6Mxw9xv9R7biwRsCAJ6LBsGtk/edit?gid=891450308#gid=891450308"

hoja_trabajo <- "fb"

usar_google_sheet_publico <- TRUE

ruta_base <- if (dir.exists("D:/")) {
  "D:/haba_R"
} else {
  file.path(getwd(), "haba_R")
}

# La fuente original solo reporta incidencia de mancha chocolate.
# Por rigor, no se imputa severidad ni grados para esa enfermedad.
forzar_na_severidad_mancha_chocolate <- TRUE

# ------------------------------------------------------------
# 01. PAQUETES
# ------------------------------------------------------------

library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(janitor)
library(readr)
library(ggplot2)
library(ggtext)
library(writexl)
library(broom)
library(tibble)
library(ggrepel)
library(scales)

# ------------------------------------------------------------
# 02. CARPETAS DE SALIDA
# ------------------------------------------------------------

dir_raw         <- file.path(ruta_base, "01_datos_raw")
dir_procesados <- file.path(ruta_base, "02_datos_procesados")
dir_tablas      <- file.path(ruta_base, "03_tablas")
dir_figuras     <- file.path(ruta_base, "04_figuras")
dir_modelos     <- file.path(ruta_base, "05_modelos")

dir.create(ruta_base, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_procesados, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_modelos, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 03. FUNCIONES AUXILIARES
# ------------------------------------------------------------

limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_squish(stringr::str_trim(x))
  x[x == ""] <- NA_character_
  x
}

normalizar_categoria <- function(x) {
  x <- limpiar_texto(x)
  ifelse(
    is.na(x),
    NA_character_,
    x %>%
      stringr::str_to_lower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      stringr::str_replace_all("[^a-z0-9]+", "_") %>%
      stringr::str_replace_all("^_|_$", "")
  )
}

convertir_numero <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "%", "")
  x <- stringr::str_replace_all(x, ",", ".")
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(x))
}

convertir_fecha <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  x_chr <- stringr::str_trim(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  
  fecha <- suppressWarnings(as.Date(x_chr))
  numero <- suppressWarnings(as.numeric(x_chr))
  indice_serial <- is.na(fecha) & !is.na(numero)
  fecha[indice_serial] <- as.Date(numero[indice_serial], origin = "1899-12-30")
  fecha
}

media_segura <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

min_seguro <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

max_seguro <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

sd_segura <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

cor_segura <- function(x, y) {
  datos <- tibble::tibble(x = x, y = y) %>%
    dplyr::filter(!is.na(x), !is.na(y))
  
  if (nrow(datos) < 3) return(NA_real_)
  if (dplyr::n_distinct(datos$x) < 2 || dplyr::n_distinct(datos$y) < 2) return(NA_real_)
  
  suppressWarnings(cor(datos$x, datos$y, method = "spearman"))
}

renombrar_si_existe <- function(data, nuevo, candidatos) {
  if (nuevo %in% names(data)) return(data)
  encontrado <- intersect(candidatos, names(data))
  if (length(encontrado) > 0) {
    names(data)[names(data) == encontrado[1]] <- nuevo
  }
  data
}

guardar_grafico <- function(grafico, nombre, ancho = 10, alto = 6) {
  ggplot2::ggsave(
    filename = file.path(dir_figuras, paste0(nombre, ".png")),
    plot = grafico,
    width = ancho,
    height = alto,
    dpi = 320,
    bg = "white"
  )
  ggplot2::ggsave(
    filename = file.path(dir_figuras, paste0(nombre, ".pdf")),
    plot = grafico,
    width = ancho,
    height = alto,
    bg = "white"
  )
}

tema_pro <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(face = "bold", size = 14),
      plot.subtitle = ggtext::element_markdown(size = 11),
      plot.caption = ggtext::element_markdown(size = 9, color = "grey30"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggtext::element_markdown(),
      strip.text = ggtext::element_markdown(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", color = NA)
    )
}

ajustar_modelo <- function(expr, nombre) {
  advertencias <- character()
  
  resultado <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        message("No se pudo ajustar ", nombre, ": ", e$message)
        NULL
      }
    ),
    warning = function(w) {
      advertencias <<- c(advertencias, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  attr(resultado, "advertencias") <- advertencias
  resultado
}

# ------------------------------------------------------------
# 04. IMPORTACIÓN DE DATOS
# ------------------------------------------------------------

if (entrada_datos == "google_sheet") {
  if (usar_google_sheet_publico) {
    googlesheets4::gs4_deauth()
  } else {
    googlesheets4::gs4_auth()
  }
  
  fb_raw <- googlesheets4::read_sheet(
    ss = url_sheet,
    sheet = hoja_trabajo,
    col_names = TRUE,
    .name_repair = "minimal"
  )
  
} else if (entrada_datos == "excel") {
  fb_raw <- readxl::read_excel(
    path = archivo_excel,
    sheet = hoja_trabajo
  )
  
} else {
  stop("entrada_datos debe ser 'google_sheet' o 'excel'.")
}

readr::write_csv(fb_raw, file.path(dir_raw, "fb_original.csv"), na = "")

# ------------------------------------------------------------
# 05. LIMPIEZA DE ENCABEZADOS Y VALIDACIÓN DE COLUMNAS
# ------------------------------------------------------------

fb <- fb_raw %>%
  janitor::clean_names() %>%
  janitor::remove_empty(which = "rows") %>%
  janitor::remove_empty(which = "cols")

# Compatibilidad con versiones anteriores de la hoja fb.
fb <- renombrar_si_existe(fb, "campana_agricola", c("campana", "campania", "campana_agricola"))
fb <- renombrar_si_existe(fb, "anio_evaluacion", c("anio", "ano", "year"))
fb <- renombrar_si_existe(fb, "fecha_aprox", c("fecha", "fecha_aproximada"))
fb <- renombrar_si_existe(fb, "temp_prom_c", c("temperatura_promedio_c", "temperatura_c", "temp_c"))
fb <- renombrar_si_existe(fb, "humedad_relativa_pct", c("hr_pct", "humedad_pct", "humedad_relativa"))
fb <- renombrar_si_existe(fb, "precipitacion_mm", c("precip_mm", "precipitacion"))
fb <- renombrar_si_existe(fb, "viento_ms", c("velocidad_viento_ms", "viento_m_s"))
fb <- renombrar_si_existe(fb, "plantas_sanas", c("plantas_aparentemente_sanas", "plantas_no_infectadas"))
fb <- renombrar_si_existe(fb, "plantas_infectadas", c("plantas_enfermas"))

columnas_requeridas <- c(
  "id_obs",
  "campana_agricola",
  "anio_evaluacion",
  "fecha_aprox",
  "mes",
  "distrito",
  "utm_este",
  "utm_norte",
  "altitud_msnm",
  "sistema_siembra",
  "cultivo_asociado",
  "enfermedad",
  "agente_causal",
  "tipo_patogeno",
  "temp_prom_c",
  "humedad_relativa_pct",
  "precipitacion_mm",
  "viento_ms",
  "plantas_evaluadas",
  "plantas_sanas",
  "plantas_infectadas",
  "incidencia_pct",
  "severidad_pct",
  "grado_1_n",
  "grado_2_n",
  "grado_3_n",
  "grado_4_n",
  "grado_5_n"
)

columnas_faltantes <- setdiff(columnas_requeridas, names(fb))

if (length(columnas_faltantes) > 0) {
  stop(
    paste(
      "Faltan columnas requeridas en la hoja fb:",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

readr::write_csv(fb, file.path(dir_procesados, "fb_columnas_limpias.csv"), na = "")

# ------------------------------------------------------------
# 06. ESTANDARIZACIÓN DE TIPOS, FACTORES Y ETIQUETAS
# ------------------------------------------------------------

grado_cols <- c("grado_1_n", "grado_2_n", "grado_3_n", "grado_4_n", "grado_5_n")

datos_analisis <- fb %>%
  dplyr::mutate(
    id_obs = limpiar_texto(id_obs),
    campana_agricola = limpiar_texto(campana_agricola),
    anio_evaluacion = as.integer(convertir_numero(anio_evaluacion)),
    fecha_aprox = convertir_fecha(fecha_aprox),
    
    mes = normalizar_categoria(mes),
    mes_label = dplyr::recode(
      mes,
      "enero" = "Enero",
      "febrero" = "Febrero",
      .default = stringr::str_to_sentence(mes)
    ),
    
    distrito_original = limpiar_texto(distrito),
    distrito_id = normalizar_categoria(distrito),
    distrito_label = dplyr::case_when(
      distrito_id == "banos_del_inca" ~ "Baños del Inca",
      distrito_id == "jesus" ~ "Jesús",
      distrito_id == "la_encanada" ~ "La Encañada",
      distrito_id == "namora" ~ "Namora",
      TRUE ~ distrito_original
    ),
    
    utm_este = convertir_numero(utm_este),
    utm_norte = convertir_numero(utm_norte),
    altitud_msnm = convertir_numero(altitud_msnm),
    
    sistema_siembra = normalizar_categoria(sistema_siembra),
    sistema_siembra_label = dplyr::recode(
      sistema_siembra,
      "asociado" = "Asociado",
      "monocultivo" = "Monocultivo",
      .default = stringr::str_to_sentence(sistema_siembra)
    ),
    
    cultivo_asociado = normalizar_categoria(cultivo_asociado),
    cultivo_asociado_label = dplyr::recode(
      cultivo_asociado,
      "maiz" = "Maíz",
      "avena" = "Avena",
      "ninguno" = "Ninguno",
      .default = stringr::str_to_sentence(cultivo_asociado)
    ),
    
    enfermedad = normalizar_categoria(enfermedad),
    enfermedad_label = dplyr::case_when(
      enfermedad == "roya" ~ "Roya (<i>Uromyces fabae</i>)",
      enfermedad == "fusariosis" ~ "Fusariosis (<i>Fusarium solani</i> f. sp. <i>phaseoli</i>)",
      enfermedad == "mancha_chocolate" ~ "Mancha chocolate (<i>Botrytis fabae</i>)",
      TRUE ~ enfermedad
    ),
    
    agente_causal = limpiar_texto(agente_causal),
    tipo_patogeno = normalizar_categoria(tipo_patogeno),
    
    temp_prom_c = convertir_numero(temp_prom_c),
    humedad_relativa_pct = convertir_numero(humedad_relativa_pct),
    precipitacion_mm = convertir_numero(precipitacion_mm),
    viento_ms = convertir_numero(viento_ms),
    
    plantas_evaluadas = convertir_numero(plantas_evaluadas),
    plantas_sanas = convertir_numero(plantas_sanas),
    plantas_infectadas = convertir_numero(plantas_infectadas),
    incidencia_pct = convertir_numero(incidencia_pct),
    severidad_pct = convertir_numero(severidad_pct),
    
    dplyr::across(dplyr::all_of(grado_cols), convertir_numero)
  ) %>%
  dplyr::mutate(
    plantas_sanas = dplyr::if_else(
      is.na(plantas_sanas) & !is.na(plantas_evaluadas) & !is.na(plantas_infectadas),
      plantas_evaluadas - plantas_infectadas,
      plantas_sanas
    ),
    incidencia_pct = dplyr::if_else(
      is.na(incidencia_pct) & !is.na(plantas_infectadas) & !is.na(plantas_evaluadas) & plantas_evaluadas > 0,
      round((plantas_infectadas / plantas_evaluadas) * 100, 2),
      incidencia_pct
    ),
    incidencia_prop = incidencia_pct / 100,
    severidad_prop = severidad_pct / 100
  )

datos_analisis <- datos_analisis %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    n_grados_reportados = sum(!is.na(dplyr::c_across(dplyr::all_of(grado_cols)))),
    grado_total_n = dplyr::if_else(
      n_grados_reportados > 0,
      sum(dplyr::c_across(dplyr::all_of(grado_cols)), na.rm = TRUE),
      NA_real_
    ),
    severidad_calculada = dplyr::if_else(
      n_grados_reportados > 0 & !is.na(plantas_evaluadas) & plantas_evaluadas > 0,
      round(
        (
          dplyr::coalesce(grado_1_n, 0) * 0 +
            dplyr::coalesce(grado_2_n, 0) * 25 +
            dplyr::coalesce(grado_3_n, 0) * 50 +
            dplyr::coalesce(grado_4_n, 0) * 75 +
            dplyr::coalesce(grado_5_n, 0) * 100
        ) / plantas_evaluadas,
        2
      ),
      NA_real_
    ),
    severidad_pct = dplyr::if_else(
      is.na(severidad_pct) & !is.na(severidad_calculada),
      severidad_calculada,
      severidad_pct
    ),
    severidad_prop = severidad_pct / 100
  ) %>%
  dplyr::ungroup()

if (forzar_na_severidad_mancha_chocolate) {
  datos_analisis <- datos_analisis %>%
    dplyr::mutate(
      severidad_pct = dplyr::if_else(enfermedad == "mancha_chocolate", NA_real_, severidad_pct),
      severidad_prop = dplyr::if_else(enfermedad == "mancha_chocolate", NA_real_, severidad_prop),
      severidad_calculada = dplyr::if_else(enfermedad == "mancha_chocolate", NA_real_, severidad_calculada),
      dplyr::across(dplyr::all_of(grado_cols), ~ dplyr::if_else(enfermedad == "mancha_chocolate", NA_real_, .x)),
      n_grados_reportados = dplyr::if_else(enfermedad == "mancha_chocolate", 0, n_grados_reportados),
      grado_total_n = dplyr::if_else(enfermedad == "mancha_chocolate", NA_real_, grado_total_n)
    )
}

datos_analisis <- datos_analisis %>%
  dplyr::mutate(
    distrito_label = factor(
      distrito_label,
      levels = c("Baños del Inca", "Jesús", "La Encañada", "Namora")
    ),
    enfermedad = factor(
      enfermedad,
      levels = c("roya", "fusariosis", "mancha_chocolate")
    ),
    sistema_siembra = factor(sistema_siembra),
    cultivo_asociado = factor(cultivo_asociado),
    incidencia_clase = cut(
      incidencia_pct,
      breaks = c(-Inf, 0, 25, 50, 75, Inf),
      labels = c("Sin incidencia", "Baja", "Media", "Alta", "Muy alta"),
      right = TRUE
    ),
    severidad_clase = cut(
      severidad_pct,
      breaks = c(-Inf, 0, 25, 40, 75, Inf),
      labels = c("Sin severidad", "Baja", "Moderada", "Alta", "Muy alta"),
      right = TRUE
    )
  ) %>%
  dplyr::arrange(enfermedad, distrito_label)

readr::write_csv(
  datos_analisis,
  file.path(dir_procesados, "base_principal_analisis_R.csv"),
  na = ""
)

# ------------------------------------------------------------
# 07. CONTROL DE CALIDAD
# ------------------------------------------------------------

control_estructura <- datos_analisis %>%
  dplyr::count(distrito_label, enfermedad, name = "n_filas") %>%
  dplyr::mutate(estructura_ok = n_filas == 1)

validacion_base <- datos_analisis %>%
  dplyr::mutate(
    plantas_suma = plantas_sanas + plantas_infectadas,
    plantas_ok = dplyr::case_when(
      is.na(plantas_suma) | is.na(plantas_evaluadas) ~ NA,
      abs(plantas_suma - plantas_evaluadas) < 0.001 ~ TRUE,
      TRUE ~ FALSE
    ),
    incidencia_calculada = dplyr::if_else(
      !is.na(plantas_infectadas) & !is.na(plantas_evaluadas) & plantas_evaluadas > 0,
      round((plantas_infectadas / plantas_evaluadas) * 100, 2),
      NA_real_
    ),
    diferencia_incidencia = round(incidencia_pct - incidencia_calculada, 4),
    incidencia_ok = dplyr::case_when(
      is.na(incidencia_pct) & is.na(incidencia_calculada) ~ TRUE,
      !is.na(diferencia_incidencia) & abs(diferencia_incidencia) < 0.01 ~ TRUE,
      TRUE ~ FALSE
    ),
    grados_suman_ok = dplyr::case_when(
      enfermedad == "mancha_chocolate" ~ TRUE,
      is.na(grado_total_n) ~ NA,
      abs(grado_total_n - plantas_evaluadas) < 0.001 ~ TRUE,
      TRUE ~ FALSE
    ),
    diferencia_severidad = round(severidad_pct - severidad_calculada, 4),
    severidad_ok = dplyr::case_when(
      enfermedad == "mancha_chocolate" ~ TRUE,
      is.na(severidad_pct) & is.na(severidad_calculada) ~ TRUE,
      !is.na(diferencia_severidad) & abs(diferencia_severidad) < 0.01 ~ TRUE,
      TRUE ~ FALSE
    ),
    validacion_general = dplyr::if_else(
      plantas_ok %in% TRUE & incidencia_ok %in% TRUE & severidad_ok %in% TRUE & grados_suman_ok %in% TRUE,
      "OK",
      "REVISAR"
    )
  ) %>%
  dplyr::select(
    id_obs,
    distrito_label,
    enfermedad,
    plantas_evaluadas,
    plantas_sanas,
    plantas_infectadas,
    plantas_ok,
    incidencia_pct,
    incidencia_calculada,
    diferencia_incidencia,
    incidencia_ok,
    severidad_pct,
    severidad_calculada,
    diferencia_severidad,
    severidad_ok,
    grado_total_n,
    grados_suman_ok,
    validacion_general
  )

reporte_faltantes <- datos_analisis %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "n_faltantes"
  ) %>%
  dplyr::arrange(dplyr::desc(n_faltantes))

readr::write_csv(control_estructura, file.path(dir_tablas, "control_estructura_base.csv"), na = "")
readr::write_csv(validacion_base, file.path(dir_tablas, "validacion_base.csv"), na = "")
readr::write_csv(reporte_faltantes, file.path(dir_tablas, "reporte_datos_faltantes.csv"), na = "")

# ------------------------------------------------------------
# 08. TABLAS DESCRIPTIVAS
# ------------------------------------------------------------

resumen_por_enfermedad <- datos_analisis %>%
  dplyr::group_by(enfermedad, enfermedad_label, agente_causal) %>%
  dplyr::summarise(
    n_observaciones = dplyr::n(),
    incidencia_media = media_segura(incidencia_pct),
    incidencia_minima = min_seguro(incidencia_pct),
    incidencia_maxima = max_seguro(incidencia_pct),
    severidad_media = media_segura(severidad_pct),
    severidad_minima = min_seguro(severidad_pct),
    severidad_maxima = max_seguro(severidad_pct),
    plantas_evaluadas_total = sum(plantas_evaluadas, na.rm = TRUE),
    plantas_infectadas_total = sum(plantas_infectadas, na.rm = TRUE),
    .groups = "drop"
  )

resumen_por_distrito <- datos_analisis %>%
  dplyr::group_by(distrito_label) %>%
  dplyr::summarise(
    n_enfermedades = dplyr::n_distinct(enfermedad),
    incidencia_media = media_segura(incidencia_pct),
    incidencia_maxima = max_seguro(incidencia_pct),
    severidad_media = media_segura(severidad_pct),
    severidad_maxima = max_seguro(severidad_pct),
    plantas_evaluadas_total = sum(plantas_evaluadas, na.rm = TRUE),
    plantas_infectadas_total = sum(plantas_infectadas, na.rm = TRUE),
    temp_prom_c = dplyr::first(temp_prom_c),
    humedad_relativa_pct = dplyr::first(humedad_relativa_pct),
    precipitacion_mm = dplyr::first(precipitacion_mm),
    viento_ms = dplyr::first(viento_ms),
    altitud_msnm = dplyr::first(altitud_msnm),
    sistema_siembra = dplyr::first(as.character(sistema_siembra)),
    cultivo_asociado = dplyr::first(as.character(cultivo_asociado)),
    .groups = "drop"
  )

resumen_por_sistema <- datos_analisis %>%
  dplyr::group_by(sistema_siembra, cultivo_asociado, enfermedad) %>%
  dplyr::summarise(
    n_observaciones = dplyr::n(),
    incidencia_media = media_segura(incidencia_pct),
    severidad_media = media_segura(severidad_pct),
    plantas_infectadas_total = sum(plantas_infectadas, na.rm = TRUE),
    .groups = "drop"
  )

tabla_incidencia <- datos_analisis %>%
  dplyr::select(distrito_label, enfermedad, incidencia_pct) %>%
  tidyr::pivot_wider(names_from = enfermedad, values_from = incidencia_pct)

tabla_severidad <- datos_analisis %>%
  dplyr::select(distrito_label, enfermedad, severidad_pct) %>%
  tidyr::pivot_wider(names_from = enfermedad, values_from = severidad_pct)

readr::write_csv(resumen_por_enfermedad, file.path(dir_tablas, "resumen_por_enfermedad.csv"), na = "")
readr::write_csv(resumen_por_distrito, file.path(dir_tablas, "resumen_por_distrito.csv"), na = "")
readr::write_csv(resumen_por_sistema, file.path(dir_tablas, "resumen_por_sistema_siembra.csv"), na = "")
readr::write_csv(tabla_incidencia, file.path(dir_tablas, "tabla_incidencia_distrito_enfermedad.csv"), na = "")
readr::write_csv(tabla_severidad, file.path(dir_tablas, "tabla_severidad_distrito_enfermedad.csv"), na = "")

# ------------------------------------------------------------
# 09. DECISIÓN TÉCNICA DE MODELOS
# ------------------------------------------------------------

decision_modelos <- tibble::tribble(
  ~variable_respuesta, ~naturaleza, ~modelo_recomendado, ~uso_correcto, ~limitacion_clave,
  "incidencia_pct / plantas_infectadas", "Proporción basada en conteos de plantas infectadas sobre plantas evaluadas", "GLM quasibinomial con cbind(plantas_infectadas, plantas_sanas)", "Explorar diferencias de incidencia entre enfermedades", "Base agregada por distrito × enfermedad; no reemplaza repeticiones por parcela",
  "severidad_pct", "Índice porcentual calculado desde grados de infección", "Modelo logístico ordinal usando conteos por grado como pesos", "Aprovechar la estructura ordinal de los grados 1 a 5", "Interpretación exploratoria porque no hay datos crudos por planta/parcela",
  "mancha_chocolate - severidad", "Dato no reportado", "No modelar severidad", "Conservar NA para no inventar información", "Solo se reporta incidencia",
  "variables ambientales", "Covariables de contexto por distrito", "Correlación de Spearman y gráficos exploratorios", "Explorar patrones descriptivos", "Solo cuatro distritos; no permite inferencia causal fuerte"
)

readr::write_csv(decision_modelos, file.path(dir_modelos, "decision_tecnica_modelos.csv"), na = "")

# ------------------------------------------------------------
# 10. MODELO DE INCIDENCIA
# ------------------------------------------------------------

datos_modelo_incidencia <- datos_analisis %>%
  dplyr::filter(
    !is.na(plantas_infectadas),
    !is.na(plantas_sanas),
    !is.na(plantas_evaluadas),
    plantas_evaluadas > 0
  ) %>%
  dplyr::mutate(enfermedad = forcats::fct_drop(enfermedad))

modelo_incidencia <- ajustar_modelo(
  glm(
    cbind(plantas_infectadas, plantas_sanas) ~ enfermedad,
    family = quasibinomial(link = "logit"),
    data = datos_modelo_incidencia
  ),
  "modelo_incidencia_quasibinomial"
)

if (!is.null(modelo_incidencia)) {
  tabla_modelo_incidencia <- broom::tidy(modelo_incidencia) %>%
    dplyr::mutate(
      odds_ratio = exp(estimate),
      modelo = "GLM quasibinomial",
      formula = "cbind(plantas_infectadas, plantas_sanas) ~ enfermedad"
    ) %>%
    dplyr::select(modelo, formula, dplyr::everything())
  
  readr::write_csv(tabla_modelo_incidencia, file.path(dir_modelos, "modelo_incidencia_quasibinomial.csv"), na = "")
} else {
  tabla_modelo_incidencia <- tibble::tibble(
    modelo = "GLM quasibinomial",
    observacion = "No se pudo ajustar el modelo de incidencia."
  )
}

advertencias_incidencia <- attr(modelo_incidencia, "advertencias")

# ------------------------------------------------------------
# 11. MODELO ORDINAL DE SEVERIDAD POR GRADOS
# ------------------------------------------------------------

datos_grados <- datos_analisis %>%
  dplyr::filter(enfermedad %in% c("roya", "fusariosis")) %>%
  dplyr::select(
    id_obs,
    distrito_label,
    enfermedad,
    enfermedad_label,
    dplyr::all_of(grado_cols)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(grado_cols),
    names_to = "grado_variable",
    values_to = "n_plantas"
  ) %>%
  dplyr::filter(!is.na(n_plantas), n_plantas > 0) %>%
  dplyr::mutate(
    grado = dplyr::recode(
      grado_variable,
      "grado_1_n" = "Grado 1",
      "grado_2_n" = "Grado 2",
      "grado_3_n" = "Grado 3",
      "grado_4_n" = "Grado 4",
      "grado_5_n" = "Grado 5"
    ),
    grado = ordered(grado, levels = paste("Grado", 1:5)),
    enfermedad = forcats::fct_drop(enfermedad)
  )

readr::write_csv(datos_grados, file.path(dir_procesados, "datos_grados_ordinales.csv"), na = "")

modelo_severidad_ordinal <- ajustar_modelo(
  MASS::polr(
    formula = grado ~ enfermedad,
    data = datos_grados,
    weights = n_plantas,
    Hess = TRUE,
    method = "logistic"
  ),
  "modelo_severidad_ordinal"
)

if (!is.null(modelo_severidad_ordinal)) {
  tabla_modelo_severidad <- broom::tidy(modelo_severidad_ordinal) %>%
    dplyr::mutate(
      odds_ratio = dplyr::if_else(stringr::str_detect(term, "\\|"), NA_real_, exp(estimate)),
      modelo = "Logístico ordinal con pesos por número de plantas",
      formula = "grado ~ enfermedad"
    ) %>%
    dplyr::select(modelo, formula, dplyr::everything())
  
  readr::write_csv(tabla_modelo_severidad, file.path(dir_modelos, "modelo_severidad_ordinal.csv"), na = "")
} else {
  tabla_modelo_severidad <- tibble::tibble(
    modelo = "Logístico ordinal",
    observacion = "No se pudo ajustar el modelo ordinal de severidad."
  )
}

advertencias_severidad <- attr(modelo_severidad_ordinal, "advertencias")

reporte_advertencias_modelos <- tibble::tibble(
  modelo = c(
    rep("incidencia_quasibinomial", length(advertencias_incidencia)),
    rep("severidad_ordinal", length(advertencias_severidad))
  ),
  advertencia = c(advertencias_incidencia, advertencias_severidad)
)

if (nrow(reporte_advertencias_modelos) == 0) {
  reporte_advertencias_modelos <- tibble::tibble(
    modelo = "sin_advertencias",
    advertencia = "No se capturaron advertencias en el ajuste de modelos."
  )
}

readr::write_csv(reporte_advertencias_modelos, file.path(dir_modelos, "advertencias_modelos.csv"), na = "")

# ------------------------------------------------------------
# 12. CORRELACIONES EXPLORATORIAS
# ------------------------------------------------------------

correlaciones_ambientales <- datos_analisis %>%
  dplyr::group_by(enfermedad) %>%
  dplyr::summarise(
    n_observaciones = dplyr::n(),
    cor_incidencia_temperatura = cor_segura(incidencia_pct, temp_prom_c),
    cor_incidencia_humedad = cor_segura(incidencia_pct, humedad_relativa_pct),
    cor_incidencia_precipitacion = cor_segura(incidencia_pct, precipitacion_mm),
    cor_incidencia_viento = cor_segura(incidencia_pct, viento_ms),
    cor_incidencia_altitud = cor_segura(incidencia_pct, altitud_msnm),
    cor_severidad_temperatura = cor_segura(severidad_pct, temp_prom_c),
    cor_severidad_humedad = cor_segura(severidad_pct, humedad_relativa_pct),
    cor_severidad_precipitacion = cor_segura(severidad_pct, precipitacion_mm),
    cor_severidad_viento = cor_segura(severidad_pct, viento_ms),
    cor_severidad_altitud = cor_segura(severidad_pct, altitud_msnm),
    .groups = "drop"
  )

readr::write_csv(correlaciones_ambientales, file.path(dir_tablas, "correlaciones_ambientales_exploratorias.csv"), na = "")

# ------------------------------------------------------------
# 13. GRÁFICOS RECOMENDADOS
# ------------------------------------------------------------

pal_enfermedad <- c(
  "roya" = "#C2410C",
  "fusariosis" = "#7F1D1D",
  "mancha_chocolate" = "#6B4F3A"
)

label_enfermedad <- c(
  "roya" = "Roya<br><i>Uromyces fabae</i>",
  "fusariosis" = "Fusariosis<br><i>Fusarium solani</i> f. sp. <i>phaseoli</i>",
  "mancha_chocolate" = "Mancha chocolate<br><i>Botrytis fabae</i>"
)

pal_grado <- c(
  "Grado 1" = "#D9F99D",
  "Grado 2" = "#A7F3D0",
  "Grado 3" = "#FDE68A",
  "Grado 4" = "#FDBA74",
  "Grado 5" = "#991B1B"
)

# Figura 01: Incidencia por distrito y enfermedad
fig_01 <- ggplot2::ggplot(
  datos_analisis,
  ggplot2::aes(x = distrito_label, y = incidencia_pct, fill = enfermedad)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.82), width = 0.72) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(incidencia_pct, 1), "%")),
    position = ggplot2::position_dodge(width = 0.82),
    vjust = -0.35,
    size = 3.1
  ) +
  ggplot2::scale_fill_manual(values = pal_enfermedad, labels = label_enfermedad) +
  ggplot2::coord_cartesian(ylim = c(0, 108)) +
  ggplot2::labs(
    title = "Incidencia de fitoenfermedades del haba (<i>Vicia faba</i> L.)",
    subtitle = "Comparación por distrito y agente causal",
    x = "Distrito",
    y = "Incidencia (%)",
    fill = "Enfermedad",
    caption = "Fuente: base fb estructurada para análisis en R."
  ) +
  tema_pro()

guardar_grafico(fig_01, "01_incidencia_por_distrito_enfermedad", 10.5, 6)

# Figura 02: Severidad por distrito y enfermedad
fig_02 <- datos_analisis %>%
  dplyr::filter(!is.na(severidad_pct)) %>%
  ggplot2::ggplot(ggplot2::aes(x = distrito_label, y = severidad_pct, fill = enfermedad)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.82), width = 0.72) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(severidad_pct, 1), "%")),
    position = ggplot2::position_dodge(width = 0.82),
    vjust = -0.35,
    size = 3.1
  ) +
  ggplot2::scale_fill_manual(values = pal_enfermedad, labels = label_enfermedad) +
  ggplot2::coord_cartesian(ylim = c(0, 55)) +
  ggplot2::labs(
    title = "Severidad de roya y fusariosis en haba (<i>Vicia faba</i> L.)",
    subtitle = "Mancha chocolate no se incluye porque la fuente solo reporta incidencia",
    x = "Distrito",
    y = "Severidad (%)",
    fill = "Enfermedad",
    caption = "La severidad se valida con los grados de infección reportados."
  ) +
  tema_pro()

guardar_grafico(fig_02, "02_severidad_por_distrito_enfermedad", 10.5, 6)

# Figura 03: Mapa de calor de incidencia
fig_03 <- ggplot2::ggplot(
  datos_analisis,
  ggplot2::aes(x = enfermedad_label, y = distrito_label, fill = incidencia_pct)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(incidencia_pct, 1), "%")), size = 3.5) +
  ggplot2::scale_fill_gradient(low = "#F8FAFC", high = "#7F1D1D") +
  ggplot2::labs(
    title = "Mapa de calor de incidencia de fitoenfermedades",
    x = "Enfermedad",
    y = "Distrito",
    fill = "Incidencia (%)"
  ) +
  tema_pro() +
  ggplot2::theme(axis.text.x = ggtext::element_markdown(angle = 20, hjust = 1))

guardar_grafico(fig_03, "03_mapa_calor_incidencia", 9.5, 5.5)

# Figura 04: Mapa de calor de severidad
fig_04 <- datos_analisis %>%
  dplyr::filter(!is.na(severidad_pct)) %>%
  ggplot2::ggplot(ggplot2::aes(x = enfermedad_label, y = distrito_label, fill = severidad_pct)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(severidad_pct, 1), "%")), size = 3.5) +
  ggplot2::scale_fill_gradient(low = "#F8FAFC", high = "#7F1D1D") +
  ggplot2::labs(
    title = "Mapa de calor de severidad de fitoenfermedades",
    x = "Enfermedad",
    y = "Distrito",
    fill = "Severidad (%)"
  ) +
  tema_pro() +
  ggplot2::theme(axis.text.x = ggtext::element_markdown(angle = 20, hjust = 1))

guardar_grafico(fig_04, "04_mapa_calor_severidad", 8.5, 5.2)

# Figura 05: Distribución porcentual de grados de infección
datos_grados_prop <- datos_grados %>%
  dplyr::group_by(enfermedad, enfermedad_label, distrito_label) %>%
  dplyr::mutate(
    total_plantas_grado = sum(n_plantas, na.rm = TRUE),
    prop_plantas = (n_plantas / total_plantas_grado) * 100
  ) %>%
  dplyr::ungroup()

fig_05 <- ggplot2::ggplot(
  datos_grados_prop,
  ggplot2::aes(x = distrito_label, y = prop_plantas, fill = grado)
) +
  ggplot2::geom_col(width = 0.72) +
  ggplot2::facet_wrap(~ enfermedad_label) +
  ggplot2::scale_fill_manual(values = pal_grado) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggplot2::labs(
    title = "Distribución relativa de grados de infección",
    subtitle = "Composición de grados dentro de cada distrito",
    x = "Distrito",
    y = "Proporción de plantas evaluadas",
    fill = "Grado"
  ) +
  tema_pro()

guardar_grafico(fig_05, "05_distribucion_porcentual_grados_infeccion", 10.5, 6)

# Figura 06: Relación incidencia-severidad
fig_06 <- datos_analisis %>%
  dplyr::filter(!is.na(severidad_pct)) %>%
  ggplot2::ggplot(ggplot2::aes(x = incidencia_pct, y = severidad_pct, color = enfermedad, label = distrito_label)) +
  ggplot2::geom_point(size = 3.6, alpha = 0.9) +
  ggrepel::geom_text_repel(size = 3.2, show.legend = FALSE) +
  ggplot2::facet_wrap(~ enfermedad_label) +
  ggplot2::scale_color_manual(values = pal_enfermedad, labels = label_enfermedad) +
  ggplot2::labs(
    title = "Relación entre incidencia y severidad",
    subtitle = "Lectura exploratoria; no debe interpretarse como causalidad",
    x = "Incidencia (%)",
    y = "Severidad (%)",
    color = "Enfermedad"
  ) +
  tema_pro()

guardar_grafico(fig_06, "06_relacion_incidencia_severidad", 10, 5.8)

# Figura 07: Distribución UTM aproximada e incidencia
fig_07 <- ggplot2::ggplot(
  datos_analisis,
  ggplot2::aes(x = utm_este, y = utm_norte, size = incidencia_pct, color = enfermedad, label = distrito_label)
) +
  ggplot2::geom_point(alpha = 0.78) +
  ggrepel::geom_text_repel(size = 3, show.legend = FALSE) +
  ggplot2::facet_wrap(~ enfermedad_label) +
  ggplot2::scale_color_manual(values = pal_enfermedad, labels = label_enfermedad) +
  ggplot2::scale_size_continuous(range = c(1.5, 8)) +
  ggplot2::labs(
    title = "Distribución espacial aproximada de la incidencia",
    subtitle = "Coordenadas UTM referenciales por distrito",
    x = "UTM Este",
    y = "UTM Norte",
    size = "Incidencia (%)",
    color = "Enfermedad"
  ) +
  tema_pro()

guardar_grafico(fig_07, "07_distribucion_utm_incidencia", 10.5, 6.2)

# Figura 08: Perfil ambiental estandarizado
ambiente_largo <- datos_analisis %>%
  dplyr::distinct(
    distrito_label,
    temp_prom_c,
    humedad_relativa_pct,
    precipitacion_mm,
    viento_ms,
    altitud_msnm
  ) %>%
  tidyr::pivot_longer(
    cols = c(temp_prom_c, humedad_relativa_pct, precipitacion_mm, viento_ms, altitud_msnm),
    names_to = "variable_ambiental",
    values_to = "valor"
  ) %>%
  dplyr::mutate(
    variable_ambiental = dplyr::recode(
      variable_ambiental,
      "temp_prom_c" = "Temperatura promedio (°C)",
      "humedad_relativa_pct" = "Humedad relativa (%)",
      "precipitacion_mm" = "Precipitación (mm)",
      "viento_ms" = "Velocidad del viento (m/s)",
      "altitud_msnm" = "Altitud (m s. n. m.)"
    )
  ) %>%
  dplyr::group_by(variable_ambiental) %>%
  dplyr::mutate(
    valor_z = {
      media_val <- mean(valor, na.rm = TRUE)
      sd_val <- stats::sd(valor, na.rm = TRUE)
      
      if (is.na(sd_val) || sd_val == 0) {
        ifelse(is.na(valor), NA_real_, 0)
      } else {
        (valor - media_val) / sd_val
      }
    }
  ) %>%
  dplyr::ungroup()
# Figura 09: Plantas sanas e infectadas
datos_sanas_infectadas <- datos_analisis %>%
  dplyr::select(distrito_label, enfermedad, enfermedad_label, plantas_sanas, plantas_infectadas) %>%
  tidyr::pivot_longer(
    cols = c(plantas_sanas, plantas_infectadas),
    names_to = "estado_planta",
    values_to = "n_plantas"
  ) %>%
  dplyr::mutate(
    estado_planta = dplyr::recode(
      estado_planta,
      "plantas_sanas" = "Aparentemente sanas",
      "plantas_infectadas" = "Infectadas"
    )
  )

fig_09 <- ggplot2::ggplot(
  datos_sanas_infectadas,
  ggplot2::aes(x = distrito_label, y = n_plantas, fill = estado_planta)
) +
  ggplot2::geom_col(width = 0.72) +
  ggplot2::facet_wrap(~ enfermedad_label) +
  ggplot2::scale_fill_manual(values = c("Aparentemente sanas" = "#DCFCE7", "Infectadas" = "#B91C1C")) +
  ggplot2::labs(
    title = "Composición de plantas evaluadas por estado sanitario",
    x = "Distrito",
    y = "Número de plantas",
    fill = "Estado"
  ) +
  tema_pro()

guardar_grafico(fig_09, "09_plantas_sanas_infectadas", 10.5, 6)

# Figura 10: Incidencia por cultivo asociado
fig_10 <- datos_analisis %>%
  dplyr::group_by(cultivo_asociado_label, enfermedad, enfermedad_label) %>%
  dplyr::summarise(
    incidencia_media = media_segura(incidencia_pct),
    severidad_media = media_segura(severidad_pct),
    .groups = "drop"
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = cultivo_asociado_label, y = incidencia_media, fill = enfermedad)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(incidencia_media, 1), "%")),
    position = ggplot2::position_dodge(width = 0.8),
    vjust = -0.35,
    size = 3.1
  ) +
  ggplot2::scale_fill_manual(values = pal_enfermedad, labels = label_enfermedad) +
  ggplot2::coord_cartesian(ylim = c(0, 108)) +
  ggplot2::labs(
    title = "Incidencia promedio según cultivo asociado",
    subtitle = "Comparación descriptiva; el sistema de siembra está ligado al distrito",
    x = "Cultivo asociado",
    y = "Incidencia media (%)",
    fill = "Enfermedad"
  ) +
  tema_pro()

guardar_grafico(fig_10, "10_incidencia_por_cultivo_asociado", 10, 5.8)

# ------------------------------------------------------------
# 14. EXPORTAR EXCEL FINAL DE RESULTADOS
# ------------------------------------------------------------

salida_excel <- list(
  base_principal_analisis_R = datos_analisis,
  control_estructura = control_estructura,
  validacion_base = validacion_base,
  reporte_faltantes = reporte_faltantes,
  resumen_por_enfermedad = resumen_por_enfermedad,
  resumen_por_distrito = resumen_por_distrito,
  resumen_por_sistema = resumen_por_sistema,
  tabla_incidencia = tabla_incidencia,
  tabla_severidad = tabla_severidad,
  decision_tecnica_modelos = decision_modelos,
  modelo_incidencia = tabla_modelo_incidencia,
  modelo_severidad_ordinal = tabla_modelo_severidad,
  advertencias_modelos = reporte_advertencias_modelos,
  correlaciones_ambientales = correlaciones_ambientales
)

writexl::write_xlsx(
  salida_excel,
  path = file.path(dir_tablas, "resultados_pipeline_haba_VERIFICADO.xlsx")
)

# ------------------------------------------------------------
# 15. REGISTRO FINAL
# ------------------------------------------------------------

registro_pipeline <- tibble::tibble(
  elemento = c(
    "Ruta base",
    "Hoja de trabajo",
    "Base original",
    "Base procesada",
    "Excel de resultados",
    "Figuras",
    "Modelos",
    "Nota metodológica"
  ),
  ubicacion = c(
    ruta_base,
    hoja_trabajo,
    file.path(dir_raw, "fb_original.csv"),
    file.path(dir_procesados, "base_principal_analisis_R.csv"),
    file.path(dir_tablas, "resultados_pipeline_haba_VERIFICADO.xlsx"),
    dir_figuras,
    dir_modelos,
    "Modelos exploratorios por tratarse de datos agregados por distrito × enfermedad."
  )
)

readr::write_csv(registro_pipeline, file.path(ruta_base, "registro_pipeline_VERIFICADO.csv"), na = "")

cat("\n============================================================\n")
cat("PIPELINE VERIFICADO FINALIZADO\n")
cat("============================================================\n")
cat("\nCarpeta principal:\n")
cat(ruta_base, "\n")
cat("\nBase principal para R:\n")
cat(file.path(dir_procesados, "base_principal_analisis_R.csv"), "\n")
cat("\nExcel de resultados:\n")
cat(file.path(dir_tablas, "resultados_pipeline_haba_VERIFICADO.xlsx"), "\n")
cat("\nFiguras PNG/PDF:\n")
cat(dir_figuras, "\n")
cat("\nModelos exploratorios:\n")
cat(dir_modelos, "\n")
cat("\nDecisión metodológica:\n")
cat("Incidencia: GLM quasibinomial. Severidad: modelo logístico ordinal por grados. Mancha chocolate: solo incidencia.\n")
cat("============================================================\n")
