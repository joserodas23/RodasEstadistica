#' Crear una tabla descriptiva a partir de un resumen estadístico
#'
#' Esta función toma un dataframe de resumen estadístico (creado previamente con group_by y summarise)
#' y lo convierte en una tabla formateada con kableExtra, con grupos según la primera variable de agrupación.
#' También maneja adecuadamente el caso de datos sin agrupar.
#'
#' @param resumen Un dataframe que contiene estadísticas descriptivas.
#' @param columnas_renombrar Un vector con nombres para renombrar las columnas.
#'   Los valores del vector deben corresponder con las columnas existentes en el dataframe,
#'   y los nombres del vector serán los nuevos nombres de columnas.
#' @param variable_grupo Nombre de la columna de agrupación principal (por defecto, la primera columna del dataframe si existe).
#' @param nombre_sin_grupo Título para datos sin agrupar.
#' @param colores_grupos Vector con colores para los diferentes grupos.
#' @param caption Texto para el título de la tabla.
#' @param formato Formato de salida, por defecto "html".
#'
#' @return Un objeto kable formateado.
#'
#' @import dplyr
#' @import kableExtra
#'
#' @examples
#' \dontrun{
#' # Con datos agrupados
#' resumen <- datos %>%
#'   group_by(Temperatura, Cultivo) %>%
#'   summarise(
#'     n = n(),
#'     media = mean(Rendimiento),
#'     mediana = median(Rendimiento),
#'     desv_est = sd(Rendimiento),
#'     .groups = 'drop'
#'   )
#' 
#' tabla_descriptivo(resumen)
#' 
#' # Con datos sin agrupar
#' resumen_simple <- datos %>%
#'   summarise(
#'     n = n(),
#'     media = mean(Rendimiento),
#'     mediana = median(Rendimiento),
#'     desv_est = sd(Rendimiento)
#'   )
#' 
#' tabla_descriptivo(
#'   resumen_simple, 
#'   nombre_sin_grupo = "Estadísticas globales"
#' )
#' }
#'
#' @export
tabla_descriptivo <- function(resumen, 
                              columnas_renombrar = NULL,
                              variable_grupo = NULL,
                              nombre_sin_grupo = "Datos globales",
                              colores_grupos = c("#e6f7ff", "#fff2e6", "#ffe6e6", "#e6ffe6", "#f7e6ff"),
                              caption = "Estadísticas descriptivas",
                              formato = "html") {
  
  # Verificar que los paquetes necesarios estén instalados
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("El paquete kableExtra es necesario para esta función. Por favor instálalo con install.packages('kableExtra')")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("El paquete dplyr es necesario para esta función. Por favor instálalo con install.packages('dplyr')")
  }
  
  # Crear una copia del dataframe original
  tabla <- resumen %>% dplyr::as_tibble()
  
  # Detectar si hay variables de agrupación
  posibles_grupos <- colnames(tabla)[sapply(tabla, function(x) !is.numeric(x) && length(unique(x)) < length(x))]
  tiene_grupos <- length(posibles_grupos) > 0
  
  # Si se especifica variable_grupo, verificar que existe
  if (!is.null(variable_grupo)) {
    if (!(variable_grupo %in% colnames(tabla))) {
      stop(paste("La variable de agrupación", variable_grupo, "no existe en el dataframe"))
    }
  } else if (tiene_grupos) {
    # Si no se especifica pero hay grupos posibles, usar el primero
    variable_grupo <- posibles_grupos[1]
  }
  
  # Redondear columnas numéricas a 2 decimales
  tabla <- tabla %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))
  
  # Renombrar columnas si se proporciona el parámetro
  if (!is.null(columnas_renombrar)) {
    # Verificar que los valores (columnas originales) existen en el dataframe
    columnas_originales <- as.character(columnas_renombrar)
    if (!all(columnas_originales %in% colnames(tabla))) {
      stop("Algunas columnas especificadas para renombrar no existen en el dataframe")
    }
    
    # Crear un vector de renombrado para dplyr::rename
    renombrado <- stats::setNames(columnas_originales, names(columnas_renombrar))
    
    # Renombrar las columnas
    tabla <- tabla %>% dplyr::rename(!!!renombrado)
  }
  
  # Definir alineación basada en el tipo de datos
  alineacion <- sapply(tabla, function(x) {
    if (is.numeric(x)) return("c") 
    else return("l")
  })
  
  # Crear la tabla base con kable
  tabla_kable <- tabla %>%
    kableExtra::kable(
      format = formato,
      caption = caption,
      align = alineacion
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center"
    )
  
  # Determinar qué columnas destacar
  if (tiene_grupos) {
    # Destacar columnas de agrupación
    cols_destacar <- which(colnames(tabla) %in% posibles_grupos)
    if (length(cols_destacar) > 0) {
      tabla_kable <- tabla_kable %>% 
        kableExtra::column_spec(cols_destacar, bold = TRUE)
    }
    
    # Procesar grupos si hay una variable de agrupación
    if (!is.null(variable_grupo)) {
      # Identificar valores únicos en la variable de agrupación
      grupos_unicos <- unique(tabla[[variable_grupo]])
      
      # Obtener índices de cada grupo
      indices_grupos <- list()
      inicio <- 1
      
      for (grupo in grupos_unicos) {
        # Contar filas en el grupo actual
        n_filas <- sum(tabla[[variable_grupo]] == grupo)
        fin <- inicio + n_filas - 1
        
        # Guardar información de inicio y fin
        indices_grupos[[paste0("grupo_", grupo)]] <- list(
          nombre = paste(variable_grupo, grupo),  # Usar el nombre de la variable + valor
          inicio = inicio,
          fin = fin,
          color = colores_grupos[which(grupos_unicos == grupo) %% length(colores_grupos) + 1]
        )
        
        # Actualizar índice de inicio para el siguiente grupo
        inicio <- fin + 1
      }
      
      # Agregar grupos según los índices calculados
      for (grupo_info in indices_grupos) {
        tabla_kable <- tabla_kable %>%
          kableExtra::pack_rows(
            group_label = grupo_info$nombre,
            start_row = grupo_info$inicio,
            end_row = grupo_info$fin,
            background = grupo_info$color
          )
      }
    }
  } else {
    # Para datos sin agrupar, mejorar la presentación
    # Modificar el orden para mostrar primero el título del grupo y luego los encabezados
    n_filas <- nrow(tabla)
    if (n_filas > 0) {
      # Primero crear el encabezado de grupo
      tabla_kable <- tabla_kable %>%
        kableExtra::add_header_above(c(nombre_sin_grupo = ncol(tabla))) %>%
        kableExtra::row_spec(0, background = colores_grupos[1], color = "black", 
                             font_size = 14, bold = TRUE)
      
      # Luego dar formato a los encabezados de columnas
      tabla_kable <- tabla_kable %>% 
        kableExtra::row_spec(1, bold = TRUE, background = "#f2f2f2")
    }
  }
  
  return(tabla_kable)
}