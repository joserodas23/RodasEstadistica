#' Crear una tabla descriptiva a partir de un resumen estadístico
#'
#' Esta función toma un dataframe de resumen estadístico (creado previamente con group_by y summarise)
#' y lo convierte en una tabla formateada con kableExtra, con grupos según la primera variable de agrupación.
#'
#' @param resumen Un dataframe que contiene estadísticas descriptivas creado previamente con group_by y summarise.
#'   Debe contener al menos una variable de agrupación y métricas estadísticas como n, media, etc.
#' @param columnas_renombrar Un vector con nombres para renombrar las columnas.
#'   Los valores del vector deben corresponder con las columnas existentes en el dataframe,
#'   y los nombres del vector serán los nuevos nombres de columnas.
#' @param variable_grupo Nombre de la columna de agrupación principal (por defecto, la primera columna del dataframe).
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
#' # Primero se crea el resumen estadístico con dplyr
#' library(dplyr)
#' 
#' resumen <- datos %>%
#'   group_by(Temperatura, Cultivo) %>%
#'   summarise(
#'     n = n(),
#'     media = mean(Rendimiento),
#'     mediana = median(Rendimiento),
#'     desv_est = sd(Rendimiento),
#'     min = min(Rendimiento),
#'     max = max(Rendimiento),
#'     rango = max - min,
#'     q1 = quantile(Rendimiento, 0.25),
#'     q3 = quantile(Rendimiento, 0.75),
#'     iqr = q3 - q1,
#'     cv = sd(Rendimiento)/mean(Rendimiento)*100,
#'     .groups = 'drop'
#'   )
#' 
#' # Luego se crea la tabla descriptiva
#' columnas_nuevas <- c(
#'   "N" = "n", 
#'   "Media" = "media", 
#'   "Mediana" = "mediana",
#'   "D.E." = "desv_est",
#'   "Mínimo" = "min",
#'   "Máximo" = "max",
#'   "Rango" = "rango",
#'   "Q1" = "q1",
#'   "Q3" = "q3",
#'   "IQR" = "iqr",
#'   "CV (%)" = "cv"
#' )
#' 
#' tabla_descriptivo(
#'   resumen = resumen,
#'   columnas_renombrar = columnas_nuevas,
#'   caption = "Estadísticas descriptivas del rendimiento por temperatura y cultivo"
#' )
#' }
#'
#' @export
tabla_descriptivo <- function(resumen, 
                              columnas_renombrar = NULL,
                              variable_grupo = NULL,
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
  
  # Si no se especifica variable_grupo, usar la primera columna
  if (is.null(variable_grupo)) {
    variable_grupo <- colnames(tabla)[1]
  }
  
  # Verificar que la variable de agrupación existe
  if (!(variable_grupo %in% colnames(tabla))) {
    stop(paste("La variable de agrupación", variable_grupo, "no existe en el dataframe"))
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
  
  # Crear la tabla base con kable
  tabla_kable <- tabla %>%
    kableExtra::kable(
      format = formato,
      caption = caption,
      align = c(rep("l", 2), rep("c", ncol(tabla) - 2))
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center"
    ) %>%
    kableExtra::column_spec(1:2, bold = TRUE)  # Destacar primeras dos columnas
  
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
  
  return(tabla_kable)
}