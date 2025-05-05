#' Crea una tabla de frecuencias para variables cualitativas
#'
#' Esta función genera una tabla de frecuencias para variables cualitativas o categóricas,
#' incluyendo frecuencias absolutas y relativas con opciones de personalización.
#' Para variables ordinales, también incluye frecuencias y porcentajes acumulados.
#'
#' @param datos Vector con los datos categóricos a analizar
#' @param ordinal Lógico. Si TRUE, la variable se considera ordinal y se calculan frecuencias acumuladas
#' @param ordenar Método para ordenar las categorías: "ninguno", "alfabetico", "frecuencia" (por defecto "ninguno")
#' @param decrecer Lógico. Si TRUE y ordenar="frecuencia", ordena de mayor a menor frecuencia
#' @param mostrar_tabla Lógico. Si TRUE, muestra la tabla formateada
#' @param color_encabezado Color para el encabezado de la tabla (por defecto "steelblue")
#' @param color_total Color de fondo para la fila de totales (por defecto "#F5F5F5")
#' @param decimales Número de decimales para los porcentajes (por defecto 2)
#' @param tabla_rayada Lógico. Si TRUE, aplica estilo rayado a las filas de la tabla (por defecto TRUE)
#' @param tabla_hover Lógico. Si TRUE, aplica efecto hover a las filas (por defecto TRUE)
#' @param ancho_completo Lógico. Si TRUE, la tabla ocupa el ancho completo (por defecto FALSE)
#' @param mostrar_porcentaje Lógico. Si TRUE, muestra el símbolo % en los porcentajes (por defecto TRUE)
#' @param mostrar_bordes Lógico. Si TRUE, muestra bordes entre columnas (por defecto TRUE)
#' @param formato_tabla Formato para la tabla: "html", "markdown", "latex" o "simple" (por defecto "html")
#' @param nombres_columnas Vector con los nombres personalizados para las columnas (por defecto NULL, se define según sea ordinal o no)
#' @param titulo_tabla Título para la tabla (por defecto NULL, no muestra título)
#' @param pie_tabla Nota al pie de la tabla (por defecto NULL, no muestra pie)
#'
#' @return Un objeto de tabla formateado (kable) o un data frame con los datos
#'
#' @examples
#' # Datos de ejemplo
#' generos <- c("Masculino", "Femenino", "Femenino", "Masculino", "Femenino",
#'             "Masculino", "Femenino", "Femenino", "Masculino", "Femenino")
#'
#' # Tabla básica
#' tabla_cualitativa(generos)
#'
#' # Variable ordinal (por ejemplo, nivel educativo)
#' niveles_educ <- c("Primaria", "Secundaria", "Universitario", "Primaria",
#'                  "Secundaria", "Universitario", "Postgrado", "Secundaria")
#' tabla_cualitativa(niveles_educ, ordinal = TRUE)
#'
#' # Personalizar aspecto
#' tabla_cualitativa(generos, color_encabezado = "darkblue",
#'                  nombres_columnas = c("Género", "Frecuencia", "Porcentaje"))
#'
#' @export
tabla_cualitativa <- function(datos,
                              ordinal = FALSE,
                              ordenar = "ninguno",
                              decrecer = TRUE,
                              mostrar_tabla = TRUE,
                              color_encabezado = "steelblue",
                              color_total = "#F5F5F5",
                              decimales = 2,
                              tabla_rayada = TRUE,
                              tabla_hover = TRUE,
                              ancho_completo = FALSE,
                              mostrar_porcentaje = TRUE,
                              mostrar_bordes = TRUE,
                              formato_tabla = "html",
                              nombres_columnas = NULL,
                              titulo_tabla = NULL,
                              pie_tabla = NULL) {

  # Definir el operador %>% localmente si magrittr no está disponible
  `%>%` <- function(lhs, rhs) {
    lhs <- force(lhs)
    if (is.function(rhs)) {
      rhs(lhs)
    } else {
      eval(substitute(rhs(lhs)), envir = parent.frame())
    }
  }

  # Validar parámetros
  if (!ordenar %in% c("ninguno", "alfabetico", "frecuencia")) {
    stop("El método de ordenación debe ser 'ninguno', 'alfabetico' o 'frecuencia'")
  }

  if (!formato_tabla %in% c("html", "markdown", "latex", "simple")) {
    stop("El formato de tabla debe ser 'html', 'markdown', 'latex' o 'simple'")
  }

  # Eliminar valores NA
  datos <- datos[!is.na(datos)]

  # Calcular tabla de frecuencias
  tabla_frec <- table(datos)

  # Ordenar según el parámetro
  if (ordenar == "alfabetico") {
    tabla_frec <- sort(tabla_frec)
  } else if (ordenar == "frecuencia") {
    if (decrecer) {
      tabla_frec <- sort(tabla_frec, decreasing = TRUE)
    } else {
      tabla_frec <- sort(tabla_frec, decreasing = FALSE)
    }
  }

  # Crear data frame con las estadísticas
  if (ordinal) {
    # Para variables ordinales, incluir frecuencias y porcentajes acumulados
    tabla_final <- data.frame(
      Categoria = names(tabla_frec),
      fi = as.numeric(tabla_frec),
      hi = round((as.numeric(tabla_frec) / sum(tabla_frec)) * 100, decimales)
    )

    # Calcular frecuencias acumuladas y porcentajes acumulados
    tabla_final$Fi <- cumsum(tabla_final$fi)
    tabla_final$Hi <- round(cumsum(tabla_final$hi), decimales)

    # Asegurar que el último porcentaje acumulado sea exactamente 100%
    if (nrow(tabla_final) > 0) {
      tabla_final$Hi[nrow(tabla_final)] <- 100
    }

    # Definir nombres de columnas predeterminados para variables ordinales
    if (is.null(nombres_columnas)) {
      nombres_columnas <- c("Categoría", "(fi)", "(hi%)", "(Fi)", "(Hi%)")
    }
  } else {
    # Para variables no ordinales, mantener la estructura original
    tabla_final <- data.frame(
      Categoria = names(tabla_frec),
      fi = as.numeric(tabla_frec),
      hi = round((as.numeric(tabla_frec) / sum(tabla_frec)) * 100, decimales)
    )

    # Definir nombres de columnas predeterminados para variables no ordinales
    if (is.null(nombres_columnas)) {
      nombres_columnas <- c("Categoría", "(fi)", "(hi%)")
    }
  }

  # Calcular totales
  total_fi <- sum(tabla_final$fi)
  # Forzar que el total sea exactamente 100%
  total_hi <- 100

  # Crear fila de totales
  if (ordinal) {
    fila_total <- c("Total", total_fi, total_hi, total_fi, total_hi)
  } else {
    fila_total <- c("Total", total_fi, total_hi)
  }

  # Agregar fila de totales al dataframe
  tabla_final <- rbind(tabla_final, fila_total)

  # Asegurar que las columnas numéricas sean numéricas
  tabla_final$fi <- as.numeric(tabla_final$fi)
  tabla_final$hi <- as.numeric(tabla_final$hi)

  # Forzar que el total sea exactamente 100%
  tabla_final$hi[nrow(tabla_final)] <- 100

  if (ordinal) {
    tabla_final$Fi <- as.numeric(tabla_final$Fi)
    tabla_final$Hi <- as.numeric(tabla_final$Hi)

    # Asegurar que el último acumulado antes de los totales sea 100%
    if (nrow(tabla_final) > 1) {
      tabla_final$Hi[nrow(tabla_final)-1] <- 100  # Penúltima fila (último valor real)
    }

    # El acumulado final también debe ser 100%
    tabla_final$Hi[nrow(tabla_final)] <- 100
  }

  # Determinar si estamos en un contexto Quarto/RMarkdown
  in_rmarkdown <- tryCatch({
    !is.null(knitr::opts_knit$get("rmarkdown.version"))
  }, error = function(e) FALSE)

  in_quarto <- tryCatch({
    !is.null(knitr::opts_knit$get("quarto.version"))
  }, error = function(e) FALSE)

  # Formatear los porcentajes si se requiere mostrar el símbolo %
  if (mostrar_porcentaje) {
    # Asegurarse que la última fila (total) sea exactamente 100%
    tabla_final$hi[nrow(tabla_final)] <- 100

    # Formatear porcentajes con símbolo %
    tabla_final$hi <- paste0(tabla_final$hi, "%")

    if (ordinal) {
      # Para variables ordinales, el acumulado también debe ser 100%
      tabla_final$Hi[nrow(tabla_final)] <- 100
      tabla_final$Hi <- paste0(tabla_final$Hi, "%")
    }
  }

  # Si no se requiere mostrar la tabla formateada, devolver el data frame
  if (!mostrar_tabla) {
    return(tabla_final)
  }

  # Verificar si kableExtra está disponible
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    warning("Se requiere el paquete 'kableExtra' para formatear tablas. Se devolverá un dataframe simple.")
    return(tabla_final)
  }

  # Crear la tabla formateada
  tabla_mostrada <- kableExtra::kbl(
    tabla_final,
    col.names = nombres_columnas,
    align = if(ordinal) c("l", "c", "c", "c", "c") else c("l", "c", "c"),  # Alineación según tipo
    format = formato_tabla,
    escape = FALSE,  # Evitar que se escape el HTML
    caption = titulo_tabla
  )

  # Aplicar estilo según el formato
  if (formato_tabla %in% c("html", "markdown", "latex")) {
    # Configurar opciones de bootstrap
    bootstrap_options <- c()
    if (tabla_rayada) bootstrap_options <- c(bootstrap_options, "striped")
    if (tabla_hover) bootstrap_options <- c(bootstrap_options, "hover")

    # Aplicar estilos básicos
    tabla_mostrada <- kableExtra::kable_styling(
      tabla_mostrada,
      bootstrap_options = bootstrap_options,
      full_width = ancho_completo,
      position = "center"
    )

    # Aplicar color al encabezado
    tabla_mostrada <- kableExtra::row_spec(
      tabla_mostrada,
      0,
      background = color_encabezado,
      color = "white",
      bold = TRUE
    )

    # Aplicar estilo a la fila de totales
    tabla_mostrada <- kableExtra::row_spec(
      tabla_mostrada,
      nrow(tabla_final),
      bold = TRUE,
      background = color_total
    )

    # Agregar bordes a la tabla si se solicita
    if (mostrar_bordes && formato_tabla == "html") {
      # Determinar número de columnas para agregar bordes
      num_cols <- if(ordinal) 4 else 2

      # Aplicar bordes entre columnas
      for (i in 1:num_cols) {
        tabla_mostrada <- kableExtra::column_spec(
          tabla_mostrada,
          i,
          border_right = TRUE
        )
      }
    }

    # Agregar nota al pie si se proporciona
    if (!is.null(pie_tabla) && formato_tabla %in% c("html", "latex")) {
      tabla_mostrada <- kableExtra::footnote(
        tabla_mostrada,
        general = pie_tabla,
        general_title = "",
        footnote_as_chunk = TRUE
      )
    }
  }

  # Mostrar la tabla según el contexto
  if (in_rmarkdown || in_quarto) {
    # Devolver el objeto para documentos R Markdown o Quarto
    return(tabla_mostrada)
  } else {
    # En la consola, imprimir la tabla
    print(tabla_mostrada)

    # Devolver invisiblemente para asignación
    invisible(tabla_final)
  }
}
