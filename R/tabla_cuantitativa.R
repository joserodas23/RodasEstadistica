#' Crea una tabla de frecuencias con intervalos de clase
#'
#' Esta función genera una tabla de frecuencias completa para una variable continua,
#' incluyendo frecuencias absolutas, relativas, acumuladas y marcas de clase.
#'
#' @param datos Vector numérico con los datos a analizar
#' @param k Número de intervalos o clases deseados (por defecto 5)
#' @param digitos Número de decimales a mostrar en los resultados (por defecto 2)
#' @param metodo Método para calcular los breaks ("sturges", "fd", "scott" o "manual")
#' @param breaks Vector de puntos de corte (solo si metodo="manual")
#' @param mostrar_tabla Lógico. Si TRUE, muestra la tabla formateada
#' @param color_encabezado Color para el encabezado de la tabla (por defecto "steelblue")
#' @param color_total Color de fondo para la fila de totales si se incluye (por defecto "#F5F5F5")
#' @param intervalo_cerrado Lógico. Si TRUE, usa formato [a b] para intervalos cerrados en ambos extremos
#' @param labels Vector de caracteres con etiquetas personalizadas para los intervalos
#' @param tabla_rayada Lógico. Si TRUE, aplica estilo rayado a las filas de la tabla (por defecto TRUE)
#' @param tabla_hover Lógico. Si TRUE, aplica efecto hover a las filas (por defecto TRUE)
#' @param ancho_completo Lógico. Si TRUE, la tabla ocupa el ancho completo (por defecto FALSE)
#' @param mostrar_porcentaje Lógico. Si TRUE, muestra el símbolo % en los porcentajes (por defecto TRUE)
#' @param mostrar_bordes Lógico. Si TRUE, muestra bordes entre columnas (por defecto TRUE)
#' @param formato_tabla Formato para la tabla: "html", "markdown", "latex" o "simple" (por defecto "html")
#' @param nombres_columnas Vector con nombres personalizados para las columnas (por defecto NULL)
#' @param titulo_tabla Título para la tabla (por defecto NULL, no muestra título)
#' @param pie_tabla Nota al pie de la tabla (por defecto NULL, no muestra pie)
#'
#' @return Un objeto de tabla formateado (kable) o una lista con los datos de la tabla y los puntos de corte
#'
#' @examples
#' # Datos de ejemplo
#' datos_ejemplo <- rnorm(100, mean = 50, sd = 10)
#'
#' # Tabla básica con 5 intervalos
#' tabla_cuantitativa(datos_ejemplo)
#'
#' # Tabla con 7 intervalos y 3 decimales
#' tabla_cuantitativa(datos_ejemplo, k = 7, digitos = 3)
#'
#' # Usar método de Freedman-Diaconis para calcular intervalos
#' tabla_cuantitativa(datos_ejemplo, metodo = "fd")
#'
#' # Definir puntos de corte manualmente
#' tabla_cuantitativa(datos_ejemplo, metodo = "manual",
#'                    breaks = c(20, 30, 40, 50, 60, 70, 80))
#'
#' # Personalizar aspecto
#' tabla_cuantitativa(datos_ejemplo, color_encabezado = "darkblue",
#'                    mostrar_porcentaje = TRUE)
#'
#' @export
tabla_cuantitativa <- function(datos,
                               k = 5,
                               digitos = 2,
                               metodo = "sturges",
                               breaks = NULL,
                               mostrar_tabla = TRUE,
                               color_encabezado = "steelblue",
                               color_total = "#F5F5F5",
                               intervalo_cerrado = FALSE,
                               labels = NULL,
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
  if (!is.numeric(datos)) {
    stop("Los datos deben ser numéricos")
  }

  if (!metodo %in% c("sturges", "fd", "scott", "manual")) {
    stop("El método debe ser 'sturges', 'fd', 'scott' o 'manual'")
  }

  if (metodo == "manual" && is.null(breaks)) {
    stop("Si el método es 'manual', debe proporcionar los puntos de corte en 'breaks'")
  }

  if (!formato_tabla %in% c("html", "markdown", "latex", "simple")) {
    stop("El formato de tabla debe ser 'html', 'markdown', 'latex' o 'simple'")
  }

  # Eliminar valores NA
  datos <- datos[!is.na(datos)]

  # Determinar puntos de corte según el método
  if (metodo == "manual") {
    puntos_corte <- breaks
  } else {
    # Si el usuario especifica k, siempre usar ese valor para el número de intervalos
    # independientemente del método
    rango <- range(datos)
    puntos_corte <- seq(from = rango[1], to = rango[2], length.out = k + 1)
  }

  # Evitar notación científica
  options(scipen = 999)

  # Crear etiquetas de intervalos
  if (metodo == "manual" && !is.null(labels)) {
    # Si se proporcionan etiquetas personalizadas, usarlas
    etiquetas_intervalos <- labels
  } else {
    # Determinar el tipo de formato de intervalo a usar
    if (intervalo_cerrado) {
      # Formato [a b]
      etiquetas_intervalos <- paste0("[", round(head(puntos_corte, -1), digitos),
                                     " ",
                                     round(tail(puntos_corte, -1), digitos), "]")
    } else {
      # Formato tradicional (a - b]
      etiquetas_intervalos <- paste0("(", round(head(puntos_corte, -1), digitos),
                                     " - ",
                                     round(tail(puntos_corte, -1), digitos), "]")
    }
  }

  # Crear intervalos y calcular frecuencias
  intervalos <- cut(datos,
                    breaks = puntos_corte,
                    labels = etiquetas_intervalos,
                    include.lowest = TRUE)

  tabla_frec <- table(intervalos)

  # Obtener marcas de clase
  marcas_clase <- hist(datos, breaks = puntos_corte, plot = FALSE)$mids

  # Crear data frame con todas las estadísticas
  tabla_final <- data.frame(
    Intervalo = names(tabla_frec),
    xi = round(marcas_clase, digitos),
    fi = as.numeric(tabla_frec),
    Fi = cumsum(as.numeric(tabla_frec)),
    hi = round((as.numeric(tabla_frec) / sum(tabla_frec)) * 100, digitos),
    Hi = round(cumsum((as.numeric(tabla_frec) / sum(tabla_frec)) * 100), digitos)
  )

  # Formatear los porcentajes si se requiere mostrar el símbolo %
  if (mostrar_porcentaje) {
    tabla_final$hi <- paste0(tabla_final$hi, "%")
    tabla_final$Hi <- paste0(tabla_final$Hi, "%")
  }

  # Usar nombres de columnas personalizados si se proporcionan
  if (is.null(nombres_columnas)) {
    nombres_columnas <- c("Intervalo", "Marca de clase", "fi", "Fi", "hi", "Hi")
  }

  # Determinar si estamos en un contexto Quarto/RMarkdown
  in_rmarkdown <- tryCatch({
    !is.null(knitr::opts_knit$get("rmarkdown.version"))
  }, error = function(e) FALSE)

  in_quarto <- tryCatch({
    !is.null(knitr::opts_knit$get("quarto.version"))
  }, error = function(e) FALSE)

  # Crear un objeto resultado que contenga la tabla y parámetros importantes
  resultado <- list(
    tabla = tabla_final,
    puntos_corte = puntos_corte,
    k = k,
    metodo = metodo,
    digitos = digitos
  )

  # Si no se requiere mostrar la tabla formateada, devolver solo el resultado
  if (!mostrar_tabla) {
    return(resultado)
  }

  # Verificar si kableExtra está disponible
  has_kableExtra <- requireNamespace("kableExtra", quietly = TRUE)

  if (!has_kableExtra) {
    warning("Se requiere el paquete 'kableExtra' para formatear tablas. Se mostrará una tabla simple.")
    # Usar knitr::kable si está disponible
    if (requireNamespace("knitr", quietly = TRUE)) {
      tabla_mostrada <- knitr::kable(
        tabla_final,
        col.names = nombres_columnas,
        format = formato_tabla,
        caption = titulo_tabla
      )

      # Devolver según el contexto
      if (in_rmarkdown || in_quarto) {
        return(tabla_mostrada)
      } else {
        print(tabla_mostrada)
        invisible(resultado)
      }
    } else {
      # Si ni siquiera knitr está disponible, mostrar la tabla básica
      print(tabla_final)
      invisible(resultado)
    }
  } else {
    # Crear la tabla formateada con kableExtra
    tabla_mostrada <- kableExtra::kbl(
      tabla_final,
      col.names = nombres_columnas,
      align = c("l", "c", "c", "c", "c", "c"),  # Primera columna a la izquierda, resto centradas
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

      # Agregar bordes a la tabla si se solicita
      if (mostrar_bordes && formato_tabla == "html") {
        for (i in 1:5) {  # Agregar bordes a las primeras 5 columnas
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
      invisible(resultado)
    }
  }
}
