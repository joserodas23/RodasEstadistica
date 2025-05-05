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
#' @param mostrar_tabla Lógico. Si TRUE, muestra la tabla formateada con kableExtra
#' @param color_encabezado Color para el encabezado de la tabla (por defecto "steelblue")
#' @param grafico Tipo de gráfico a generar: "ninguno", "histograma", "poligono", "ojiva", o "todos"
#' @param color_grafico Color principal para los gráficos (por defecto "steelblue")
#' @param titulo_grafico Título para los gráficos (por defecto NULL, usa un título genérico)
#' @param subtitulo_grafico Subtítulo para los gráficos (por defecto NULL)
#' @param caption_grafico Caption (pie de gráfico) para los gráficos (por defecto NULL)
#' @param eje_x_nombre Nombre personalizado para el eje X (por defecto NULL, usa nombres genéricos)
#' @param eje_y_nombre Nombre personalizado para el eje Y (por defecto NULL, usa nombres genéricos)
#' @param intervalo_cerrado Lógico. Si TRUE, usa formato [a b] para intervalos cerrados en ambos extremos
#' @param labels Vector de caracteres con etiquetas personalizadas para los intervalos
#' @param formato_tabla Formato para la tabla: "html", "markdown", "latex" o "simple" (por defecto "html")
#'
#' @return Una lista con dos elementos: tabla (data frame con la tabla de frecuencias) y graficos (lista de objetos ggplot si se solicitaron)
#'
#' @examples
#' # Datos de ejemplo
#' datos_ejemplo <- rnorm(100, mean = 50, sd = 10)
#'
#' # Tabla básica con 5 intervalos
#' tabla_frecuencia(datos_ejemplo)
#'
#' # Tabla con 7 intervalos y 3 decimales
#' tabla_frecuencia(datos_ejemplo, k = 7, digitos = 3)
#'
#' # Usar método de Freedman-Diaconis para calcular intervalos
#' tabla_frecuencia(datos_ejemplo, metodo = "fd")
#'
#' # Definir puntos de corte manualmente
#' tabla_frecuencia(datos_ejemplo, metodo = "manual",
#'                  breaks = c(20, 30, 40, 50, 60, 70, 80))
#'
#' # Generar tabla con histograma
#' tabla_frecuencia(datos_ejemplo, grafico = "histograma")
#'
#' # Generar todos los gráficos con colores personalizados
#' tabla_frecuencia(datos_ejemplo, grafico = "todos",
#'                  color_grafico = "darkblue",
#'                  titulo_grafico = "Análisis de mi variable")
#'
#' # Usar intervalos cerrados en ambos extremos [a b]
#' tabla_frecuencia(datos_ejemplo, intervalo_cerrado = TRUE)
#'
#' # Usar etiquetas personalizadas
#' tabla_frecuencia(datos_ejemplo, metodo = "manual",
#'                  breaks = c(20, 30, 40, 50, 60, 70, 80),
#'                  labels = c("[0 8]", "[9 15]", "[16 25]"))
#'
#' @export
tabla_frecuencia <- function(datos,
                             k = 5,
                             digitos = 2,
                             metodo = "sturges",
                             breaks = NULL,
                             mostrar_tabla = TRUE,
                             color_encabezado = "steelblue",
                             grafico = "ninguno",
                             color_grafico = "steelblue",
                             titulo_grafico = NULL,
                             subtitulo_grafico = NULL,
                             caption_grafico = NULL,
                             eje_x_nombre = NULL,
                             eje_y_nombre = NULL,
                             intervalo_cerrado = FALSE,
                             labels = NULL,
                             formato_tabla = "html") {

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

  if (!grafico %in% c("ninguno", "histograma", "poligono", "ojiva", "todos")) {
    stop("El tipo de gráfico debe ser 'ninguno', 'histograma', 'poligono', 'ojiva' o 'todos'")
  }

  # Verificar que ggplot2 esté instalado si se solicitan gráficos
  if (grafico != "ninguno" && !requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Se requiere el paquete 'ggplot2' para generar gráficos. No se generarán gráficos.")
    grafico <- "ninguno"
  }

  # Eliminar valores NA
  datos <- datos[!is.na(datos)]

  # Determinar puntos de corte según el método
  if (metodo == "manual") {
    puntos_corte <- breaks
  } else if (metodo == "sturges") {
    # Usar exactamente k intervalos en lugar de usar la implementación de hist()
    rango <- range(datos)
    ancho <- (rango[2] - rango[1]) / k
    puntos_corte <- seq(from = rango[1], to = rango[2], by = ancho)

    # Asegurar que tengamos exactamente k+1 puntos de corte
    if (length(puntos_corte) != k + 1) {
      puntos_corte <- seq(from = rango[1], to = rango[2], length.out = k + 1)
    }
  } else if (metodo == "fd") {
    puntos_corte <- hist(datos, breaks = "FD", plot = FALSE)$breaks

    # Ajustar para tener exactamente k intervalos si es posible
    if (length(puntos_corte) - 1 != k) {
      rango <- range(datos)
      puntos_corte <- seq(from = rango[1], to = rango[2], length.out = k + 1)
    }
  } else if (metodo == "scott") {
    puntos_corte <- hist(datos, breaks = "scott", plot = FALSE)$breaks

    # Ajustar para tener exactamente k intervalos si es posible
    if (length(puntos_corte) - 1 != k) {
      rango <- range(datos)
      puntos_corte <- seq(from = rango[1], to = rango[2], length.out = k + 1)
    }
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

  # Si se solicita mostrar la tabla formateada
  if (mostrar_tabla && requireNamespace("kableExtra", quietly = TRUE)) {
    # Crear la tabla formateada
    tabla_mostrada <- kableExtra::kbl(
      tabla_final,
      col.names = c("Intervalo", "Marca de clase", "fi", "Fi", paste0("hi", "%"), paste0("Hi", "%")),
      align = c("l", "c", "c", "c", "c", "c"),  # Primera columna a la izquierda, resto centradas
      format = formato_tabla  # Usar el formato especificado
    )

    # Aplicar estilo según el formato
    if (formato_tabla %in% c("html", "markdown", "latex")) {
      tabla_mostrada <- tabla_mostrada %>%
        kableExtra::kable_styling(full_width = FALSE, position = "center") %>%
        kableExtra::row_spec(0, background = color_encabezado, color = "white")
    }

    # SIEMPRE imprimir la tabla, independientemente del entorno
    print(tabla_mostrada)
  } else if (mostrar_tabla) {
    # Si kableExtra no está disponible, mostrar la tabla básica
    print(tabla_final)
  }

  # Crear gráficos si se solicitan
  graficos <- list()

  if (grafico %in% c("histograma", "todos") && requireNamespace("ggplot2", quietly = TRUE)) {
    # Crear título para el histograma
    titulo_hist <- ifelse(is.null(titulo_grafico),
                          paste0("Histograma de frecuencias"),
                          paste0(titulo_grafico, " - Histograma"))

    # Crear histograma
    p_hist <- ggplot2::ggplot(data.frame(x = datos), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(breaks = puntos_corte,
                              fill = color_grafico,
                              color = "white",
                              alpha = 0.7) +
      ggplot2::labs(
        title = titulo_hist,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Valor", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia", eje_y_nombre)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$histograma <- p_hist

    # Mostrar el gráfico si se solicita
    if (mostrar_tabla) {
      print(p_hist)
    }
  }

  if (grafico %in% c("poligono", "todos") && requireNamespace("ggplot2", quietly = TRUE)) {
    # Crear título para el polígono de frecuencias
    titulo_pol <- ifelse(is.null(titulo_grafico),
                         paste0("Polígono de frecuencias"),
                         paste0(titulo_grafico, " - Polígono"))

    # Crear polígono de frecuencias
    p_pol <- ggplot2::ggplot(tabla_final, ggplot2::aes(x = xi, y = fi)) +
      ggplot2::geom_line(color = color_grafico, linewidth = 1) +
      ggplot2::geom_point(color = color_grafico, size = 3) +
      ggplot2::labs(
        title = titulo_pol,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia absoluta", eje_y_nombre)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$poligono <- p_pol

    # Mostrar el gráfico si se solicita
    if (mostrar_tabla) {
      print(p_pol)
    }
  }

  if (grafico %in% c("ojiva", "todos") && requireNamespace("ggplot2", quietly = TRUE)) {
    # Crear título para la ojiva
    titulo_ojiva <- ifelse(is.null(titulo_grafico),
                           paste0("Ojiva (frecuencias acumuladas)"),
                           paste0(titulo_grafico, " - Ojiva"))

    # Crear ojiva (frecuencias acumuladas)
    p_ojiva <- ggplot2::ggplot(tabla_final, ggplot2::aes(x = xi, y = Fi)) +
      ggplot2::geom_line(color = color_grafico, linewidth = 1) +
      ggplot2::geom_point(color = color_grafico, size = 3) +
      ggplot2::labs(
        title = titulo_ojiva,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia acumulada", eje_y_nombre)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$ojiva <- p_ojiva

    # Mostrar el gráfico si se solicita
    if (mostrar_tabla) {
      print(p_ojiva)
    }
  }

  # Devolver una lista con la tabla y los gráficos
  resultado <- list(
    tabla = tabla_final,
    graficos = graficos
  )

  # Si estamos en un documento que se está renderizando o si se especifica
  # no mostrar el resultado en consola, devolver solo el resultado sin imprimir estructura
  if (!mostrar_tabla || formato_tabla == "html") {
    # Retornar sin mostrar la estructura del objeto
    invisible(resultado)
  } else {
    # Para la consola, devolver visible el resultado (comportamiento tradicional)
    return(resultado)
  }
}
