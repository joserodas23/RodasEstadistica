#' Crea gráficos para visualizar datos cuantitativos agrupados en intervalos
#'
#' Esta función genera gráficos para una variable continua, incluyendo histograma,
#' polígono de frecuencias, ojiva (frecuencias acumuladas) y diagrama de cajas.
#'
#' @param datos Vector numérico con los datos a analizar o resultado de tabla_cuantitativa
#' @param k Número de intervalos o clases deseados (por defecto 5, solo si datos es un vector)
#' @param metodo Método para calcular los breaks ("sturges", "fd", "scott" o "manual") (solo si datos es un vector)
#' @param breaks Vector de puntos de corte (solo si metodo="manual" y datos es un vector)
#' @param puntos_corte Vector de puntos de corte (alternativa a breaks)
#' @param tabla_datos Data frame con la tabla de frecuencias (alternativa a datos)
#' @param grafico Tipo de gráfico a generar: "histograma", "poligono", "poligono_barras", "ojiva", "boxplot" o "todos" (por defecto "histograma")
#' @param color_grafico Color principal para los gráficos (por defecto "steelblue")
#' @param color_secundario Color secundario para elementos adicionales en los gráficos (por defecto "tomato")
#' @param titulo_grafico Título para los gráficos (por defecto NULL, usa un título genérico)
#' @param subtitulo_grafico Subtítulo para los gráficos (por defecto NULL)
#' @param caption_grafico Caption (pie de gráfico) para los gráficos (por defecto NULL)
#' @param eje_x_nombre Nombre personalizado para el eje X (por defecto NULL, usa nombres genéricos)
#' @param eje_y_nombre Nombre personalizado para el eje Y (por defecto NULL, usa nombres genéricos)
#' @param mostrar_grafico Lógico. Si TRUE, muestra el gráfico inmediatamente
#' @param tema_grafico Tema de ggplot2 a utilizar (por defecto "minimal")
#' @param mostrar_datos_boxplot Lógico. Si TRUE, muestra los puntos de datos en el diagrama de cajas (por defecto TRUE)
#' @param mostrar_media_boxplot Lógico. Si TRUE, muestra la media en el diagrama de cajas (por defecto TRUE)
#' @param formato_salida Formato para devolver los resultados: "grafico", "datos" o "ambos" (por defecto "grafico")
#'
#' @return Un objeto ggplot2 o una lista con el gráfico y los datos según formato_salida
#'
#' @examples
#' # Datos de ejemplo
#' datos_ejemplo <- rnorm(100, mean = 50, sd = 10)
#'
#' # Primero crear la tabla y luego usar los resultados para los gráficos
#' resultados_tabla <- tabla_cuantitativa(datos_ejemplo, mostrar_tabla = FALSE)
#' graficos_cuantitativos(tabla_datos = resultados_tabla$tabla,
#'                        puntos_corte = resultados_tabla$puntos_corte,
#'                        grafico = "todos")
#'
#' # O directamente usar el vector de datos
#' graficos_cuantitativos(datos_ejemplo, k = 6)  # Histograma por defecto
#'
#' # Polígono con barras
#' graficos_cuantitativos(datos_ejemplo, grafico = "poligono_barras")
#'
#' # Diagrama de cajas
#' graficos_cuantitativos(datos_ejemplo, grafico = "boxplot")
#'
#' # Personalizar título y colores
#' graficos_cuantitativos(datos_ejemplo, grafico = "todos",
#'                       color_grafico = "darkblue",
#'                       titulo_grafico = "Análisis de mi variable")
#'
#' @export
graficos_cuantitativos <- function(datos = NULL,
                                   k = 5,
                                   metodo = "sturges",
                                   breaks = NULL,
                                   puntos_corte = NULL,
                                   tabla_datos = NULL,
                                   grafico = "histograma",
                                   color_grafico = "steelblue",
                                   color_secundario = "tomato",
                                   titulo_grafico = NULL,
                                   subtitulo_grafico = NULL,
                                   caption_grafico = NULL,
                                   eje_x_nombre = NULL,
                                   eje_y_nombre = NULL,
                                   mostrar_grafico = TRUE,
                                   tema_grafico = "minimal",
                                   mostrar_datos_boxplot = TRUE,
                                   mostrar_media_boxplot = TRUE,
                                   formato_salida = "grafico") {

  # Verificar que ggplot2 esté instalado
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere el paquete 'ggplot2' para generar gráficos.")
  }

  # Validar el tipo de gráfico
  tipos_graficos_validos <- c("histograma", "poligono", "poligono_barras", "ojiva", "boxplot", "todos")
  if (!grafico %in% tipos_graficos_validos) {
    stop(paste0("El tipo de gráfico debe ser uno de los siguientes: '",
                paste(tipos_graficos_validos, collapse = "', '"), "'"))
  }

  # Verificar que tengamos los datos necesarios
  if (is.null(datos) && (is.null(tabla_datos) || is.null(puntos_corte))) {
    stop("Debe proporcionar un vector de 'datos' o ambos 'tabla_datos' y 'puntos_corte'")
  }

  # Si tenemos un vector de datos, pero no puntos de corte ni tabla, calcularlos
  if (!is.null(datos) && is.null(puntos_corte) && is.null(tabla_datos)) {
    # Validar datos
    if (!is.numeric(datos)) {
      stop("Los datos deben ser numéricos")
    }

    # Eliminar valores NA
    datos <- datos[!is.na(datos)]

    # Determinar puntos de corte según el método
    if (metodo == "manual" && !is.null(breaks)) {
      puntos_corte <- breaks
    } else {
      # Si el usuario especifica k, siempre usar ese valor para el número de intervalos,
      # pero podemos usar el método como guía inicial

      # Siempre aseguramos exactamente k intervalos
      rango <- range(datos)
      puntos_corte <- seq(from = rango[1], to = rango[2], length.out = k + 1)
    }

    # Obtener marcas de clase
    marcas_clase <- hist(datos, breaks = puntos_corte, plot = FALSE)$mids

    # Calcular frecuencias
    hist_datos <- hist(datos, breaks = puntos_corte, plot = FALSE)

    # Crear tabla de datos para los gráficos
    tabla_datos <- data.frame(
      xi = marcas_clase,
      fi = hist_datos$counts,
      Fi = cumsum(hist_datos$counts)
    )
  }

  # O si tenemos el resultado de tabla_cuantitativa
  if (is.list(datos) && !is.null(datos$tabla) && !is.null(datos$puntos_corte)) {
    tabla_datos <- datos$tabla
    puntos_corte <- datos$puntos_corte

    # Tomar los parámetros del resultado de tabla_cuantitativa si están disponibles
    if (!is.null(datos$k)) k <- datos$k
    if (!is.null(datos$metodo)) metodo <- datos$metodo
  }

  # Crear datos originales si solo tenemos tabla de frecuencias y se requiere boxplot
  datos_originales <- NULL
  if (grafico %in% c("boxplot", "todos") && !is.null(tabla_datos) && is.null(datos)) {
    # Reconstruir aproximadamente los datos originales desde la tabla de frecuencias
    # Esto es una aproximación para poder generar el boxplot
    datos_originales <- numeric(0)
    for (i in 1:nrow(tabla_datos)) {
      # Obtener valores aproximados basados en la marca de clase
      if (i < length(puntos_corte)) {
        valores_intervalo <- rep(tabla_datos$xi[i], tabla_datos$fi[i])
        datos_originales <- c(datos_originales, valores_intervalo)
      }
    }
  } else if (!is.null(datos)) {
    # Si tenemos los datos originales, usarlos
    datos_originales <- datos
  }

  # Determinar el tema de ggplot2 a utilizar
  tema <- switch(tema_grafico,
                 "minimal" = ggplot2::theme_minimal(),
                 "classic" = ggplot2::theme_classic(),
                 "bw" = ggplot2::theme_bw(),
                 "light" = ggplot2::theme_light(),
                 "dark" = ggplot2::theme_dark(),
                 ggplot2::theme_minimal())  # Valor por defecto si no coincide

  # Crear lista para almacenar los gráficos
  graficos <- list()

  # Histograma
  if (grafico %in% c("histograma", "todos")) {
    # Crear título para el histograma
    titulo_hist <- ifelse(is.null(titulo_grafico),
                          paste0("Histograma de frecuencias"),
                          paste0(titulo_grafico, " - Histograma"))

    # Si tenemos un vector de datos, usar geom_histogram
    if (!is.null(datos_originales) && is.vector(datos_originales)) {
      p_hist <- ggplot2::ggplot(data.frame(x = datos_originales), ggplot2::aes(x = x)) +
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
        tema +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
        )
    } else {
      # Si solo tenemos la tabla, usar geom_col
      p_hist <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = fi)) +
        ggplot2::geom_col(fill = color_grafico,
                          color = "white",
                          alpha = 0.7,
                          width = diff(puntos_corte)[1] * 0.9) +
        ggplot2::labs(
          title = titulo_hist,
          subtitle = subtitulo_grafico,
          caption = caption_grafico,
          x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
          y = ifelse(is.null(eje_y_nombre), "Frecuencia", eje_y_nombre)
        ) +
        tema +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
        )
    }

    graficos$histograma <- p_hist

    # Mostrar el gráfico si se solicita
    if (mostrar_grafico && grafico == "histograma") {
      print(p_hist)
    }
  }

  # Polígono de frecuencias
  if (grafico %in% c("poligono", "todos")) {
    # Crear título para el polígono de frecuencias
    titulo_pol <- ifelse(is.null(titulo_grafico),
                         paste0("Polígono de frecuencias"),
                         paste0(titulo_grafico, " - Polígono"))

    # Crear polígono de frecuencias
    p_pol <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = fi)) +
      ggplot2::geom_line(color = color_grafico, linewidth = 1) +
      ggplot2::geom_point(color = color_grafico, size = 3) +
      ggplot2::labs(
        title = titulo_pol,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia absoluta", eje_y_nombre)
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$poligono <- p_pol

    # Mostrar el gráfico si se solicita
    if (mostrar_grafico && grafico == "poligono") {
      print(p_pol)
    }
  }

  # Polígono de frecuencias con barras
  if (grafico %in% c("poligono_barras", "todos")) {
    # Crear título para el polígono de frecuencias con barras
    titulo_polbar <- ifelse(is.null(titulo_grafico),
                            paste0("Polígono de frecuencias con barras"),
                            paste0(titulo_grafico, " - Polígono con barras"))

    # Crear polígono de frecuencias con barras
    p_polbar <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = fi)) +
      ggplot2::geom_col(fill = color_grafico, alpha = 0.5,
                        width = diff(puntos_corte)[1] * 0.9,
                        color = "white") +
      ggplot2::geom_line(color = color_secundario, linewidth = 1.2) +
      ggplot2::geom_point(color = color_secundario, size = 3) +
      ggplot2::labs(
        title = titulo_polbar,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia absoluta", eje_y_nombre)
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$poligono_barras <- p_polbar

    # Mostrar el gráfico si se solicita
    if (mostrar_grafico && grafico == "poligono_barras") {
      print(p_polbar)
    }
  }

  # Ojiva (frecuencias acumuladas)
  if (grafico %in% c("ojiva", "todos")) {
    # Crear título para la ojiva
    titulo_ojiva <- ifelse(is.null(titulo_grafico),
                           paste0("Ojiva (frecuencias acumuladas)"),
                           paste0(titulo_grafico, " - Ojiva"))

    # Crear ojiva (frecuencias acumuladas)
    p_ojiva <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = Fi)) +
      ggplot2::geom_line(color = color_grafico, linewidth = 1) +
      ggplot2::geom_point(color = color_grafico, size = 3) +
      ggplot2::labs(
        title = titulo_ojiva,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia acumulada", eje_y_nombre)
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$ojiva <- p_ojiva

    # Mostrar el gráfico si se solicita
    if (mostrar_grafico && grafico == "ojiva") {
      print(p_ojiva)
    }
  }

  # Diagrama de cajas (boxplot)
  if (grafico %in% c("boxplot", "todos") && !is.null(datos_originales)) {
    # Crear título para el diagrama de cajas
    titulo_box <- ifelse(is.null(titulo_grafico),
                         paste0("Diagrama de cajas"),
                         paste0(titulo_grafico, " - Diagrama de cajas"))

    # Crear base del diagrama de cajas
    p_box <- ggplot2::ggplot(data.frame(valor = datos_originales), ggplot2::aes(y = valor)) +
      ggplot2::geom_boxplot(fill = color_grafico, alpha = 0.7, width = 0.5) +
      ggplot2::coord_flip() +  # Voltear el boxplot horizontalmente
      ggplot2::labs(
        title = titulo_box,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        y = ifelse(is.null(eje_x_nombre), "Valor", eje_x_nombre),
        x = NULL  # No mostrar etiqueta en eje x
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
        axis.text.y = ggplot2::element_blank(),  # Ocultar etiquetas del eje Y
        axis.ticks.y = ggplot2::element_blank()  # Ocultar marcas del eje Y
      )

    # Añadir puntos si se solicita
    if (mostrar_datos_boxplot) {
      p_box <- p_box +
        ggplot2::geom_jitter(width = 0.2, height = 0, alpha = 0.5,
                             color = color_secundario, size = 1.5)
    }

    # Añadir media si se solicita
    if (mostrar_media_boxplot) {
      # Calcular media
      media_valor <- mean(datos_originales, na.rm = TRUE)

      p_box <- p_box +
        ggplot2::geom_hline(yintercept = media_valor,
                            color = color_secundario,
                            linetype = "dashed",
                            linewidth = 1) +
        ggplot2::annotate("text",
                          x = 0.6,
                          y = media_valor,
                          label = paste("Media:", round(media_valor, 2)),
                          color = color_secundario,
                          hjust = 0)
    }

    graficos$boxplot <- p_box

    # Mostrar el gráfico si se solicita
    if (mostrar_grafico && grafico == "boxplot") {
      print(p_box)
    }
  }

  # Si se solicitan todos los gráficos y mostrar_grafico es TRUE, mostrarlos
  if (mostrar_grafico && grafico == "todos") {
    # Crear un arreglo de gráficos si está disponible gridExtra
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      # Determinar cuántos gráficos hay
      n_graficos <- length(graficos)

      # Determinar la disposición (layout) de los gráficos
      if (n_graficos <= 2) {
        # Si hay 1 o 2 gráficos, ponerlos en una fila
        n_cols <- n_graficos
        n_rows <- 1
      } else if (n_graficos <= 4) {
        # Si hay 3 o 4 gráficos, ponerlos en una cuadrícula 2x2
        n_cols <- 2
        n_rows <- ceiling(n_graficos / 2)
      } else {
        # Si hay más de 4 gráficos, usar 3 columnas
        n_cols <- 3
        n_rows <- ceiling(n_graficos / 3)
      }

      # Crear el arreglo de gráficos
      grid <- do.call(gridExtra::grid.arrange,
                      c(graficos,
                        list(ncol = n_cols,
                             nrow = n_rows,
                             top = ifelse(is.null(titulo_grafico),
                                          "Análisis gráfico de los datos",
                                          titulo_grafico))))

      # Imprimir el arreglo de gráficos
      print(grid)
    } else {
      # Si gridExtra no está disponible, imprimir los gráficos uno por uno
      for (g in graficos) {
        print(g)
      }
    }
  }

  # Determinar qué devolver según formato_salida
  if (formato_salida == "datos") {
    return(list(
      tabla_datos = tabla_datos,
      puntos_corte = puntos_corte,
      datos_originales = datos_originales
    ))
  } else if (formato_salida == "ambos") {
    return(list(
      graficos = graficos,
      tabla_datos = tabla_datos,
      puntos_corte = puntos_corte,
      datos_originales = datos_originales
    ))
  } else {
    # Por defecto, devolver los gráficos
    if (grafico == "todos") {
      return(graficos)
    } else {
      return(graficos[[grafico]])
    }
  }
}
