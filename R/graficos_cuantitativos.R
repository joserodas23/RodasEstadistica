#' Crea gráficos para visualizar datos cuantitativos agrupados en intervalos
#'
#' Esta función genera gráficos para una variable continua, incluyendo histograma,
#' polígono de frecuencias, ojiva (frecuencias acumuladas), diagrama de cajas,
#' gráfico de densidad, Q-Q plot y diagrama de violín.
#'
#' @param datos Vector numérico con los datos a analizar o resultado de tabla_cuantitativa
#' @param k Número de intervalos o clases deseados (por defecto 5, solo si datos es un vector)
#' @param metodo Método para calcular los breaks ("sturges", "fd", "scott" o "manual") (solo si datos es un vector)
#' @param breaks Vector de puntos de corte (solo si metodo="manual" y datos es un vector)
#' @param puntos_corte Vector de puntos de corte (alternativa a breaks)
#' @param tabla_datos Data frame con la tabla de frecuencias (alternativa a datos)
#' @param grafico Tipo de gráfico: "histograma", "poligono", "poligono_barras", "ojiva", "boxplot", "densidad", "qqplot", "violin" o "todos"
#' @param color_grafico Color principal para los gráficos (por defecto "steelblue")
#' @param color_secundario Color secundario para elementos adicionales (por defecto "tomato")
#' @param titulo_grafico Título para los gráficos (por defecto NULL, usa un título genérico)
#' @param subtitulo_grafico Subtítulo para los gráficos (por defecto NULL)
#' @param caption_grafico Caption (pie de gráfico) para los gráficos (por defecto NULL)
#' @param eje_x_nombre Nombre personalizado para el eje X (por defecto NULL)
#' @param eje_y_nombre Nombre personalizado para el eje Y (por defecto NULL)
#' @param mostrar_grafico Lógico. Si TRUE, muestra el gráfico inmediatamente
#' @param tema_grafico Tema de ggplot2: "minimal", "classic", "bw", "light", "dark" (por defecto "minimal")
#' @param mostrar_datos_boxplot Lógico. Si TRUE, muestra los puntos de datos en el diagrama de cajas
#' @param mostrar_media_boxplot Lógico. Si TRUE, muestra la media en el diagrama de cajas
#' @param grid_ncol Número de columnas en la grilla de gráficos (por defecto NULL, automático)
#' @param formato_salida Formato para devolver: "grafico", "datos" o "ambos" (por defecto "grafico")
#'
#' @return Un objeto ggplot2 o una lista con el gráfico y los datos según formato_salida
#'
#' @examples
#' # Datos de ejemplo
#' datos_ejemplo <- rnorm(100, mean = 50, sd = 10)
#'
#' # Histograma básico
#' graficos_cuantitativos(datos_ejemplo)
#'
#' # Todos los gráficos
#' graficos_cuantitativos(datos_ejemplo, grafico = "todos")
#'
#' # Polígono con barras
#' graficos_cuantitativos(datos_ejemplo, grafico = "poligono_barras")
#'
#' # Diagrama de cajas personalizado
#' graficos_cuantitativos(datos_ejemplo, grafico = "boxplot",
#'                       color_grafico = "darkgreen",
#'                       titulo_grafico = "Distribución de datos")
#'
#' # Gráfico Q-Q para verificar normalidad
#' graficos_cuantitativos(datos_ejemplo, grafico = "qqplot")
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
                                   grid_ncol = NULL,
                                   formato_salida = "grafico") {

  # Verificar que ggplot2 esté instalado
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere el paquete 'ggplot2' para generar gráficos.")
  }

  # Validar el tipo de gráfico
  tipos_graficos_validos <- c("histograma", "poligono", "poligono_barras",
                              "ojiva", "boxplot", "densidad", "qqplot",
                              "violin", "todos")
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

  # Crear datos originales si solo tenemos tabla de frecuencias
  datos_originales <- NULL
  if (!is.null(tabla_datos) && is.null(datos)) {
    # Reconstruir aproximadamente los datos originales desde la tabla de frecuencias
    datos_originales <- numeric(0)
    for (i in 1:nrow(tabla_datos)) {
      if (i < length(puntos_corte)) {
        valores_intervalo <- rep(tabla_datos$xi[i], tabla_datos$fi[i])
        datos_originales <- c(datos_originales, valores_intervalo)
      }
    }
  } else if (!is.null(datos)) {
    datos_originales <- datos
  }

  # Determinar el tema de ggplot2 a utilizar
  tema <- switch(tema_grafico,
                 "minimal" = ggplot2::theme_minimal(),
                 "classic" = ggplot2::theme_classic(),
                 "bw"      = ggplot2::theme_bw(),
                 "light"   = ggplot2::theme_light(),
                 "dark"    = ggplot2::theme_dark(),
                 ggplot2::theme_minimal())

  # Crear lista para almacenar los gráficos
  graficos <- list()

  # ── HISTOGRAMA ────────────────────────────────────────────────────────────────
  if (grafico %in% c("histograma", "todos")) {
    titulo_hist <- ifelse(is.null(titulo_grafico),
                          "Histograma de frecuencias",
                          paste0(titulo_grafico, " - Histograma"))

    if (!is.null(datos_originales) && is.vector(datos_originales)) {
      p_hist <- ggplot2::ggplot(data.frame(x = datos_originales), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(breaks = puntos_corte,
                                fill = color_grafico,
                                color = "white",
                                alpha = 0.7) +
        ggplot2::labs(
          title    = titulo_hist,
          subtitle = subtitulo_grafico,
          caption  = caption_grafico,
          x = ifelse(is.null(eje_x_nombre), "Valor", eje_x_nombre),
          y = ifelse(is.null(eje_y_nombre), "Frecuencia", eje_y_nombre)
        ) +
        tema +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          plot.caption  = ggplot2::element_text(hjust = 0, face = "italic")
        )
    } else {
      p_hist <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = fi)) +
        ggplot2::geom_col(fill = color_grafico,
                          color = "white",
                          alpha = 0.7,
                          width = diff(puntos_corte)[1] * 0.9) +
        ggplot2::labs(
          title    = titulo_hist,
          subtitle = subtitulo_grafico,
          caption  = caption_grafico,
          x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
          y = ifelse(is.null(eje_y_nombre), "Frecuencia", eje_y_nombre)
        ) +
        tema +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          plot.caption  = ggplot2::element_text(hjust = 0, face = "italic")
        )
    }

    graficos$histograma <- p_hist
  }

  # ── POLÍGONO DE FRECUENCIAS ───────────────────────────────────────────────────
  if (grafico %in% c("poligono", "todos")) {
    titulo_pol <- ifelse(is.null(titulo_grafico),
                         "Polígono de frecuencias",
                         paste0(titulo_grafico, " - Polígono"))

    p_pol <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = fi)) +
      ggplot2::geom_line(color = color_grafico, linewidth = 1) +
      ggplot2::geom_point(color = color_grafico, size = 3) +
      ggplot2::labs(
        title    = titulo_pol,
        subtitle = subtitulo_grafico,
        caption  = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia absoluta", eje_y_nombre)
      ) +
      tema +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption  = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$poligono <- p_pol
  }

  # ── POLÍGONO CON BARRAS ───────────────────────────────────────────────────────
  if (grafico %in% c("poligono_barras", "todos")) {
    titulo_polbar <- ifelse(is.null(titulo_grafico),
                            "Polígono de frecuencias con barras",
                            paste0(titulo_grafico, " - Polígono con barras"))

    p_polbar <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = fi)) +
      ggplot2::geom_col(fill = color_grafico, alpha = 0.5,
                        width = diff(puntos_corte)[1] * 0.9,
                        color = "white") +
      ggplot2::geom_line(color = color_secundario, linewidth = 1.2) +
      ggplot2::geom_point(color = color_secundario, size = 3) +
      ggplot2::labs(
        title    = titulo_polbar,
        subtitle = subtitulo_grafico,
        caption  = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia absoluta", eje_y_nombre)
      ) +
      tema +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption  = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$poligono_barras <- p_polbar
  }

  # ── OJIVA ─────────────────────────────────────────────────────────────────────
  if (grafico %in% c("ojiva", "todos")) {
    titulo_ojiva <- ifelse(is.null(titulo_grafico),
                           "Ojiva (frecuencias acumuladas)",
                           paste0(titulo_grafico, " - Ojiva"))

    p_ojiva <- ggplot2::ggplot(tabla_datos, ggplot2::aes(x = xi, y = Fi)) +
      ggplot2::geom_line(color = color_grafico, linewidth = 1) +
      ggplot2::geom_point(color = color_grafico, size = 3) +
      ggplot2::labs(
        title    = titulo_ojiva,
        subtitle = subtitulo_grafico,
        caption  = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Marca de clase", eje_x_nombre),
        y = ifelse(is.null(eje_y_nombre), "Frecuencia acumulada", eje_y_nombre)
      ) +
      tema +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption  = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$ojiva <- p_ojiva
  }

  # ── BOXPLOT ───────────────────────────────────────────────────────────────────
  # ── BOXPLOT ───────────────────────────────────────────────────────────────────
  if (grafico %in% c("boxplot", "todos") && !is.null(datos_originales)) {
    titulo_box <- ifelse(is.null(titulo_grafico),
                         "Diagrama de cajas",
                         paste0(titulo_grafico, " - Diagrama de cajas"))

    p_box <- ggplot2::ggplot(data.frame(x = "", valor = datos_originales),
                             ggplot2::aes(x = x, y = valor)) +
      ggplot2::geom_boxplot(fill = color_grafico, alpha = 0.7, width = 0.5) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title    = titulo_box,
        subtitle = subtitulo_grafico,
        caption  = caption_grafico,
        y = ifelse(is.null(eje_x_nombre), "Valor", eje_x_nombre),
        x = NULL
      ) +
      tema +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption  = ggplot2::element_text(hjust = 0, face = "italic"),
        axis.text.y   = ggplot2::element_blank(),
        axis.ticks.y  = ggplot2::element_blank()
      )

    if (mostrar_datos_boxplot) {
      p_box <- p_box +
        ggplot2::geom_jitter(ggplot2::aes(x = x, y = valor),  # ← AÑADIR aes() aquí
                             width = 0.2, height = 0, alpha = 0.5,
                             color = color_secundario, size = 1.5)
    }

    if (mostrar_media_boxplot) {
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
  }
  # ── GRÁFICO DE DENSIDAD ───────────────────────────────────────────────────────
  if (grafico %in% c("densidad", "todos") && !is.null(datos_originales)) {
    titulo_dens <- ifelse(is.null(titulo_grafico),
                          "Gráfico de densidad",
                          paste0(titulo_grafico, " - Densidad"))

    p_dens <- ggplot2::ggplot(data.frame(x = datos_originales),
                              ggplot2::aes(x = x)) +
      ggplot2::geom_density(fill = color_grafico, alpha = 0.5,
                            color = color_grafico, linewidth = 1) +
      ggplot2::labs(
        title = titulo_dens,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        x = ifelse(is.null(eje_x_nombre), "Valor", eje_x_nombre),
        y = "Densidad"
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$densidad <- p_dens
  }

  # ── GRÁFICO Q-Q ───────────────────────────────────────────────────────────────
  if (grafico %in% c("qqplot", "todos") && !is.null(datos_originales)) {
    titulo_qq <- ifelse(is.null(titulo_grafico),
                        "Gráfico Q-Q (Normalidad)",
                        paste0(titulo_grafico, " - Q-Q"))

    p_qq <- ggplot2::ggplot(data.frame(x = datos_originales),
                            ggplot2::aes(sample = x)) +
      ggplot2::stat_qq(color = color_grafico, size = 2) +
      ggplot2::stat_qq_line(color = color_secundario, linewidth = 1) +
      ggplot2::labs(
        title = titulo_qq,
        subtitle = "Los puntos deben seguir la línea si los datos son normales",
        caption = caption_grafico,
        x = "Cuantiles teóricos",
        y = "Cuantiles de la muestra"
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic")
      )

    graficos$qqplot <- p_qq
  }

  # ── DIAGRAMA DE VIOLÍN ────────────────────────────────────────────────────────
  if (grafico %in% c("violin", "todos") && !is.null(datos_originales)) {
    titulo_violin <- ifelse(is.null(titulo_grafico),
                            "Diagrama de violín",
                            paste0(titulo_grafico, " - Violín"))

    p_violin <- ggplot2::ggplot(data.frame(x = "", valor = datos_originales),
                                ggplot2::aes(x = x, y = valor)) +
      ggplot2::geom_violin(fill = color_grafico, alpha = 0.5) +
      ggplot2::geom_boxplot(width = 0.1, fill = color_secundario, alpha = 0.7) +
      ggplot2::labs(
        title = titulo_violin,
        subtitle = subtitulo_grafico,
        caption = caption_grafico,
        y = ifelse(is.null(eje_x_nombre), "Valor", eje_x_nombre),
        x = NULL
      ) +
      tema +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )

    graficos$violin <- p_violin
  }

  # ── MOSTRAR GRÁFICO(S) ────────────────────────────────────────────────────────
  if (mostrar_grafico) {
    if (grafico == "todos") {
      # Mostrar todos juntos con gridExtra si está disponible
      if (requireNamespace("gridExtra", quietly = TRUE)) {
        n_graficos <- length(graficos)

        if (is.null(grid_ncol)) {
          # Lógica automática
          if (n_graficos <= 2) {
            n_cols <- n_graficos; n_rows <- 1
          } else if (n_graficos <= 4) {
            n_cols <- 2; n_rows <- ceiling(n_graficos / 2)
          } else {
            n_cols <- 3; n_rows <- ceiling(n_graficos / 3)
          }
        } else {
          n_cols <- grid_ncol
          n_rows <- ceiling(n_graficos / n_cols)
        }

        # gridExtra::grid.arrange ya imprime automáticamente, NO usar print()
        do.call(gridExtra::grid.arrange,
                c(graficos,
                  list(ncol = n_cols,
                       nrow = n_rows,
                       top  = ifelse(is.null(titulo_grafico),
                                     "Análisis gráfico de los datos",
                                     titulo_grafico))))
      } else {
        # Sin gridExtra: imprimir uno por uno
        for (g in graficos) print(g)
      }
    } else {
      # Imprimir solo el gráfico individual solicitado
      print(graficos[[grafico]])
    }
  }

  # ── RETORNO ───────────────────────────────────────────────────────────────────
  if (formato_salida == "datos") {
    return(invisible(list(
      tabla_datos      = tabla_datos,
      puntos_corte     = puntos_corte,
      datos_originales = datos_originales
    )))
  } else if (formato_salida == "ambos") {
    return(invisible(list(
      graficos         = graficos,
      tabla_datos      = tabla_datos,
      puntos_corte     = puntos_corte,
      datos_originales = datos_originales
    )))
  } else {
    # Devolver invisible para evitar auto-impresión
    if (grafico == "todos") {
      return(invisible(graficos))
    } else {
      return(invisible(graficos[[grafico]]))
    }
  }
}
