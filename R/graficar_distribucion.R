#' Graficar distribuciones estadisticas con areas de rechazo
#'
#' Esta funcion permite visualizar las distribuciones Normal estandar (Z),
#' t-Student, Chi-cuadrado y F de Fisher, marcando las areas de rechazo segun
#' el nivel de confianza y tipo de prueba especificados.
#'
#' @param distribucion Tipo de distribucion a graficar, puede ser: "z" (Normal estandar),
#'        "t" (t-Student), "chi" (Chi-cuadrado) o "f" (F de Fisher).
#' @param nivel_confianza Nivel de confianza para la prueba (por defecto 0.95).
#' @param tipo_prueba Tipo de prueba a realizar: "dos_colas", "una_cola_derecha" o "una_cola_izquierda".
#'        Para distribuciones Chi-cuadrado y F, siempre se utilizara "una_cola_derecha".
#' @param color_rechazo Color para las areas de rechazo (por defecto "red").
#' @param tamanio_texto Tamanio base del texto en el grafico (por defecto 12).
#' @param gl Grados de libertad. Para t-Student y Chi-cuadrado debe ser un valor unico.
#'        Para F, debe ser un vector de dos elementos: c(gl1, gl2).
#' @param valor_cal Valor calculado del estadistico para marcar en el grafico (opcional).
#' @param xlim_personalizado Limites personalizados para el eje X. Vector de dos elementos c(min, max).
#' @param num_puntos Numero de puntos para calcular la curva (mayor precision con mas puntos).
#'
#' @return Un objeto ggplot2 con la distribucion graficada.
#'
#' @examples
#' # Distribucion Z (Normal estandar) de dos colas
#' graficar_distribucion("z", nivel_confianza = 0.95, tipo_prueba = "dos_colas")
#'
#' # Distribucion t-Student con 15 grados de libertad, prueba de una cola derecha
#' graficar_distribucion("t", nivel_confianza = 0.90, tipo_prueba = "una_cola_derecha", gl = 15)
#'
#' # Distribucion Chi-cuadrado con 5 grados de libertad
#' graficar_distribucion("chi", nivel_confianza = 0.95, gl = 5)
#'
#' # Distribucion F con 5 y 20 grados de libertad, marcando un valor calculado
#' graficar_distribucion("f", nivel_confianza = 0.95, gl = c(5, 20), valor_cal = 2.5)
#'
#' # Usar limites personalizados para el eje X
#' graficar_distribucion("chi", nivel_confianza = 0.95, gl = 1, xlim_personalizado = c(0, 8))
#'
#' @import ggplot2
#' @importFrom stats dnorm qnorm dt qt dchisq qchisq df qf median
#' @importFrom scales oob_keep
#' @export
graficar_distribucion <- function(distribucion = "z",
                                  nivel_confianza = 0.95,
                                  tipo_prueba = "dos_colas",
                                  color_rechazo = "red",
                                  tamanio_texto = 12,
                                  gl = NULL,
                                  valor_cal = NULL,
                                  xlim_personalizado = NULL,
                                  num_puntos = 1000) {

  # Validar el tipo de prueba
  tipos_validos <- c("dos_colas", "una_cola_derecha", "una_cola_izquierda")
  if (!tipo_prueba %in% tipos_validos) {
    stop("Tipo de prueba no valido. Debe ser: 'dos_colas', 'una_cola_derecha' o 'una_cola_izquierda'")
  }

  # Configurar distribucion
  switch(tolower(distribucion),
         "z" = {
           fun_densidad <- dnorm
           fun_cuantil <- qnorm
           xlim <- c(-4, 4)
           titulo <- "Distribucion Normal Estandar (Z)"
           distribucion_una_cola <- FALSE
         },
         "t" = {
           if (is.null(gl)) stop("Debe especificar gl (grados de libertad) para distribucion t")
           if (gl <= 0) stop("Los grados de libertad deben ser positivos")

           fun_densidad <- function(x) dt(x, df = gl)
           fun_cuantil <- function(p) qt(p, df = gl)
           xlim <- c(-4, 4)
           titulo <- paste("Distribucion t-Student (gl =", gl, ")")
           distribucion_una_cola <- FALSE
         },
         "chi" = {
           if (is.null(gl)) stop("Debe especificar gl (grados de libertad) para distribucion chi-cuadrado")
           if (gl <= 0) stop("Los grados de libertad deben ser positivos")

           fun_densidad <- function(x) dchisq(x, df = gl)
           fun_cuantil <- function(p) qchisq(p, df = gl)

           # Limites mejorados para Chi-cuadrado con manejo especial para gl bajos
           if (is.null(xlim_personalizado)) {
             if (gl <= 2) {
               # Para gl muy bajos, usar limites especiales
               critical_value <- qchisq(0.95, df = gl)  # Valor critico al 95%
               max_val <- max(12, qchisq(0.999, df = gl))
               xlim <- c(0, max_val)
             } else {
               max_val <- max(qchisq(0.999, df = gl), gl + 5*sqrt(2*gl))
               xlim <- c(0, max_val)
             }
           } else {
             xlim <- xlim_personalizado
           }

           titulo <- paste("Distribucion Chi-cuadrado (gl =", gl, ")")
           distribucion_una_cola <- TRUE
           # Para Chi-cuadrado, forzar a una cola derecha
           if (tipo_prueba != "una_cola_derecha") {
             message("Nota: La distribucion Chi-cuadrado solo tiene cola derecha. Se ha ajustado automaticamente.")
             tipo_prueba <- "una_cola_derecha"
           }
         },
         "f" = {
           if (is.null(gl)) stop("Debe especificar gl para distribucion F")
           if (length(gl) != 2) stop("Para distribucion F, gl debe ser un vector con dos valores: c(gl1, gl2)")
           if (any(gl <= 0)) stop("Los grados de libertad deben ser positivos")

           fun_densidad <- function(x) df(x, df1 = gl[1], df2 = gl[2])
           fun_cuantil <- function(p) qf(p, df1 = gl[1], df2 = gl[2])

           # Limites mejorados para F
           if (is.null(xlim_personalizado)) {
             max_val <- max(5, qf(0.999, gl[1], gl[2]))
             xlim <- c(0, max_val)
           } else {
             xlim <- xlim_personalizado
           }

           titulo <- paste("Distribucion F (gl1 =", gl[1], ", gl2 =", gl[2], ")")
           distribucion_una_cola <- TRUE
           # Para F, forzar a una cola derecha
           if (tipo_prueba != "una_cola_derecha") {
             message("Nota: La distribucion F de Fisher solo tiene cola derecha. Se ha ajustado automaticamente.")
             tipo_prueba <- "una_cola_derecha"
           }
         },
         stop("Distribucion debe ser: 'z', 't', 'chi' o 'f'")
  )

  # Si se proporcionaron limites personalizados para Z o t, usarlos
  if (!is.null(xlim_personalizado) && !distribucion_una_cola) {
    xlim <- xlim_personalizado
  }

  # Calcular valores criticos segun el tipo de prueba
  alpha <- 1 - nivel_confianza
  tryCatch({
    if(tipo_prueba == "dos_colas") {
      valor_critico <- round(fun_cuantil(1 - alpha/2), 5)
      valores_criticos <- c(-valor_critico, valor_critico)
    } else if(tipo_prueba == "una_cola_derecha") {
      valor_critico <- round(fun_cuantil(1 - alpha), 5)
      valores_criticos <- valor_critico
    } else {
      valor_critico <- round(fun_cuantil(alpha), 5)
      valores_criticos <- valor_critico
    }
  }, error = function(e) {
    stop(paste("Error al calcular valores criticos:", e$message))
  })

  # Crear datos para la curva con manejo especial para distribuciones de una cola con pocos gl
  points_count <- num_puntos

  # Manejar especialmente distribuciones con pocos grados de libertad
  if (distribucion_una_cola && ((tolower(distribucion) == "chi" && gl <= 2) ||
                                (tolower(distribucion) == "f" && gl[1] <= 2))) {
    # Concentrar mas puntos cerca del origen para gl pequenos
    min_val <- max(1e-6, xlim[1])  # Asegurar que no tenemos valores negativos para chi o F
    mid_val <- min(1, xlim[2]/3)

    # Evitar valores duplicados o muy cercanos
    if (mid_val - min_val > 1e-4) {
      x_dense <- seq(min_val, mid_val, length.out = round(points_count * 0.4))
      x_sparse <- seq(mid_val, xlim[2], length.out = round(points_count * 0.6))
      x <- sort(unique(c(0, x_dense, x_sparse)))
    } else {
      # Si el rango es muy pequeno, usar una secuencia simple
      x <- seq(0, xlim[2], length.out = points_count)
    }
  } else {
    x <- seq(xlim[1], xlim[2], length.out = points_count)
  }

  # Calcular valores de densidad con manejo de errores para valores extremos
  y <- sapply(x, function(val) {
    result <- tryCatch({
      fun_densidad(val)
    }, error = function(e) {
      NA  # Usar NA para valores problematicos
    }, warning = function(w) {
      NA  # Capturar advertencias tambien
    })
    return(result)
  })

  # Filtrar valores NA o infinitos
  valid_indices <- !is.na(y) & is.finite(y)
  if (sum(valid_indices) < 2) {
    stop("No hay suficientes puntos validos para crear el grafico. Intenta con otros parametros.")
  }

  x <- x[valid_indices]
  y <- y[valid_indices]

  # Limitar valores extremos de y para evitar advertencias de escala
  if (length(y) > 0) {
    y_median <- median(y)
    y_max_threshold <- y_median * 100  # Umbral razonable

    extreme_values <- y > y_max_threshold
    if (any(extreme_values)) {
      y[extreme_values] <- y_max_threshold
    }
  }

  datos <- data.frame(x = x, y = y)

  # Calcular el valor maximo de y para escalar mejor la altura
  y_max <- max(y, na.rm = TRUE)

  # Crear grafico base con aspecto mejorado
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    ggplot2::geom_line(linewidth = 1)

  # Funcion segura para areas sombreadas
  safe_fill_area <- function(plot, lim_inf, lim_sup, fun, color) {
    tryCatch({
      plot + ggplot2::stat_function(
        fun = fun,
        xlim = c(lim_inf, lim_sup),
        geom = "area",
        fill = color,
        alpha = 0.9
      )
    }, error = function(e) {
      # En caso de error, devolver el plot original
      warning(paste("No se pudo sombrear el area:", e$message))
      return(plot)
    })
  }

  # Agregar areas de rechazo segun el tipo de prueba
  if(tipo_prueba == "dos_colas") {
    p <- safe_fill_area(p, xlim[1], -valor_critico, fun_densidad, color_rechazo)
    p <- safe_fill_area(p, valor_critico, xlim[2], fun_densidad, color_rechazo)
  } else if(tipo_prueba == "una_cola_derecha") {
    p <- safe_fill_area(p, valor_critico, xlim[2], fun_densidad, color_rechazo)
  } else {
    p <- safe_fill_area(p, xlim[1], valor_critico, fun_densidad, color_rechazo)
  }

  # Agregar lineas verticales para los valores criticos
  p <- p + ggplot2::geom_vline(xintercept = valores_criticos,
                               linetype = "dashed",
                               linewidth = 0.8)

  # Mejorar la presentacion del valor observado si se proporciona
  if (!is.null(valor_cal)) {
    p <- p + ggplot2::geom_vline(xintercept = valor_cal,
                                 color = "blue",
                                 linewidth = 0.8)

    # Mejorar posicionamiento de etiquetas
    if (distribucion_una_cola) {
      # Calcular una posicion vertical proporcional al maximo, pero limitada
      y_position <- min(y_max * 0.5, y_max * 0.8)

      # Estrategia de posicionamiento mejorada para diferentes casos
      if (tolower(distribucion) == "chi" && gl <= 2) {
        # Para Chi-cuadrado con pocos gl, usar posicionamiento especial
        label_position <- max(valor_cal + 0.2, valor_cal * 1.2)
        label_position <- min(label_position, xlim[2] * 0.8) # Evitar salirse del limite

        # Verificar que la posicion esta dentro de los limites del grafico
        if (label_position >= xlim[1] && label_position <= xlim[2] &&
            y_position > 0 && y_position <= y_max * 1.1) {
          p <- p + ggplot2::annotate("label",
                                     x = label_position,
                                     y = y_position,
                                     label = paste("Cal. =", round(valor_cal, 3)),
                                     color = "blue",
                                     fill = "white",
                                     alpha = 0.7,
                                     hjust = 0,
                                     size = 3.5)
        }
      } else if (tolower(distribucion) == "f" && gl[1] <= 2) {
        # Para F con pocos gl
        label_position <- max(valor_cal + 0.2, valor_cal * 1.2)
        label_position <- min(label_position, xlim[2] * 0.8)

        # Verificar que la posicion esta dentro de los limites del grafico
        if (label_position >= xlim[1] && label_position <= xlim[2] &&
            y_position > 0 && y_position <= y_max * 1.1) {
          p <- p + ggplot2::annotate("label",
                                     x = label_position,
                                     y = y_position,
                                     label = paste("Cal. =", round(valor_cal, 3)),
                                     color = "blue",
                                     fill = "white",
                                     alpha = 0.7,
                                     hjust = 0,
                                     size = 3.5)
        }
      } else {
        # Para otros casos de distribuciones de una cola
        p <- p + ggplot2::annotate("text",
                                   x = valor_cal,
                                   y = y_position,
                                   label = paste("Cal. =", round(valor_cal, 3)),
                                   color = "blue",
                                   angle = 0,
                                   hjust = -0.1,
                                   size = 3.5)
      }
    } else {
      # Para Z y t-Student, posicionamiento adaptativo
      y_position <- y_max * 0.8
      hjust_value <- ifelse(valor_cal > 0, -0.1, 1.1)

      # Si el valor esta cerca del centro, colocar la etiqueta arriba
      if (abs(valor_cal) < 0.5) {
        y_position <- y_max * 0.9
        hjust_value <- 0.5
      }

      p <- p + ggplot2::annotate("text",
                                 x = valor_cal,
                                 y = y_position,
                                 label = paste("Cal. =", round(valor_cal, 3)),
                                 color = "blue",
                                 angle = 0,
                                 hjust = hjust_value,
                                 size = 3.5)
    }
  }

  # Funcion auxiliar para crear breaks mas inteligentes
  crear_breaks_inteligentes <- function(xlim, valor_critico, valor_cal = NULL, tipo_prueba, dist_una_cola) {
    if (dist_una_cola) {
      # Para chi y F - crear breaks mas informativos
      n_breaks <- 5

      # Para distribuciones con gl bajos, incluir mas puntos cerca del origen
      is_low_gl_case <- (tolower(distribucion) == "chi" && gl <= 2) ||
        (tolower(distribucion) == "f" && gl[1] <= 2)

      if (is_low_gl_case) {
        # Breaks no uniformes con mas detalle en valores bajos
        base_breaks <- c(0, 1, valor_critico, round(xlim[2]/2, 2), round(xlim[2], 2))

        # Si valor_cal esta definido, incluirlo
        if (!is.null(valor_cal)) {
          base_breaks <- c(base_breaks, round(valor_cal, 3))
        }

        breaks_x <- sort(unique(base_breaks))
      } else {
        breaks_x <- round(seq(0, xlim[2], length.out = n_breaks), 2)
        # Asegurar que el valor critico esta incluido
        breaks_x <- sort(unique(c(breaks_x, round(valor_critico, 2))))

        # Si valor_cal esta definido, incluirlo
        if (!is.null(valor_cal)) {
          breaks_x <- sort(unique(c(breaks_x, round(valor_cal, 2))))
        }
      }
    } else if(tipo_prueba == "dos_colas") {
      n_breaks <- 7
      breaks_x <- round(seq(xlim[1], xlim[2], length.out = n_breaks), 2)
      # Asegurar que los valores criticos y el cero estan incluidos
      breaks_x <- sort(unique(c(breaks_x, round(-valor_critico, 2), 0, round(valor_critico, 2))))

      # Si valor_cal esta definido, incluirlo
      if (!is.null(valor_cal)) {
        breaks_x <- sort(unique(c(breaks_x, round(valor_cal, 2))))
      }
    } else if(tipo_prueba == "una_cola_derecha") {
      n_breaks <- 5
      breaks_x <- round(seq(xlim[1], xlim[2], length.out = n_breaks), 2)
      breaks_x <- sort(unique(c(breaks_x, 0, round(valor_critico, 2))))

      # Si valor_cal esta definido, incluirlo
      if (!is.null(valor_cal)) {
        breaks_x <- sort(unique(c(breaks_x, round(valor_cal, 2))))
      }
    } else {
      n_breaks <- 5
      breaks_x <- round(seq(xlim[1], xlim[2], length.out = n_breaks), 2)
      breaks_x <- sort(unique(c(breaks_x, round(valor_critico, 2), 0)))

      # Si valor_cal esta definido, incluirlo
      if (!is.null(valor_cal)) {
        breaks_x <- sort(unique(c(breaks_x, round(valor_cal, 2))))
      }
    }

    # Eliminar breaks muy cercanos
    if (length(breaks_x) > 2) {
      breaks_x_ordenados <- sort(breaks_x)
      diffs <- diff(breaks_x_ordenados)
      min_diff <- min(abs(xlim[2] - xlim[1]) / 10, 0.2)

      keep <- c(TRUE, diffs > min_diff)
      breaks_x <- breaks_x_ordenados[keep]
    }

    return(breaks_x)
  }

  # Crear breaks inteligentes
  breaks_x <- crear_breaks_inteligentes(xlim, valor_critico, valor_cal, tipo_prueba, distribucion_una_cola)

  # Modificar subtitulo para distribuciones de una cola
  subtitulo <- paste("Nivel de confianza:", nivel_confianza * 100, "%")
  if (!distribucion_una_cola) {
    subtitulo <- paste(subtitulo, "-", gsub("_", " ", tipo_prueba))
  } else {
    subtitulo <- paste(subtitulo, "- una cola derecha")
  }

  # Funcion para determinar limites del eje Y
  calcular_limites_y <- function(dist, gl_value, y_max) {
    if (tolower(dist) == "chi" && gl_value <= 2) {
      # Limitar el eje Y para distribuciones chi-cuadrado con picos muy altos
      c(0, min(y_max * 0.7, 6))
    } else if (tolower(dist) == "f" && gl_value[1] <= 2) {
      # Limitar el eje Y para distribuciones F con picos muy altos
      c(0, min(y_max * 1.2, fun_densidad(0.1) * 1.5))
    } else {
      # Sin limites para otros casos
      NULL
    }
  }

  # Completar el grafico con tema mejorado
  p <- p +
    ggplot2::labs(title = titulo,
                  subtitle = subtitulo,
                  x = "",
                  y = "Densidad",
                  caption = "RDS") +

    ggplot2::scale_x_continuous(breaks = breaks_x, labels = function(x) format(x, nsmall = 2)) +

    # Ajustar el eje Y con limites apropiados y evitar advertencias
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      limits = calcular_limites_y(distribucion, gl, y_max),
      # Configurar oob para manejar datos fuera de los limites sin advertencias
      oob = scales::oob_keep
    ) +

    ggplot2::theme_minimal(base_size = tamanio_texto) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_line(color = "gray95"),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.caption = ggplot2::element_text(size = 8, color = "gray50", hjust = 1))

  # Devolver el grafico
  return(p)
}
