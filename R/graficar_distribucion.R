#' Grafica una distribucion estadistica con areas de rechazo y valores criticos
#'
#' @param distribucion Tipo de distribucion: "z", "t", "chi" o "f"
#' @param nivel_confianza Valor entre 0 y 1 que representa el nivel de confianza
#' @param tipo_prueba Tipo de prueba: "dos_colas", "una_cola_derecha", "una_cola_izquierda" o "intervalo_confianza"
#' @param color_rechazo Color para las areas de rechazo
#' @param tamanio_texto Tamanio de texto base para el grafico
#' @param gl Grados de libertad: un valor para t y chi, y vector c(gl1, gl2) para F
#' @param valor_cal Valor calculado para marcar en el grafico (opcional)
#' @param xlim_personalizado Vector c(min, max) para definir limites personalizados en eje X
#' @param num_puntos Numero de puntos para generar la curva
#' @param rotar_etiquetas_x Logico: si TRUE, rota las etiquetas del eje X para evitar sobreposicion
#' @param tamanio_etiqueta_cal Tamanio de la etiqueta del valor calculado
#'
#' @return Un objeto ggplot con el grafico de la distribucion
#' @export
#'
#' @examples
#' # Distribucion Z, dos colas, 95% de confianza
#' graficar_distribucion("z", nivel_confianza = 0.95, tipo_prueba = "dos_colas")
#'
#' # Distribucion t-Student con 10 grados de libertad y valor calculado
#' graficar_distribucion("t", gl = 10, valor_cal = 2.5, tipo_prueba = "una_cola_derecha")
graficar_distribucion <- function(distribucion = "z",
                                  nivel_confianza = 0.95,
                                  tipo_prueba = "dos_colas",
                                  color_rechazo = "red",
                                  tamanio_texto = 12,
                                  gl = NULL,
                                  valor_cal = NULL,
                                  xlim_personalizado = NULL,
                                  num_puntos = 1500,
                                  rotar_etiquetas_x = TRUE,
                                  tamanio_etiqueta_cal = 3.5) {

  # Verificar y cargar dependencias necesarias
  cargar_dependencias <- function() {
    # Lista de paquetes requeridos
    paquetes_requeridos <- c("ggplot2", "scales")

    # Comprobar cada paquete e instalarlo si es necesario
    paquetes_a_instalar <- paquetes_requeridos[!sapply(paquetes_requeridos, requireNamespace, quietly = TRUE)]

    if (length(paquetes_a_instalar) > 0) {
      mensaje <- paste("Instalando paquetes necesarios:", paste(paquetes_a_instalar, collapse = ", "))
      message(mensaje)
      install.packages(paquetes_a_instalar, repos = "https://cloud.r-project.org/")
    }

    # Cargar cada paquete
    for (pkg in paquetes_requeridos) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }

    # Verificar que se han cargado
    paquetes_cargados <- sapply(paquetes_requeridos, require, character.only = TRUE)
    if (!all(paquetes_cargados)) {
      paquetes_faltantes <- paquetes_requeridos[!paquetes_cargados]
      stop(paste("No se pudieron cargar los siguientes paquetes necesarios:",
                 paste(paquetes_faltantes, collapse = ", ")))
    }
  }

  # Intentar cargar las dependencias
  tryCatch({
    cargar_dependencias()
  }, error = function(e) {
    stop(paste("Error al cargar dependencias: ", e$message,
               "\nPor favor, instale manualmente los paquetes 'ggplot2' y 'scales' antes de usar esta función."))
  })

  # Validar el tipo de prueba
  tipos_validos <- c("dos_colas", "una_cola_derecha", "una_cola_izquierda", "intervalo_confianza")
  if (!tipo_prueba %in% tipos_validos) {
    stop("Tipo de prueba no valido. Debe ser: 'dos_colas', 'una_cola_derecha', 'una_cola_izquierda' o 'intervalo_confianza'")
  }

  #Configuración de distribuciones
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
           # Para Chi-cuadrado, forzar a una cola derecha para pruebas de hipotesis
           # Pero permitir intervalo_confianza
           if (tipo_prueba != "una_cola_derecha" && tipo_prueba != "intervalo_confianza") {
             message("Nota: Para pruebas de hipotesis, la distribucion Chi-cuadrado solo tiene cola derecha. Se ha ajustado automaticamente.")
             tipo_prueba <- "una_cola_derecha"
           }
         },

         #Configuración específica para distribución F (con mejoras para GL altos)
         "f" = {
           if (is.null(gl)) stop("Debe especificar gl para distribucion F")
           if (length(gl) != 2) stop("Para distribucion F, gl debe ser un vector con dos valores: c(gl1, gl2)")
           if (any(gl <= 0)) stop("Los grados de libertad deben ser positivos")

           # Función de densidad estándar para todos los casos
           fun_densidad <- function(x) df(x, df1 = gl[1], df2 = gl[2])
           fun_cuantil <- function(p) qf(p, df1 = gl[1], df2 = gl[2])

           # Limites mejorados para F con soporte para valores grandes de valor_cal
           if (is.null(xlim_personalizado)) {
             # Calcular valor máximo apropiado
             vc_99 <- qf(0.999, gl[1], gl[2])

             # Valor predeterminado para el límite superior
             max_val_for_display <- max(5, vc_99 * 1.5)

             # Si existe un valor_cal, asegurarse de que esté dentro del rango visible
             if (!is.null(valor_cal)) {
               max_val_for_display <- max(max_val_for_display, valor_cal * 1.2)  # Añadir margen de 20%
             }

             # Para valores altos de gl (>30), la distribución se vuelve más concentrada
             if (gl[1] >= 30 && gl[2] >= 30 && is.null(valor_cal)) {
               # Usar un rango más estrecho para visualizar mejor
               max_val_for_display <- min(5, max(2, vc_99 * 1.5))
             }

             xlim <- c(0, max_val_for_display)
           } else {
             xlim <- xlim_personalizado
           }

           titulo <- paste("Distribucion F (gl1 =", gl[1], ", gl2 =", gl[2], ")")
           distribucion_una_cola <- TRUE
           # Para F, forzar a una cola derecha para pruebas de hipotesis
           # Pero permitir intervalo_confianza
           if (tipo_prueba != "una_cola_derecha" && tipo_prueba != "intervalo_confianza") {
             message("Nota: Para pruebas de hipotesis, la distribucion F de Fisher solo tiene cola derecha. Se ha ajustado automaticamente.")
             tipo_prueba <- "una_cola_derecha"
           }
         },
         stop("Distribucion debe ser: 'z', 't', 'chi' o 'f'")
  )

  # Asegurarse de que valor_cal está dentro de los límites del gráfico (para todas las distribuciones)
  if (!is.null(valor_cal)) {
    if (valor_cal > xlim[2]) {
      xlim[2] <- max(xlim[2], valor_cal * 1.2)  # Extender límite derecho con margen
    } else if (!distribucion_una_cola && valor_cal < xlim[1]) {
      xlim[1] <- min(xlim[1], valor_cal * 1.2)  # Extender límite izquierdo para distribuciones simétricas
    }
  }

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
    } else if(tipo_prueba == "una_cola_izquierda") {
      valor_critico <- round(fun_cuantil(alpha), 5)
      valores_criticos <- valor_critico
    } else if(tipo_prueba == "intervalo_confianza") {
      # Para intervalo de confianza, calcular ambos limites
      if(distribucion_una_cola) {
        # Para distribuciones Chi-cuadrado y F
        limite_inferior <- round(fun_cuantil(alpha/2), 5)
        limite_superior <- round(fun_cuantil(1 - alpha/2), 5)
        valor_critico <- limite_superior  # Para mantener compatibilidad
        valores_criticos <- c(limite_inferior, limite_superior)
      } else {
        # Para otras distribuciones
        valor_critico <- round(fun_cuantil(1 - alpha/2), 5)
        valores_criticos <- c(-valor_critico, valor_critico)
      }
    }
  }, error = function(e) {
    stop(paste("Error al calcular valores criticos:", e$message))
  })

  # Crear datos para la curva con distribución optimizada de puntos
  points_count <- num_puntos

  # Manejo especial para distribución F: enfoque mejorado
  if (tolower(distribucion) == "f") {
    # Aumento de precisión para distribuciones F
    # Crear puntos con distribución no uniforme para capturar mejor la forma

    # Estimar el modo (pico) de la distribución F
    if (gl[2] > 2) {
      modo_aprox <- (gl[2] - 2) / gl[2] * gl[1] / (gl[1] + 2)
    } else {
      modo_aprox <- 0.1  # Valor aproximado para gl2 <= 2
    }

    # Asegurar que modo_aprox sea positivo y válido
    modo_aprox <- max(0.1, modo_aprox)

    # Calcular la media y la varianza aproximadas para distribución F
    media_f <- ifelse(gl[2] > 2, gl[2] / (gl[2] - 2), 1)
    # Para gl altos, la distribución se vuelve más concentrada
    es_alta_precision <- gl[1] >= 30 && gl[2] >= 30

    # Preparar valores importantes para considerar en el muestreo
    valor_critico_f <- ifelse(length(valores_criticos) > 0, valores_criticos[1], 0)
    cal_val_f <- ifelse(!is.null(valor_cal), valor_cal, 0)

    # Determinar rango para la densidad de puntos
    if (es_alta_precision) {
      # Para gl altos, concentrar puntos en un rango más estrecho
      rango_bajo <- max(0.001, media_f * 0.5)
      rango_alto <- min(xlim[2], media_f * 2)

      # Crear secuencias de puntos con más densidad alrededor del modo y valores críticos
      n_antes_modo <- floor(points_count * 0.3)
      n_alrededor_modo <- floor(points_count * 0.4)
      n_despues_modo <- points_count - n_antes_modo - n_alrededor_modo

      # Puntos antes del modo
      x_antes <- seq(0, modo_aprox * 0.98, length.out = n_antes_modo)

      # Puntos alrededor del modo (mayor densidad)
      x_modo <- seq(modo_aprox * 0.98, modo_aprox * 1.5, length.out = n_alrededor_modo)

      # Puntos después del modo hasta el final
      x_despues <- seq(modo_aprox * 1.5, xlim[2], length.out = n_despues_modo)

      # Agregar puntos exactos en los valores críticos y calculado
      x_exactos <- c(modo_aprox, valor_critico_f)
      if (!is.null(valor_cal)) {
        x_exactos <- c(x_exactos, valor_cal)
      }

      # Combinar todos los puntos
      x <- sort(unique(c(0, x_antes, x_modo, x_despues, x_exactos)))
    } else {
      # Para casos generales, usar la distribución de puntos más genérica
      n_inicio <- floor(points_count * 0.25)
      n_medio <- floor(points_count * 0.5)
      n_fin <- points_count - n_inicio - n_medio

      # Crear secuencias de puntos de forma no uniforme
      x_inicio <- seq(0, modo_aprox * 0.9, length.out = n_inicio)

      # Calcular un punto final para x_medio que incluya valor_cal si es necesario
      punto_final_medio <- max(modo_aprox * 1.5, valor_critico_f * 1.1)
      if (!is.null(valor_cal) && valor_cal > punto_final_medio && valor_cal < xlim[2]) {
        punto_final_medio <- valor_cal * 0.9 # Punto cercano a valor_cal
      }

      x_medio <- seq(modo_aprox * 0.9, punto_final_medio, length.out = n_medio)
      x_fin <- seq(punto_final_medio, xlim[2], length.out = n_fin)

      # Agregar puntos exactos en los valores importantes
      x_exactos <- c(modo_aprox, valor_critico_f)
      if (!is.null(valor_cal)) {
        x_exactos <- c(x_exactos, valor_cal)
      }

      # Combinar todos los puntos
      x <- sort(unique(c(0, x_inicio, x_medio, x_fin, x_exactos)))
    }
  }
  # Manejar distribuciones de una cola con pocos grados de libertad
  else if (distribucion_una_cola && ((tolower(distribucion) == "chi" && gl <= 2) ||
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
    # Caso estándar para otras distribuciones
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
  } else if(tipo_prueba == "una_cola_izquierda") {
    p <- safe_fill_area(p, xlim[1], valor_critico, fun_densidad, color_rechazo)
  } else if(tipo_prueba == "intervalo_confianza") {
    # Para intervalo de confianza, resaltar el area de confianza (complemento del rechazo)
    if(distribucion_una_cola) {
      # Para Chi-cuadrado y F, resaltar el area entre los limites
      p <- safe_fill_area(p, valores_criticos[1], valores_criticos[2], fun_densidad, "lightblue")
      # Y marcar las areas de rechazo
      p <- safe_fill_area(p, 0, valores_criticos[1], fun_densidad, color_rechazo)
      p <- safe_fill_area(p, valores_criticos[2], xlim[2], fun_densidad, color_rechazo)
    } else {
      # Para distribuciones simetricas
      p <- safe_fill_area(p, -valor_critico, valor_critico, fun_densidad, "lightblue")
      p <- safe_fill_area(p, xlim[1], -valor_critico, fun_densidad, color_rechazo)
      p <- safe_fill_area(p, valor_critico, xlim[2], fun_densidad, color_rechazo)
    }
  }

  # Agregar lineas verticales para los valores criticos
  p <- p + ggplot2::geom_vline(xintercept = valores_criticos,
                               linetype = "dashed",
                               linewidth = 0.8)


  # Crear breaks inteligentes para el eje X
  breaks_x <- .crear_breaks_inteligentes_v2(xlim, valor_critico, valor_cal, tipo_prueba, distribucion_una_cola, distribucion, gl, valores_criticos)

  # Modificar subtitulo segun tipo de prueba
  subtitulo <- paste("Nivel de confianza:", nivel_confianza * 100, "%")
  if (tipo_prueba == "intervalo_confianza") {
    subtitulo <- paste(subtitulo, "- Intervalo de Confianza")
  } else if (!distribucion_una_cola) {
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
    } else if (tolower(dist) == "f" && gl_value[1] >= 30 && gl_value[2] >= 30) {
      # Limitar el eje Y para distribuciones F con gl altos
      # Proporcionar un poco más de espacio encima del máximo para la etiqueta
      c(0, y_max * 1.3)
    } else {
      # Sin limites para otros casos
      NULL
    }
  }

  # Determinar angulo de rotacion para etiquetas del eje X
  # Por defecto, rotar siempre para evitar sobreposiciones
  angulo_rotacion <- 45
  hjust_x <- 1
  vjust_x <- 1

  # Solo usar configuracion horizontal si hay muy pocos breaks
  if (!rotar_etiquetas_x && length(breaks_x) <= 4) {
    angulo_rotacion <- 0
    hjust_x <- 0.5
    vjust_x <- 0
  }

  # Completar el grafico con tema mejorado
  p <- p +
    ggplot2::labs(title = titulo,
                  subtitle = subtitulo,
                  x = "",
                  y = "Densidad",
                  caption = "RDS") +

    # Formato mejorado para el eje X: garantizar 2 decimales exactos
    ggplot2::scale_x_continuous(
      breaks = breaks_x,
      labels = function(x) sprintf("%.2f", x)
    ) +

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
      axis.text.x = ggplot2::element_text(angle = angulo_rotacion, hjust = hjust_x, vjust = vjust_x),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_line(color = "gray95"),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.caption = ggplot2::element_text(size = 8, color = "gray50", hjust = 1))

  # Agregar el valor calculado (Cal.) si se proporciona, con mejor posicionamiento
  if (!is.null(valor_cal)) {
    # Agregar primero la linea vertical para el valor calculado
    p <- p + ggplot2::geom_vline(xintercept = valor_cal,
                                 color = "blue",
                                 linewidth = 0.8)

    # Función mejorada para colocar etiqueta valor_cal
    posicionar_etiqueta_cal <- function() {
      # Determinar la posición de la etiqueta evitando áreas sombreadas y sobreposiciones
      x_pos <- valor_cal  # Posición por defecto

      # Ajustar posición x para casos extremos
      if (valor_cal > xlim[2] * 0.9) {
        # Si está muy cerca del límite derecho
        x_pos <- xlim[2] * 0.8
      } else if (!distribucion_una_cola && valor_cal < xlim[1] * 0.9) {
        # Si está muy cerca del límite izquierdo en distribuciones simétricas
        x_pos <- xlim[1] * 0.8
      }

      # Verificar si hay valores críticos cerca del valor_cal
      cercano_a_critico <- FALSE
      for (vc in valores_criticos) {
        if (abs(valor_cal - vc) < (xlim[2] - xlim[1]) * 0.05) {
          cercano_a_critico <- TRUE
          break
        }
      }

      # Ajustar posición si está cerca de un valor crítico
      if (cercano_a_critico) {
        # Desplazar la etiqueta para evitar sobreposición
        direccion <- sign(valor_cal - median(valores_criticos))
        x_pos <- valor_cal + direccion * (xlim[2] - xlim[1]) * 0.08
      }

      # Calcular posición y para la etiqueta
      y_pos <- y_max * 0.08  # Justo encima del eje X

      # En distribuciones con densidad alta cerca del origen, ajustar
      if (distribucion_una_cola && valor_cal < 2) {
        y_pos <- min(y_max * 0.05, fun_densidad(max(0.1, valor_cal)) * 0.2)
      } else if (!distribucion_una_cola && abs(valor_cal) < 0.5) {
        y_pos <- min(y_max * 0.05, fun_densidad(valor_cal) * 0.1)
      }

      # Crear la etiqueta con estilo mejorado
      p <- p + ggplot2::annotate(
        "label",
        x = x_pos,
        y = y_pos,
        label = paste("Cal =", sprintf("%.2f", valor_cal)),
        color = "white",  # Texto blanco
        fill = "#0066cc",  # Fondo azul más oscuro
        alpha = 1,  # Sin transparencia
        hjust = 0.5,  # Centrado
        vjust = 0.5,  # Centrado
        size = tamanio_etiqueta_cal,
        fontface = "bold"  # Texto en negrita
      )

      # Si el valor está fuera o muy cerca de los límites, agregar una flecha
      if (valor_cal > xlim[2] * 0.98 || valor_cal < xlim[1] * 1.02) {
        direccion <- sign(valor_cal - x_pos)
        p <- p + ggplot2::annotate(
          "segment",
          x = x_pos,
          xend = x_pos + direccion * (xlim[2] - xlim[1]) * 0.05,
          y = y_pos,
          yend = y_pos,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
          color = "#0066cc"
        )
      }

      return(p)
    }

    # Aplicar el posicionamiento mejorado de la etiqueta
    p <- posicionar_etiqueta_cal()
  }

  # Devolver el grafico
  return(p)
}

# Funcion auxiliar para crear breaks inteligentes
.crear_breaks_inteligentes_v2 <- function(xlim, valor_critico, valor_cal = NULL, tipo_prueba, dist_una_cola, distribucion, gl, valores_criticos) {
  # Enfoque minimalista: mostrar solo los puntos esenciales
  # Para distribucion Z o t con dos colas (caso mas comun), mostrar solo estos 5 puntos:
  # xlim[1], -valor_critico, 0, valor_critico, xlim[2]

  # Para otras distribuciones, ser igualmente minimalista

  if (dist_una_cola) {
    # Para chi y F (distribuciones de una cola)
    if (tipo_prueba == "intervalo_confianza") {
      # Para intervalo de confianza: mostrar extremos, valores criticos y 0
      breaks_x <- c(0, valores_criticos[1], valores_criticos[2], xlim[2])
    } else {
      # Para prueba de hipotesis: mostrar extremos, valor critico y un punto intermedio
      breaks_x <- c(0, valor_critico, xlim[2])

      # Añadir un punto intermedio en la region de aceptacion si hay espacio
      if (valor_critico > 2) {
        breaks_x <- c(breaks_x, round(valor_critico/2, 1))
      }
    }
  } else if (tipo_prueba == "dos_colas") {
    # Para distribuciones simetricas con dos colas
    breaks_x <- c(xlim[1], -valor_critico, 0, valor_critico, xlim[2])
  } else if (tipo_prueba == "una_cola_derecha") {
    # Cola derecha
    breaks_x <- c(xlim[1], 0, valor_critico, xlim[2])
  } else if (tipo_prueba == "una_cola_izquierda") {
    # Cola izquierda
    breaks_x <- c(xlim[1], valor_critico, 0, xlim[2])
  } else {
    # Intervalo de confianza
    breaks_x <- c(xlim[1], -valor_critico, 0, valor_critico, xlim[2])
  }

  # Añadir valor calculado si existe y no esta demasiado cerca de otro break
  if (!is.null(valor_cal)) {
    # Verificar si el valor calculado esta suficientemente lejos de otros breaks
    demasiado_cerca <- FALSE
    for (brk in breaks_x) {
      if (abs(valor_cal - brk) < 0.3) {
        demasiado_cerca <- TRUE
        break
      }
    }

    if (!demasiado_cerca) {
      breaks_x <- c(breaks_x, valor_cal)
    }
  }

  # Ordenar y eliminar duplicados aproximados (con tolerancia)
  breaks_x <- sort(breaks_x)

  # Eliminacion de breaks demasiado cercanos con preservacion de puntos clave
  if (length(breaks_x) > 3) {
    # Valores que siempre queremos preservar
    valores_clave <- c()

    # Siempre preservar extremos
    valores_clave <- c(valores_clave, xlim[1], xlim[2])

    # Preservar 0 y valores criticos
    if (dist_una_cola) {
      valores_clave <- c(valores_clave, 0, valor_critico)
      if (tipo_prueba == "intervalo_confianza") {
        valores_clave <- c(valores_clave, valores_criticos)
      }
    } else {
      valores_clave <- c(valores_clave, 0)
      if (tipo_prueba == "dos_colas" || tipo_prueba == "intervalo_confianza") {
        valores_clave <- c(valores_clave, -valor_critico, valor_critico)
      } else if (tipo_prueba == "una_cola_derecha") {
        valores_clave <- c(valores_clave, valor_critico)
      } else {
        valores_clave <- c(valores_clave, valor_critico)
      }
    }

    # Filtrar breaks para mantener solo valores clave y unos pocos adicionales
    breaks_filtrados <- c()

    for (brk in breaks_x) {
      # Verificar si este break es un valor clave o esta cerca de uno
      es_clave <- FALSE
      for (key in valores_clave) {
        if (abs(brk - key) < 1e-6) {
          es_clave <- TRUE
          break
        }
      }

      if (es_clave) {
        breaks_filtrados <- c(breaks_filtrados, brk)
      } else {
        # Para valores no clave, verificar si esta demasiado cerca de algun valor ya incluido
        demasiado_cerca <- FALSE
        for (included in breaks_filtrados) {
          if (abs(brk - included) < 0.3) {
            demasiado_cerca <- TRUE
            break
          }
        }

        if (!demasiado_cerca) {
          breaks_filtrados <- c(breaks_filtrados, brk)
        }
      }
    }

    breaks_x <- sort(breaks_filtrados)
  }

  # Redondear todos los breaks a 2 decimales para mejor visualizacion
  breaks_x <- round(breaks_x, 2)

  # Eliminar duplicados exactos
  breaks_x <- unique(breaks_x)

  return(breaks_x)
}
