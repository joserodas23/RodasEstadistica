#' Graficar distribuciones estadísticas con áreas de rechazo
#'
#' Esta función permite visualizar las distribuciones Normal estándar (Z),
#' t-Student, Chi-cuadrado y F de Fisher, marcando las áreas de rechazo según
#' el nivel de confianza y tipo de prueba especificados.
#'
#' @param distribucion Tipo de distribución a graficar, puede ser: "z" (Normal estándar),
#'        "t" (t-Student), "chi" (Chi-cuadrado) o "f" (F de Fisher).
#' @param nivel_confianza Nivel de confianza para la prueba (por defecto 0.95).
#' @param tipo_prueba Tipo de prueba a realizar: "dos_colas", "una_cola_derecha" o "una_cola_izquierda".
#'        Para distribuciones Chi-cuadrado y F, siempre se utilizará "una_cola_derecha".
#' @param color_rechazo Color para las áreas de rechazo (por defecto "red").
#' @param tamanio_texto Tamaño base del texto en el gráfico (por defecto 12).
#' @param gl Grados de libertad. Para t-Student y Chi-cuadrado debe ser un valor único.
#'        Para F, debe ser un vector de dos elementos: c(gl1, gl2).
#' @param valor_cal Valor calculado del estadístico para marcar en el gráfico (opcional).
#'
#' @return Un objeto ggplot2 con la distribución graficada.
#'
#' @examples
#' # Distribución Z (Normal estándar) de dos colas
#' graficar_distribucion("z", nivel_confianza = 0.95, tipo_prueba = "dos_colas")
#'
#' # Distribución t-Student con 15 grados de libertad, prueba de una cola derecha
#' graficar_distribucion("t", nivel_confianza = 0.90, tipo_prueba = "una_cola_derecha", gl = 15)
#'
#' # Distribución Chi-cuadrado con 5 grados de libertad
#' graficar_distribucion("chi", nivel_confianza = 0.95, gl = 5)
#'
#' # Distribución F con 5 y 20 grados de libertad, marcando un valor calculado
#' graficar_distribucion("f", nivel_confianza = 0.95, gl = c(5, 20), valor_cal = 2.5)
#'
#' @import ggplot2
#' @importFrom stats dnorm qnorm dt qt dchisq qchisq df qf
#' @export
graficar_distribucion <- function(distribucion = "z",
                                  nivel_confianza = 0.95,
                                  tipo_prueba = "dos_colas",
                                  color_rechazo = "red",
                                  tamanio_texto = 12,
                                  gl = NULL,
                                  valor_cal = NULL) {

  # Validar el tipo de prueba
  tipos_validos <- c("dos_colas", "una_cola_derecha", "una_cola_izquierda")
  if (!tipo_prueba %in% tipos_validos) {
    stop("Tipo de prueba no válido")
  }

  # Configurar distribución
  switch(tolower(distribucion),
         "z" = {
           fun_densidad <- dnorm
           fun_cuantil <- qnorm
           xlim <- c(-4, 4)
           titulo <- "Distribución Normal Estándar (Z)"
           distribucion_una_cola <- FALSE
         },
         "t" = {
           if (is.null(gl)) stop("Especificar gl para t")
           fun_densidad <- function(x) dt(x, df = gl)
           fun_cuantil <- function(p) qt(p, df = gl)
           xlim <- c(-4, 4)
           titulo <- paste("Distribución t-Student (gl =", gl, ")")
           distribucion_una_cola <- FALSE
         },
         "chi" = {
           if (is.null(gl)) stop("Especificar gl para chi")
           fun_densidad <- function(x) dchisq(x, df = gl)
           fun_cuantil <- function(p) qchisq(p, df = gl)
           xlim <- c(0, max(30, gl + 4*sqrt(2*gl)))
           titulo <- paste("Distribución Chi-cuadrado (gl =", gl, ")")
           distribucion_una_cola <- TRUE
           # Para Chi-cuadrado, forzar a una cola derecha
           tipo_prueba <- "una_cola_derecha"
         },
         "f" = {
           if (length(gl) != 2) stop("Especificar gl1 y gl2 para F")
           fun_densidad <- function(x) df(x, df1 = gl[1], df2 = gl[2])
           fun_cuantil <- function(p) qf(p, df1 = gl[1], df2 = gl[2])
           xlim <- c(0, max(5, qf(0.99, gl[1], gl[2])))
           titulo <- paste("Distribución F (gl1 =", gl[1], ", gl2 =", gl[2], ")")
           distribucion_una_cola <- TRUE
           # Para F, forzar a una cola derecha
           tipo_prueba <- "una_cola_derecha"
         },
         stop("Distribución debe ser: 'z', 't', 'chi' o 'f'")
  )

  # Advertir al usuario si intenta usar dos colas con Chi o F
  if (distribucion_una_cola && (tipo_prueba == "dos_colas" || tipo_prueba == "una_cola_izquierda")) {
    message("Nota: Las distribuciones Chi-cuadrado y F de Fisher solo tienen cola derecha. Se ha ajustado automáticamente.")
  }

  # Calcular valores críticos según el tipo de prueba
  alpha <- 1 - nivel_confianza
  if(tipo_prueba == "dos_colas") {
    valor_critico <- round(fun_cuantil(1 - alpha/2), 3)
    valores_criticos <- c(-valor_critico, valor_critico)
  } else if(tipo_prueba == "una_cola_derecha") {
    valor_critico <- round(fun_cuantil(1 - alpha), 3)
    valores_criticos <- valor_critico
  } else {
    valor_critico <- round(fun_cuantil(alpha), 3)
    valores_criticos <- valor_critico
  }

  # Crear datos para la curva
  x <- seq(xlim[1], xlim[2], length.out = 1000)
  y <- fun_densidad(x)
  datos <- data.frame(x = x, y = y)

  # Crear gráfico base
  p <- ggplot(datos, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_line(linewidth = 1)

  # Agregar áreas de rechazo según el tipo de prueba
  if(tipo_prueba == "dos_colas") {
    p <- p + stat_function(
      fun = fun_densidad,
      xlim = c(xlim[1], -valor_critico),
      geom = "area",
      fill = color_rechazo,
      alpha = 0.9
    ) +
      stat_function(
        fun = fun_densidad,
        xlim = c(valor_critico, xlim[2]),
        geom = "area",
        fill = color_rechazo,
        alpha = 0.9
      )
  } else if(tipo_prueba == "una_cola_derecha") {
    p <- p + stat_function(
      fun = fun_densidad,
      xlim = c(valor_critico, xlim[2]),
      geom = "area",
      fill = color_rechazo,
      alpha = 0.9
    )
  } else {
    p <- p + stat_function(
      fun = fun_densidad,
      xlim = c(xlim[1], valor_critico),
      geom = "area",
      fill = color_rechazo,
      alpha = 0.9
    )
  }

  # Agregar líneas verticales para los valores críticos
  p <- p + geom_vline(xintercept = valores_criticos,
                      linetype = "dashed",
                      linewidth = 0.8)

  # Agregar línea vertical para el valor observado si se proporciona
  if (!is.null(valor_cal)) {
    p <- p + geom_vline(xintercept = valor_cal,
                        color = "blue",
                        linewidth = 0.8)

    # Posicionamiento diferente para Chi-cuadrado y F
    if (distribucion_una_cola) {
      # Para Chi-cuadrado y F
      p <- p + annotate("text",
                        x = valor_cal,
                        y = 1 + min(y) * 0.2,  # Posición específica para Chi y F
                        label = paste("Cal.=", round(valor_cal, 3)),
                        color = "blue",
                        angle = 90,
                        vjust = 2,
                        size = 4)
    } else {
      # Para Z y t-Student
      p <- p + annotate("text",
                        x = valor_cal,
                        y = max(y) * 0.8,  # Posición para distribuciones simétricas
                        label = paste("Cal.=", round(valor_cal, 3)),
                        color = "blue",
                        angle = 90,
                        vjust = 2,
                        size = 4)
    }
  }

  # Configurar breaks del eje X según el tipo de prueba y distribución
  if (distribucion_una_cola) {
    # Para chi y F
    breaks_x <- c(0, valor_critico, round(xlim[2] * 0.5), xlim[2])
  } else if(tipo_prueba == "dos_colas") {
    breaks_x <- c(xlim[1], -valor_critico, 0, valor_critico, xlim[2])
  } else if(tipo_prueba == "una_cola_derecha") {
    breaks_x <- c(xlim[1], 0, valor_critico, xlim[2])
  } else {
    breaks_x <- c(xlim[1], valor_critico, 0, xlim[2])
  }

  # Modificar subtítulo para distribuciones de una cola
  subtitulo <- paste("Nivel de confianza:", nivel_confianza * 100, "%")
  if (!distribucion_una_cola) {
    subtitulo <- paste(subtitulo, "-", gsub("_", " ", tipo_prueba))
  } else {
    subtitulo <- paste(subtitulo, "- una cola derecha")
  }

  # Completar el gráfico
  p <- p +
    labs(title = titulo,
         subtitle = subtitulo,
         x = "",
         y = "Densidad") +

    scale_x_continuous(breaks = breaks_x, limits = xlim) +

    theme_minimal(base_size = tamanio_texto) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.margin = margin(20, 20, 20, 20))

  return(p)
}
