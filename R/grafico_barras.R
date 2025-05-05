#' Crea gráficos de barras personalizados para variables cualitativas
#'
#' Esta función genera gráficos de barras para variables cualitativas o categóricas,
#' con líneas de cuadrícula horizontales y sin bordes, mostrando porcentajes y frecuencias absolutas.
#'
#' @param datos Vector con los datos categóricos a analizar
#' @param ordenar Método para ordenar las categorías: "ninguno", "alfabetico", "frecuencia" (por defecto "frecuencia")
#' @param decrecer Lógico. Si TRUE y ordenar="frecuencia", ordena de mayor a menor frecuencia (por defecto TRUE)
#' @param colores Vector de colores para las barras o un nombre de paleta de RColorBrewer
#' @param titulo_grafico Título para el gráfico (por defecto "Distribución de Frecuencias")
#' @param subtitulo_grafico Subtítulo para el gráfico (por defecto NULL)
#' @param caption_grafico Caption (pie de gráfico) para el gráfico (por defecto NULL)
#' @param eje_x_nombre Nombre personalizado para el eje X (por defecto "Categorías")
#' @param eje_y_nombre Nombre personalizado para el eje Y (por defecto "Porcentaje (%)")
#' @param mostrar_grilla Lógico. Si TRUE, muestra líneas de cuadrícula horizontales (por defecto TRUE)
#' @param mostrar_valores Lógico. Si TRUE, muestra valores sobre las barras (por defecto TRUE)
#' @param tamano_texto_valores Tamaño del texto para los valores sobre las barras (por defecto 3.5)
#' @param tamano_texto_ejes Tamaño del texto en los ejes (por defecto 10)
#' @param tamano_texto_titulo Tamaño del texto del título (por defecto 14)
#' @param ancho_barras Ancho de las barras entre 0 y 1 (por defecto 0.7)
#' @param color_fondo Color de fondo del gráfico (por defecto "white")
#' @param valor_y_min Valor mínimo para el eje Y (por defecto NULL, se calcula automáticamente)
#' @param valor_y_max Valor máximo para el eje Y (por defecto NULL, se calcula automáticamente)
#'
#' @return Un objeto ggplot2 que puede ser modificado con funciones adicionales de ggplot2
#'
#' @examples
#' # Datos de ejemplo
#' especies <- c("Anchoveta", "Caballa", "Jurel", "Merluza", "Sardina",
#'              "Anchoveta", "Caballa", "Jurel", "Merluza", "Sardina")
#'
#' # Gráfico básico
#' grafico_barras_personalizado(especies)
#'
#' # Gráfico con personalización
#' colores_personalizados <- c("Anchoveta" = "#ff9999", "Caballa" = "#cccc00",
#'                            "Jurel" = "#00cc99", "Merluza" = "#3399ff",
#'                            "Sardina" = "#cc99ff")
#' grafico_barras_personalizado(
#'   especies,
#'   titulo_grafico = "Distribución de Especies",
#'   subtitulo_grafico = "Figura 01 - Porcentajes y Frecuencias",
#'   colores = colores_personalizados
#' )
#'
#' @export
grafico_barras<- function(datos,
                                         ordenar = "frecuencia",
                                         decrecer = TRUE,
                                         colores = NULL,
                                         titulo_grafico = "Distribución de Frecuencias",
                                         subtitulo_grafico = NULL,
                                         caption_grafico = NULL,
                                         eje_x_nombre = "Categorías",
                                         eje_y_nombre = "Porcentaje (%)",
                                         mostrar_grilla = TRUE,
                                         mostrar_valores = TRUE,
                                         tamano_texto_valores = 3.5,
                                         tamano_texto_ejes = 10,
                                         tamano_texto_titulo = 14,
                                         ancho_barras = 0.7,
                                         color_fondo = "white",
                                         valor_y_min = NULL,
                                         valor_y_max = NULL) {

  # Verificar que ggplot2 esté instalado
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere el paquete 'ggplot2' para generar gráficos.")
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

  # Crear data frame para el gráfico
  tabla_grafico <- data.frame(
    Categoria = names(tabla_frec),
    fi = as.numeric(tabla_frec),
    hi = round((as.numeric(tabla_frec) / sum(tabla_frec)) * 100, 2)
  )

  # Asegurarse de que la categoría se muestre en el orden correcto
  tabla_grafico$Categoria <- factor(tabla_grafico$Categoria, levels = tabla_grafico$Categoria)

  # Preparar colores para los gráficos
  if (is.null(colores)) {
    colores_grafico <- c("#ff9999", "#99cc99", "#9999ff", "#ffcc99", "#cc99ff",
                         "#ff99cc", "#99ccff", "#ccff99", "#ffff99", "#99ffcc")
    colores_grafico <- rep(colores_grafico, length.out = nrow(tabla_grafico))
  } else if (length(colores) == 1) {
    # Intentar usar como nombre de paleta
    if (requireNamespace("RColorBrewer", quietly = TRUE) &&
        colores %in% rownames(RColorBrewer::brewer.pal.info)) {
      n_colors <- min(nrow(tabla_grafico), RColorBrewer::brewer.pal.info[colores, "maxcolors"])
      colores_grafico <- RColorBrewer::brewer.pal(n_colors, colores)
      if (nrow(tabla_grafico) > n_colors) {
        colores_grafico <- colorRampPalette(colores_grafico)(nrow(tabla_grafico))
      }
    } else {
      colores_grafico <- scales::hue_pal()(nrow(tabla_grafico))
    }
  } else {
    # Usar colores proporcionados
    colores_grafico <- rep(colores, length.out = nrow(tabla_grafico))
  }

  # Crear el gráfico de barras
  p <- ggplot2::ggplot(tabla_grafico, ggplot2::aes(x = Categoria, y = hi, fill = Categoria)) +
    ggplot2::geom_col(width = ancho_barras, show.legend = FALSE, color = NA) +  # color = NA elimina el borde negro
    ggplot2::labs(
      title = titulo_grafico,
      subtitle = subtitulo_grafico,
      caption = caption_grafico,
      x = eje_x_nombre,
      y = eje_y_nombre
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = tamano_texto_titulo),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 1, face = "italic"),
      axis.text = ggplot2::element_text(size = tamano_texto_ejes),
      axis.title = ggplot2::element_text(size = tamano_texto_ejes + 2),
      panel.background = ggplot2::element_rect(fill = color_fondo, color = NA),
      plot.background = ggplot2::element_rect(fill = color_fondo, color = NA),
      panel.grid.major.x = ggplot2::element_blank(),  # Quitar líneas de cuadrícula verticales
      panel.grid.minor = ggplot2::element_blank(),     # Quitar líneas de cuadrícula menores
      panel.grid.major.y = if (mostrar_grilla) ggplot2::element_line(color = "gray90") else ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),  # Eliminar líneas de los ejes
      panel.border = ggplot2::element_blank()  # Eliminar bordes del panel
    ) +
    ggplot2::scale_fill_manual(values = colores_grafico)

  # Añadir valores sobre las barras si se solicita
  if (mostrar_valores) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(hi, "%(", fi, ")")),
      vjust = -0.5,
      size = tamano_texto_valores
    )
  }

  # Configurar límites del eje Y si se especifican
  if (!is.null(valor_y_min) || !is.null(valor_y_max)) {
    y_limits <- c(
      ifelse(is.null(valor_y_min), NA, valor_y_min),
      ifelse(is.null(valor_y_max), NA, valor_y_max)
    )
    p <- p + ggplot2::scale_y_continuous(limits = y_limits)
  } else {
    # Ajustar el eje Y para dejar espacio para las etiquetas
    max_value <- max(tabla_grafico$hi)
    p <- p + ggplot2::scale_y_continuous(
      limits = c(NA, max_value * 1.1),
      breaks = c(seq(floor(min(tabla_grafico$hi)), ceiling(max(tabla_grafico$hi)), by = 1)),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    )
  }

  return(p)
}
