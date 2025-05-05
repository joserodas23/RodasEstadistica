#' Crea gráficos circulares limpios para variables cualitativas
#'
#' Esta función genera gráficos circulares (pie charts) para variables cualitativas o categóricas,
#' sin bordes ni cuadros alrededor, mostrando porcentajes y frecuencias absolutas.
#'
#' @param datos Vector con los datos categóricos a analizar
#' @param ordenar Método para ordenar las categorías: "ninguno", "alfabetico", "frecuencia" (por defecto "frecuencia")
#' @param decrecer Lógico. Si TRUE y ordenar="frecuencia", ordena de mayor a menor frecuencia (por defecto TRUE)
#' @param colores Vector de colores para los sectores o un nombre de paleta de RColorBrewer
#' @param titulo_grafico Título para el gráfico (por defecto "Distribución de Frecuencias")
#' @param subtitulo_grafico Subtítulo para el gráfico (por defecto NULL)
#' @param caption_grafico Caption (pie de gráfico) para el gráfico (por defecto NULL)
#' @param mostrar_valores Lógico. Si TRUE, muestra valores dentro de los sectores (por defecto TRUE)
#' @param mostrar_porcentaje Lógico. Si TRUE, muestra porcentajes en las etiquetas (por defecto TRUE)
#' @param mostrar_leyenda Lógico. Si TRUE, muestra la leyenda (por defecto TRUE)
#' @param posicion_leyenda Posición de la leyenda: "right", "left", "bottom", "top" (por defecto "right")
#' @param tamano_texto_valores Tamaño del texto para los valores en los sectores (por defecto 3.5)
#' @param tamano_texto_leyenda Tamaño del texto en la leyenda (por defecto 10)
#' @param tamano_texto_titulo Tamaño del texto del título (por defecto 14)
#' @param minimo_porcentaje_etiquetas Porcentaje mínimo para mostrar etiquetas (por defecto 5)
#' @param etiquetas_con_caja Lógico. Si TRUE, muestra etiquetas con fondo (por defecto FALSE)
#' @param relacion_circular Relación de aspecto del gráfico, valores < 1 lo hacen más ancho (por defecto 1)
#'
#' @return Un objeto ggplot2 que puede ser modificado con funciones adicionales de ggplot2
#'
#' @examples
#' # Datos de ejemplo
#' especies <- c("Anchoveta", "Caballa", "Jurel", "Merluza", "Sardina",
#'              "Anchoveta", "Caballa", "Jurel", "Merluza", "Sardina")
#'
#' # Gráfico circular básico
#' grafico_circular_limpio(especies)
#'
#' # Gráfico circular con personalización
#' colores_personalizados <- c("Anchoveta" = "#66c2a5",
#'                            "Caballa" = "#fc8d62",
#'                            "Jurel" = "#8da0cb",
#'                            "Merluza" = "#e78ac3",
#'                            "Sardina" = "#a6d854")
#' grafico_circular_limpio(
#'   especies,
#'   titulo_grafico = "Distribución de Especies Capturadas",
#'   subtitulo_grafico = "Figura 02 - Datos de captura pesquera",
#'   colores = colores_personalizados,
#'   posicion_leyenda = "right",
#'   tamano_texto_valores = 4
#' )
#'
#' @export
grafico_circular<- function(datos,
                                    ordenar = "frecuencia",
                                    decrecer = TRUE,
                                    colores = NULL,
                                    titulo_grafico = "Distribución de Frecuencias",
                                    subtitulo_grafico = NULL,
                                    caption_grafico = NULL,
                                    mostrar_valores = TRUE,
                                    mostrar_porcentaje = TRUE,
                                    mostrar_leyenda = TRUE,
                                    posicion_leyenda = "right",
                                    tamano_texto_valores = 3.5,
                                    tamano_texto_leyenda = 10,
                                    tamano_texto_titulo = 14,
                                    minimo_porcentaje_etiquetas = 5,
                                    etiquetas_con_caja = FALSE,
                                    relacion_circular = 1) {

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

  # Preparar colores
  if (is.null(colores)) {
    colores_grafico <- scales::hue_pal()(nrow(tabla_grafico))
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

  # Crear gráfico circular (pie)
  p <- ggplot2::ggplot(tabla_grafico,
                       ggplot2::aes(x = "",
                                    y = fi,
                                    fill = Categoria)) +
    ggplot2::geom_bar(stat = "identity",
                      width = 1,
                      colour = NA) +  # Sin borde
    ggplot2::coord_polar("y", start = 0)

  # Aplicar tema completamente vacío
  p <- p + ggplot2::theme_void()

  # Configurar la posición de la leyenda
  leyenda_pos <- ifelse(mostrar_leyenda, posicion_leyenda, "none")

  # Eliminar todos los elementos del tema excepto lo esencial
  p <- p + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
    plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.position = leyenda_pos,
    legend.text = ggplot2::element_text(size = tamano_texto_leyenda),
    legend.title = ggplot2::element_blank(), # Sin título de leyenda
    plot.margin = ggplot2::margin(0, 0, 0, 0),
    plot.title = ggplot2::element_text(hjust = 0.5,
                                       face = "bold",
                                       size = tamano_texto_titulo,
                                       margin = ggplot2::margin(b = 10)),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

  # Añadir título, subtítulo y caption
  p <- p + ggplot2::labs(
    title = titulo_grafico,
    subtitle = subtitulo_grafico,
    caption = caption_grafico
  )

  # Ajustar relación de aspecto si se solicita
  if (relacion_circular != 1) {
    p <- p + ggplot2::theme(aspect.ratio = relacion_circular)
  }

  # Añadir colores
  p <- p + ggplot2::scale_fill_manual(values = colores_grafico)

  # Añadir etiquetas dentro del gráfico circular si se solicita
  if (mostrar_valores) {
    if (mostrar_porcentaje) {
      etiqueta <- paste0(tabla_grafico$hi, "% (", tabla_grafico$fi, ")")
    } else {
      etiqueta <- as.character(tabla_grafico$fi)
    }

    # Solo mostrar etiquetas para categorías con porcentaje mayor al mínimo
    etiqueta_mostrada <- ifelse(tabla_grafico$hi >= minimo_porcentaje_etiquetas,
                                etiqueta,
                                "")

    # Estilo de las etiquetas
    if (etiquetas_con_caja) {
      # Etiquetas con caja alrededor
      p <- p + ggplot2::geom_label(
        ggplot2::aes(
          label = etiqueta_mostrada
        ),
        position = ggplot2::position_stack(vjust = 0.5),
        size = tamano_texto_valores,
        show.legend = FALSE
      )
    } else {
      # Etiquetas simples sin caja
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          label = etiqueta_mostrada
        ),
        position = ggplot2::position_stack(vjust = 0.5),
        size = tamano_texto_valores,
        show.legend = FALSE
      )
    }
  }

  return(p)
}
