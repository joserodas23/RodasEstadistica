# ---- Imports roxygen ----
#' @importFrom dplyr group_by summarise n_distinct across all_of mutate select left_join starts_with arrange desc
#' @importFrom tidyr pivot_longer
#' @importFrom rlang ensym as_name
#' @importFrom stats quantile
NULL

# =========================================================
# aiken_v
# =========================================================
#' V de Aiken para validez de contenido
#'
#' Calcula el índice **V de Aiken** para un vector de calificaciones en una escala
#' discreta \code{lo}–\code{hi}. Opcionalmente estima **intervalos de confianza**
#' por bootstrap percentil.
#'
#' Fórmula (Aiken, 1985): \deqn{V = \frac{\sum (x - lo)}{n \,(hi - lo)}}
#'
#' @param x Numeric. Vector de calificaciones (p. ej., 1–4). Se ignoran \code{NA} si \code{na.rm=TRUE}.
#' @param lo,hi Números mínimo y máximo de la escala (por defecto 1 y 4).
#' @param na.rm Logical. Si \code{TRUE}, elimina \code{NA} antes de calcular.
#' @param ci Logical. Si \code{TRUE}, devuelve intervalo de confianza (bootstrap).
#' @param R Integer. Réplicas bootstrap (default 2000).
#' @param conf Numeric. Nivel de confianza (default 0.95).
#' @param seed Integer o \code{NULL}. Semilla para reproducibilidad del bootstrap.
#'
#' @return
#' - Si \code{ci = FALSE}: un número (V).
#' - Si \code{ci = TRUE}: una lista con \code{V}, \code{lower}, \code{upper}, \code{R}.
#'
#' @examples
#' aiken_v(c(4,4,3,3,4), lo = 1, hi = 4)
#' aiken_v(c(4,4,3,3,4), lo = 1, hi = 4, ci = TRUE, R = 1000, seed = 123)
#' @export
aiken_v <- function(x, lo = 1, hi = 4,
                    na.rm = TRUE,
                    ci = FALSE, R = 2000, conf = 0.95, seed = NULL) {
  x <- as.numeric(x)
  if (na.rm) x <- x[!is.na(x)]

  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    stop("Verifica 'lo' y 'hi': deben ser finitos y cumplir hi > lo.")
  }
  n <- length(x)
  if (n == 0L) {
    return(if (ci) list(V = NA_real_, lower = NA_real_, upper = NA_real_, R = 0L) else NA_real_)
  }

  denom <- n * (hi - lo)
  V <- sum(x - lo) / denom

  if (!ci) return(V)

  if (!is.null(seed)) set.seed(seed)
  boot <- replicate(R, {
    xb <- sample(x, size = n, replace = TRUE)
    sum(xb - lo) / denom
  })

  a <- (1 - conf) / 2
  qs <- stats::quantile(boot, probs = c(a, 1 - a), na.rm = TRUE, names = FALSE, type = 6)
  list(V = V, lower = qs[1], upper = qs[2], R = R)
}

# =========================================================
# aiken_tbl
# =========================================================
#' V de Aiken en formato "tidy" (por ítem/criterio y global)
#'
#' Calcula la V de Aiken por ítem y por cada criterio (p. ej., relevancia, claridad,
#' coherencia) a partir de un data frame en formato largo (una fila = un juez por ítem).
#' También calcula una V global por ítem y su decisión (Mantener/Revisar/Reescribir),
#' con umbrales personalizables.
#'
#' @param data Data frame largo: una fila = un juez valorando un ítem.
#' @param item_col,judge_col Columna identificadora de ítem y de juez. Acepta
#'   **bare names** (p. ej., \code{item_id}) o **strings** (p. ej., \code{"item_id"}).
#' @param rating_cols Vector de nombres de columnas de criterios (p. ej.,
#'   \code{c("relevancia","claridad","coherencia")}).
#' @param lo,hi Límites inferior y superior de la escala (default 1 y 4).
#' @param global Logical. Si \code{TRUE}, añade V global por ítem.
#' @param global_method \code{"concat"} (default) concatena todas las valoraciones de
#'   los criterios para V global; \code{"mean"} promedia las V por criterio.
#' @param cut_keep,cut_review Umbrales para la decisión (default 0.80 y 0.70).
#' @param lab_keep,lab_review,lab_rewrite Etiquetas para la decisión.
#'
#' @return Tibble con: \code{n_jueces}, \code{V_<criterio>}, \code{dec_V_<criterio>},
#' y, si \code{global=TRUE}, \code{V_global} y \code{dec_global}.
#'
#' @examples
#' # aiken_tbl(df, item_col = item_id, judge_col = judge_id,
#' #           rating_cols = c("relevancia","claridad","coherencia"),
#' #           lo = 1, hi = 4, global = TRUE)
#' @export
aiken_tbl <- function(data,
                      item_col, judge_col, rating_cols,
                      lo = 1, hi = 4,
                      global = TRUE,
                      global_method = c("concat","mean"),
                      cut_keep = 0.80, cut_review = 0.70,
                      lab_keep = "Mantener",
                      lab_review = "Revisar",
                      lab_rewrite = "Reescribir/Eliminar") {

  global_method <- match.arg(global_method)

  # Acepta bare names o strings
  item_sym  <- rlang::ensym(item_col)
  judge_sym <- rlang::ensym(judge_col)

  # Comprobaciones
  if (!all(rating_cols %in% names(data))) {
    faltan <- setdiff(rating_cols, names(data))
    stop("Las columnas de 'rating_cols' no existen en 'data': ", paste(faltan, collapse = ", "))
  }

  # Decisión con umbrales personalizables
  decision_fun <- function(v) dplyr::case_when(
    is.na(v)        ~ "Sin datos",
    v >= cut_keep   ~ lab_keep,
    v >= cut_review ~ lab_review,
    TRUE            ~ lab_rewrite
  )

  # --- V por criterio ---
  por_criterio <- data |>
    dplyr::group_by(!!item_sym) |>
    dplyr::summarise(
      n_jueces = dplyr::n_distinct(!!judge_sym),
      dplyr::across(dplyr::all_of(rating_cols),
                    ~ aiken_v(.x, lo = lo, hi = hi),
                    .names = "V_{.col}"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("V_"),
                    ~ decision_fun(.x),
                    .names = "dec_{.col}")
    )

  if (!global) return(por_criterio)

  # --- V global por ítem ---
  if (global_method == "concat") {
    vglob <- data |>
      dplyr::group_by(!!item_sym) |>
      dplyr::summarise(
        V_global = {
          cols <- dplyr::across(dplyr::all_of(rating_cols))
          aiken_v(as.numeric(unlist(cols)), lo = lo, hi = hi)
        },
        .groups = "drop"
      )
  } else { # "mean" de V por criterio
    vcols <- grep("^V_", names(por_criterio), value = TRUE)
    vglob <- por_criterio |>
      dplyr::select(!!item_sym, dplyr::all_of(vcols)) |>
      tidyr::pivot_longer(-!!item_sym, names_to = "crit", values_to = "V") |>
      dplyr::group_by(!!item_sym) |>
      dplyr::summarise(V_global = mean(V, na.rm = TRUE), .groups = "drop")
  }

  vglob <- vglob |>
    dplyr::mutate(dec_global = decision_fun(V_global))

  dplyr::left_join(por_criterio, vglob, by = rlang::as_name(item_sym))
}

# =========================================================
# aiken_report  (tabla bonita + exportación)
# =========================================================
#' Reporte bonito de V de Aiken (tabla con colores y exportación)
#'
#' Genera una tabla formateada a partir de valoraciones de expertos y del cálculo
#' de V de Aiken por ítem (por criterio y global). Si \code{data} ya trae columnas
#' \code{V_*}, se usan esas; si no, se calcula V con \code{aiken_tbl()}.
#'
#' @param data Data frame crudo (formato largo) o con columnas V_* ya calculadas.
#' @param item_col,judge_col Columnas identificadoras (bare o string). Requeridas si hay que calcular V.
#' @param rating_cols Vector con columnas de criterios. Requerido si hay que calcular V.
#' @param lo,hi Escala mínima y máxima (default 1 y 4).
#' @param keep_cols Columnas extra a mostrar (p. ej., \code{c("item_text","dimension")}).
#' @param group_col Columna para agrupar visualmente (default "dimension" si existe).
#' @param cut_keep,cut_review,lab_keep,lab_review,lab_rewrite Umbrales y etiquetas.
#' @param digits Decimales en columnas V (default 2).
#' @param title,subtitle Título/subtítulo de la tabla.
#' @param output Ruta de salida opcional. Extensión define formato:
#'   \code{.html} / \code{.png} usan \pkg{gt}; \code{.docx} usa \pkg{flextable}.
#'
#' @return (invisible) lista con \code{table} (objeto gt/flextable), \code{data}
#' y \code{path} (si se guardó).
#' @details Requiere tener instalados: \pkg{gt}, \pkg{scales} (si HTML/PNG) y
#' opcionalmente \pkg{webshot2} (para PNG) o \pkg{flextable}+\pkg{officer} (para DOCX).
#' @examples
#' # aiken_report(df, item_col = item_id, judge_col = judge_id,
#' #   rating_cols = c("relevancia","claridad","coherencia"),
#' #   keep_cols = c("item_text","dimension"),
#' #   output = "F:/Validacion de intrumentos/tabla_aiken.html")
#' @export
aiken_report <- function(data,
                         item_col = NULL, judge_col = NULL, rating_cols = NULL,
                         lo = 1, hi = 4,
                         keep_cols = c("item_text","dimension"),
                         group_col = NULL,
                         cut_keep = 0.80, cut_review = 0.70,
                         lab_keep = "Mantener",
                         lab_review = "Revisar",
                         lab_rewrite = "Reescribir/Eliminar",
                         digits = 2,
                         title = "Validación de contenido – V de Aiken",
                         subtitle = "Panel de expertos",
                         output = NULL) {

  need <- function(pkg) if (!requireNamespace(pkg, quietly = TRUE))
    stop("Necesitas instalar '", pkg, "' para usar aiken_report().", call. = FALSE)

  # ¿Ya hay columnas V_*?
  has_V <- any(grepl("^V_", names(data)))
  res <- data

  # Determinar nombres de columnas clave (acepta bare o string)
  # Si no se pasan, intenta defaults
  item_nm  <- if (!is.null(item_col)) rlang::as_name(rlang::ensym(item_col)) else "item_id"
  judge_nm <- if (!is.null(judge_col)) rlang::as_name(rlang::ensym(judge_col)) else "judge_id"

  if (!has_V) {
    if (is.null(rating_cols))
      stop("Si 'data' no trae columnas V_*, indica 'rating_cols' (p. ej., c('relevancia','claridad','coherencia')).")

    res <- aiken_tbl(
      data        = data,
      item_col    = item_nm,          # pasamos strings para evitar NSE
      judge_col   = judge_nm,
      rating_cols = rating_cols,
      lo = lo, hi = hi, global = TRUE,
      cut_keep = cut_keep, cut_review = cut_review,
      lab_keep = lab_keep, lab_review = lab_review, lab_rewrite = lab_rewrite
    )

    # anexar columnas extra si existen
    keep_cols <- keep_cols[keep_cols %in% names(data)]
    if (length(keep_cols)) {
      res <- dplyr::left_join(
        res,
        dplyr::select(data, dplyr::all_of(c(item_nm, keep_cols))) |> dplyr::distinct(),
        by = item_nm
      )
    }
  }

  # Columna de agrupación por defecto
  if (is.null(group_col)) group_col <- if ("dimension" %in% names(res)) "dimension" else NULL

  # Armar dataset a mostrar
  cols_v <- grep("^V_", names(res), value = TRUE)
  show_cols <- c(group_col, "item_id", keep_cols[keep_cols %in% names(res)],
                 "n_jueces", intersect(c("V_relevancia","V_claridad","V_coherencia","V_global"), cols_v),
                 "dec_global")
  show_cols <- unique(show_cols[!is.na(show_cols) & show_cols %in% names(res)])
  dat <- res[, show_cols, drop = FALSE]

  # Orden para presentación (base R, evita .data pronoun)
  if (!is.null(group_col) && group_col %in% names(dat) && "V_global" %in% names(dat)) {
    ord <- order(dat[[group_col]], -dat[["V_global"]])
    dat <- dat[ord, , drop = FALSE]
  } else if ("V_global" %in% names(dat)) {
    ord <- order(-dat[["V_global"]])
    dat <- dat[ord, , drop = FALSE]
  }

  # Tabla con gt
  need("gt"); need("scales")
  pal_v <- scales::col_bin(
    palette = c("#FFEBEE", "#FFF8E1", "#E6F4EA"),
    bins = c(0, cut_review, cut_keep, 1)
  )

  if (!is.null(group_col) && group_col %in% names(dat)) {
    g <- gt::gt(dat, groupname_col = group_col)
  } else {
    g <- gt::gt(dat)
  }

  g <- g |>
    gt::tab_header(title = gt::md(paste0("**", title, "**")), subtitle = subtitle) |>
    gt::cols_label(
      item_id      = "Ítem",
      item_text    = "Redacción del ítem",
      n_jueces     = "Jueces",
      V_relevancia = "V (Relev.)",
      V_claridad   = "V (Clar.)",
      V_coherencia = "V (Coher.)",
      V_global     = "V global",
      dec_global   = "Decisión"
    ) |>
    gt::fmt_number(columns = gt::starts_with("V_"), decimals = digits) |>
    gt::data_color(columns = gt::starts_with("V_"), colors = pal_v) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "#E6F4EA"), gt::cell_text(color = "#1B5E20", weight = "bold")),
      locations = gt::cells_body(columns = "dec_global", rows = dec_global == lab_keep)
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "#FFF8E1"), gt::cell_text(color = "#7A5901", weight = "bold")),
      locations = gt::cells_body(columns = "dec_global",
                                 rows = dec_global == lab_review | grepl("^Revisar", dec_global))
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "#FFEBEE"), gt::cell_text(color = "#B71C1C", weight = "bold")),
      locations = gt::cells_body(columns = "dec_global", rows = dec_global == lab_rewrite)
    ) |>
    gt::cols_width(
      gt::everything() ~ gt::px(90),
      gt::matches("item_text") ~ gt::px(520),
      gt::matches("item_id") ~ gt::px(70),
      gt::matches("n_jueces") ~ gt::px(70)
    ) |>
    gt::opt_table_font(stack = "system") |>
    gt::tab_source_note(
      gt::md(paste0(
        "Criterios: **", lab_keep, " ≥ ", sprintf("%.2f", cut_keep),
        "** · **", lab_review, " ", sprintf("%.2f", cut_review), "–", sprintf("%.2f", cut_keep - .01),
        "** · **", lab_rewrite, " < ", sprintf("%.2f", cut_review), "**"
      ))
    )

  # Guardar si se pide
  out_path <- NULL
  if (!is.null(output)) {
    ext <- tolower(tools::file_ext(output))
    if (ext %in% c("html","png")) {
      if (ext == "png") need("webshot2")
      gt::gtsave(g, filename = output)
      out_path <- normalizePath(output, winslash = "/", mustWork = FALSE)
    } else if (ext == "docx") {
      need("flextable"); need("officer")
      ft <- flextable::flextable(dat) |> flextable::autofit()
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, title, style = "heading 1")
      doc <- officer::body_add_par(doc, subtitle, style = "Normal")
      doc <- officer::body_add_flextable(doc, ft)
      print(doc, target = output)
      out_path <- normalizePath(output, winslash = "/", mustWork = FALSE)
    } else {
      warning("Extensión no soportada: use .html, .png o .docx")
    }
  }

  print(g)
  invisible(list(table = g, data = res, path = out_path))
}
