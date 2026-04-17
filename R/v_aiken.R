## =========================================================
## Aiken's V - funciones base (sin dependencias)
## =========================================================

# 1) V de Aiken para un vector ----------------------------------------------
# Fórmula: V = sum(x - lo) / ( n * (hi - lo) )
aiken_v <- function(x, lo = 1, hi = 4, na.rm = TRUE) {
  x <- as.numeric(x)
  if (na.rm) x <- x[!is.na(x)]
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    stop("Verifica 'lo' y 'hi': deben ser finitos y cumplir hi > lo.")
  }
  n <- length(x)
  if (n == 0L) return(NA_real_)
  sum(x - lo) / (n * (hi - lo))
}

# 2) V de Aiken + IC por bootstrap (opcional) --------------------------------
aiken_v_ci <- function(x, lo = 1, hi = 4, na.rm = TRUE,
                       R = 2000, conf = 0.95, seed = NULL) {
  x <- as.numeric(x)
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) return(list(V = NA_real_, lower = NA_real_, upper = NA_real_, R = 0L))
  V <- aiken_v(x, lo, hi, na.rm = FALSE)
  if (!is.null(seed)) set.seed(seed)
  boot <- replicate(R, {
    xb <- sample(x, size = n, replace = TRUE)
    aiken_v(xb, lo, hi, na.rm = FALSE)
  })
  a <- (1 - conf) / 2
  qs <- quantile(boot, probs = c(a, 1 - a), names = FALSE, type = 6, na.rm = TRUE)
  list(V = V, lower = qs[1], upper = qs[2], R = R)
}

# 3) Tabla por ítem y criterio (y V global) ----------------------------------
# data: data.frame con columnas (por defecto):
#   "item_id", "judge_id", y criterios: "relevancia","claridad","coherencia"
# item_col, judge_col, rating_cols: **strings** con nombres de columnas
# global_method:
#   - "concat": concatena todas las valoraciones de los criterios para V_global
#   - "mean"  : promedio de las V por criterio (cada criterio pesa igual)
aiken_table <- function(data,
                        item_col    = "item_id",
                        judge_col   = "judge_id",
                        rating_cols = c("relevancia","claridad","coherencia"),
                        lo = 1, hi = 4,
                        global = TRUE,
                        global_method = c("concat","mean"),
                        cut_keep = 0.80, cut_review = 0.70,
                        lab_keep = "Mantener",
                        lab_review = "Revisar",
                        lab_rewrite = "Reescribir/Eliminar",
                        keep_cols = c("item_text","dimension")) {

  global_method <- match.arg(global_method)

  # --- comprobaciones
  need <- c(item_col, judge_col, rating_cols)
  missing <- setdiff(need, names(data))
  if (length(missing)) {
    stop("Faltan columnas en 'data': ", paste(missing, collapse = ", "))
  }

  decision_fun <- function(v) {
    ifelse(is.na(v), "Sin datos",
           ifelse(v >= cut_keep, lab_keep,
                  ifelse(v >= cut_review, lab_review, lab_rewrite)))
  }

  # Extrae columnas extra (si existen)
  keep_cols <- keep_cols[keep_cols %in% names(data)]

  # Split por ítem
  by_item <- split(data, data[[item_col]])

  out_list <- lapply(names(by_item), function(it) {
    df_i <- by_item[[it]]

    # n de jueces
    n_jueces <- length(unique(df_i[[judge_col]]))

    # V por criterio
    V_crit <- sapply(rating_cols, function(col) aiken_v(df_i[[col]], lo = lo, hi = hi))
    names(V_crit) <- paste0("V_", rating_cols)

    # Decisión por criterio
    DEC_crit <- sapply(V_crit, decision_fun)
    names(DEC_crit) <- sub("^V_", "dec_V_", names(DEC_crit))

    # V global
    if (global) {
      if (global_method == "concat") {
        vec <- unlist(df_i[rating_cols], use.names = FALSE)
        V_global <- aiken_v(vec, lo = lo, hi = hi)
      } else {
        V_global <- mean(unname(V_crit), na.rm = TRUE)
      }
      dec_global <- decision_fun(V_global)
    } else {
      V_global <- NA_real_
      dec_global <- NA_character_
    }

    # columnas extra únicas (item_text, dimension, etc.)
    extras <- list()
    if (length(keep_cols)) {
      # toma el primer valor no NA por columna
      for (kc in keep_cols) {
        val <- df_i[[kc]]
        extras[[kc]] <- if (all(is.na(val))) NA else val[which(!is.na(val))[1]]
      }
    }

    c(list(
      item_id   = it,
      n_jueces  = n_jueces
    ),
    extras,
    as.list(V_crit),
    as.list(DEC_crit),
    list(V_global = V_global, dec_global = dec_global))
  })

  # a data.frame
  out <- do.call(rbind, lapply(out_list, function(x) {
    # asegurar nombres consistentes
    all_names <- unique(unlist(lapply(out_list, names)))
    x[setdiff(all_names, names(x))] <- NA
    x[all_names]
  }))

  # convertir a data.frame “bonito”
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  # coaccionar numéricas
  num_cols <- c(paste0("V_", rating_cols), "V_global", "n_jueces")
  num_cols <- intersect(num_cols, names(out))
  for (nc in num_cols) out[[nc]] <- as.numeric(out[[nc]])

  # ordenar columnas: item_id, (extras), n_jueces, V_*, dec_V_*, V_global, dec_global
  v_cols   <- paste0("V_", rating_cols)
  dec_cols <- paste0("dec_V_", rating_cols)
  first_cols <- c("item_id", keep_cols, "n_jueces")
  last_cols  <- c("V_global","dec_global")
  middle     <- c(intersect(v_cols, names(out)), intersect(dec_cols, names(out)))
  order_cols <- c(intersect(first_cols, names(out)), middle, intersect(last_cols, names(out)))
  out <- out[, order_cols, drop = FALSE]

  rownames(out) <- NULL
  out
}

## === V de Aiken (vector) ===
aiken_v <- function(x, lo = 1, hi = 4, na.rm = TRUE) {
  x <- as.numeric(x)
  if (na.rm) x <- x[!is.na(x)]
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    stop("Verifica 'lo' y 'hi': deben ser finitos y cumplir hi > lo.")
  }
  n <- length(x)
  if (n == 0L) return(NA_real_)
  sum(x - lo) / (n * (hi - lo))
}

## === Resumen por ítem/criterio y V global (usa STRINGS de columnas) ===
aiken_table <- function(data,
                        item_col    = "item_id",
                        judge_col   = "judge_id",
                        rating_cols = c("relevancia","claridad","coherencia"),
                        lo = 1, hi = 4,
                        global = TRUE,
                        global_method = c("concat","mean"),
                        cut_keep = 0.80, cut_review = 0.70,
                        lab_keep = "Mantener",
                        lab_review = "Revisar",
                        lab_rewrite = "Reescribir/Eliminar",
                        keep_cols = c("item_text","dimension")) {

  global_method <- match.arg(global_method)

  need <- c(item_col, judge_col, rating_cols)
  missing <- setdiff(need, names(data))
  if (length(missing)) stop("Faltan columnas: ", paste(missing, collapse = ", "))

  decision_fun <- function(v) {
    ifelse(is.na(v), "Sin datos",
           ifelse(v >= cut_keep, lab_keep,
                  ifelse(v >= cut_review, lab_review, lab_rewrite)))
  }

  keep_cols <- keep_cols[keep_cols %in% names(data)]
  by_item <- split(data, data[[item_col]])

  out_list <- lapply(names(by_item), function(it) {
    df_i <- by_item[[it]]
    n_jueces <- length(unique(df_i[[judge_col]]))

    V_crit <- sapply(rating_cols, function(col) aiken_v(df_i[[col]], lo, hi))
    names(V_crit) <- paste0("V_", rating_cols)
    DEC_crit <- sapply(V_crit, decision_fun)
    names(DEC_crit) <- sub("^V_", "dec_V_", names(DEC_crit))

    if (global) {
      if (global_method == "concat") {
        vec <- unlist(df_i[rating_cols], use.names = FALSE)
        V_global <- aiken_v(vec, lo, hi)
      } else {
        V_global <- mean(unname(V_crit), na.rm = TRUE)
      }
      dec_global <- decision_fun(V_global)
    } else {
      V_global <- NA_real_; dec_global <- NA_character_
    }

    extras <- list()
    if (length(keep_cols)) {
      for (kc in keep_cols) {
        val <- df_i[[kc]]
        extras[[kc]] <- if (all(is.na(val))) NA else val[which(!is.na(val))[1]]
      }
    }

    c(list(item_id = it, n_jueces = n_jueces),
      extras, as.list(V_crit), as.list(DEC_crit),
      list(V_global = V_global, dec_global = dec_global))
  })

  out <- do.call(rbind, lapply(out_list, function(x) {
    all_names <- unique(unlist(lapply(out_list, names)))
    x[setdiff(all_names, names(x))] <- NA
    x[all_names]
  }))
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  num_cols <- c(paste0("V_", rating_cols), "V_global", "n_jueces")
  num_cols <- intersect(num_cols, names(out))
  for (nc in num_cols) out[[nc]] <- as.numeric(out[[nc]])

  v_cols   <- paste0("V_", rating_cols)
  dec_cols <- paste0("dec_V_", rating_cols)
  first_cols <- c("item_id", keep_cols, "n_jueces")
  last_cols  <- c("V_global", "dec_global")
  middle <- c(intersect(v_cols, names(out)), intersect(dec_cols, names(out)))
  order_cols <- c(intersect(first_cols, names(out)), middle, intersect(last_cols, names(out)))
  out <- out[, order_cols, drop = FALSE]
  rownames(out) <- NULL
  out
}
