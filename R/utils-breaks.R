# Calcula los puntos de corte para una variable cuantitativa segun 'metodo'.
# Uso interno -- comparten esta logica tabla_frecuencia(), tabla_cuantitativa()
# y graficos_cuantitativos() para no repetirla (y no desincronizarla) 3 veces.
#
# Si el usuario no fija 'k', el metodo elegido manda de verdad: sturges usa
# grDevices::nclass.Sturges() para decidir cuantas clases de igual ancho usar
# (la misma regla que usa hist() por defecto); fd/scott usan hist(breaks=...)
# tal cual, que ya calcula el ancho real de esas reglas (y por lo tanto el
# numero de clases no siempre es un numero "lindo").
#
# Si el usuario SI fija 'k', eso manda sobre el metodo (pedir una regla
# automatica Y un numero de clases fijo es contradictorio) -- se avisa con
# un message() cuando el metodo no era 'sturges', para no hacerlo en silencio.
.rodas_puntos_corte <- function(datos, k = NULL, metodo = "sturges", breaks = NULL) {
  if (metodo == "manual") {
    if (is.null(breaks)) {
      stop("Si el metodo es 'manual', debe proporcionar los puntos de corte en 'breaks'")
    }
    return(breaks)
  }

  rango <- range(datos)

  if (!is.null(k)) {
    if (metodo != "sturges") {
      message(
        "Se especifico k = ", k, ": se usan ", k, " intervalos de igual ancho y se ",
        "ignora metodo = '", metodo, "' (esa regla sirve justamente para que k salga ",
        "sola a partir de los datos; si ya fijas k, no hay nada que calcular)."
      )
    }
    return(seq(from = rango[1], to = rango[2], length.out = k + 1))
  }

  if (metodo == "sturges") {
    k_auto <- grDevices::nclass.Sturges(datos)
    return(seq(from = rango[1], to = rango[2], length.out = k_auto + 1))
  } else if (metodo == "fd") {
    return(hist(datos, breaks = "FD", plot = FALSE)$breaks)
  } else if (metodo == "scott") {
    return(hist(datos, breaks = "scott", plot = FALSE)$breaks)
  }

  stop("El metodo debe ser 'sturges', 'fd', 'scott' o 'manual'")
}
