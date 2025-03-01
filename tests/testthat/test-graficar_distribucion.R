test_that("graficar_distribucion crea gráficos correctamente", {
  # Prueba 1: Distribución Z básica
  p_z <- graficar_distribucion("z", nivel_confianza = 0.95, tipo_prueba = "dos_colas")
  expect_s3_class(p_z, "gg")

  # Prueba 2: Distribución t con validación de parámetros
  p_t <- graficar_distribucion("t", nivel_confianza = 0.90, tipo_prueba = "una_cola_derecha", gl = 15)
  expect_s3_class(p_t, "gg")

  # Prueba 3: Distribución Chi-cuadrado
  p_chi <- graficar_distribucion("chi", nivel_confianza = 0.95, gl = 5)
  expect_s3_class(p_chi, "gg")

  # Prueba 4: Distribución F con valor calculado
  p_f <- graficar_distribucion("f", nivel_confianza = 0.95, gl = c(5, 20), valor_cal = 2.5)
  expect_s3_class(p_f, "gg")

  # Prueba 5: Verificar validación de parámetros
  expect_error(graficar_distribucion("t", nivel_confianza = 0.95))
  expect_error(graficar_distribucion("chi", nivel_confianza = 0.95))
  expect_error(graficar_distribucion("f", nivel_confianza = 0.95, gl = 5))

  # Prueba 6: Verificar intervalo de confianza
  p_int <- graficar_distribucion("chi", nivel_confianza = 0.95, tipo_prueba = "intervalo_confianza", gl = 10)
  expect_s3_class(p_int, "gg")
})
