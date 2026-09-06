test_that("detecta la variable por su etiqueta, no por su nombre", {
  m <- estudio_moderno()
  a <- estudio_antiguo()
  expect_equal(.detecta(m, .PATRON$genero),      "SEXO")
  expect_equal(.detecta(a, .PATRON$genero),      "P23")
  expect_equal(.detecta(m, .PATRON$ideologia),   "ESCIDEOL")
  expect_equal(.detecta(a, .PATRON$ideologia),   "P17")
  expect_equal(.detecta(m, .PATRON$profesional), "SITPROF")
  expect_equal(.detecta(a, .PATRON$profesional), "P43")
})

test_that("devuelve NA si el concepto no esta en el fichero", {
  m <- estudio_moderno()
  m$CLASESUB <- NULL
  expect_true(is.na(.detecta(m, .PATRON$clase)))
})

test_that("el orden de los patrones manda: 'nivel alcanzado' antes que 'estudios de'", {
  m <- estudio_moderno()
  m$NIVELEST <- haven::labelled(
    c(1, 2, 3, 4, 5, 6, 9, 2),
    labels = c(`Sin estudios` = 1, `Educación primaria` = 2),
    label = "Nivel de estudios alcanzado por la persona entrevistada")
  expect_equal(.detecta(m, .PATRON$estudios), "NIVELEST")
  # y la agrupada sigue disponible como respaldo
  expect_equal(.detecta_todas(m, .PATRON$estudios), c("NIVELEST", "ESTUDIOS"))
})

test_that("a igualdad de patron gana la que trae etiquetas de valor", {
  m <- estudio_moderno()
  m$SEXOR <- structure(c(1, 2, 1, 2, 1, 2, 1, 2),
                       label = "Sexo de la persona entrevistada")
  expect_equal(.detecta(m, .PATRON$genero), "SEXO")
})

test_that("detecta la frecuencia de asistencia a oficios", {
  expect_equal(.detecta(estudio_antiguo(), .PAT_PRACTICA), "P31")
  expect_true(is.na(.detecta(estudio_moderno(), .PAT_PRACTICA)))
})
