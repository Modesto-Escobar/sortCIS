m <- estudio_moderno()
a <- estudio_antiguo()

test_that("genero", {
  expect_equal(.rec_genero(m$SEXO), c(1, 2, 1, 2, 1, 2, 1, 2))
  expect_equal(.rec_genero(a$P23),  c(1, 2, 1, 2, 1, 2, 1, 2))
})

test_that("el 99 se descarta porque la etiqueta lo declara N.C., no por ser 99", {
  expect_equal(.rec_edad_anyos(m$EDAD), c(25, 40, 55, 70, 85, 30, NA, 45))
  # si el 99 no esta etiquetado como N.C., es una edad y se respeta
  x <- haven::labelled(c(25, 99), labels = c(`No procede` = 98),
                       label = "Edad de la persona entrevistada")
  expect_equal(.rec_edad_anyos(x), c(25, 99))
  # sin etiquetas de valor no hay mas remedio que ir por el numero
  expect_equal(.rec_edad_anyos(c(25, 99, 999)), c(25, NA, NA))
})

test_that("la edad fuera de rango plausible va a perdido", {
  x <- haven::labelled(c(3, 16, 120, 130), label = "Edad")
  expect_equal(.rec_edad_anyos(x), c(NA, 16, 120, NA))
})

test_that("tramos de edad", {
  expect_equal(.rec_edad_tramos(c(20, 34, 35, 49, 50, 64, 65, 79, 80, NA)),
               c(1, 1, 2, 2, 3, 3, 4, 4, 5, NA))
})

test_that("generacion sale del anyo de nacimiento", {
  expect_equal(.rec_generacion(c(25, 40, 55, 70, 85, 30, NA, 45), 2026),
               c(8, 7, 6, 5, 4, 7, NA, 7))
  # sin anyo de campo no hay generacion, pero no revienta
  expect_equal(.rec_generacion(c(25, 40), NA), c(NA_real_, NA_real_))
})

test_that("ideologia en tres tramos, con N.S. y N.C. a perdido", {
  expect_equal(.rec_ideologia(m$ESCIDEOL), c(1, 2, 3, 1, 2, 3, NA, NA))
  expect_equal(.rec_ideologia(a$P17),      c(1, 2, 3, 1, 2, 3, NA, NA))
})

test_that("religion: el esquema moderno distingue practica por si solo", {
  expect_equal(.rec_religion(m$RELIGION), c(1, 2, 3, 3, 3, 1, 2, NA))
})

test_that("religion: el esquema antiguo necesita la asistencia a oficios", {
  # sin la variable de practica, todo catolico cuenta como practicante
  expect_equal(.rec_religion(a$P30), c(1, 1, 3, 3, 3, 1, 1, NA))
  # con ella, el que casi nunca va pasa a catolico no practicante
  expect_equal(.rec_religion(a$P30, a$P31), c(1, 2, 3, 3, 3, 2, 1, NA))
})

test_that("estudios: la F.P. va a secundaria superior en ambos estudios", {
  esperado <- c(100, 200, 200, 300, 300, 500, NA, 200)
  expect_equal(.rec_estudios(m$ESTUDIOS), esperado)
  expect_equal(.rec_estudios(a$P28),      esperado)
})

test_that("estudios: la agrupada rellena los huecos de la detallada", {
  detallada <- haven::labelled(c(NA, 6, NA), labels = c(`Superiores` = 6))
  agrupada  <- haven::labelled(c(1, 1, 9),
                               labels = c(`Sin estudios` = 1, `N.C.` = 9))
  expect_equal(.rec_estudios(detallada, agrupada), c(100, 500, NA))
})

test_that("laboral", {
  esperado <- c(1, 2, 3, 4, 5, NA, 1, NA)
  expect_equal(.rec_laboral(m$SITLAB), esperado)
  expect_equal(.rec_laboral(a$P40),    esperado)
})

test_that("profesional: mismas categorias con codigos distintos dan el mismo resultado", {
  # "Ayuda familiar" es el codigo 5 en el estudio moderno y el 4 en el
  # antiguo; "Otra situacion" el 6 y el 7. Recodificar por numero daria
  # resultados distintos; por etiqueta salen iguales, que es de lo que
  # depende que los dos ficheros se puedan apilar.
  esperado <- c(1, 2, 2, 6, 6, 0, 1, NA)
  expect_equal(.rec_profesional(m$SITPROF), esperado)
  expect_equal(.rec_profesional(a$P43),     esperado)
})

test_that("clase subjetiva", {
  esperado <- c(1, 1, 2, 4, 3, 4, 9, NA)
  expect_equal(.rec_clase(m$CLASESUB), esperado)
  expect_equal(.rec_clase(a$P32),      esperado)
})

test_that(".por_etiqueta cae al codigo solo si no hay etiquetas", {
  expect_true(all(is.na(.por_etiqueta(c(1, 2), c(hombre = 1)))))
  expect_equal(.rec_genero(c(1, 2, 3)), c(1, 2, NA))
})
