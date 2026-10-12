# Escalas y Likert del resto del fichero, con salida = "completo".

# Likert al modo del CIS: primero el polo alto, el "Regular" entre
# parentesis de (NO LEER) y los perdidos en 8 y 9.
.likert5 <- function(x) .v(x, "Valoracion de la situacion economica personal",
  c(`Muy buena` = 1, Buena = 2, `(NO LEER) Regular` = 3, Mala = 4,
    `Muy mala` = 5, `N.S.` = 8, `N.C.` = 9))

.confianza4 <- function(x) .v(x, "Grado de confianza en el presidente",
  c(`Mucha confianza` = 1, `Bastante confianza` = 2, `Poca confianza` = 3,
    `Ninguna confianza` = 4, `N.C.` = 9))

# Escala de estudio reciente: los intermedios etiquetados con su numero.
.escala_moderna <- function(x) .v(x, "Pedro Sanchez",
  c(`1 Muy mal` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7,
    `8` = 8, `9` = 9, `10 Muy bien` = 10, `No conoce` = 97, `N.S.` = 98,
    `N.C.` = 99))

con_preguntas <- function() {
  m <- estudio_moderno()
  m$ECOPER   <- .likert5(c(1, 2, 3, 4, 5, 8, 9, 1))
  m$CONFPTE  <- .confianza4(c(1, 2, 3, 4, 9, 1, 2, 3))
  m$VALSAN   <- .escala_moderna(c(1, 5, 10, 97, 98, 3, 7, 99))
  m$ESTCIVIL <- .v(c(1, 2, 1, 2, 1, 2, 1, 9), "Estado civil",
                   c(`Casado/a` = 1, `Soltero/a` = 2, `N.C.` = 9))
  m$IA_I543  <- .v(c(0, 1, 2, 3, 4, 5, 6, 7), "Otras incidencias", c(No = 0))
  m
}

completo <- function(...) {
  f <- sav_temporal(con_preguntas(), "3557")
  cis_sociodem(f, salida = "completo", tabla = tabla_temporal(),
               escribir = FALSE, avisos = FALSE, ...)
}

test_that("reconoce escalas y Likert por las etiquetas de valor", {
  m <- con_preguntas()
  expect_equal(.tipo_pregunta(m$ECOPER), "likert")
  expect_equal(.tipo_pregunta(m$CONFPTE), "likert")
  expect_equal(.tipo_pregunta(m$VALSAN), "escala")
  # escala antigua: sin etiquetas para los puntos
  expect_equal(.tipo_pregunta(m$ESCIDEOL), "escala")
  expect_equal(.tipo_pregunta(m$ESTCIVIL), "")
  expect_equal(.tipo_pregunta(m$SEXO), "")
  # los paradatos no son preguntas aunque tengan forma de escala
  expect_equal(.tipo_pregunta(m$IA_I543, "IA_I543"), "")
})

test_that("las Likert se invierten: el polo positivo es el mas alto", {
  d <- completo()
  expect_equal(d$ecoper, c(5, 4, 3, 2, 1, NA, NA, 5), ignore_attr = TRUE)
  expect_equal(d$confpte, c(4, 3, 2, 1, NA, 4, 3, 2), ignore_attr = TRUE)
})

test_that("centrada = TRUE pone el punto medio en 0 y salta el 0 si no lo hay", {
  d <- completo(centrada = TRUE)
  expect_equal(d$ecoper, c(2, 1, 0, -1, -2, NA, NA, 2), ignore_attr = TRUE)
  expect_equal(d$confpte, c(2, 1, -1, -2, NA, 2, 1, -1), ignore_attr = TRUE)
})

test_that("una Likert que empieza por el polo bajo no se invierte", {
  x <- .v(c(1, 2, 3, 4), "Al reves", c(`Muy mala` = 1, Mala = 2, Buena = 3,
                                       `Muy buena` = 4))
  expect_equal(.likert_numerica(x), c(1, 2, 3, 4))
  expect_equal(levels(.likert_factor(x)), c("Muy mala", "Mala", "Buena", "Muy buena"))
})

test_that("las escalas pasan a numero tal cual, con los perdidos a NA", {
  d <- completo()
  expect_equal(d$valsan, c(1, 5, 10, NA, NA, 3, 7, NA), ignore_attr = TRUE)
  expect_equal(d$escideol, c(1, 5, 9, 3, 6, 10, NA, NA), ignore_attr = TRUE)
})

test_that("likert y escala = 'factor' dan factores ordenados de bajo a alto", {
  d <- completo(likert = "factor", escala = "factor")
  expect_true(is.ordered(d$ecoper))
  expect_equal(levels(d$ecoper),
               c("Muy mala", "Mala", "(NO LEER) Regular", "Buena", "Muy buena"))
  expect_true(is.ordered(d$valsan))
  expect_equal(levels(d$valsan)[c(1, 10)], c("1 Muy mal", "10 Muy bien"))
})

test_that("orden de columnas: claves, sociodemograficas, convertidas, originales", {
  d <- completo()
  socio <- names(.ETIQ)
  expect_equal(names(d)[1:11], c("orden", "numentr", socio))
  conv <- c("escideol", "ecoper", "confpte", "valsan")
  expect_setequal(names(d)[12:15], conv)
  expect_true(all(c("ECOPER", "VALSAN", "ESCIDEOL") %in% names(d)[-(1:15)]))
})

test_that("las IA_ se quitan salvo con IAs = TRUE, y nunca se convierten", {
  expect_false(any(grepl("^IA_", names(completo()), ignore.case = TRUE)))
  d <- completo(IAs = TRUE)
  expect_true("IA_I543" %in% names(d))
  expect_false("ia_i543" %in% names(d))
  # tambien en el fichero escrito
  f <- sav_temporal(con_preguntas(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, salida = "completo",
               tabla = tabla_temporal(), avisos = FALSE)
  expect_false("IA_I543" %in% names(haven::read_sav(paste0(destino, ".sav"))))
  expect_error(completo(IAs = "no"), "IAs")
})

test_that("con factores = TRUE el resto etiquetado pasa a factor", {
  d <- completo()
  expect_true(is.factor(d$ESTCIVIL))
  expect_equal(levels(d$ESTCIVIL), c("Casado/a", "Soltero/a"))
  expect_true(is.na(d$ESTCIVIL[8]))
  # un valor sin etiqueta: es una cantidad, pasa a numero
  expect_true(is.numeric(completo(IAs = TRUE)$IA_I543))
  d2 <- completo(NAs = FALSE)
  expect_equal(as.character(d2$ESTCIVIL[8]), "N.C.")
})

test_that("con factores = FALSE el original se queda etiquetado", {
  d <- completo(factores = FALSE)
  expect_s3_class(d$ESTCIVIL, "haven_labelled")
  expect_equal(d$ecoper, c(5, 4, 3, 2, 1, NA, NA, 5), ignore_attr = TRUE)
})

test_that("el fichero escrito no cambia con las conversiones", {
  f <- sav_temporal(con_preguntas(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, salida = "completo",
               tabla = tabla_temporal(), avisos = FALSE)
  vuelta <- haven::read_sav(paste0(destino, ".sav"))
  expect_false("ecoper" %in% names(vuelta))
  expect_s3_class(vuelta$ECOPER, "haven_labelled")
})

test_that("likert, escala y centrada rechazan valores que no son suyos", {
  expect_error(completo(likert = "texto"))
  expect_error(completo(escala = "texto"))
  expect_error(completo(centrada = "si"), "centrada")
})

test_that("las convertidas conservan la etiqueta de la pregunta", {
  d <- completo()
  expect_equal(attr(d$ecoper, "label"), "Valoracion de la situacion economica personal")
})
