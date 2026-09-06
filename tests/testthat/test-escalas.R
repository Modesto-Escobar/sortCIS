# Escala de ideologia de longitud variable. Los estudios antiguos usan una
# escala 1-7 cuyos N.S. y N.C. son el 8 y el 9: por numero caen dentro del
# rango 1-10 y no se distinguen de una respuesta.

esc <- function(v, n, ns = NULL) {
  etq <- if (is.null(ns)) NULL else ns
  haven::labelled(as.numeric(v), labels = etq,
                  label = paste0("Escala de autoubicacion ideologica (1-", n, ")"))
}

test_that("la escala 1-10 se recodifica como siempre", {
  x <- esc(c(1, 4, 5, 6, 7, 10, 98, 99), 10, c(`N.S.` = 98, `N.C.` = 99))
  expect_equal(.rec_ideologia(x), c(1, 1, 2, 2, 3, 3, NA, NA))
})

test_that("la escala 1-7 da 1-3 izquierda, 4 centro y 5-7 derecha", {
  x <- esc(1:7, 7, c(Izquierda = 1, Derecha = 7))
  expect_equal(.rec_ideologia(x), c(1, 1, 1, 2, 3, 3, 3))
})

test_that("en la escala 1-7 el 8 y el 9 son N.S./N.C., no respuestas", {
  x <- esc(c(3, 4, 7, 8, 9), 7,
           c(Izquierda = 1, Derecha = 7, `No sabe` = 8, `No contesta` = 9))
  expect_equal(.rec_ideologia(x), c(1, 2, 3, NA, NA))
})

test_that("el rango se lee de la etiqueta de la variable", {
  expect_equal(.rango_ideologia(esc(1:7, 7)), 7)
  expect_equal(.rango_ideologia(esc(1:10, 10)), 10)
})

test_that("sin rango en la etiqueta se usa el mayor codigo no perdido", {
  x <- haven::labelled(c(1, 7, 8), label = "Escala de autoubicacion ideologica",
                       labels = c(Izquierda = 1, Derecha = 7, `No sabe` = 8))
  expect_equal(.rango_ideologia(x), 7)
  expect_equal(.rec_ideologia(x), c(1, 3, NA))
})

test_that("sin etiquetas de ninguna clase se supone 1-10", {
  expect_equal(.rango_ideologia(c(1, 5, 10)), 10)
})

# Formulaciones antiguas de estudios y de situacion laboral.

lab <- function(v, etiquetas) haven::labelled(as.numeric(v), labels = etiquetas)

test_that("las categorias de estudios de 1979 se recodifican", {
  x <- lab(1:8, c(`Menos de estudios primarios, no sabe leer` = 1,
                  `Menos de estudios primarios, sabe leer`    = 2,
                  `Estudios primarios completos`              = 3,
                  `Bachiller elemental`                       = 4,
                  `Bachiller superior`                        = 5,
                  `Estudios de formacion profesional`         = 6,
                  `Estudios de grado medio`                   = 7,
                  `Universitarios o tecnicos de grado superior` = 8))
  expect_equal(.rec_estudios(x), c(100, 100, 200, 200, 300, 300, 500, 500))
})

test_that("'Estudios primarios sin finalizar' es Sin estudios, no Primaria", {
  x <- lab(1:2, c(`Estudios primarios sin finalizar` = 1,
                  `Estudios primarios`               = 2))
  expect_equal(.rec_estudios(x), c(100, 200))
})

test_that("'Sus labores' es trabajo domestico", {
  x <- lab(1:3, c(Trabaja = 1, `Sus labores` = 2,
                  `Trabajo domestico no remunerado` = 3))
  expect_equal(.rec_laboral(x), c(1, 5, 5))
})

test_that("'Esta parado' cuenta como parado", {
  x <- lab(1:3, c(`Esta parado` = 1, `Parado y ha trabajado antes` = 2,
                  `En paro` = 3))
  expect_equal(.rec_laboral(x), c(3, 3, 3))
})
