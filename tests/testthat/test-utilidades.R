test_that(".norm quita acentos, mayusculas y aclaraciones", {
  expect_equal(.norm("Escala de autoubicación ideológica (1-10)"),
               "escala de autoubicacion ideologica")
  expect_equal(.norm("  SEXO   del    Entrevistado "), "sexo del entrevistado")
  expect_equal(.norm("Católico/a NO practicante"), "catolico/a no practicante")
  expect_equal(.norm("AÑO"), "ano")
})

test_that(".norm quita solo el parentesis, no lo que va detras", {
  # Asi lo etiqueta el 3577; cortar desde el "(" dejaba "escala"
  expect_equal(.norm("Escala (1-10) de autoubicación ideológica"),
               "escala de autoubicacion ideologica")
  expect_equal(.norm("Escala (1-10) de autoubicación (sic"), "escala de autoubicacion")
})

test_that("detecta la ideologia con el parentesis en medio de la etiqueta", {
  d <- data.frame(ESCIDEOL = 1:3)
  attr(d$ESCIDEOL, "label") <- "Escala (1-10) de autoubicación ideológica"
  expect_equal(.detecta(d, .PATRON$ideologia), "ESCIDEOL")
})

test_that(".norm tolera lo vacio y lo nulo", {
  expect_equal(.norm(NULL), character(0))
  expect_equal(.norm(character(0)), character(0))
})

test_that(".etiqueta devuelve cadena vacia si no hay etiqueta", {
  expect_equal(.etiqueta(1:3), "")
  expect_equal(.etiqueta(haven::labelled(1:3, label = "Una etiqueta")),
               "Una etiqueta")
})

test_that(".valores indexa las etiquetas normalizadas por su codigo", {
  x <- haven::labelled(c(1, 2), labels = c(`Católico` = 1, `N.C.` = 9))
  expect_equal(.valores(x), c(`1` = "catolico", `9` = "n.c."))
  expect_null(.valores(1:3))
})
