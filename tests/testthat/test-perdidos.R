# Umbral para los perdidos explicitos.

# 50 a, 40 b, 8 N.S., 2 N.C. y 100 huecos de filtro
.con_perdidos <- function(perdidos = c("N.S.", "N.C.")) {
  f <- factor(c(rep("a", 50), rep("b", 40), rep("N.S.", 8), rep("N.C.", 2),
                rep(NA, 100)), levels = c("a", "b", "N.S.", "N.C."))
  attr(f, "label") <- "Una pregunta"
  attr(f, "perdidos") <- perdidos
  f
}

test_that("cada nivel de perdido se mira por separado", {
  f <- .recorta(.con_perdidos(), "P1", 0.05)
  # el N.C. (2%) se va y su nivel se quita; el N.S. (8%) se queda
  expect_equal(levels(f), c("a", "b", "N.S."))
  expect_equal(sum(f == "N.S.", na.rm = TRUE), 8)
  expect_equal(sum(is.na(f)), 102)
  expect_equal(attr(f, "label"), "Una pregunta")
  expect_equal(attr(f, "perdidos"), "N.S.")
})

test_that("los huecos de un filtro no cuentan en el denominador", {
  # el N.S. es el 8% de los que tienen valor, y el 4% de todos
  f <- .recorta(.con_perdidos(), "P1", 0.06)
  expect_true("N.S." %in% levels(f))
})

test_that("el umbral no toca lo que no es perdido", {
  f <- .recorta(.con_perdidos(), "P1", 0.5)
  expect_equal(levels(f), c("a", "b"))
  expect_equal(sum(f == "a", na.rm = TRUE), 50)
})

test_that("en las armonizadas el nivel se queda aunque se vacie", {
  f <- .recorta(.con_perdidos(), "ideologia", 0.5)
  expect_equal(levels(f), c("a", "b", "N.S.", "N.C."))
  expect_false(any(f %in% c("N.S.", "N.C.")))
})

test_that("sin el atributo, los perdidos se reconocen por la etiqueta", {
  f <- .con_perdidos()
  attr(f, "perdidos") <- NULL
  expect_equal(levels(.recorta(f, "P1", 0.05)), c("a", "b", "N.S."))
  # el N.P. de profesional es una categoria, no un perdido
  p <- factor(c(rep("N.P.", 3), rep("Asalariado", 97), "N.C."),
              levels = c("N.P.", "Asalariado", "N.S.", "N.C."))
  r <- .recorta(p, "profesional", 0.05)
  expect_equal(sum(r == "N.P.", na.rm = TRUE), 3)
  expect_true(is.na(r[101]))
})

test_that("sobre un estudio: alcanza a todas, armonizadas incluidas", {
  f <- sav_temporal(con_preguntas(), "3557")
  d <- cis_sociodem(f, salida = "completo", tabla = tabla_temporal(),
                    escribir = FALSE, avisos = FALSE)
  # ESTCIVIL: un N.C. de ocho, el 12,5%
  expect_equal(as.character(recorta_perdidos(d, 0.10)$ESTCIVIL[8]), "N.C.")
  r <- recorta_perdidos(d, 0.30)
  expect_true(is.na(r$ESTCIVIL[8]))
  expect_false("N.C." %in% levels(r$ESTCIVIL))
  # laboral: dos N.C. de ocho (uno es "Otra situacion"). Se van, pero el
  # nivel se queda para que se apile
  expect_true(is.na(r$laboral[8]))
  expect_equal(tail(levels(r$laboral), 2), c("N.S.", "N.C."))
  # 'variables' lo limita
  r <- recorta_perdidos(d, 0.30, variables = "ESTCIVIL")
  expect_equal(as.character(r$laboral[8]), "N.C.")
})

test_that("recorta_perdidos sobre estudios apilados", {
  f <- sav_temporal(estudio_moderno(), "3557")
  a <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  todo <- recorta_perdidos(rbind(a, a), umbral = 0.30)
  expect_true(is.na(todo$laboral[8]))
  expect_equal(levels(todo$laboral), levels(a$laboral))
  expect_warning(recorta_perdidos(a, 0.1, "NOEXISTE"), "NOEXISTE")
})

test_that("umbral solo admite una proporcion", {
  expect_error(recorta_perdidos(data.frame(x = 1), umbral = 10), "umbral")
  expect_error(recorta_perdidos(data.frame(x = 1), umbral = c(0.1, 0.2)), "umbral")
  expect_error(recorta_perdidos(data.frame(x = 1), umbral = NULL), "umbral")
})

test_that("el No procede de un filtro va siempre a NA, nunca a un nivel", {
  m <- con_preguntas()
  m$ESTCIVIL <- .v(c(1, 2, 0, 9, 1, 0, 1, 2), "Estado civil",
                   c(`No procede (filtro)` = 0, `Casado/a` = 1, `Soltero/a` = 2,
                     `N.C.` = 9))
  # y en una armonizada: el 0 No procede de la religiosidad
  m$RELIGION <- .v(c(1, 2, 3, 4, 5, 1, 0, 9),
                   "Religiosidad de la persona entrevistada",
                   c(`No procede` = 0, `Católico/a practicante` = 1,
                     `Católico/a no practicante` = 2,
                     `Creyente de otra religión` = 3,
                     `No creyente` = 4, `Ateo/a` = 5, `N.C.` = 9))
  f <- sav_temporal(m, "3557")
  d <- cis_sociodem(f, salida = "completo", tabla = tabla_temporal(),
                    escribir = FALSE, avisos = FALSE, NAs = FALSE)
  expect_equal(levels(d$ESTCIVIL), c("Casado/a", "Soltero/a", "N.C."))
  expect_true(all(is.na(d$ESTCIVIL[c(3, 6)])))
  expect_equal(as.character(d$ESTCIVIL[4]), "N.C.")
  expect_true(is.na(d$religion[7]))
  expect_equal(as.character(d$religion[8]), "N.C.")
})
