# Escalas y Likert del resto del fichero, con salida = "completo".

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
  d <- completo(NAs = TRUE)
  expect_true(is.factor(d$ESTCIVIL))
  expect_equal(levels(d$ESTCIVIL), c("Casado/a", "Soltero/a"))
  expect_true(is.na(d$ESTCIVIL[8]))
  # un valor sin etiqueta: es una cantidad, pasa a numero
  expect_true(is.numeric(completo(IAs = TRUE)$IA_I543))
  # por defecto, sin orden: el perdido es un nivel, con su etiqueta
  d2 <- completo()
  expect_equal(levels(d2$ESTCIVIL), c("Casado/a", "Soltero/a", "N.C."))
  expect_equal(as.character(d2$ESTCIVIL[8]), "N.C.")
})

test_that("cada etiqueta de perdido del original es su propio nivel", {
  m <- con_preguntas()
  m$ESTCIVIL <- .v(c(1, 2, 8, 9, 1, NA, 1, 9), "Estado civil",
                   c(`Casado/a` = 1, `Soltero/a` = 2, `N.S.` = 8, `N.C.` = 9))
  f <- sav_temporal(m, "3557")
  d <- cis_sociodem(f, salida = "completo", tabla = tabla_temporal(),
                    escribir = FALSE, avisos = FALSE)
  expect_equal(levels(d$ESTCIVIL), c("Casado/a", "Soltero/a", "N.S.", "N.C."))
  expect_equal(as.character(d$ESTCIVIL[3:4]), c("N.S.", "N.C."))
  # un hueco sin codigo no es ni N.S. ni N.C.
  expect_true(is.na(d$ESTCIVIL[6]))
})

test_that("escalas y Likert: numericas siempre a NA; como factor, segun NAs", {
  d <- completo(NAs = FALSE)
  expect_equal(d$ecoper, c(5, 4, 3, 2, 1, NA, NA, 5), ignore_attr = TRUE)
  expect_equal(d$valsan, c(1, 5, 10, NA, NA, 3, 7, NA), ignore_attr = TRUE)
  # como factor y por defecto: ordenadas, asi que NA
  d <- completo(likert = "factor", escala = "factor")
  expect_true(is.na(d$ecoper[6]))
  expect_false("N.S." %in% levels(d$ecoper))
  # pedidas explicitas: siguen ordenadas, con los perdidos detras
  d <- completo(likert = "factor", escala = "factor", NAs = FALSE)
  expect_true(is.ordered(d$ecoper))
  expect_equal(tail(levels(d$ecoper), 2), c("N.S.", "N.C."))
  expect_equal(as.character(d$ecoper[6:7]), c("N.S.", "N.C."))
  expect_equal(as.character(d$valsan[4]), "No conoce")
})

test_that("NAs con nombres: esas a NA y las demas explicitas", {
  d <- completo(NAs = c("ESTCIVIL", "religion"))
  expect_true(is.na(d$ESTCIVIL[8]))
  expect_true(is.na(d$religion[8]))
  expect_equal(as.character(d$laboral[8]), "N.C.")
  # una ordenada que no se nombra tambien va explicita
  expect_equal(as.character(d$ideologia[7:8]), c("N.S.", "N.C."))
  expect_warning(completo(NAs = c("NOEXISTE", "religion")), "NOEXISTE")
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
