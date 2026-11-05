# cisConvert() is cis_sociodem() in English: the same data, with other
# argument names. Here it is checked that both give the same thing.

# What changes besides the arguments: the attribute names. Brought to
# those of cis_sociodem() to compare.
.to_sociodem <- function(d) {
  attr(d, "detected") <- NULL
  if (!is.null(attr(d, "varnames"))) {
    attr(d, "nombres") <- attr(d, "varnames")
    attr(d, "varnames") <- NULL
  }
  for (v in seq_along(d)) {
    p <- attr(d[[v]], "missingLevels")
    if (!is.null(p)) {
      attr(d[[v]], "missingLevels") <- NULL
      attr(d[[v]], "perdidos") <- p
    }
  }
  d
}

.both <- function(f, en = list(), es = list()) {
  a <- do.call(cis_sociodem, c(list(f, tabla = tabla_temporal(),
                                    escribir = FALSE, avisos = FALSE), es))
  b <- do.call(cisConvert, c(list(f, write = FALSE, verbose = FALSE), en))
  list(es = a, en = .to_sociodem(b))
}

test_that("cisConvert gives the same as cis_sociodem", {
  fm <- sav_temporal(con_preguntas(), "3557")
  fa <- sav_temporal(estudio_antiguo(), "2798")
  cases <- list(
    list(en = list(), es = list()),
    list(en = list(output = "audit"), es = list(salida = "auditoria")),
    list(en = list(output = "full"), es = list(salida = "completo")),
    list(en = list(factors = FALSE), es = list(factores = FALSE)),
    list(en = list(NAs = FALSE), es = list(NAs = FALSE)),
    list(en = list(NAs = TRUE), es = list(NAs = TRUE)),
    list(en = list(output = "full", NAs = FALSE, likert = "factor",
                   scale = "factor"),
         es = list(salida = "completo", NAs = FALSE, likert = "factor",
                   escala = "factor")),
    list(en = list(output = "full", centered = TRUE, IAs = TRUE),
         es = list(salida = "completo", centrada = TRUE, IAs = TRUE)),
    list(en = list(output = "full", labels = TRUE),
         es = list(salida = "completo", etiquetas = TRUE)),
    list(en = list(year = 2000), es = list(anyo = 2000))
  )
  for (f in c(fm, fa)) for (k in cases) {
    r <- .both(f, k$en, k$es)
    expect_identical(r$en, r$es, info = paste(basename(f), deparse(k$en)))
  }
})

test_that("the written files are the same", {
  f <- sav_temporal(con_preguntas(), "3557")
  for (k in list(list(en = "original", es = "original"),
                 list(en = "as_R", es = "como_R"))) {
    a <- tempfile(); b <- tempfile()
    cis_sociodem(f, destino = a, tabla = tabla_temporal(), avisos = FALSE,
                 salida = "completo", grabar = k$es, formato = "ambos")
    cisConvert(f, dest = b, verbose = FALSE, output = "full",
                writeAs = k$en, format = "both")
    # Read back and not by md5: the .sav header carries the time of writing
    expect_identical(haven::read_sav(paste0(b, ".sav")),
                     haven::read_sav(paste0(a, ".sav")), info = k$en)
    expect_identical(haven::read_dta(paste0(b, ".dta")),
                     haven::read_dta(paste0(a, ".dta")), info = k$en)
  }
})

test_that("cisConvert writes no correspondence table", {
  dir <- file.path(tempdir(), "no_tsv")
  dir.create(file.path(dir, "datos"), recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, "3557.sav")
  haven::write_sav(estudio_moderno(), f)
  cisConvert(f, verbose = FALSE)
  expect_setequal(list.files(dir, recursive = TRUE),
                  c("3557.sav", "3557_soc.sav"))
})

test_that("'variables' overrides the detection", {
  m <- estudio_moderno()
  attr(m$ESCIDEOL, "label") <- "Autoubicacion"   # unusual label
  f <- sav_temporal(m, "3557")
  d <- cisConvert(f, write = FALSE, verbose = FALSE)
  expect_true(all(is.na(d$ideologia)))
  d <- cisConvert(f, write = FALSE, verbose = FALSE,
                   variables = c(ideologia = "ESCIDEOL"))
  expect_equal(as.character(d$ideologia)[1:6],
               c("Izquierda", "Centro", "Derecha", "Izquierda", "Centro",
                 "Derecha"))
  det <- attr(d, "detected")
  expect_equal(det$variable[det$concept == "ideologia"], "ESCIDEOL")
  expect_equal(det$source[det$concept == "ideologia"], "manual")
  expect_equal(det$source[det$concept == "genero"], "auto")

  # "" declares that the concept is not in the study
  d <- cisConvert(f, write = FALSE, verbose = FALSE,
                   variables = c(clase = ""))
  expect_true(all(is.na(d$clase)))
})

test_that("'variables' is checked", {
  f <- sav_temporal(estudio_moderno(), "3557")
  expect_error(cisConvert(f, write = FALSE, verbose = FALSE,
                           variables = c(sex = "SEXO")), "unknown concepts")
  expect_error(cisConvert(f, write = FALSE, verbose = FALSE,
                           variables = c(genero = "NOPE")), "not in the file")
  expect_error(cisConvert(f, write = FALSE, verbose = FALSE,
                           variables = "SEXO"), "named character")
})

test_that("the arguments are checked in English", {
  f <- sav_temporal(estudio_moderno(), "3557")
  expect_error(cisConvert(f, write = FALSE, factors = NA), "must be TRUE or FALSE")
  expect_error(cisConvert(f, write = FALSE, output = "breve"), "should be one of")
  expect_error(cisConvert("nope.sav"), "File not found")
})

test_that("a question without a variable label is written with as_R", {
  # attr(x, "label") matches "labels" partially: without exact = TRUE the
  # converted variable took the value labels as its label and write_sav
  # failed (P43 and P44 of 2798).
  m <- con_preguntas()
  attr(m$ECOPER, "label") <- NULL
  f <- sav_temporal(m, "3557")
  d <- cisConvert(f, write = FALSE, verbose = FALSE, output = "full")
  expect_null(attr(d$ecoper, "label"))
  out <- tempfile()
  expect_no_error(cisConvert(f, dest = out, verbose = FALSE, output = "full",
                              writeAs = "as_R"))
  expect_true(file.exists(paste0(out, ".sav")))
})

test_that("trimMissing gives the same as recorta_perdidos", {
  f <- sav_temporal(con_preguntas(), "3557")
  for (k in list(list(en = list(output = "full", NAs = FALSE),
                      es = list(salida = "completo", NAs = FALSE)),
                 list(en = list(output = "full", labels = TRUE),
                      es = list(salida = "completo", etiquetas = TRUE)),
                 list(en = list(), es = list()))) {
    r <- .both(f, k$en, k$es)
    b <- cisConvert(f, write = FALSE, verbose = FALSE, output = "full")
    for (th in c(0.05, 0.10, 0.30)) {
      es <- recorta_perdidos(r$es, th)
      en <- .to_sociodem(trimMissing(do.call(cisConvert,
              c(list(f, write = FALSE, verbose = FALSE), k$en)), th))
      expect_identical(en, es, info = paste(deparse(k$en), th))
      # stacked, where the attribute is lost
      expect_identical(.to_sociodem(trimMissing(rbind(b, b), th)),
                       recorta_perdidos(rbind(.both(f, list(output = "full"),
                         list(salida = "completo"))$es,
                         .both(f, list(output = "full"),
                               list(salida = "completo"))$es), th),
                       info = paste("rbind", th))
    }
  }
  expect_warning(trimMissing(r$en, 0.1, "NOPE"), "NOPE")
  expect_error(trimMissing(data.frame(x = 1), threshold = 10), "threshold")
})
