test_that("por defecto sigue escribiendo .sav", {
  f <- sav_temporal(estudio_moderno(), "3557")
  cis_sociodem(f, tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(sub("[.]sav$", "_soc.sav", f)))
})

test_that("formato='dta' escribe Stata y se puede releer", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, formato = "dta",
               tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(paste0(destino, ".dta")))
  expect_false(file.exists(paste0(destino, ".sav")))
  vuelta <- haven::read_dta(paste0(destino, ".dta"))
  expect_equal(names(vuelta), .SALIDA)
  expect_equal(nrow(vuelta), 8)
})

test_that("formato='ambos' escribe los dos con la misma raiz", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, formato = "ambos",
               tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(paste0(destino, ".sav")))
  expect_true(file.exists(paste0(destino, ".dta")))
  a <- haven::read_sav(paste0(destino, ".sav"))
  b <- haven::read_dta(paste0(destino, ".dta"))
  expect_equal(names(a), names(b))
  expect_equal(as.numeric(a$numentr), as.numeric(b$numentr))
})

test_that("las etiquetas sobreviven al viaje a Stata", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, formato = "dta",
               tabla = tabla_temporal(), avisos = FALSE)
  d <- haven::read_dta(paste0(destino, ".dta"))
  expect_equal(attr(d$genero, "label"), "Género")
  expect_equal(attr(d$genero, "labels")[["Mujer"]], 2)
  expect_equal(attr(d$estudios, "labels")[["Universitarios"]], 500)
})

test_that("un destino acabado en .dta basta para pedir Stata", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile(fileext = ".dta")
  cis_sociodem(f, destino = destino, tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(destino))
  expect_false(file.exists(sub("[.]dta$", ".sav", destino)))
})

test_that("un formato explicito manda sobre la extension del destino", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile(fileext = ".dta")
  cis_sociodem(f, destino = destino, formato = "sav",
               tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(sub("[.]dta$", ".sav", destino)))
  expect_false(file.exists(destino))
})

test_that("el destino por defecto en Stata lleva el sufijo _soc", {
  f <- sav_temporal(estudio_moderno(), "3557")
  cis_sociodem(f, formato = "ambos", tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(sub("[.]sav$", "_soc.dta", f)))
  expect_true(file.exists(sub("[.]sav$", "_soc.sav", f)))
})

test_that("salida='completo' tambien va a Stata si los nombres valen", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, formato = "dta", salida = "completo",
               tabla = tabla_temporal(), avisos = FALSE)
  d <- haven::read_dta(paste0(destino, ".dta"))
  expect_true(all(c("SEXO", "ESTUDIOS_ORIG", "genero") %in% names(d)))
})

test_that("las etiquetas de valores con decimales no se graban", {
  m <- estudio_moderno()
  pesos <- c(2.87887, 1.34564, 0.64517, 1, 1.10473, 0.50187, 2.58487, 1)
  m$PESO <- .v(pesos, "Ponderación",
               setNames(unique(pesos), format(unique(pesos))))
  f <- sav_temporal(m, "3557")
  destino <- tempfile()
  expect_output(
    cis_sociodem(f, destino = destino, formato = "ambos", salida = "completo",
                 tabla = tabla_temporal()),
    "PESO va\\(n\\) sin etiquetas")
  d <- haven::read_dta(paste0(destino, ".dta"))
  expect_equal(as.numeric(d$PESO), pesos)
  expect_null(attr(d$PESO, "labels"))
  expect_equal(attr(d$PESO, "label"), "Ponderación")
  expect_equal(attr(d$SEXO, "labels")[["Mujer"]], 2)
  s <- haven::read_sav(paste0(destino, ".sav"))
  expect_equal(as.numeric(s$PESO), pesos)
  expect_null(attr(s$PESO, "labels"))
  expect_equal(attr(s$PESO, "label"), "Ponderación")
  expect_equal(attr(s$SEXO, "labels")[["Mujer"]], 2)
})

test_that("un nombre que Stata no admite da un error que lo nombra", {
  m <- estudio_moderno()
  names(m)[names(m) == "SEXO"] <- "P25.A"
  f <- sav_temporal(m, "3557")
  expect_error(
    cis_sociodem(f, destino = tempfile(), formato = "dta", salida = "completo",
                 tabla = tabla_temporal(), avisos = FALSE),
    "P25[.]A")
})

test_that("escribir=FALSE no escribe nada, sea cual sea el formato", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, formato = "ambos", escribir = FALSE,
               tabla = tabla_temporal(), avisos = FALSE)
  expect_false(file.exists(paste0(destino, ".sav")))
  expect_false(file.exists(paste0(destino, ".dta")))
})

test_that("un formato que no existe da error", {
  f <- sav_temporal(estudio_moderno(), "3557")
  expect_error(cis_sociodem(f, formato = "csv", tabla = tabla_temporal(),
                            escribir = FALSE, avisos = FALSE))
})
