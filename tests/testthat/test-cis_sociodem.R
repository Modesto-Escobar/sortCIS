test_that("la salida breve lleva siempre las mismas columnas en el mismo orden", {
  f <- sav_temporal(estudio_moderno(), "3557")
  d <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  expect_equal(names(d), .SALIDA)
  expect_equal(nrow(d), 8)
  expect_equal(unique(d$estudio), 3557)
  expect_equal(as.numeric(d$numentr), 3557 * 100000 + 1:8)
})

test_that("dos estudios distintos se pueden apilar", {
  fm <- sav_temporal(estudio_moderno(), "3557")
  fa <- sav_temporal(estudio_antiguo(), "2798")
  m <- cis_sociodem(fm, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  a <- cis_sociodem(fa, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  expect_equal(names(m), names(a))
  junto <- rbind(a, m)
  expect_equal(nrow(junto), 16)
  # las columnas que no dependen del anyo de campo salen identicas pese a
  # los nombres y codigos distintos de los dos ficheros
  for (v in c("genero", "edad", "ideologia", "estudios", "laboral",
              "profesional", "clase")) {
    expect_equal(as.numeric(m[[v]]), as.numeric(a[[v]]), info = v)
  }
})

test_that("el numero de estudio y el anyo se sacan del propio fichero", {
  f <- sav_temporal(estudio_antiguo(), "sin_numero_en_el_nombre")
  d <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  expect_equal(unique(d$estudio), 2798)
  # 2798 es de 2009: quien tiene 25 anyos nacio en 1984, generacion Y (7)
  expect_equal(as.numeric(d$generacion[1]), 7)
})

test_that("sin anyo de campo solo se resiente 'generacion'", {
  m <- estudio_moderno()
  m$E103 <- NULL
  f <- sav_temporal(m, "9999")
  d <- cis_sociodem(f, estudio = 9999, tabla = tabla_temporal(),
                    escribir = FALSE, avisos = FALSE)
  expect_true(all(is.na(d$generacion)))
  expect_false(any(is.na(d$genero)))
})

test_that("un concepto ausente se crea como columna vacia", {
  m <- estudio_moderno()
  m$CLASESUB <- NULL
  f <- sav_temporal(m, "3557")
  d <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  expect_equal(names(d), .SALIDA)
  expect_true(all(is.na(d$clase)))
})

test_that("las etiquetas del esquema comun viajan con los datos", {
  f <- sav_temporal(estudio_moderno(), "3557")
  d <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE,
                    factores = FALSE)
  expect_equal(attr(d$genero, "label"), "Sexo de la persona entrevistada")
  expect_equal(attr(d$profesional, "labels")[["Otra situacion"]], 6)
  expect_equal(attr(d$estudios, "labels")[["Universitarios"]], 500)
})

test_that("por defecto devuelve factores, ordenados solo los ordinales", {
  f <- sav_temporal(estudio_moderno(), "3557")
  d <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE)
  for (v in .ORDINALES) expect_true(is.ordered(d[[v]]), info = v)
  for (v in c("genero", "religion", "laboral", "profesional")) {
    expect_true(is.factor(d[[v]]), info = v)
    expect_false(is.ordered(d[[v]]), info = v)
  }
  # niveles en el orden de los codigos, todos aunque no haya casos
  expect_equal(levels(d$estudios), c("Sin estudios", "Primaria y secundaria elemental",
                                     "Secundaria superior", "Universitarios"))
  expect_equal(levels(d$generacion)[1], "G. 98")
  expect_equal(attr(d$genero, "label"), "Sexo de la persona entrevistada")
  # el factor dice lo mismo que el codigo etiquetado
  e <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE,
                    factores = FALSE)
  expect_equal(as.character(d$estudios), as.character(haven::as_factor(e$estudios)))
  expect_equal(is.na(d$clase), is.na(e$clase))
})

test_that("NAs = FALSE pone los perdidos en un nivel N.C. al final", {
  m <- estudio_moderno()
  m$CLASESUB <- NULL
  f <- sav_temporal(m, "3557")
  d <- cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE, avisos = FALSE,
                    NAs = FALSE)
  expect_equal(tail(levels(d$clase), 1), "N.C.")
  expect_true(is.ordered(d$clase))
  expect_true(all(d$clase == "N.C."))
  expect_false(anyNA(d$clase))
  # el nivel esta aunque no haya perdidos, para que los estudios casen
  expect_equal(tail(levels(d$genero), 1), "N.C.")
})

test_that("los ficheros se escriben etiquetados aunque se devuelvan factores", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile()
  cis_sociodem(f, destino = destino, tabla = tabla_temporal(), avisos = FALSE)
  vuelta <- haven::read_sav(paste0(destino, ".sav"))
  expect_s3_class(vuelta$estudios, "haven_labelled")
  expect_equal(attr(vuelta$estudios, "labels")[["Universitarios"]], 500)
})

test_that("factores y NAs solo admiten TRUE o FALSE", {
  f <- sav_temporal(estudio_moderno(), "3557")
  expect_error(cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE,
                            avisos = FALSE, factores = "si"), "factores")
  expect_error(cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE,
                            avisos = FALSE, NAs = NA), "NAs")
})

test_that("salida='auditoria' anyade las variables crudas con prefijo o_", {
  f <- sav_temporal(estudio_moderno(), "3557")
  d <- cis_sociodem(f, salida = "auditoria", tabla = tabla_temporal(),
                    escribir = FALSE, avisos = FALSE)
  expect_equal(names(d)[1:13], .SALIDA)
  expect_true(all(c("o_sexo", "o_escideol", "o_sitprof") %in% names(d)))
})

test_that("salida='completo' conserva el fichero y renombra los choques con _ORIG", {
  f <- sav_temporal(estudio_moderno(), "3557")
  d <- cis_sociodem(f, salida = "completo", tabla = tabla_temporal(),
                    escribir = FALSE, avisos = FALSE)
  expect_true("SEXO" %in% names(d))
  # ESTUDIOS, EDAD y RELIGION del fichero original chocarian con las
  # armonizadas, que se escriben en minusculas; SPSS no distingue
  # mayusculas, asi que las originales se guardan con el sufijo _ORIG
  expect_true(all(c("ESTUDIOS_ORIG", "EDAD_ORIG", "RELIGION_ORIG") %in% names(d)))
  # van las nueve armonizadas mas orden y numentr; 'estudio' y 'cues' no,
  # porque el fichero completo ya trae sus propios identificadores
  expect_true(all(c(setdiff(.SALIDA, c("estudio", "cues"))) %in% names(d)))
  expect_false(any(c("estudio", "cues") %in% names(d)))
  expect_true(all(c("ESTU", "CUES") %in% names(d)))
})

test_that("escribir=TRUE deja el .sav y se puede releer", {
  f <- sav_temporal(estudio_moderno(), "3557")
  destino <- tempfile(fileext = ".sav")
  cis_sociodem(f, destino = destino, tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(destino))
  vuelta <- haven::read_sav(destino)
  expect_equal(names(vuelta), .SALIDA)
})

test_that("el destino por defecto es el mismo nombre con el sufijo _soc", {
  f <- sav_temporal(estudio_moderno(), "3557")
  cis_sociodem(f, tabla = tabla_temporal(), avisos = FALSE)
  expect_true(file.exists(sub("[.]sav$", "_soc.sav", f)))
})

test_that("la tabla de correspondencias registra que variable se ha elegido", {
  f <- sav_temporal(estudio_moderno(), "3557")
  tsv <- tabla_temporal()
  cis_sociodem(f, tabla = tsv, escribir = FALSE, avisos = FALSE)
  t <- utils::read.delim(tsv, stringsAsFactors = FALSE)
  expect_setequal(t$concepto, .CONCEPTOS)
  expect_equal(t$variable[t$concepto == "genero"], "SEXO")
  expect_true(all(t$origen == "auto"))
})

test_that("una fila manual manda sobre la deteccion automatica", {
  m <- estudio_moderno()
  m$MIVAR <- haven::labelled(c(2, 1, 2, 1, 2, 1, 2, 1),
                             labels = c(Hombre = 1, Mujer = 2),
                             label = "Una etiqueta que no casa ningun patron")
  f <- sav_temporal(m, "3557")
  tsv <- tabla_temporal()
  utils::write.table(
    data.frame(estudio = 3557, concepto = "genero", variable = "MIVAR",
               etiqueta = "puesta a mano", origen = "manual"),
    tsv, sep = "\t", row.names = FALSE, quote = FALSE)
  d <- cis_sociodem(f, tabla = tsv, escribir = FALSE, avisos = FALSE)
  expect_equal(as.numeric(d$genero), c(2, 1, 2, 1, 2, 1, 2, 1))
  # y la fila manual sigue ahi despues de la pasada
  t <- utils::read.delim(tsv, stringsAsFactors = FALSE)
  expect_equal(t$origen[t$concepto == "genero"], "manual")
})

test_that("una variable a '' con origen manual declara el concepto inexistente", {
  f <- sav_temporal(estudio_moderno(), "3557")
  tsv <- tabla_temporal()
  utils::write.table(
    data.frame(estudio = 3557, concepto = "clase", variable = "",
               etiqueta = "no la queremos", origen = "manual"),
    tsv, sep = "\t", row.names = FALSE, quote = FALSE)
  d <- cis_sociodem(f, tabla = tsv, escribir = FALSE, avisos = FALSE)
  expect_true(all(is.na(d$clase)))
})

test_that("la tabla que manda usar una variable inexistente da error", {
  f <- sav_temporal(estudio_moderno(), "3557")
  tsv <- tabla_temporal()
  utils::write.table(
    data.frame(estudio = 3557, concepto = "genero", variable = "NOEXISTE",
               etiqueta = "-", origen = "manual"),
    tsv, sep = "\t", row.names = FALSE, quote = FALSE)
  expect_error(cis_sociodem(f, tabla = tsv, escribir = FALSE, avisos = FALSE),
               "no esta en el fichero")
})

test_that("un fichero que no existe da un error claro", {
  expect_error(cis_sociodem("no_existe_este_fichero.sav"),
               "No existe el fichero")
})

test_that("sin numero de estudio determinable, error", {
  m <- estudio_moderno()
  m$ESTU <- NULL
  f <- sav_temporal(m, "sin_digitos")
  expect_error(cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE,
                            avisos = FALSE),
               "numero de estudio")
})

test_that("avisos=TRUE imprime el informe", {
  f <- sav_temporal(estudio_moderno(), "3557")
  expect_output(cis_sociodem(f, tabla = tabla_temporal(), escribir = FALSE,
                             avisos = TRUE),
                "estudio 3557")
})
