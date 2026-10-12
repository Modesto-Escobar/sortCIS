# ---------------------------------------------------------------------
# Ficheros sinteticos con la pinta de los del CIS.
#
# Los dos "estudios" de aqui abajo dicen lo mismo con nombres de
# variable distintos y, lo que mas importa, con CODIGOS distintos:
# "Ayuda familiar" es el 5 en el moderno y el 4 en el antiguo, y
# "Otra situacion" el 6 y el 7. Si la recodificacion fuera por numero,
# los dos ficheros no se podrian apilar. Es justo lo que se comprueba.
# ---------------------------------------------------------------------

# Atajo para crear una columna etiquetada al modo de SPSS.
.v <- function(x, etiqueta, valores = NULL) {
  haven::labelled(as.numeric(x), labels = valores, label = etiqueta)
}

# Estudio reciente: nombres nemotecnicos (SEXO, ESCIDEOL...) y una
# variable de religiosidad que ya distingue practicantes.
estudio_moderno <- function() {
  data.frame(
    ESTU     = .v(rep(3557, 8), "Numero de estudio"),
    CUES     = .v(1:8, "Cuestionario"),
    E103     = .v(rep(2026, 8), "Fecha de realizacion: anyo"),
    SEXO     = .v(c(1, 2, 1, 2, 1, 2, 1, 2),
                  "Sexo de la persona entrevistada",
                  c(Hombre = 1, Mujer = 2)),
    EDAD     = .v(c(25, 40, 55, 70, 85, 30, 99, 45),
                  "Edad de la persona entrevistada",
                  c(`N.C.` = 99)),
    ESCIDEOL = .v(c(1, 5, 9, 3, 6, 10, 98, 99),
                  "Escala de autoubicación ideológica (1-10)",
                  c(`N.S.` = 98, `N.C.` = 99)),
    RELIGION = .v(c(1, 2, 3, 4, 5, 1, 2, 9),
                  "Religiosidad de la persona entrevistada",
                  c(`Católico/a practicante` = 1,
                    `Católico/a no practicante` = 2,
                    `Creyente de otra religión` = 3,
                    `No creyente` = 4, `Ateo/a` = 5, `N.C.` = 9)),
    ESTUDIOS = .v(c(1, 2, 3, 4, 5, 6, 9, 2),
                  "Estudios de la persona entrevistada",
                  c(`Sin estudios` = 1, `Educación primaria` = 2,
                    `Secundaria 1a etapa` = 3, `Secundaria 2a etapa` = 4,
                    `F.P.` = 5, `Superiores` = 6, `N.C.` = 9)),
    SITLAB   = .v(c(1, 2, 3, 4, 5, 6, 1, 9),
                  "Situación laboral de la persona entrevistada",
                  c(Trabaja = 1, `Jubilado/a o pensionista` = 2,
                    `Parado/a` = 3, `Estudiante` = 4,
                    `Trabajo doméstico no remunerado` = 5,
                    `Otra situación` = 6, `N.C.` = 9)),
    SITPROF  = .v(c(1, 2, 3, 5, 6, 0, 1, 9),
                  "Situación profesional de la persona entrevistada",
                  c(`N.P.` = 0, `Asalariado/a fijo` = 1, `Empresario/a` = 2,
                    `Autónomo/a` = 3, `Ayuda familiar` = 5,
                    `Otra situación` = 6, `N.C.` = 9)),
    CLASESUB = .v(c(1, 2, 3, 4, 5, 6, 7, 9),
                  "Clase social subjetiva",
                  c(`Clase alta` = 1, `Clase media-alta` = 2,
                    `Clase media-media` = 3, `Clase media-baja` = 4,
                    `Clase trabajadora` = 5, `Clase baja` = 6,
                    `Gente común` = 7, `N.C.` = 9)),
    stringsAsFactors = FALSE)
}

# Estudio antiguo: las mismas preguntas llamadas P23, P17, P43..., la
# religiosidad SIN distinguir practica (hay que mirar la asistencia a
# oficios) y los codigos corridos respecto al moderno.
estudio_antiguo <- function() {
  data.frame(
    ESTUDIO = .v(rep(2798, 8), "Numero de estudio"),
    CUEST   = .v(1:8, "Cuestionario"),
    E103    = .v(rep(2009, 8), "Fecha de realizacion: anyo"),
    P23     = .v(c(1, 2, 1, 2, 1, 2, 1, 2),
                 "Sexo del entrevistado", c(Hombre = 1, Mujer = 2)),
    P24     = .v(c(25, 40, 55, 70, 85, 30, 99, 45),
                 "Edad del entrevistado", c(`N.C.` = 99)),
    P17     = .v(c(1, 5, 9, 3, 6, 10, 98, 99),
                 "Escala de autoubicación ideológica",
                 c(`N.S.` = 98, `N.C.` = 99)),
    P30     = .v(c(1, 1, 2, 3, 4, 1, 1, 9),
                 "Religiosidad",
                 c(`Católico` = 1, `Creyente de otra religión` = 2,
                   `No creyente` = 3, `Ateo` = 4, `N.C.` = 9)),
    P31     = .v(c(4, 1, 2, 2, 2, 1, 5, 9),
                 "Frecuencia de asistencia a oficios religiosos",
                 c(`Casi nunca` = 1, `Varias veces al año` = 2,
                   `Alguna vez al mes` = 3, `Casi todos los domingos` = 4,
                   `Varias veces por semana` = 5, `N.C.` = 9)),
    P28     = .v(c(1, 2, 3, 4, 5, 6, 9, 2),
                 "Estudios del entrevistado",
                 c(`Sin estudios` = 1, `Educación primaria` = 2,
                   `Secundaria 1a etapa` = 3, `Secundaria 2a etapa` = 4,
                   `Formación profesional` = 5, `Universitario` = 6,
                   `N.C.` = 9)),
    P40     = .v(c(1, 2, 3, 4, 5, 7, 1, 9),
                 "Situación laboral",
                 c(Trabaja = 1, `Jubilado/a o pensionista` = 2,
                   `En paro` = 3, `Estudiante` = 4,
                   `Trabajo doméstico no remunerado` = 5,
                   `Otra situación` = 7, `N.C.` = 9)),
    P43     = .v(c(1, 2, 3, 4, 7, 0, 1, 9),
                 "Situación profesional",
                 c(`N.P.` = 0, `Asalariado/a eventual` = 1,
                   `Empresario/a` = 2, `Autónomo/a` = 3,
                   `Ayuda familiar` = 4, `Miembro de cooperativa` = 5,
                   `Otra situación` = 7, `N.C.` = 9)),
    P32     = .v(c(1, 2, 3, 4, 5, 6, 7, 9),
                 "Clase social subjetiva",
                 c(`Clase alta` = 1, `Clase media-alta` = 2,
                   `Clase media-media` = 3, `Clase media-baja` = 4,
                   `Clase obrera` = 5, `Clase baja` = 6,
                   `Proletariado` = 7, `N.C.` = 9)),
    stringsAsFactors = FALSE)
}

# Escribe uno de los anteriores como .sav en un fichero temporal.
sav_temporal <- function(d, nombre = "estudio") {
  f <- file.path(tempdir(), paste0(nombre, ".sav"))
  haven::write_sav(d, f)
  f
}

# Una tabla de correspondencias siempre nueva, para que los tests no se
# pisen entre si ni escriban en el proyecto.
tabla_temporal <- function() tempfile(fileext = ".tsv")

# Preguntas de escala y Likert al modo del CIS, para salida = "completo".
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
