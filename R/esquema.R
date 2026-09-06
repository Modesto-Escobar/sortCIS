
# ---------------------------------------------------------------------
# Etiquetas del esquema comun (las de FID3978abrev.dta)
# ---------------------------------------------------------------------
.ETIQ <- list(
  genero      = list(lab = "Sexo de la persona entrevistada",
                     val = c(Hombre = 1, Mujer = 2)),
  generacion  = list(lab = "Generacion",
                     val = c(`G. 98` = 1, `G. 14` = 2, `G.27` = 3, Postguerra = 4,
                             Boomers = 5, X = 6, Y = 7, Z = 8)),
  edad        = list(lab = "Edad",
                     val = c(Joven = 1, Adulto = 2, Maduro = 3, Mayor = 4, Vejez = 5)),
  ideologia   = list(lab = "Ideologia",
                     val = c(Izquierda = 1, Centro = 2, Derecha = 3)),
  religion    = list(lab = "Religion",
                     val = c(Practicante = 1, Catolico = 2, Otras = 3)),
  estudios    = list(lab = "Estudios",
                     val = c(`Sin estudios` = 100, `Primaria y secundaria elemental` = 200,
                             `Secundaria superior` = 300, Universitarios = 500)),
  laboral     = list(lab = "Situacion laboral",
                     val = c(Trabaja = 1, Pensionista = 2, Parado = 3,
                             Estudiante = 4, `Domestico` = 5)),
  profesional = list(lab = "Situacion profesional",
                     val = c(`N.P.` = 0, `Asalariado/a por cuenta ajena` = 1,
                             `Empresario/autonomo` = 2, `Otra situacion` = 6)),
  clase       = list(lab = "Clase subjetiva",
                     val = c(`Alta/medio alta` = 1, Media = 2, Trabajador = 3,
                             Baja = 4, Otras = 9))
)

.SALIDA <- c("estudio", "cues", "orden", "numentr", "genero", "generacion",
             "edad", "ideologia", "religion", "estudios", "laboral",
             "profesional", "clase")

# ---------------------------------------------------------------------
# Año de campo de los estudios de la serie.
#
# Va aqui dentro, y no solo en datos/estudios_fecha.csv, para que este
# fichero se baste a si mismo: el año hace falta para 'generacion' (el
# año de nacimiento es año de campo menos edad) y trece de los estudios
# de la serie, del 3279 en adelante, dejaron de traer la variable E103
# con la fecha de realizacion. Sin esta tabla habria que llevar el CSV
# al lado o pasar anyo= a mano en esos trece.
#
# Procedencia: el 'añomes' de FID3978abrev (constante dentro de cada
# estudio, comprobado) para los 33 que estan en el acumulado, mas la
# ficha tecnica FT3557.pdf para el 3557. Contrastado ademas contra la
# variable E103 en los 21 estudios que la traen: coinciden los 21.
#
# Para un estudio que no este aqui, la funcion sigue mirando E103 y
# datos/estudios_fecha.csv, y admite anyo= .
# ---------------------------------------------------------------------
.ANYOS <- c(
  `2798` = 2009, `2815` = 2009, `2834` = 2010, `2847` = 2010, `2885` = 2011,
  `2941` = 2012, `2960` = 2012, `2984` = 2013, `3001` = 2013, `3021` = 2014,
  `3041` = 2014, `3080` = 2015, `3114` = 2015, `3134` = 2016, `3156` = 2016,
  `3173` = 2017, `3191` = 2017, `3210` = 2018, `3226` = 2018, `3238` = 2019,
  `3257` = 2019, `3279` = 2020, `3296` = 2020, `3318` = 2021, `3337` = 2021,
  `3359` = 2022, `3380` = 2022, `3403` = 2023, `3423` = 2023, `3450` = 2024,
  `3478` = 2024, `3505` = 2025, `3528` = 2025, `3557` = 2026
)

