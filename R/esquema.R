
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

# Variables con un orden natural entre sus categorias: salen como factor
# ordenado. Las demas, como factor sin orden.
.ORDINALES <- c("generacion", "edad", "ideologia", "estudios", "clase")

# ¿Los perdidos de esta variable van como niveles explicitos (TRUE) o a
# NA (FALSE)? 'NAs' es lo que se paso a cis_sociodem():
#   TRUE    todas a NA
#   FALSE   todas explicitas
#   "auto"  NA donde hay orden, explicitas donde no. Un nivel "N.C." al
#           final de un factor ordenado dice que N.C. esta por encima de
#           "Derecha", y polr() o los contrastes polinomicos se lo creen.
#   nombres esas a NA y el resto explicitas
.na_explicito <- function(nombre, ordenado, NAs) {
  if (isTRUE(NAs)) return(FALSE)
  if (isFALSE(NAs)) return(TRUE)
  if (identical(NAs, "auto")) return(!ordenado)
  !nombre %in% NAs
}

# Los perdidos explicitos de las nueve armonizadas: siempre los dos
# niveles, aunque el estudio no tenga casos en alguno, para que se apilen.
.NIVELES_PERDIDO <- c("N.S.", "N.C.")

# Pasa las nueve variables del esquema a factor, con los niveles en el
# orden de los codigos. Se ponen todos los niveles del esquema, aunque el
# estudio no tenga casos en alguno (G. 98 en un barometro de 2026): asi
# los factores de dos estudios distintos son iguales y se apilan sin
# sorpresas. Si los perdidos van explicitos, van en "N.S." y "N.C." al
# final, segun 'motivos' (vease .motivo); sin motivo, se quedan en NA.
.a_factores <- function(out, NAs = TRUE, motivos = list()) {
  for (cc in names(.ETIQ)) {
    val <- .ETIQ[[cc]]$val
    val <- val[order(val)]
    ordenado <- cc %in% .ORDINALES
    f <- factor(as.numeric(out[[cc]]), levels = unname(val),
                labels = names(val), ordered = ordenado)
    if (.na_explicito(cc, ordenado, NAs)) {
      m <- motivos[[cc]]
      if (is.null(m)) m <- rep(NA_character_, length(f))
      txt <- as.character(f)
      txt[is.na(txt)] <- m[is.na(txt)]
      f <- factor(txt, levels = c(names(val), .NIVELES_PERDIDO),
                  ordered = ordenado)
      attr(f, "perdidos") <- .NIVELES_PERDIDO
    }
    attr(f, "label") <- .ETIQ[[cc]]$lab
    out[[cc]] <- f
  }
  out
}

# Por que falta el dato en una armonizada, mirando el codigo que tenia en
# el original: "N.S." si esta etiquetado como no sabe, "N.C." si es
# cualquier otro (no contesta, "Otros" que el esquema no recoge). Si la
# pregunta no se hizo, porque es un "No procede", la casilla esta vacia o
# la variable no esta en el estudio, nadie dejo de contestar: NA. Tambien
# NA donde la armonizada tiene valor.
#
# 'fuentes' son las variables originales en el orden en que se miran, y
# leidas con user_na = TRUE: si el fichero declara el 98 como missing de
# SPSS, la lectura normal lo deja en NA y ya no se sabe que era un N.S.
.ES_NS <- "^n[.]?s[.]?($|[^a-z])|^no sabe"

.motivo <- function(y, fuentes = list()) {
  m <- rep(NA_character_, length(y))
  pendiente <- is.na(y)
  for (x in fuentes) {
    if (is.null(x)) next
    v <- as.numeric(unclass(x))
    et <- .valores(x)
    txt <- if (is.null(et)) rep(NA_character_, length(v)) else unname(et[as.character(v)])
    usa <- pendiente & !is.na(v)
    t <- ifelse(is.na(txt[usa]), "", txt[usa])
    m[usa] <- ifelse(grepl(.ES_NS, t), "N.S.",
                     ifelse(grepl(.NO_PROCEDE, t), NA_character_, "N.C."))
    pendiente <- pendiente & is.na(v)
  }
  m
}

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
# Despues se añadieron el 3567 (barometro de junio de 2026, segun su
# cuestionario) y el 3577 (septiembre de 2026, segun FT3577.pdf), que
# tampoco traen E103.
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
  `3478` = 2024, `3505` = 2025, `3528` = 2025, `3557` = 2026, `3567` = 2026,
  `3577` = 2026
)

