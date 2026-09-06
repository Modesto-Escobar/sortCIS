# =====================================================================
# cis_sociodem.R
# ---------------------------------------------------------------------
# cis_sociodem(fichero) toma UN fichero de datos del CIS (.sav de SPSS,
# o .dta) y escribe otro .sav con las variables sociodemograficas
# recodificadas al esquema comun de FID3978abrev.dta, de modo que dos
# ficheros convertidos con esta funcion se puedan apilar directamente.
#
#   cis_sociodem("datos/sav/3557.sav")
#   -> datos/sav/3557_soc.sav
#
# ---------------------------------------------------------------------
# POR QUE NO BASTA CON UNA LISTA DE NOMBRES DE VARIABLE
#
# El CIS no nombra igual las variables en todos los estudios. La misma
# pregunta se llama SEXO en los recientes y P23/P24/P28/P29/P30/P31/
# P32/P40/P43 en los antiguos; la ideologia es ESCIDEOL o P17/P20/P21/
# P22/P23/P25/P28/P32/P34. Comprobado sobre los 34 ficheros del
# proyecto: 8 conceptos x 34 estudios dan 60 nombres distintos.
#
# Lo que SI es estable es la ETIQUETA de la variable ("Sexo de la
# persona entrevistada", "Escala de autoubicacion ideologica (1-10)").
# Por eso la deteccion va por etiqueta, no por nombre.
#
# Y lo mismo pasa un nivel mas abajo, con los CODIGOS: "Ayuda familiar"
# es el codigo 5 en unos estudios y el 4 en otros; "Otra situacion" es
# 7 u 8 segun el estudio. Recodificar por numero da resultados
# silenciosamente equivocados. Por eso la recodificacion tambien va por
# la etiqueta del valor, no por el valor.
#
# ---------------------------------------------------------------------
# DE DONDE SALEN LOS MAPAS DE RECODIFICACION
#
# No estan inventados: se derivaron cruzando los 32 ficheros .sav que
# ya estan en FID3978abrev.dta contra el propio FID, fila a fila, y
# tabulando categoria de origen -> categoria de destino. Es decir,
# reproducen lo que de hecho hizo quien construyo el acumulado.
#
# El cruce es por POSICION DE FILA (numentr = estudio*100000 + orden),
# no por numero de cuestionario: comprobado, por orden acierta el 100%
# y por 'cues' entre el 54% y el 62% en los estudios antiguos.
#
# Las reglas coinciden con las de Analiza.do, con dos matices que se
# ven en los datos de FID pero no en el texto de aquel do-file:
#   - 'estudios' llega a FID con la F.P. ya fundida en 300 (Secundaria
#     superior); el codigo 400 existe en las etiquetas pero no en los
#     datos.
#   - 'profesional' llega con el recode (4/6=6)(3=2) ya aplicado; las
#     etiquetas de FID se quedaron sin actualizar y por eso enganan.
# En ambos casos manda el dato, no la etiqueta.
#
# ---------------------------------------------------------------------
# ARGUMENTOS
#   fichero  ruta del .sav (o .dta) de entrada.
#   destino  ruta del .sav de salida. Por defecto, el mismo nombre con
#            el sufijo "_soc".
#   salida   que columnas lleva el fichero de salida:
#            "breve"     claves + las 9 recodificadas. Siempre las
#                        mismas columnas en el mismo orden: es la que
#                        garantiza que dos ficheros se apilen.
#            "auditoria" lo anterior + las variables crudas de las que
#                        sale cada una, con prefijo o_ (o_sexo,
#                        o_escideol...), para poder comprobar la
#                        recodificacion. Las o_ cambian de un estudio a
#                        otro; las 12 primeras no.
#            "completo"  el fichero de entrada intacto + las 9 nuevas
#                        al final.
#   estudio  numero de estudio. Por defecto se busca en las variables
#            ESTU/ESTUDIO y, si no estan, en el nombre del fichero.
#   anyo     año del trabajo de campo, necesario para 'generacion'. Solo
#            hace falta para un estudio que no sea de la serie: por
#            defecto se busca, en este orden, en la variable E103 del
#            propio fichero, en datos/estudios_fecha.csv si esta, y en
#            la tabla .ANYOS de mas abajo, que trae los 34 estudios de
#            la serie. Si no aparece en ninguno se avisa y 'generacion'
#            queda a perdido; las otras ocho variables no se resienten.
#
# ---------------------------------------------------------------------
# QUE HACE FALTA PARA USARLA
#
# Este fichero y nada mas. No hay que llevar ningun CSV al lado: los
# años de la serie van dentro (tabla .ANYOS).
#
# datos/sociodem_correspondencias.tsv NO es una entrada: lo escribe la
# propia funcion para dejar constancia de que variable ha elegido. Solo
# se toca para forzar una eleccion (ver el final de esta cabecera).
#   tabla    TSV de correspondencias revisable (ver abajo).
#   escribir FALSE devuelve el data.frame sin escribir nada.
#   avisos   FALSE silencia el informe por pantalla.
#
# VALOR: invisiblemente, el data.frame de salida.
#
# ---------------------------------------------------------------------
# LA TABLA DE CORRESPONDENCIAS
#
# En datos/sociodem_correspondencias.tsv la funcion deja escrito que
# variable ha elegido para cada concepto y por que. Las filas con
# origen="auto" las reescribe en cada pasada; las que se pongan a mano
# con origen="manual" MANDAN sobre la deteccion automatica y no se
# tocan. Asi un estudio con una etiqueta atipica se arregla editando
# una linea del TSV, sin tocar este fichero, y queda constancia.
#
# Para declarar que un concepto no existe en un estudio, poner la
# variable a "" con origen="manual".
# =====================================================================

suppressMessages({library(haven)})

# ---------------------------------------------------------------------
# Utilidades de texto: las etiquetas del CIS varian en acentos, mayus-
# culas, barras de genero y aclaraciones entre parentesis. Se comparan
# siempre normalizadas.
# ---------------------------------------------------------------------
.norm <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x <- chartr("áéíóúüñÁÉÍÓÚÜÑ",
              "aeiouunAEIOUUN", x)
  x <- tolower(x)
  x <- sub("\\(.*$", "", x)          # fuera la aclaracion entre parentesis
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.etiqueta <- function(x) {
  L <- attr(x, "label")
  if (is.null(L) || length(L) != 1 || is.na(L)) return("")
  as.character(L)
}

# Etiquetas de los valores de una variable, ya normalizadas y en el
# orden de los codigos.
.valores <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(NULL)
  setNames(.norm(names(lv)), as.character(as.numeric(lv)))
}

# ---------------------------------------------------------------------
# Deteccion de la variable de cada concepto, por su etiqueta.
#
# Cada concepto lleva sus patrones EN ORDEN DE PREFERENCIA: se prueba el
# primero en todas las variables, y solo si ninguna casa se pasa al
# siguiente. Importa donde el CIS ofrece la misma informacion dos veces:
# en muchos estudios conviven ESTUDIOS ("Estudios de la persona
# entrevistada", ya agrupada) y NIVELESTENTREV / P31A ("Nivel de
# estudios alcanzado...", con el detalle de titulaciones). El acumulado
# se construyo con la primera, asi que esa es la que se busca antes.
# ---------------------------------------------------------------------
.PATRON <- list(
  genero      = "^sexo (de la persona entrevistada|del entrevistado)",
  edad        = "^edad (de la persona entrevistada|del entrevistado)",
  ideologia   = "escala de autoubicacion ideologica",
  religion    = "^religiosidad",
  estudios    = c("^nivel de estudios alcanzado",
                  "^estudios (de la persona entrevistada|del entrevistado)"),
  laboral     = "^situacion laboral",
  profesional = "^situacion profesional",
  clase       = "^clase social subjetiva"
)

# Frecuencia de asistencia a oficios religiosos: hace falta para
# separar practicantes de no practicantes en los estudios antiguos,
# cuya variable de religion no lo distingue.
.PAT_PRACTICA <- "frecuencia de asistencia a oficios religiosos|practica religiosa"

.CONCEPTOS <- names(.PATRON)

# Todas las candidatas, por orden de preferencia de patron. La primera
# es la que se usa; para 'estudios' la segunda sirve de respaldo.
.detecta_todas <- function(d, patrones) {
  out <- character(0)
  for (p in patrones) {
    v <- .detecta(d, p)
    if (!is.na(v)) out <- c(out, v)
  }
  unique(out)
}

.detecta <- function(d, patrones) {
  et <- .norm(vapply(d, .etiqueta, character(1)))
  for (p in patrones) {
    hit <- names(d)[grepl(p, et)]
    if (!length(hit)) next
    # Si varias casan el mismo patron: preferir las que traen etiquetas
    # de valor y, a igualdad, el nombre mas corto (ESTUDIOS antes que
    # ESTUDIOSR, RELIGION antes que RELIGIONR).
    if (length(hit) > 1) {
      con_lab <- vapply(hit, function(v) !is.null(attr(d[[v]], "labels")), logical(1))
      if (any(con_lab)) hit <- hit[con_lab]
      hit <- hit[order(nchar(hit))]
    }
    return(hit[1])
  }
  NA_character_
}

# ---------------------------------------------------------------------
# Recodificadores. Cada uno recibe el vector crudo y sus etiquetas de
# valor normalizadas, y devuelve el codigo del esquema comun.
# Todos trabajan sobre la ETIQUETA del valor; solo cuando la variable
# no trae etiquetas se cae al codigo numerico.
# ---------------------------------------------------------------------

# Aplica un vector de reglas c(patron = destino) sobre las etiquetas.
# Devuelve NA para lo que no case (que es lo correcto: N.C., N.S.,
# "Otros" y "Otra situacion" van a perdido en el esquema comun).
.por_etiqueta <- function(x, reglas, sin_etiquetas = NULL) {
  v <- as.numeric(zap_missing(x))
  et <- .valores(x)
  if (is.null(et)) {
    if (is.null(sin_etiquetas)) return(rep(NA_real_, length(v)))
    return(sin_etiquetas(v))
  }
  txt <- et[as.character(v)]
  out <- rep(NA_real_, length(v))
  for (i in seq_along(reglas)) {
    pendiente <- is.na(out) & !is.na(txt)
    if (!any(pendiente)) break
    casa <- pendiente & grepl(names(reglas)[i], txt)
    out[casa] <- as.numeric(reglas[i])
  }
  out
}

.rec_genero <- function(x) {
  .por_etiqueta(x, c("^hombre" = 1, "^mujer" = 2),
                sin_etiquetas = function(v) ifelse(v %in% c(1, 2), v, NA_real_))
}

# La edad viaja en años cumplidos. Son perdidos los N.C. declarados como
# tales (por etiqueta o por el missing de SPSS, que zap_missing ya
# retira) y lo que cae fuera de un rango plausible.
#
# El 99 no se descarta a ciegas, sino solo cuando la variable lo declara
# N.C. Es lo que pasa en toda la serie, asi que el resultado practico es
# el mismo, con una diferencia: en 2798, 2815, 2847 y 3156 hay seis
# entrevistados con un 99 etiquetado "N.C." que FID dio por edad real y
# convirtio en nacidos en 1910. Aqui quedan a perdido, que es lo que
# dice la etiqueta.
.rec_edad_anyos <- function(x) {
  v <- as.numeric(zap_missing(x))
  et <- .valores(x)
  if (!is.null(et)) {
    nc <- as.numeric(names(et)[grepl("^n[.]?c|^n[.]?s", et)])
    v[v %in% nc] <- NA
  } else {
    v[v %in% c(99, 999)] <- NA   # sin etiquetas no hay otra forma de saberlo
  }
  v[!is.na(v) & (v < 16 | v > 120)] <- NA
  v
}

.rec_edad_tramos <- function(a) {
  cut(a, breaks = c(-Inf, 34, 49, 64, 79, Inf), labels = FALSE)
}

.rec_generacion <- function(a, anyo) {
  if (is.na(anyo)) return(rep(NA_real_, length(a)))
  nace <- anyo - a
  cut(nace, breaks = c(1881, 1897, 1913, 1930, 1949, 1965, 1980, 1996, 2012),
      labels = FALSE)
}

# Escala 1-10. 98 (N.S.) y 99 (N.C.) a perdido.
.rec_ideologia <- function(x) {
  v <- as.numeric(zap_missing(x))
  v[!is.na(v) & (v < 1 | v > 10)] <- NA
  cut(v, breaks = c(0, 4, 6, 10), labels = FALSE)
}

# Dos familias de esquema:
#   moderno : 1 Cat. practicante / 2 Cat. no practicante / 3-6 otras
#   antiguo : 1 Catolico / 2 Creyente de otra religion / 3 No creyente
#             / 4 Ateo    -> no distingue practica, hay que mirar la
#                            frecuencia de asistencia a oficios.
.rec_religion <- function(x, practica = NULL) {
  et <- .valores(x)
  v  <- as.numeric(zap_missing(x))
  out <- rep(NA_real_, length(v))
  if (is.null(et)) return(out)
  txt <- et[as.character(v)]
  moderno <- any(grepl("practicante", et))
  if (moderno) {
    out[grepl("^catolico.* no practicante", txt)] <- 2
    out[is.na(out) & grepl("^catolico.* practicante", txt)] <- 1
    out[is.na(out) & !is.na(txt) & !grepl("^n[.]?c", txt)] <- 3
  } else {
    es_cat <- grepl("^catolico", txt)
    # Por defecto el catolico cuenta como practicante; pasa a "catolico
    # no practicante" solo si dice que no va casi nunca a oficios.
    out[es_cat] <- 1
    if (!is.null(practica)) {
      pe <- .valores(practica)
      pv <- as.numeric(zap_missing(practica))
      if (!is.null(pe)) {
        ptxt <- pe[as.character(pv)]
        out[es_cat & !is.na(ptxt) & grepl("^casi nunca|^nunca", ptxt)] <- 2
      }
    }
    out[!es_cat & !is.na(txt) & !grepl("^n[.]?c", txt)] <- 3
  }
  out
}

# 100 Sin estudios / 200 Primaria y secundaria elemental /
# 300 Secundaria superior (incluida toda la F.P.) / 500 Universitarios.
# "Otros", N.S. y N.C. quedan a perdido.
# El orden de las reglas es el que resuelve los solapamientos:
# "Secundaria 1a etapa" antes que "Secundaria" a secas; la F.P. antes
# que nada, porque sus etiquetas mencionan grados y ciclos.
.REGLAS_ESTUDIOS <- c(
  "^n[.]?c|^n[.]?s"                                      = NA,
  "^otros"                                               = NA,
  "^sin estudios|menos de 5 anos|^no procede"            = 100,
  "^educacion primaria|^primaria$|^primaria[^a-z]"       = 200,
  "1a etapa|^eso|^educacion secundaria|graduado escolar" = 200,
  "f[.]?p|formacion profesional|cualificacion profesional" = 300,
  "^bachillerato|2a etapa|^secundaria"                   = 300,
  "diplomat|diplomad|licenciat|^grado|^estudios de grado|master|doctorado|postgrado|posgrado|arquitect|ingenier|universitario|^superiores" = 500
)

# El CIS pregunta el nivel alcanzado (con todo el detalle de
# titulaciones) SOLO a quien tiene estudios; quien no los tiene se queda
# sin respuesta. Por eso los huecos de la variable detallada se rellenan
# con la agrupada, que si los clasifica. Sin ese relleno se pierden unos
# 125 entrevistados por estudio, casi todos "Sin estudios".
.rec_estudios <- function(x, respaldo = NULL) {
  out <- .por_etiqueta(x, .REGLAS_ESTUDIOS)
  if (!is.null(respaldo)) {
    alt <- .por_etiqueta(respaldo, .REGLAS_ESTUDIOS)
    out[is.na(out)] <- alt[is.na(out)]
  }
  out
}

.REGLAS_LABORAL <- c(
  "^n[.]?c"                       = NA,
  "^otra situacion"               = NA,
  "^trabaja"                      = 1,
  "^jubilado|^pensionista"        = 2,
  "^parado|^en paro"              = 3,
  "^estudiante"                   = 4,
  "domestico"                     = 5
)
.rec_laboral <- function(x) .por_etiqueta(x, .REGLAS_LABORAL)

# Ojo: en FID el 3 (autonomo sin asalariados) esta fundido en el 2 y el
# 4/5 (ayuda familiar, cooperativa) en el 6. Las etiquetas de FID no lo
# reflejan, los datos si.
.REGLAS_PROFESIONAL <- c(
  "^n[.]?c"                          = NA,
  "^no procede|^n[.]?p"              = 0,
  "^asalariado"                      = 1,
  "^empresario|autonomo"             = 2,
  "^ayuda familiar|cooperativa|^otra situacion" = 6
)
.rec_profesional <- function(x) .por_etiqueta(x, .REGLAS_PROFESIONAL)

.REGLAS_CLASE <- c(
  "^n[.]?c"                                   = NA,
  "^clase alta|media-alta"                    = 1,
  "media-media"                               = 2,
  "trabajadora|obrera"                        = 3,
  "media-baja|^clase baja"                    = 4,
  "^clase pobre|infraclase|proletariado|abajo|excluido|gente comun|^otras|no cree en las clases|^no sabe" = 9
)
.rec_clase <- function(x) .por_etiqueta(x, .REGLAS_CLASE)

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

# ---------------------------------------------------------------------
# Numero de estudio y año de campo
# ---------------------------------------------------------------------
.saca_estudio <- function(d, fichero) {
  for (v in c("ESTU", "ESTUDIO", "estu", "estudio")) {
    if (v %in% names(d)) {
      u <- unique(as.numeric(zap_missing(d[[v]])))
      u <- u[!is.na(u)]
      if (length(u) == 1) return(u)
    }
  }
  m <- regmatches(basename(fichero), regexpr("[0-9]{4}", basename(fichero)))
  if (length(m)) return(as.numeric(m))
  NA_real_
}

# La raiz del proyecto se busca subiendo desde el fichero de entrada (y,
# si eso falla, desde el directorio de trabajo) hasta dar con una carpeta
# que tenga las señas del proyecto. No se deduce de la ubicacion de este
# script, porque con source() eso no es fiable.
.raiz_proyecto <- function(fichero) {
  senyas <- function(p) file.exists(file.path(p, "FID3978abrev.dta")) ||
                        file.exists(file.path(p, "datos", "estudios_fecha.csv"))
  for (inicio in c(dirname(normalizePath(fichero, mustWork = FALSE)),
                   normalizePath(getwd(), mustWork = FALSE))) {
    p <- inicio
    for (i in 1:6) {
      if (senyas(p)) return(p)
      q <- dirname(p)
      if (q == p) break
      p <- q
    }
  }
  getwd()
}

.saca_anyo <- function(d, estudio, base) {
  # 1) la propia variable de fecha de realizacion
  et <- .norm(vapply(d, .etiqueta, character(1)))
  v  <- names(d)[grepl("^fecha de realizacion", et) & grepl("ano", et)]
  if (!length(v)) v <- intersect("E103", names(d))
  if (length(v)) {
    a <- unique(as.numeric(zap_missing(d[[v[1]]])))
    a <- a[!is.na(a)]
    if (length(a) >= 1) {
      a <- as.numeric(names(sort(table(as.numeric(zap_missing(d[[v[1]]]))),
                                 decreasing = TRUE))[1])
      if (!is.na(a)) {
        if (a < 100) a <- a + ifelse(a < 50, 2000, 1900)
        if (a > 1960 && a < 2100) return(a)
      }
    }
  }
  # 2) datos/estudios_fecha.csv, si esta. Va antes que la tabla de
  #    dentro para que ampliar o corregir la serie siga siendo cuestion
  #    de editar el CSV, sin tocar este fichero.
  f <- file.path(base, "datos", "estudios_fecha.csv")
  if (file.exists(f) && !is.na(estudio)) {
    t <- try(utils::read.csv(f, stringsAsFactors = FALSE), silent = TRUE)
    if (!inherits(t, "try-error")) {
      col <- intersect(c("anyo", "ano", "año"), names(t))
      if (length(col) && "estudio" %in% names(t)) {
        a <- t[[col[1]]][t$estudio == estudio]
        if (length(a) == 1 && !is.na(a)) return(as.numeric(a))
      }
    }
  }
  # 3) la tabla de arriba, para que el fichero se baste solo
  if (!is.na(estudio) && as.character(estudio) %in% names(.ANYOS))
    return(unname(.ANYOS[as.character(estudio)]))
  NA_real_
}

# ---------------------------------------------------------------------
# La tabla de correspondencias revisable
# ---------------------------------------------------------------------
.lee_tabla <- function(ruta) {
  if (!file.exists(ruta))
    return(data.frame(estudio = numeric(0), concepto = character(0),
                      variable = character(0), etiqueta = character(0),
                      origen = character(0), stringsAsFactors = FALSE))
  t <- utils::read.delim(ruta, stringsAsFactors = FALSE, colClasses = "character",
                         na.strings = character(0))
  t$estudio <- suppressWarnings(as.numeric(t$estudio))
  t
}

.guarda_tabla <- function(t, ruta) {
  t <- t[order(t$estudio, match(t$concepto, .CONCEPTOS)), ]
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  utils::write.table(t, ruta, sep = "\t", row.names = FALSE, quote = FALSE,
                     fileEncoding = "UTF-8")
}

# =====================================================================
# La funcion
# =====================================================================
cis_sociodem <- function(fichero,
                         destino  = NULL,
                         salida   = c("breve", "auditoria", "completo"),
                         estudio  = NULL,
                         anyo     = NULL,
                         tabla    = NULL,
                         escribir = TRUE,
                         avisos   = TRUE) {

  salida <- match.arg(salida)
  if (!file.exists(fichero)) stop("No existe el fichero: ", fichero)

  base <- .raiz_proyecto(fichero)
  if (is.null(tabla))
    tabla <- file.path(base, "datos", "sociodem_correspondencias.tsv")

  d <- suppressWarnings(
    if (grepl("[.]sav$", fichero, ignore.case = TRUE)) read_sav(fichero)
    else read_dta(fichero))

  if (is.null(estudio)) estudio <- .saca_estudio(d, fichero)
  if (is.na(estudio))
    stop("No he podido determinar el numero de estudio. Pasalo en 'estudio='.")
  if (is.null(anyo)) anyo <- .saca_anyo(d, estudio, base)

  # ---- que variable es cada concepto -------------------------------
  T <- .lee_tabla(tabla)
  manual <- T[!is.na(T$estudio) & T$estudio == estudio &
              tolower(T$origen) == "manual", ]
  elegida <- setNames(rep(NA_character_, length(.CONCEPTOS)), .CONCEPTOS)
  fuente  <- setNames(rep("auto", length(.CONCEPTOS)), .CONCEPTOS)
  for (cc in .CONCEPTOS) {
    if (cc %in% manual$concepto) {
      v <- manual$variable[manual$concepto == cc][1]
      elegida[cc] <- if (is.na(v) || v == "") NA_character_ else v
      fuente[cc]  <- "manual"
      if (!is.na(elegida[cc]) && !elegida[cc] %in% names(d))
        stop("La tabla manda usar '", elegida[cc], "' para ", cc,
             " en el estudio ", estudio, ", y esa variable no esta en el fichero.")
    } else {
      elegida[cc] <- .detecta(d, .PATRON[[cc]])
    }
  }
  practica <- .detecta(d, .PAT_PRACTICA)

  # ---- recodificar --------------------------------------------------
  n <- nrow(d)
  vacio <- function() rep(NA_real_, n)
  g <- function(cc) if (is.na(elegida[cc])) NULL else d[[elegida[cc]]]

  out <- data.frame(estudio = rep(as.numeric(estudio), n))
  cues <- intersect(c("CUES", "CUEST", "cues"), names(d))
  out$cues    <- if (length(cues)) as.numeric(zap_missing(d[[cues[1]]])) else NA_real_
  out$orden   <- seq_len(n)
  out$numertr <- NULL
  out$numentr <- as.numeric(estudio) * 100000 + out$orden

  x <- g("genero");      out$genero    <- if (is.null(x)) vacio() else .rec_genero(x)
  x <- g("edad")
  anyos <- if (is.null(x)) vacio() else .rec_edad_anyos(x)
  out$generacion <- .rec_generacion(anyos, anyo)
  out$edad       <- .rec_edad_tramos(anyos)
  x <- g("ideologia");   out$ideologia <- if (is.null(x)) vacio() else .rec_ideologia(x)
  x <- g("religion")
  out$religion  <- if (is.null(x)) vacio() else
                   .rec_religion(x, if (is.na(practica)) NULL else d[[practica]])
  x <- g("estudios")
  respaldo_est <- setdiff(.detecta_todas(d, .PATRON$estudios), elegida["estudios"])
  respaldo_est <- if (length(respaldo_est)) d[[respaldo_est[1]]] else NULL
  out$estudios  <- if (is.null(x)) vacio() else .rec_estudios(x, respaldo_est)
  x <- g("laboral");     out$laboral   <- if (is.null(x)) vacio() else .rec_laboral(x)
  x <- g("profesional"); out$profesional <- if (is.null(x)) vacio() else .rec_profesional(x)
  x <- g("clase");       out$clase     <- if (is.null(x)) vacio() else .rec_clase(x)

  out <- out[, .SALIDA]

  # ---- etiquetas ----------------------------------------------------
  attr(out$estudio, "label") <- "Numero de estudio CIS"
  attr(out$cues,    "label") <- "Numero de cuestionario (tal cual viene en el fichero)"
  attr(out$orden,   "label") <- "Posicion en el fichero de origen (clave con FID3978abrev)"
  attr(out$numentr, "label") <- "N unico de entrevistado (estudio*100000+orden)"
  for (cc in names(.ETIQ)) {
    out[[cc]] <- labelled(as.numeric(out[[cc]]), labels = .ETIQ[[cc]]$val,
                          label = .ETIQ[[cc]]$lab)
  }

  # ---- las columnas crudas, si se piden -----------------------------
  if (salida == "auditoria") {
    for (cc in .CONCEPTOS) {
      if (is.na(elegida[cc])) next
      nm <- paste0("o_", tolower(elegida[cc]))
      out[[nm]] <- d[[elegida[cc]]]
    }
    if (!is.na(practica)) out[[paste0("o_", tolower(practica))]] <- d[[practica]]
  } else if (salida == "completo") {
    nuevas <- c(setdiff(names(out), c("estudio", "cues")), "orden", "numentr")
    nuevas <- unique(nuevas)
    d2 <- d
    # SPSS no distingue mayusculas en los nombres, asi que ESTUDIOS del
    # fichero original chocaria con la 'estudios' armonizada. Manda la
    # armonizada, que es la que da la compatibilidad entre ficheros; la
    # original se conserva con el sufijo _ORIG.
    choques <- names(d2)[tolower(names(d2)) %in% tolower(nuevas)]
    if (length(choques)) {
      for (v in choques) {
        nn <- paste0(v, "_ORIG")
        while (tolower(nn) %in% tolower(c(names(d2), nuevas))) nn <- paste0(nn, "1")
        names(d2)[names(d2) == v] <- nn
      }
      if (avisos)
        cat("  nota: ", paste(choques, collapse = ", "),
            " del fichero original se renombra(n) con _ORIG para no chocar",
            " con las variables armonizadas.\n", sep = "")
    }
    for (v in nuevas) d2[[v]] <- out[[v]]
    out <- d2
  }

  # ---- informe y tabla ---------------------------------------------
  faltan <- .CONCEPTOS[is.na(elegida)]
  if (avisos) {
    cat("\n=== estudio ", estudio, "  (", basename(fichero), ", ", n,
        " filas) ===\n", sep = "")
    cat("  año de campo: ", ifelse(is.na(anyo), "NO DETERMINADO", anyo),
        ifelse(is.na(anyo), "  -> 'generacion' queda a perdido", ""), "\n", sep = "")
    for (cc in .CONCEPTOS) {
      v <- elegida[cc]
      if (is.na(v)) {
        cat(sprintf("  %-12s --  NO ESTA en el fichero -> columna vacia\n", cc))
      } else {
        cob <- 100 * mean(!is.na(out[[cc]]))
        cat(sprintf("  %-12s %-14s %5.1f%% valido   [%s] %s\n", cc, v, cob,
                    fuente[cc], substr(.etiqueta(d[[v]]), 1, 44)))
      }
    }
    if (!is.na(practica))
      cat("  (practica religiosa: ", practica, ")\n", sep = "")
    if (length(faltan))
      cat("  AVISO: sin datos para ", paste(faltan, collapse = ", "),
          ". Las columnas se crean vacias para que el fichero siga",
          " siendo apilable.\n", sep = "")
  }

  nuevas <- data.frame(
    estudio  = as.numeric(estudio),
    concepto = .CONCEPTOS,
    variable = ifelse(is.na(elegida), "", elegida),
    etiqueta = vapply(.CONCEPTOS, function(cc)
                 if (is.na(elegida[cc])) "(no esta en el fichero)"
                 else .etiqueta(d[[elegida[cc]]]), character(1)),
    origen   = fuente,
    stringsAsFactors = FALSE)
  T <- T[!(T$estudio == estudio & T$origen == "auto"), , drop = FALSE]
  ya <- paste(T$estudio, T$concepto)
  nuevas <- nuevas[!paste(nuevas$estudio, nuevas$concepto) %in% ya, , drop = FALSE]
  .guarda_tabla(rbind(T, nuevas), tabla)

  # ---- escribir -----------------------------------------------------
  if (escribir) {
    if (is.null(destino))
      destino <- sub("[.](sav|dta)$", "_soc.sav", fichero, ignore.case = TRUE)
    write_sav(out, destino)
    if (avisos) cat("  -> ", destino, "  (", ncol(out), " variables)\n", sep = "")
  }
  invisible(out)
}
