
# ---------------------------------------------------------------------
# Recodificadores. Cada uno recibe el vector crudo y sus etiquetas de
# valor normalizadas, y devuelve el codigo del esquema comun.
# Todos trabajan sobre la ETIQUETA del valor; solo cuando la variable
# no trae etiquetas se cae al codigo numerico.
# ---------------------------------------------------------------------

# Aplica un vector de reglas c(patron = destino) sobre las etiquetas.
# Devuelve NA para lo que no case y para las reglas cuyo destino es NA:
# asi van a perdido los N.C., los N.S. y, segun la variable, "Otros" u
# "Otra situacion". Que va a perdido lo deciden las reglas de cada una.
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

# Etiquetas de valor que significan "no hay respuesta". Se usan donde hay
# que distinguirlas por el texto y no por el numero.
.PERDIDO <- "^n[.]?s|^n[.]?c|^no sabe|^no contesta|^no procede"

# Longitud de la escala. El CIS la declara en la propia etiqueta de la
# variable, "(1-10)" o "(1-7)", en los cuatro estudios comprobados (1181,
# 2207, 2798, 3557). Si faltara, se toma el mayor codigo etiquetado que no
# sea N.S./N.C.; y si tampoco hay etiquetas, se supone 1-10.
.rango_ideologia <- function(x) {
  L <- .etiqueta(x)
  m <- regmatches(L, regexpr("1[[:space:]]*-[[:space:]]*[0-9]+", L))
  if (length(m)) {
    n <- suppressWarnings(as.numeric(sub("^1[[:space:]]*-[[:space:]]*", "", m)))
    if (!is.na(n) && n >= 3 && n <= 11) return(n)
  }
  et <- .valores(x)
  if (!is.null(et)) {
    cod <- suppressWarnings(as.numeric(names(et)[!grepl(.PERDIDO, et)]))
    cod <- cod[!is.na(cod) & cod >= 1 & cod <= 11]
    if (length(cod)) return(max(cod))
  }
  10
}

# Escala de autoubicacion, de longitud variable segun el estudio: 1-10 en
# los modernos, 1-7 en los antiguos (el 1181 de 1979, por ejemplo).
#
# Dos cosas hay que mirar, y las dos por la ETIQUETA del valor, no por el
# numero, igual que en el resto del paquete:
#
#   - En la escala 1-7 el N.S. es el 8 y el N.C. el 9. Por numero caen
#     dentro del rango 1-10 y no hay forma de distinguirlos de una
#     respuesta: tomados como validos se irian todos a "Derecha". En el
#     1181 eso son 7.445 casos, el 96% de la categoria.
#   - El centro de la escala depende de su longitud. El 4 es izquierda en
#     una escala de 1 a 10 y es justo el centro en una de 1 a 7.
#
# Se resuelve reescalando linealmente a 1-10 y aplicando siempre los
# mismos cortes. Con una escala 1-10 el reescalado es la identidad, asi
# que los estudios de la serie salen exactamente igual que antes. Con una
# 1-7 da 1-3 izquierda, 4 centro y 5-7 derecha.
.rec_ideologia <- function(x) {
  v <- as.numeric(zap_missing(x))
  et <- .valores(x)
  if (!is.null(et)) {
    nc <- suppressWarnings(as.numeric(names(et)[grepl(.PERDIDO, et)]))
    v[v %in% nc[!is.na(nc)]] <- NA
  }
  n <- .rango_ideologia(x)
  if (n < 2) return(rep(NA_real_, length(v)))
  v[!is.na(v) & (v < 1 | v > n)] <- NA
  z <- if (n == 10) v else (v - 1) * 9 / (n - 1) + 1
  cut(z, breaks = c(0, 4, 6, 10), labels = FALSE)
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
    out[is.na(out) & !is.na(txt) & !grepl(.PERDIDO, txt)] <- 3
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
    out[!es_cat & !is.na(txt) & !grepl(.PERDIDO, txt)] <- 3
  }
  out
}

# 100 Sin estudios / 200 Primaria y secundaria elemental /
# 300 Secundaria superior (incluida toda la F.P.) / 500 Universitarios.
# "Otros", N.S. y N.C. quedan a perdido.
# El orden de las reglas es el que resuelve los solapamientos:
# "Secundaria 1a etapa" antes que "Secundaria" a secas; la F.P. antes
# que nada, porque sus etiquetas mencionan grados y ciclos.
#
# Las formulaciones antiguas van junto a las modernas en la misma regla:
# el CIS de 1979 dice "Menos de estudios primarios", "Estudios primarios
# completos" y "Bachiller elemental/superior" donde el de ahora dice
# "Educacion primaria" y "Bachillerato". Sin ellas el 1181 pierde el 89%
# de la variable, y no al azar: se quedan fuera justo las categorias
# bajas, que alli son cuatro de cada cinco entrevistados.
.REGLAS_ESTUDIOS <- c(
  "^n[.]?c|^n[.]?s"                                      = NA,
  "^otros"                                               = NA,
  # "Estudios primarios sin finalizar" tiene que ir ANTES que "Estudios
  # primarios" a secas, o la segunda se lo lleva: son 476 casos del 2207
  # que son "Sin estudios", no "Primaria completa".
  "^sin estudios|menos de 5 anos|^no procede|^menos de estudios primarios|^estudios primarios sin" = 100,
  "^educacion primaria|^primaria$|^primaria[^a-z]|^estudios primarios" = 200,
  "1a etapa|^eso|^educacion secundaria|graduado escolar|^bachiller elemental" = 200,
  "f[.]?p|formacion profesional|cualificacion profesional" = 300,
  "^bachillerato|2a etapa|^secundaria|^bachiller superior" = 300,
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

# "Sus labores" es como el CIS llamaba al trabajo domestico no remunerado
# hasta bien entrados los 2000, y "Esta parado" rompe el ancla de
# "^parado". Sin las dos, el 1181 pierde el 42% de la variable y el 2207
# el 22%, y lo que se pierde son amas de casa y parados: no es ruido, es
# sesgo.
.REGLAS_LABORAL <- c(
  "^n[.]?c"                       = NA,
  "^otra situacion"               = NA,
  "^trabaja"                      = 1,
  "^jubilado|^pensionista"        = 2,
  "^parado|^en paro|^esta parado" = 3,
  "^estudiante"                   = 4,
  "domestico|^sus labores"        = 5
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

# 'profesional' es la situacion profesional ACTUAL: a quien no trabaja,
# 0 N.P. Hace falta porque el CIS cambio la pregunta:
#   - 2207, 2798: se pregunta a todos, por el empleo actual o el ultimo.
#     Pensionistas, parados y amas de casa tienen valor.
#   - 3557 y siguientes: solo a quien trabaja. El resto lleva el 0 "No
#     procede", declarado ademas missing de SPSS, con lo que la lectura
#     normal lo deja en NA y el N.P. se perdia.
# Con el filtro de 'laboral' las dos formas dicen lo mismo. En los
# recientes coincide caso por caso con el 0 del fichero (N.P. si y solo
# si no trabaja, sin excepciones en el 3557, 3567 y 3577); en los antiguos
# se pierde el ultimo empleo de los no ocupados, que sigue en la variable
# cruda (salida = "auditoria").
#
# Va a N.P. cualquier respuesta de 'laboral' que no sea "Trabaja" ni un
# perdido; con 'laboral' perdido manda lo que diga la propia pregunta.
.rec_profesional <- function(x, laboral = NULL) {
  out <- .por_etiqueta(x, .REGLAS_PROFESIONAL)
  et <- if (is.null(laboral)) NULL else .valores(laboral)
  if (is.null(et)) return(out)
  txt <- et[as.character(as.numeric(zap_missing(laboral)))]
  no_trabaja <- !is.na(txt) & !grepl("^trabaja", txt) & !grepl(.PERDIDO, txt)
  out[no_trabaja] <- 0
  out
}

.REGLAS_CLASE <- c(
  "^n[.]?c"                                   = NA,
  "^clase alta|media-alta"                    = 1,
  "media-media"                               = 2,
  "trabajadora|obrera"                        = 3,
  "media-baja|^clase baja"                    = 4,
  "^clase pobre|infraclase|proletariado|abajo|excluido|gente comun|^otras|no cree en las clases|^no sabe" = 9
)
.rec_clase <- function(x) .por_etiqueta(x, .REGLAS_CLASE)
