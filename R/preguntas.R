
# ---------------------------------------------------------------------
# Preguntas de escala y de tipo Likert del resto del fichero.
#
# Solo cuentan con salida = "completo", y solo en lo que se devuelve en R:
# los ficheros .sav y .dta se escriben antes, etiquetados y sin tocar.
#
# Igual que con las sociodemograficas, el tipo se reconoce por la FORMA
# de las etiquetas de valor, no por el nombre de la variable. Probado
# sobre los originales del 1181, 2207, 2798, 3557, 3567 y 3577.
# ---------------------------------------------------------------------

# Las respuestas que no lo son. Mas amplio que .PERDIDO (que no se puede
# usar aqui: este fichero se carga antes que recodificacion.R): en las
# escalas aparecen tambien "No conoce" (al lider), "No consta" y "No
# recuerda".
.PERDIDO_PREG <- paste0(
  "^n[.]?s|^n[.]?c|^no sabe|^no contesta|^no procede|^no_procede|",
  "^no conoce|^no consta|^no recuerda|^n[.]?p[.]?$")

# El "No procede" de un filtro no es una respuesta que falta sino una
# pregunta que no se hizo, lo mismo que un hueco: va siempre a NA, nunca a
# un nivel explicito. ("No procede (filtro)" llega aqui sin el parentesis.)
.NO_PROCEDE <- "^no procede|^no_procede|^n[.]?p[.]?$"

# Palabras con que empiezan las categorias de una escala de intensidad.
.INTENSIDAD <- paste0(
  "^(muy|mucho|mucha|bastante|algo|poco|poca|nada|ninguna|ninguno|regular|",
  "ni |totalmente|completamente|siempre|casi|a menudo|algunas veces|a veces|",
  "raramente|nunca|mejor|igual|peor|buena|bueno|mala|malo|de acuerdo|",
  "en desacuerdo)")

# El polo bajo (malo, en desacuerdo, nada). Sirve para saber hacia donde va
# la escala: el CIS pone casi siempre primero el polo alto ("Muy buena"
# ... "Muy mala"), pero se comprueba en vez de darlo por hecho.
.POLO_BAJO <- paste0(
  "mala|malo|^muy mal$|desacuerdo|^nada|^ninguna|^ninguno|^nunca|^casi nunca|",
  "^peor|^raramente|pesimist|^poco|^poca")

# Las variables de paradatos (IA_) son recuentos de incidencias, y alguna
# tiene la forma de una escala 0-10. No son preguntas.
.NO_PREGUNTA <- "^IA_"

# Etiquetas de valor sustantivas (sin los perdidos), por orden de codigo.
.sustantivas <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(setNames(character(0), character(0)))
  et <- .norm(names(lv))
  cod <- as.numeric(lv)
  ok <- !grepl(.PERDIDO_PREG, et)
  s <- setNames(et[ok], cod[ok])
  s[order(cod[ok])]
}

.codigos_perdidos <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(numeric(0))
  as.numeric(lv)[grepl(.PERDIDO_PREG, .norm(names(lv)))]
}

# "escala", "likert" o "".
#
# Escala (0-10, 1-10, 1-7). Dos formas, segun la epoca:
#   - moderna: seis o mas puntos con codigos seguidos entre 0 y 10, y los
#     intermedios etiquetados solo con su numero (2 = "2"); los extremos
#     pueden llevar texto ("1 Izquierda", "10 Muy bien").
#   - antigua: como mucho dos etiquetas sustantivas, y todos los valores
#     observados entre 0 y 10, seis o mas distintos.
# Likert: de 3 a 7 categorias con codigos 1..k, y la primera, la ultima y
# al menos tres de cada cuatro empiezan con una palabra de intensidad.
.tipo_pregunta <- function(x, nombre = "") {
  if (!inherits(x, "haven_labelled") || grepl(.NO_PREGUNTA, nombre)) return("")
  s <- .sustantivas(x)
  cod <- as.numeric(names(s))
  v <- as.numeric(zap_missing(x))
  obs <- setdiff(unique(v[!is.na(v)]), .codigos_perdidos(x))

  if (length(cod) >= 6 && all(cod %in% 0:10) && all(diff(cod) == 1)) {
    medio <- s[-c(1, length(s))]
    if (all(medio == names(medio))) return("escala")
  }
  if (length(cod) <= 2 && length(obs) >= 6 && all(obs %in% 0:10)) return("escala")

  if (length(cod) >= 3 && length(cod) <= 7 && all(cod == seq_along(cod)) &&
      grepl(.INTENSIDAD, s[1]) && grepl(.INTENSIDAD, s[length(s)]) &&
      mean(grepl(.INTENSIDAD, s)) >= 0.75) return("likert")
  ""
}

# Escala a numero: el valor tal cual (no se reescala: una 1-7 sigue
# siendo 1-7), con los perdidos y lo que cae fuera de 0-10 a NA.
.escala_numerica <- function(x) {
  v <- as.numeric(zap_missing(x))
  v[v %in% .codigos_perdidos(x) | v < 0 | v > 10] <- NA
  v
}

.escala_factor <- function(x) {
  v <- .escala_numerica(x)
  lv <- attr(x, "labels")
  puntos <- sort(unique(c(v[!is.na(v)],
                          as.numeric(names(.sustantivas(x))))))
  nombres <- as.character(puntos)
  if (!is.null(lv)) {
    m <- match(puntos, as.numeric(lv))
    nombres[!is.na(m)] <- names(lv)[m[!is.na(m)]]
  }
  factor(v, levels = puntos, labels = nombres, ordered = TRUE)
}

# Likert: 1 el polo bajo, k el alto. Si los extremos no dicen hacia donde
# va, se supone la costumbre del CIS: primero el polo alto.
.likert_invierte <- function(s) {
  bajo_ini <- grepl(.POLO_BAJO, s[1])
  bajo_fin <- grepl(.POLO_BAJO, s[length(s)])
  !(bajo_ini && !bajo_fin)
}

.likert_numerica <- function(x, centrada = FALSE) {
  s <- .sustantivas(x)
  k <- length(s)
  v <- as.numeric(zap_missing(x))
  v[!v %in% seq_len(k)] <- NA
  if (.likert_invierte(s)) v <- k + 1 - v
  if (centrada) {
    # Con k impar el punto medio es 0; con k par no hay punto medio y se
    # salta el 0: 4 categorias dan -2, -1, 1, 2.
    z <- v - (k + 1) / 2
    v <- ifelse(z < 0, floor(z), ceiling(z))
  }
  v
}

.likert_factor <- function(x) {
  s <- .sustantivas(x)
  k <- length(s)
  lv <- attr(x, "labels")
  nombres <- names(lv)[match(seq_len(k), as.numeric(lv))]
  v <- as.numeric(zap_missing(x))
  v[!v %in% seq_len(k)] <- NA
  orden <- if (.likert_invierte(s)) rev(seq_len(k)) else seq_len(k)
  factor(v, levels = orden, labels = nombres[orden], ordered = TRUE)
}

# Codigos perdidos de una variable, con su etiqueta y por orden de codigo:
# los que lo dicen en la etiqueta ("N.S.", "No procede"...) y los que el
# fichero declara missing de SPSS. 'crudo' es la variable leida con
# user_na = TRUE; en la lectura normal los segundos ya son NA.
.perdidos_etiquetados <- function(crudo) {
  lv <- attr(crudo, "labels")
  if (is.null(lv)) return(setNames(numeric(0), character(0)))
  cod <- as.numeric(lv)
  spss <- cod %in% attr(crudo, "na_values")
  rng <- attr(crudo, "na_range")
  if (length(rng) == 2) spss <- spss | (cod >= rng[1] & cod <= rng[2])
  p <- setNames(cod, names(lv))[cod %in% .codigos_perdidos(crudo) | spss]
  p[order(p)]
}

# Lleva los perdidos de un factor a niveles explicitos al final, uno por
# cada etiqueta de perdido del original ("N.S.", "N.C.", "No conoce"...),
# en vez de juntarlos todos. El filtro, con codigo ("No procede") o sin
# el (un hueco), se queda en NA. Si el factor es ordenado lo sigue
# siendo, con los perdidos detras del polo alto.
.explicita <- function(f, crudo) {
  p <- .perdidos_etiquetados(crudo)
  p <- p[!grepl(.NO_PROCEDE, .norm(names(p)))]
  if (!length(p)) return(f)
  nuevos <- make.unique(c(levels(f), names(p)))[-seq_along(levels(f))]
  v <- as.numeric(unclass(crudo))
  txt <- as.character(f)
  hueco <- is.na(txt) & v %in% p
  txt[hueco] <- nuevos[match(v[hueco], p)]
  g <- factor(txt, levels = c(levels(f), nuevos), ordered = is.ordered(f))
  attr(g, "label") <- attr(f, "label")
  attr(g, "perdidos") <- nuevos
  g
}

# El resto de variables etiquetadas. A factor si todos sus valores tienen
# etiqueta; si no, es una cantidad con algun valor etiquetado ("10 o mas",
# el peso) y pasa a numero, con los perdidos a NA. En el factor, los
# perdidos van a NA o a sus niveles explicitos (vease .explicita).
.otra_a_factor <- function(x, explicito = FALSE, crudo = x) {
  lv <- attr(x, "labels")
  v <- as.numeric(zap_missing(x))
  v[v %in% .codigos_perdidos(x)] <- NA
  if (is.null(lv) || !all(v[!is.na(v)] %in% as.numeric(lv))) {
    attr(v, "label") <- attr(x, "label")
    return(v)
  }
  ok <- !as.numeric(lv) %in% .perdidos_etiquetados(crudo)
  lv <- lv[ok][order(as.numeric(lv[ok]))]
  f <- factor(v, levels = as.numeric(lv), labels = make.unique(names(lv)))
  attr(f, "label") <- attr(x, "label")
  if (explicito) f <- .explicita(f, crudo)
  f
}

# Monta lo que se devuelve con salida = "completo": claves, las nueve
# sociodemograficas, las escalas y Likert convertidas (columnas nuevas, en
# minusculas) y despues el fichero original.
#
# 'crudos' son las variables originales leidas con user_na = TRUE, con los
# mismos nombres que en 'out'; solo hacen falta para los perdidos
# explicitos. Devuelve, en el atributo "ordenadas_explicitas", las
# ordenadas que se han quedado con los perdidos al final, para avisar.
.prepara_completo <- function(out, likert, escala, centrada, factores, NAs,
                              crudos = list(), motivos = list()) {
  claves <- intersect(c("orden", "numentr"), names(out))
  socio  <- names(.ETIQ)
  orig   <- setdiff(names(out), c(claves, socio))
  crudo  <- function(v) if (is.null(crudos[[v]])) out[[v]] else crudos[[v]]
  avisar <- character(0)

  tipo <- vapply(orig, function(v) .tipo_pregunta(out[[v]], v), character(1))
  nuevas <- list()
  for (v in orig[tipo != ""]) {
    x <- out[[v]]
    nm <- tolower(v)
    while (nm %in% c(names(out), names(nuevas))) nm <- paste0(nm, "_r")
    nv <- if (tipo[v] == "escala") {
      if (escala == "numerica") .escala_numerica(x) else .escala_factor(x)
    } else {
      if (likert == "numerica") .likert_numerica(x, centrada) else .likert_factor(x)
    }
    # Un numero no puede llevar un "N.S." dentro: las numericas, siempre NA
    if (is.factor(nv) && .na_explicito(nm, TRUE, NAs)) {
      nv <- .explicita(nv, crudo(v))
      avisar <- c(avisar, nm)
    }
    attr(nv, "label") <- attr(x, "label")
    nuevas[[nm]] <- nv
  }

  if (factores) {
    out <- .a_factores(out, NAs, motivos)
    avisar <- c(intersect(.ORDINALES, socio[vapply(socio, function(cc)
      .na_explicito(cc, TRUE, NAs), logical(1))]), avisar)
    for (v in orig) if (inherits(out[[v]], "haven_labelled"))
      out[[v]] <- .otra_a_factor(out[[v]], .na_explicito(v, FALSE, NAs), crudo(v))
  }

  for (nm in names(nuevas)) out[[nm]] <- nuevas[[nm]]
  out <- out[c(claves, socio, names(nuevas), orig)]
  attr(out, "ordenadas_explicitas") <- avisar
  out
}
