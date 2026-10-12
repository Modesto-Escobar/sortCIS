# ---------------------------------------------------------------------
# Umbral para los perdidos explicitos.
#
# Un "N.C." con el 0,4% de los casos es ruido: ensucia las tablas y en
# un modelo es una categoria casi vacia. Un "N.S." del 13% en ideologia
# es un resultado. El umbral manda a NA los niveles de perdido que no lo
# superan y deja los demas.
#
# Se mira nivel a nivel (un N.C. pequeño no se salva porque el N.S. sea
# grande) y sobre los casos con algun valor: los huecos de un filtro no
# cuentan, o una pregunta filtrada pareceria tener muy pocos perdidos.
# Sin ponderar.
# ---------------------------------------------------------------------

# Que niveles de un factor son de perdido. cis_sociodem() los deja
# apuntados en el atributo "perdidos"; si se ha perdido por el camino
# (rbind, subset...), se reconocen por la etiqueta, como en el resto del
# paquete. En las armonizadas son siempre "N.S." y "N.C.": su "N.P." es
# una categoria del esquema (profesional), no un perdido.
.niveles_perdido <- function(f, nombre) {
  p <- attr(f, "perdidos")
  if (nombre %in% names(.ETIQ)) return(intersect(levels(f), .NIVELES_PERDIDO))
  if (!is.null(p)) return(intersect(levels(f), p))
  et <- .norm(levels(f))
  levels(f)[grepl(.PERDIDO_PREG, et) & !grepl(.NO_PROCEDE, et)]
}

# Manda a NA los niveles de perdido que no pasan del umbral. En las
# armonizadas el nivel se queda aunque se vacie, para que se apilen; en
# las demas se quita.
.recorta <- function(f, nombre, umbral) {
  p <- .niveles_perdido(f, nombre)
  n <- sum(!is.na(f))
  if (!length(p) || n == 0) return(f)
  cuota <- vapply(p, function(l) sum(f == l, na.rm = TRUE) / n, numeric(1))
  fuera <- p[cuota <= umbral]
  if (!length(fuera)) return(f)
  atr <- attributes(f)
  f[f %in% fuera] <- NA
  if (!nombre %in% names(.ETIQ)) {
    f <- droplevels(f, except = which(!levels(f) %in% fuera))
    atr$levels <- levels(f)
  }
  if (!is.null(atr$perdidos)) atr$perdidos <- setdiff(atr$perdidos, fuera)
  attributes(f) <- atr
  f
}

#' Manda a `NA` los perdidos explícitos poco frecuentes
#'
#' @description
#' En los factores que llevan los perdidos como niveles explícitos ("N.S.",
#' "N.C.", "No conoce"...; véase el argumento `NAs` de [cis_sociodem()]),
#' manda a `NA` los niveles de perdido cuya proporción no pasa de `umbral`,
#' y deja como están los que sí. Un "N.C." del 0,4 % es ruido; un "N.S." del
#' 13 % en ideología es un resultado.
#'
#' Sirve igual para un estudio que para varios apilados. Para varios, mejor
#' aplicarla **después** de apilar: si se aplica a cada estudio por
#' separado, el "N.S." de una variable podría ser una categoría en uno y `NA`
#' en otro, y al apilarlos se mezclarían dos definiciones. Sobre el
#' acumulado el umbral es uno para todos.
#'
#' @param d Una salida de [cis_sociodem()], o varias apiladas.
#' @param umbral Proporción, entre 0 y 1. Los niveles de perdido que no la
#'   superan van a `NA`. Por defecto, 0,10.
#' @param variables Nombres de las variables a las que se aplica. Por
#'   defecto, todos los factores de `d`.
#'
#' @details
#' Se mira cada nivel de perdido por separado y sobre los casos que tienen
#' algún valor, sin ponderar: los `NA` (los huecos de un filtro) no cuentan
#' en el denominador.
#'
#' Los niveles de perdido son los que [cis_sociodem()] dejó apuntados en el
#' atributo `"perdidos"` del factor o, si se ha perdido al apilar, los que
#' lo dicen en la etiqueta. En las nueve armonizadas son siempre `"N.S."` y
#' `"N.C."`, y el nivel se conserva aunque se quede vacío, para que los
#' factores sigan casando; el `"N.P."` de `profesional` es una categoría y
#' no se toca. En las demás, el nivel vaciado se quita.
#'
#' @return `d`, con los factores recortados.
#'
#' @examples
#' \dontrun{
#' # Un estudio
#' d <- recorta_perdidos(cis_sociodem("datos/sav/3557.sav", escribir = FALSE))
#'
#' # Varios: primero se apilan
#' a <- cis_sociodem("datos/sav/3528.sav", escribir = FALSE)
#' b <- cis_sociodem("datos/sav/3557.sav", escribir = FALSE)
#' todo <- recorta_perdidos(rbind(a, b), umbral = 0.10)
#'
#' # Sólo en algunas variables
#' d <- recorta_perdidos(d, variables = c("religion", "laboral"))
#' }
#' @export
recorta_perdidos <- function(d, umbral = 0.10, variables = names(d)) {
  if (!is.data.frame(d)) stop("'d' tiene que ser un data.frame.")
  if (!(is.numeric(umbral) && length(umbral) == 1 && !is.na(umbral) &&
        umbral > 0 && umbral < 1))
    stop("'umbral' tiene que ser una proporcion entre 0 y 1 (0.10 para el 10%).")
  nada <- setdiff(variables, names(d))
  if (length(nada))
    warning("No estan en 'd': ", paste(nada, collapse = ", "), call. = FALSE)
  for (v in intersect(variables, names(d)))
    if (is.factor(d[[v]])) d[[v]] <- .recorta(d[[v]], v, umbral)
  d
}
