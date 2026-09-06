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
      col <- intersect(c("anyo", "ano", "a\u00f1o"), names(t))
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
