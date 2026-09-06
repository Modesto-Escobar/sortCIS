
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

