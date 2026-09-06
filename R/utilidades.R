
# ---------------------------------------------------------------------
# Utilidades de texto: las etiquetas del CIS varian en acentos, mayus-
# culas, barras de genero y aclaraciones entre parentesis. Se comparan
# siempre normalizadas.
# ---------------------------------------------------------------------
.norm <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
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
