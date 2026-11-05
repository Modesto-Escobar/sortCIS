
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
  # Fuera las aclaraciones entre parentesis, pero solo ellas: el 3577 dice
  # "Escala (1-10) de autoubicacion ideologica", con el parentesis en
  # medio, y cortar desde el primer "(" hasta el final dejaba "escala".
  # Un parentesis sin cerrar si se lleva el resto.
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- sub("\\(.*$", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.etiqueta <- function(x) {
  L <- attr(x, "label", exact = TRUE)
  if (is.null(L) || length(L) != 1 || is.na(L)) return("")
  as.character(L)
}

# Pone a cada columna su etiqueta por nombre (cis_sociodem(etiquetas =
# TRUE)). Sin etiqueta, se queda el nombre. Si dos coinciden, la primera
# se queda con la etiqueta y las demas llevan su nombre entre parentesis:
# las armonizadas van delante, asi que el SEXO del original, que se
# etiqueta igual que 'genero', es el que se lleva el "(SEXO)". Los
# nombres de antes quedan en el atributo "nombres", con las etiquetas
# como nombres.
.nombres_etiqueta <- function(d) {
  viejo <- names(d)
  nuevo <- vapply(d, function(x) trimws(.etiqueta(x)), character(1),
                  USE.NAMES = FALSE)
  nuevo[nuevo == ""] <- viejo[nuevo == ""]
  rep <- duplicated(nuevo)
  nuevo[rep] <- paste0(nuevo[rep], " (", viejo[rep], ")")
  nuevo <- make.unique(nuevo)
  names(d) <- nuevo
  attr(d, "nombres") <- setNames(viejo, nuevo)
  d
}

# Etiquetas de los valores de una variable, ya normalizadas y en el
# orden de los codigos.
.valores <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(NULL)
  setNames(.norm(names(lv)), as.character(as.numeric(lv)))
}

# Lee un .sav sin el aviso "Invalid date string" de la libreria de lectura,
# que sale del codigo C (no es un warning de R, asi que suppressWarnings no
# lo calla). Lo provoca la fecha de creacion de la cabecera (9 bytes desde
# el 92, "dd mmm yy") cuando viene mal formada, como en el 3577 ("22 028 26").
# Si es asi, se lee una copia temporal con la fecha reemplazada; los datos y
# las etiquetas no cambian.
.lee_sav <- function(fichero, ...) {
  cab <- tryCatch(readBin(fichero, "raw", 101L), error = function(e) raw(0))
  if (length(cab) == 101L && rawToChar(cab[1:4]) == "$FL2") {
    fecha <- rawToChar(cab[93:101])
    if (!grepl("^[0-9]{2} [A-Za-z]{3} [0-9]{2}$", fecha)) {
      tmp <- tempfile(fileext = ".sav")
      on.exit(unlink(tmp), add = TRUE)
      if (file.copy(fichero, tmp)) {
        con <- file(tmp, "r+b")
        seek(con, 92L, rw = "write")
        writeBin(charToRaw("01 Jan 70"), con)
        close(con)
        fichero <- tmp
      }
    }
  }
  read_sav(fichero, ...)
}
