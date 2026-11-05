# ---------------------------------------------------------------------
# Formatos de salida.
#
# 'ambos' escribe los dos ficheros, con la misma raiz y distinta
# extension, en una sola pasada.
# ---------------------------------------------------------------------
.EXT <- list(sav = "sav", dta = "dta", ambos = c("sav", "dta"))

# Stata es mas estricto que SPSS con los nombres de variable: solo
# admite letras, digitos y el guion bajo, no puede empezar por digito y
# no pasa de 32 caracteres. Con salida="breve" o "auditoria" los nombres
# los pone esta funcion y siempre valen; con "completo" vienen del
# fichero de origen y pueden no valer. Mas vale decirlo con claridad que
# dejar salir el error de haven, que no dice que variable falla.
.NOMBRE_STATA <- "^[A-Za-z_][A-Za-z0-9_]{0,31}$"

.escribe_dta <- function(d, ruta) {
  malos <- names(d)[!grepl(.NOMBRE_STATA, names(d))]
  if (length(malos))
    stop("Stata no admite estos nombres de variable: ",
         paste(malos, collapse = ", "),
         ". Escribe en .sav, o usa salida = 'breve', o renombralos antes.")
  write_dta(d, ruta)
}

# El CIS etiqueta a veces variables con decimales: en el 3577, cada valor
# de PESO y PESOCCAA lleva como etiqueta el propio valor escrito
# ("2,87887"). Stata no admite etiquetas de valor que no sean enteras, y
# en SPSS (y en lo que lee el .sav) hacen pasar una ponderacion por una
# nominal de miles de categorias. Se les quitan las etiquetas de valor,
# que no dicen nada que no diga el numero, y se conserva la de variable.
# Los nombres de las afectadas quedan en el atributo "sin_etiquetas".
.quita_etiquetas_decimales <- function(d) {
  decimales <- vapply(d, function(x) {
    l <- attr(x, "labels", exact = TRUE)
    is.numeric(l) && any(l != round(l), na.rm = TRUE)
  }, logical(1))
  for (v in names(d)[decimales]) {
    lab <- attr(d[[v]], "label", exact = TRUE)
    d[[v]] <- as.vector(unclass(d[[v]]))
    attr(d[[v]], "label") <- lab
  }
  attr(d, "sin_etiquetas") <- names(d)[decimales]
  d
}
