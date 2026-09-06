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
