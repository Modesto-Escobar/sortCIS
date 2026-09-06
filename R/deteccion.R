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
