# ---------------------------------------------------------------------
# cisConvert(): detection of the variable for each concept, the common
# schema and the fieldwork year.
#
# The patterns and the labels of the schema are in Spanish on purpose:
# the former match the CIS labels, and the latter are the data of the
# common schema (FID3978abrev.dta), which are not translated.
# ---------------------------------------------------------------------

# Each concept has its patterns IN ORDER OF PREFERENCE: the first one is
# tried on all the variables, and only if none matches is the next one
# tried. It matters where the CIS offers the same information twice: in
# many studies ESTUDIOS ("Estudios de la persona entrevistada", already
# grouped) lives next to NIVELESTENTREV / P31A ("Nivel de estudios
# alcanzado...", with the detail of degrees). The detailed one is looked
# for first, and the grouped one is kept as backup: the level attained is
# only asked to those who have some education, and its gaps are filled
# with the grouped one (see .cis_rec_education).
.CIS_PATTERN <- list(
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

# Frequency of attendance at religious services: needed to tell
# practising from non-practising Catholics in older studies, whose
# religion variable does not distinguish them.
.CIS_PAT_PRACTICE <- "frecuencia de asistencia a oficios religiosos|practica religiosa"

.CIS_CONCEPTS <- names(.CIS_PATTERN)

# All the candidates, by order of pattern preference. The first one is
# the one used; for 'estudios' the second one is the backup.
.cis_detect_all <- function(d, patterns) {
  out <- character(0)
  for (p in patterns) {
    v <- .cis_detect(d, p)
    if (!is.na(v)) out <- c(out, v)
  }
  unique(out)
}

.cis_detect <- function(d, patterns) {
  et <- .cis_norm(vapply(d, .cis_label, character(1)))
  for (p in patterns) {
    hit <- names(d)[grepl(p, et)]
    if (!length(hit)) next
    # If several match the same pattern: prefer those with value labels
    # and, among them, the shortest name (ESTUDIOS before ESTUDIOSR,
    # RELIGION before RELIGIONR).
    if (length(hit) > 1) {
      with_lab <- vapply(hit, function(v) !is.null(attr(d[[v]], "labels")), logical(1))
      if (any(with_lab)) hit <- hit[with_lab]
      hit <- hit[order(nchar(hit))]
    }
    return(hit[1])
  }
  NA_character_
}

# ---------------------------------------------------------------------
# Labels of the common schema (those of FID3978abrev.dta)
# ---------------------------------------------------------------------
.CIS_SCHEMA <- list(
  genero      = list(lab = "G\u00e9nero",
                     val = c(Hombre = 1, Mujer = 2)),
  generacion  = list(lab = "Generaci\u00f3n",
                     val = c(`G. 98` = 1, `G. 14` = 2, `G.27` = 3, Postguerra = 4,
                             Boomers = 5, X = 6, Y = 7, Z = 8)),
  edad        = list(lab = "Edad",
                     val = c(Joven = 1, Adulto = 2, Maduro = 3, Mayor = 4, Vejez = 5)),
  ideologia   = list(lab = "Ideolog\u00eda",
                     val = c(Izquierda = 1, Centro = 2, Derecha = 3)),
  religion    = list(lab = "Religi\u00f3n",
                     val = c(Practicante = 1, Catolico = 2, Otras = 3)),
  estudios    = list(lab = "Estudios",
                     val = c(`Sin estudios` = 100, `Primaria y secundaria elemental` = 200,
                             `Secundaria superior` = 300, Universitarios = 500)),
  laboral     = list(lab = "Situaci\u00f3n laboral",
                     val = c(Trabaja = 1, Pensionista = 2, Parado = 3,
                             Estudiante = 4, `Domestico` = 5)),
  profesional = list(lab = "Situaci\u00f3n profesional",
                     val = c(`N.P.` = 0, `Asalariado/a por cuenta ajena` = 1,
                             `Empresario/autonomo` = 2, `Otra situacion` = 6)),
  clase       = list(lab = "Clase subjetiva",
                     val = c(`Alta/medio alta` = 1, Media = 2, Trabajador = 3,
                             Baja = 4, Otras = 9))
)

# Variables with a natural order among their categories: they come out
# as ordered factors. The others, as unordered factors.
.CIS_ORDINAL <- c("generacion", "edad", "ideologia", "estudios", "clase")

# Do the missing values of this variable go as explicit levels (TRUE) or
# to NA (FALSE)? 'NAs' is what was passed to cisConvert():
#   TRUE    all to NA
#   FALSE   all explicit
#   "auto"  NA where there is an order, explicit where there is not. An
#           "N.C." level at the end of an ordered factor says that N.C. is
#           above "Derecha", and polr() or polynomial contrasts believe it.
#   names   those to NA and the rest explicit
.cis_na_explicit <- function(name, ordered, NAs) {
  if (isTRUE(NAs)) return(FALSE)
  if (isFALSE(NAs)) return(TRUE)
  if (identical(NAs, "auto")) return(!ordered)
  !name %in% NAs
}

# The explicit missing levels of the nine harmonized variables: always
# both levels, even if the study has no cases in one, so that they stack.
.CIS_MISSING_LEVELS <- c("N.S.", "N.C.")

# Turns the nine schema variables into factors, with the levels in the
# order of the codes. All the levels of the schema are set, even if the
# study has no cases in some (G. 98 in a 2026 barometer): this way the
# factors of two different studies are equal and stack without surprises.
# If the missing values go explicit, they go to "N.S." and "N.C." at the
# end, according to 'reasons' (see .cis_reason); without a reason, they
# stay NA.
.cis_to_factors <- function(out, NAs = TRUE, reasons = list()) {
  for (cc in names(.CIS_SCHEMA)) {
    val <- .CIS_SCHEMA[[cc]]$val
    val <- val[order(val)]
    is_ord <- cc %in% .CIS_ORDINAL
    f <- factor(as.numeric(out[[cc]]), levels = unname(val),
                labels = names(val), ordered = is_ord)
    if (.cis_na_explicit(cc, is_ord, NAs)) {
      m <- reasons[[cc]]
      if (is.null(m)) m <- rep(NA_character_, length(f))
      txt <- as.character(f)
      txt[is.na(txt)] <- m[is.na(txt)]
      f <- factor(txt, levels = c(names(val), .CIS_MISSING_LEVELS),
                  ordered = is_ord)
      attr(f, "missingLevels") <- .CIS_MISSING_LEVELS
    }
    attr(f, "label") <- .CIS_SCHEMA[[cc]]$lab
    out[[cc]] <- f
  }
  out
}

# Why a harmonized value is missing, looking at the code it had in the
# original: "N.S." if it is labelled as don't know, "N.C." if it is
# anything else (no answer, "Otros" that the schema does not take). If the
# question was not asked, because it is a "No procede", the cell is blank
# or the variable is not in the study, nobody failed to answer: NA. Also
# NA where the harmonized variable has a value.
#
# 'sources' are the original variables in the order they are looked at,
# read with user_na = TRUE: if the file declares 98 as SPSS missing, the
# normal read leaves it NA and it is no longer known that it was an N.S.
.CIS_IS_DK <- "^n[.]?s[.]?($|[^a-z])|^no sabe"

.cis_reason <- function(y, sources = list()) {
  m <- rep(NA_character_, length(y))
  pending <- is.na(y)
  for (x in sources) {
    if (is.null(x)) next
    v <- as.numeric(unclass(x))
    et <- .cis_values(x)
    txt <- if (is.null(et)) rep(NA_character_, length(v)) else unname(et[as.character(v)])
    use <- pending & !is.na(v)
    t <- ifelse(is.na(txt[use]), "", txt[use])
    m[use] <- ifelse(grepl(.CIS_IS_DK, t), "N.S.",
                     ifelse(grepl(.CIS_NOT_APPLICABLE, t), NA_character_, "N.C."))
    pending <- pending & is.na(v)
  }
  m
}

.CIS_OUTPUT <- c("estudio", "cues", "orden", "numentr", "genero", "generacion",
                 "edad", "ideologia", "religion", "estudios", "laboral",
                 "profesional", "clase")

# ---------------------------------------------------------------------
# Fieldwork year of the studies of the series.
#
# The year is needed for 'generacion' (year of birth is fieldwork year
# minus age), and thirteen of the studies of the series, from 3279 on,
# stopped carrying the variable E103 with the date of the interview.
# Without this table 'year=' would have to be passed by hand for them.
#
# Source: the 'añomes' of FID3978abrev (constant within each study,
# checked) for the 33 that are in the cumulative file, plus the technical
# sheet FT3557.pdf for 3557. Also checked against the variable E103 in the
# 21 studies that carry it: all 21 match. Later 3567 (June 2026 barometer,
# according to its questionnaire) and 3577 (September 2026, according to
# FT3577.pdf) were added; neither carries E103.
#
# For a study that is not here, the function still looks at E103 and
# accepts year= .
# ---------------------------------------------------------------------
.CIS_YEARS <- c(
  `2798` = 2009, `2815` = 2009, `2834` = 2010, `2847` = 2010, `2885` = 2011,
  `2941` = 2012, `2960` = 2012, `2984` = 2013, `3001` = 2013, `3021` = 2014,
  `3041` = 2014, `3080` = 2015, `3114` = 2015, `3134` = 2016, `3156` = 2016,
  `3173` = 2017, `3191` = 2017, `3210` = 2018, `3226` = 2018, `3238` = 2019,
  `3257` = 2019, `3279` = 2020, `3296` = 2020, `3318` = 2021, `3337` = 2021,
  `3359` = 2022, `3380` = 2022, `3403` = 2023, `3423` = 2023, `3450` = 2024,
  `3478` = 2024, `3505` = 2025, `3528` = 2025, `3557` = 2026, `3567` = 2026,
  `3577` = 2026
)

# ---------------------------------------------------------------------
# Study number and fieldwork year
# ---------------------------------------------------------------------
.cis_get_study <- function(d, file) {
  for (v in c("ESTU", "ESTUDIO", "estu", "estudio")) {
    if (v %in% names(d)) {
      u <- unique(as.numeric(zap_missing(d[[v]])))
      u <- u[!is.na(u)]
      if (length(u) == 1) return(u)
    }
  }
  m <- regmatches(basename(file), regexpr("[0-9]{4}", basename(file)))
  if (length(m)) return(as.numeric(m))
  NA_real_
}

.cis_get_year <- function(d, study) {
  # 1) the variable with the date of the interview itself
  et <- .cis_norm(vapply(d, .cis_label, character(1)))
  # Depending on the study the label is "Fecha de realizacion: ano" (E103)
  # or "Ano de realizacion" (IA_E1_ANYO, from 3567 on), with a two-digit
  # year.
  v  <- names(d)[grepl("^(fecha|ano) de realizacion", et) & grepl("ano", et)]
  if (!length(v)) v <- intersect(c("E103", "IA_E1_ANYO"), names(d))
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
  # 2) the internal table
  if (!is.na(study) && as.character(study) %in% names(.CIS_YEARS))
    return(unname(.CIS_YEARS[as.character(study)]))
  NA_real_
}
