#' Harmonize the sociodemographic variables of a CIS study
#'
#' @description
#' Takes a data file from the Spanish Centro de Investigaciones
#' Sociológicas (CIS), as an SPSS `.sav` or a Stata `.dta`, and writes
#' another one with the sociodemographic variables recoded to the common
#' schema of `FID3978abrev.dta`, so that two files converted with this
#' function can be stacked directly.
#'
#' The variable for each concept is detected by its **label**, and its
#' categories are recoded by the **value label**, never by the variable
#' name or the numeric code: the CIS keeps neither of them stable across
#' studies. The same question is called `SEXO` in recent studies and `P23`,
#' `P24`, `P28`... in older ones; "Ayuda familiar" is code 5 in some
#' studies and code 4 in others. What is stable is the label.
#'
#' Only the function, its arguments and its messages are in English. The
#' data are not translated: the harmonized variables keep the names, labels
#' and categories of the common schema (`genero`, "Hombre", "N.S."...), and
#' the variables of the CIS file are left as they come.
#'
#' @param file Path of the input `.sav` (or `.dta`).
#' @param dest Path of the output file, **without the extension**: `format`
#'   sets it. If it is given with an extension, the extension is dropped.
#'   By default, the name of the input file with the suffix `_soc`.
#' @param output Which columns the output file carries:
#'   \describe{
#'     \item{`"short"`}{The keys plus the nine recoded variables. Always the
#'       same columns in the same order: this is the one that guarantees
#'       that two files stack. The default.}
#'     \item{`"audit"`}{The above plus the raw variables each one comes
#'       from, with the prefix `o_` (`o_sexo`, `o_escideol`...), to check
#'       the recoding. The `o_` columns change from one study to another;
#'       the first twelve columns do not.}
#'     \item{`"full"`}{The input file (without the `IA_` variables, unless
#'       `IAs = TRUE`) plus the nine new ones at the end, together with
#'       `orden` and `numentr`. `estudio` and `cues` are not added, because
#'       the file already has its own identifiers. If a name in the
#'       original file clashes with a harmonized one, the original is kept
#'       with the suffix `_ORIG`: SPSS is not case sensitive, so `ESTUDIOS`
#'       would clash with `estudios`. What is returned in R comes in
#'       another order: `orden` and `numentr`, the nine harmonized
#'       variables, the converted scales and Likert items (see the section
#'       "Scales and Likert items") and then the original file. With
#'       `factors = TRUE` the labelled variables of the original become
#'       factors if all their values have a label, and numbers if not.}
#'   }
#' @param study Study number. By default it is taken from the variables
#'   `ESTU`/`ESTUDIO` and, if they are missing, from the file name.
#' @param year Year of the fieldwork, needed for `generacion`. Only needed
#'   for a study outside the series: by default it is taken from the
#'   variable `E103` (or `IA_E1_ANYO`) of the file itself and, failing
#'   that, from an internal table with the 36 studies of the series. If it
#'   is not found, a warning is printed and `generacion` is left missing;
#'   the other eight variables are not affected.
#' @param variables Named character vector that overrides the automatic
#'   detection: the names are concepts (`"genero"`, `"edad"`,
#'   `"ideologia"`, `"religion"`, `"estudios"`, `"laboral"`,
#'   `"profesional"`, `"clase"`) and the values, the variable of the file
#'   to use for each. `""` declares that the concept is not in the study.
#'   It is the way to fix a study with an unusual label. Concepts not named
#'   are detected as usual.
#' @param write If `FALSE`, returns the `data.frame` without writing the
#'   output file.
#' @param verbose If `FALSE`, silences the report on screen.
#' @param format Format of the output file: SPSS `"sav"` (the default),
#'   Stata `"dta"`, or `"both"`, which writes the two with the same root. If
#'   `format` is not given but `dest` ends in `.dta`, `"dta"` is assumed.
#'
#'   Stata only accepts variable names made of letters, digits and
#'   underscores, not starting with a digit and no longer than 32
#'   characters. The names set by this function always comply; with
#'   `output = "full"` the names come from the source file and, if one is
#'   not valid, an error names it instead of writing half a file.
#' @param factors If `TRUE` (the default), the nine harmonized variables of
#'   the returned `data.frame` are factors, with the levels in the order of
#'   the codes: ordered `generacion`, `edad`, `ideologia`, `estudios` and
#'   `clase`, and unordered `genero`, `religion`, `laboral` and
#'   `profesional`. They always carry all the levels of the schema, even if
#'   the study has no cases in some, so that those of two studies match
#'   when stacked. If `FALSE`, they are returned labelled
#'   (`haven_labelled`), with the codes of the common schema.
#'
#'   It only affects what is returned: the `.sav` and `.dta` files are
#'   always written labelled and with the codes of the common schema.
#'
#'   Beware: `as.numeric()` on a factor gives the position of the level,
#'   not the code. In `estudios` (100-500), `profesional` (0, 1, 2, 6) and
#'   `clase` (9 Otras) they differ; for the codes, use `factors = FALSE`.
#' @param NAs What to do with the missing values ("N.S.", "N.C.", "No
#'   conoce"...) in what is returned as a factor: either they stay `NA`, or
#'   they become explicit levels at the end of the factor. To send the rare
#'   ones to `NA` afterwards, see [trimMissing()].
#'   \describe{
#'     \item{`"auto"`}{(the default) `NA` in the ordered factors and
#'       explicit in the others. A missing level at the end of an ordered
#'       factor would rank it above the highest category, and whoever uses
#'       the order (`polr()`, `<`, polynomial contrasts) would believe it.}
#'     \item{`TRUE`}{All of them `NA`.}
#'     \item{`FALSE`}{All of them explicit, also in the ordered factors,
#'       which stay ordered with the missing levels last (with a note).}
#'     \item{Variable names}{Those to `NA` and all the others explicit.
#'       Names of the harmonized variables (`"ideologia"`), of the
#'       converted scales and Likert items (`"escideol"`) and of the
#'       original ones (`"P5"`) are accepted. If one is not in what is
#'       returned, a warning is given.}
#'   }
#'   In the nine harmonized variables the explicit levels are always
#'   `"N.S."` and `"N.C."`, even if the study has no cases in one, so that
#'   they stack; what is "No sabe" in the original goes to `"N.S."` and
#'   everything else to `"N.C."`. In the other variables there is one level
#'   for each missing label of the original, with its text. Numeric scales
#'   and Likert items always take the missing values to `NA`, because a
#'   number cannot hold an "N.S.".
#'
#'   What is not a missing answer but a question that was not asked always
#'   goes to `NA`: the "No procede" of a filter, whether it has a code or is
#'   blank, a variable that is not in the study, or `generacion` without a
#'   fieldwork year. (In `profesional`, "N.P." is a category of the schema:
#'   those who do not work.)
#'
#'   It only affects what is returned in R: in the written files the
#'   missing values of the harmonized variables stay blank.
#' @param likert Only counts with `output = "full"`. How the Likert items
#'   ("Muy buena" ... "Muy mala", "Mucha" ... "Ninguna confianza", "Muy de
#'   acuerdo" ... "Muy en desacuerdo") are returned: `"numeric"` (the
#'   default) or an ordered `"factor"`. In both, the highest value is the
#'   positive or intense pole, the reverse of the CIS codes, which put "Muy
#'   buena" first. See the section "Scales and Likert items".
#' @param scale Only counts with `output = "full"`. How the 0-10, 1-10 or
#'   1-7 scales (ideology, rating of leaders, probability of voting...) are
#'   returned: `"numeric"` (the default), with the value as it is, or an
#'   ordered `"factor"`.
#' @param centered Only counts with `likert = "numeric"`. If `TRUE`, the
#'   midpoint of the scale is 0, the negative pole is negative and the
#'   positive one, positive: with five categories, -2, -1, 0, 1, 2; with
#'   four, which have no midpoint, -2, -1, 1, 2. By default, `FALSE`: from
#'   1 to k.
#' @param IAs Only counts with `output = "full"`. If `FALSE` (the
#'   default), the CIS paradata variables, those starting with `IA_`
#'   (contact incidents, interview supervision...), are dropped, both from
#'   the written file and from what is returned. With `TRUE` they are kept.
#' @param labels If `TRUE`, the columns of the returned `data.frame` are
#'   named after their label (`"Género"`, `"Ideología"`...) and not their
#'   name. Those without a label keep their name. If two columns share a
#'   label, the first keeps it and the others get their name in
#'   parentheses after it: with `output = "full"`, a scale and its
#'   conversion share a label, and the original becomes, for instance,
#'   `"Pedro Sanchez (VALSAN)"`. The harmonized variables come first, so
#'   they never get the parentheses. The previous names are kept in the
#'   `"varnames"` attribute of the `data.frame`, with the labels as names.
#'   By default, `FALSE`.
#'
#'   It only affects what is returned in R: the `.sav` and `.dta` files are
#'   always written with the variable names. `NAs` still takes the names,
#'   not the labels.
#' @param writeAs What is written to the file. With `"original"` (the
#'   default), the variables as they come from the CIS, plus the harmonized
#'   ones. With `"as_R"` and `output = "full"`, also the converted scales
#'   and Likert items. The order differs from R: first the harmonized and
#'   converted variables, then the questionnaire from `P1` on and, at the
#'   end, the header of the original (everything before `P1`: `ESTUDIO`,
#'   `REGISTRO`, `CUES`, territory, `SEXO`, `EDAD`, nationality...) with
#'   `orden` and `numentr`. In the file they always go as numbers, even if
#'   they are asked as factors in R, and without value labels (only the
#'   variable label), so that SPSS and whatever reads the `.sav` take them
#'   as quantitative and not nominal. They are oriented so that the high
#'   value is the high pole ("mucho", "muy bien", "de acuerdo"): the Likert
#'   items already reversed (and centered, with `centered = TRUE`); the
#'   scales, with their value as it is. The harmonized variables keep the
#'   codes of the common schema and the originals are kept; as SPSS is not
#'   case sensitive, the original that matches its conversion (`ESCIDEOL`
#'   and `escideol`) is written with the suffix `_ORIG`. With other outputs
#'   nothing changes.
#'
#' @return Invisibly, the output `data.frame`. Its `"detected"` attribute
#'   says which variable was used for each concept, with its label and
#'   whether it was detected (`"auto"`) or given in `variables`
#'   (`"manual"`). The main effect is the file written to `dest` (unless
#'   `write = FALSE`).
#'
#' @section Output variables:
#' With `output = "short"` the file always carries these thirteen columns,
#' in this order:
#'
#' \describe{
#'   \item{`estudio`}{CIS study number.}
#'   \item{`cues`}{Questionnaire number, as it comes in the file.}
#'   \item{`orden`}{Position in the source file; it is the key that matches
#'     `FID3978abrev`.}
#'   \item{`numentr`}{Unique respondent identifier,
#'     `estudio * 100000 + orden`.}
#'   \item{`genero`}{1 Hombre, 2 Mujer.}
#'   \item{`generacion`}{1 G. 98, 2 G. 14, 3 G. 27, 4 Postguerra, 5 Boomers,
#'     6 X, 7 Y, 8 Z. It comes from the year of birth (fieldwork year minus
#'     age), so it needs `year`.}
#'   \item{`edad`}{Age groups: 1 Joven (up to 34), 2 Adulto (35-49),
#'     3 Maduro (50-64), 4 Mayor (65-79), 5 Vejez (80 and over).}
#'   \item{`ideologia`}{1 Izquierda, 2 Centro, 3 Derecha. On the 1-10 scale
#'     of modern studies they are 1-4, 5-6 and 7-10. The length of the
#'     scale is read from the variable label, because older studies use
#'     others: 1181, from 1979, goes from 1 to 7, and there the split is
#'     1-3, 4 and 5-7. In those short scales "N.S." and "N.C." are 8 and 9,
#'     which fall inside the 1-10 range, so they are discarded by the value
#'     label and not by the number.}
#'   \item{`religion`}{1 Practicante, 2 Católico no practicante, 3 Otras.}
#'   \item{`estudios`}{100 Sin estudios, 200 Primaria y secundaria
#'     elemental, 300 Secundaria superior (including all vocational
#'     training), 500 Universitarios.}
#'   \item{`laboral`}{1 Trabaja, 2 Pensionista, 3 Parado, 4 Estudiante,
#'     5 Doméstico.}
#'   \item{`profesional`}{0 N.P., 1 Asalariado por cuenta ajena,
#'     2 Empresario o autónomo, 6 Otra situación. It is the **current**
#'     occupational status: whoever does not work, according to `laboral`,
#'     is 0 N.P. The CIS changed the question: up to 2798 it was asked to
#'     everybody, about the current or last job, and from 3557 on only to
#'     those who work. With the filter, the studies say the same; the last
#'     job of those not employed in the older studies remains in the raw
#'     variable (`output = "audit"`).}
#'   \item{`clase`}{1 Alta/media-alta, 2 Media, 3 Trabajador, 4 Baja,
#'     9 Otras.}
#' }
#'
#' "N.C." and "N.S." go to missing, and so do "Otros" in `estudios` and
#' "Otra situación" in `laboral`. There are two exceptions, which reproduce
#' what the common schema does: in `profesional`, "Otra situación"
#' (together with family help and cooperatives) goes to 6, and in `clase`,
#' "Otras", "No sabe" and the answers that do not name a class go to 9. A
#' concept that does not exist in the study is created as an empty column,
#' so that the file can still be stacked.
#'
#' @section Scales and Likert items:
#' With `output = "full"`, the scale and Likert questions of the file are
#' added converted, as new columns with the name in lower case
#' (`escideol`, `valoralideres_1`...), right after the nine harmonized
#' variables. The originals stay at the end with their name in upper case.
#' "N.S.", "N.C.", "No procede" and "No conoce" go to `NA`, and in those
#' asked as factors they can also become explicit levels (see `NAs`).
#'
#' They are recognized by the shape of the value labels, not by the name:
#' \describe{
#'   \item{Scale}{Six or more consecutive points between 0 and 10 with the
#'     intermediate ones labelled only with their number (`2 = "2"`), as in
#'     recent studies; or, as in older ones, without labels for the points
#'     and with all the observed values between 0 and 10.}
#'   \item{Likert}{From three to seven categories with codes 1..k whose
#'     labels start with intensity words ("muy", "mucho", "bastante",
#'     "poco", "nada", "ninguna", "regular", "mejor", "peor", "de
#'     acuerdo"...). The direction of the scale is read from the labels of
#'     the extremes.}
#' }
#' The paradata variables (`IA_`) are not touched.
#'
#' @examples
#' \dontrun{
#' # The usual: writes datos/sav/3557_soc.sav
#' cisConvert("datos/sav/3557.sav")
#'
#' # Check the recoding without writing anything
#' d <- cisConvert("datos/sav/3557.sav", output = "audit", write = FALSE)
#' attr(d, "detected")
#'
#' # A study outside the series without the fieldwork date
#' cisConvert("other.sav", study = 9999, year = 2021)
#'
#' # A study whose ideology question has an unusual label
#' cisConvert("other.sav", variables = c(ideologia = "P23"))
#'
#' # In Stata: writes datos/sav/3557_soc.dta
#' cisConvert("datos/sav/3557.sav", format = "dta")
#'
#' # Both at once: 3557_soc.sav and 3557_soc.dta
#' cisConvert("datos/sav/3557.sav", format = "both")
#'
#' # Same as format = "dta", because of the extension of dest
#' cisConvert("datos/sav/3557.sav", dest = "out/3557.dta")
#'
#' # Stack two converted studies
#' a <- cisConvert("datos/sav/3528.sav", write = FALSE)
#' b <- cisConvert("datos/sav/3557.sav", write = FALSE)
#' both <- rbind(a, b)
#'
#' # All missing values as explicit levels ("N.S.", "N.C."...)
#' d <- cisConvert("datos/sav/3557.sav", write = FALSE, NAs = FALSE)
#'
#' # Those of religion and ideologia to NA, the others explicit
#' d <- cisConvert("datos/sav/3557.sav", write = FALSE,
#'                  NAs = c("religion", "ideologia"))
#'
#' # Labelled with the codes of the schema, as in the file
#' d <- cisConvert("datos/sav/3557.sav", write = FALSE, factors = FALSE)
#'
#' # Columns named after their label; the file, with the names
#' d <- cisConvert("datos/sav/3557.sav", labels = TRUE)
#' attr(d, "varnames")   # from label to name
#'
#' # The file, with the scales and Likert items converted, as in R
#' d <- cisConvert("datos/sav/3557.sav", output = "full", writeAs = "as_R")
#' }
#'
#' @seealso [trimMissing()] to send the rare explicit missing levels to `NA`.
#'
#' @export
cisConvert <- function(file,
                       dest      = NULL,
                       output    = c("short", "audit", "full"),
                       study     = NULL,
                       year      = NULL,
                       variables = NULL,
                       write     = TRUE,
                       verbose   = TRUE,
                       format    = c("sav", "dta", "both"),
                       factors   = TRUE,
                       NAs       = "auto",
                       likert    = c("numeric", "factor"),
                       scale     = c("numeric", "factor"),
                       centered  = FALSE,
                       IAs       = FALSE,
                       labels    = FALSE,
                       writeAs   = c("original", "as_R")) {

  output <- match.arg(output)
  writeAs <- match.arg(writeAs)
  if (!isTRUE(IAs) && !isFALSE(IAs))
    stop("'IAs' must be TRUE or FALSE.")
  likert <- match.arg(likert)
  scale <- match.arg(scale)
  if (!isTRUE(centered) && !isFALSE(centered))
    stop("'centered' must be TRUE or FALSE.")
  if (!isTRUE(factors) && !isFALSE(factors))
    stop("'factors' must be TRUE or FALSE.")
  if (!isTRUE(labels) && !isFALSE(labels))
    stop("'labels' must be TRUE or FALSE.")
  if (!isTRUE(NAs) && !isFALSE(NAs) &&
      !(is.character(NAs) && length(NAs) && !anyNA(NAs)))
    stop("'NAs' must be TRUE, FALSE, \"auto\" or the names of the ",
         "variables whose missing values go to NA.")
  if (!is.null(variables)) {
    if (!is.character(variables) || is.null(names(variables)) ||
        anyNA(variables))
      stop("'variables' must be a named character vector, such as ",
           "c(ideologia = \"P23\").")
    bad <- setdiff(names(variables), .CIS_CONCEPTS)
    if (length(bad))
      stop("'variables' names unknown concepts: ", paste(bad, collapse = ", "),
           ". Valid ones: ", paste(.CIS_CONCEPTS, collapse = ", "), ".")
  }
  # If no format is asked for but dest ends in .dta, dest rules:
  # cisConvert(f, dest = "x.dta") does what it seems to do.
  auto_format <- missing(format)
  format <- match.arg(format)
  if (auto_format && !is.null(dest) &&
      grepl("[.]dta$", dest, ignore.case = TRUE)) format <- "dta"
  if (!file.exists(file)) stop("File not found: ", file)

  is_sav <- grepl("[.]sav$", file, ignore.case = TRUE)
  d <- suppressWarnings(if (is_sav) .cis_read_sav(file) else read_dta(file))
  # Explicit missing levels need the code each missing value had, and the
  # normal read turns the SPSS declared missing values into NA (the "No
  # procede", sometimes N.S. and N.C.). The file is read again keeping
  # them. 'd' does not change: the recoding stays the same.
  # zap_missing() on this read does not give the same as the normal one
  # (it loses classes), hence the double read.
  d_na <- if (is_sav && !isTRUE(NAs))
            suppressWarnings(.cis_read_sav(file, user_na = TRUE)) else d

  if (is.null(study)) study <- .cis_get_study(d, file)
  if (is.na(study))
    stop("Could not determine the study number. Pass it in 'study='.")
  if (is.null(year)) year <- .cis_get_year(d, study)

  # ---- which variable is each concept --------------------------------
  chosen <- setNames(rep(NA_character_, length(.CIS_CONCEPTS)), .CIS_CONCEPTS)
  origin <- setNames(rep("auto", length(.CIS_CONCEPTS)), .CIS_CONCEPTS)
  for (cc in .CIS_CONCEPTS) {
    if (cc %in% names(variables)) {
      v <- variables[[cc]]
      chosen[cc] <- if (v == "") NA_character_ else v
      origin[cc] <- "manual"
      if (!is.na(chosen[cc]) && !chosen[cc] %in% names(d))
        stop("'variables' asks to use '", chosen[cc], "' for ", cc,
             ", and that variable is not in the file.")
    } else {
      chosen[cc] <- .cis_detect(d, .CIS_PATTERN[[cc]])
    }
  }
  practice <- .cis_detect(d, .CIS_PAT_PRACTICE)

  # ---- recode ---------------------------------------------------------
  n <- nrow(d)
  empty <- function() rep(NA_real_, n)
  g <- function(cc) if (is.na(chosen[cc])) NULL else d[[chosen[cc]]]

  out <- data.frame(estudio = rep(as.numeric(study), n))
  cues <- intersect(c("CUES", "CUEST", "cues"), names(d))
  out$cues    <- if (length(cues)) as.numeric(zap_missing(d[[cues[1]]])) else NA_real_
  out$orden   <- seq_len(n)
  out$numentr <- as.numeric(study) * 100000 + out$orden

  x <- g("genero");      out$genero    <- if (is.null(x)) empty() else .cis_rec_gender(x)
  x <- g("edad")
  ages <- if (is.null(x)) empty() else .cis_rec_age_years(x)
  out$generacion <- .cis_rec_generation(ages, year)
  out$edad       <- .cis_rec_age_groups(ages)
  x <- g("ideologia");   out$ideologia <- if (is.null(x)) empty() else .cis_rec_ideology(x)
  x <- g("religion")
  out$religion  <- if (is.null(x)) empty() else
                   .cis_rec_religion(x, if (is.na(practice)) NULL else d[[practice]])
  x <- g("estudios")
  backup_edu <- setdiff(.cis_detect_all(d, .CIS_PATTERN$estudios), chosen["estudios"])
  backup_edu <- if (length(backup_edu)) d[[backup_edu[1]]] else NULL
  out$estudios  <- if (is.null(x)) empty() else .cis_rec_education(x, backup_edu)
  x <- g("laboral");     out$laboral   <- if (is.null(x)) empty() else .cis_rec_employment(x)
  # Without the occupational status question it is left empty, even if
  # there is 'laboral': with only the N.P. it would look as if the study
  # had it.
  x <- g("profesional"); out$profesional <- if (is.null(x)) empty() else
                         .cis_rec_occupation(x, g("laboral"))
  x <- g("clase");       out$clase     <- if (is.null(x)) empty() else .cis_rec_class(x)

  out <- out[, .CIS_OUTPUT]

  # Why each missing value is missing (N.S. or N.C.), for the explicit
  # levels. 'generacion' and 'edad' both come from age; 'estudios' looks
  # at the grouped variable when the detailed one was not asked.
  g_na <- function(cc) if (is.na(chosen[cc])) NULL else d_na[[chosen[cc]]]
  source_of <- c(genero = "genero", generacion = "edad", edad = "edad",
                 ideologia = "ideologia", religion = "religion",
                 estudios = "estudios", laboral = "laboral",
                 profesional = "profesional", clase = "clase")
  reasons <- lapply(setNames(names(source_of), names(source_of)), function(cc) {
    sources <- list(g_na(source_of[[cc]]))
    if (cc == "estudios" && !is.null(backup_edu))
      sources <- c(sources, list(d_na[[setdiff(.cis_detect_all(d, .CIS_PATTERN$estudios),
                                               chosen["estudios"])[1]]]))
    .cis_reason(out[[cc]], sources)
  })
  # Without a fieldwork year there is no generacion for anybody, whether
  # they gave their age or not
  if (is.na(year)) reasons$generacion[] <- NA_character_

  # ---- labels ---------------------------------------------------------
  attr(out$estudio, "label") <- "Estudio"
  attr(out$cues,    "label") <- "Cuestionario"
  attr(out$orden,   "label") <- "Posici\u00f3n en el fichero"
  attr(out$numentr, "label") <- "Estudio*100000+orden"
  for (cc in names(.CIS_SCHEMA)) {
    out[[cc]] <- labelled(as.numeric(out[[cc]]), labels = .CIS_SCHEMA[[cc]]$val,
                          label = .CIS_SCHEMA[[cc]]$lab)
  }

  # ---- the raw columns, if asked for ----------------------------------
  if (output == "audit") {
    for (cc in .CIS_CONCEPTS) {
      if (is.na(chosen[cc])) next
      nm <- paste0("o_", tolower(chosen[cc]))
      out[[nm]] <- d[[chosen[cc]]]
    }
    if (!is.na(practice)) out[[paste0("o_", tolower(practice))]] <- d[[practice]]
  } else if (output == "full") {
    added <- c(setdiff(names(out), c("estudio", "cues")), "orden", "numentr")
    added <- unique(added)
    d2 <- d
    # The CIS paradata (IA_: contact incidents, supervision...) are not
    # answers of the respondent. Out, unless they are asked for.
    if (!IAs) d2 <- d2[!grepl("^IA_", names(d2), ignore.case = TRUE)]
    # SPSS is not case sensitive in names, so ESTUDIOS in the original
    # file would clash with the harmonized 'estudios'. The harmonized one
    # wins, since it is the one that makes files compatible; the original
    # is kept with the suffix _ORIG.
    orig_of <- setNames(names(d2), names(d2))
    clashes <- names(d2)[tolower(names(d2)) %in% tolower(added)]
    if (length(clashes)) {
      for (v in clashes) {
        nn <- paste0(v, "_ORIG")
        while (tolower(nn) %in% tolower(c(names(d2), added))) nn <- paste0(nn, "1")
        names(d2)[names(d2) == v] <- nn
        names(orig_of)[names(orig_of) == v] <- nn
      }
      if (verbose)
        cat("  note: ", paste(clashes, collapse = ", "),
            " of the original file renamed with _ORIG so as not to clash",
            " with the harmonized variables.\n", sep = "")
    }
    for (v in added) d2[[v]] <- out[[v]]
    # The same variables with their SPSS missing values, under the name
    # they have in d2 (_ORIG included)
    raw <- setNames(lapply(orig_of, function(o) d_na[[o]]), names(orig_of))
    out <- d2
  }

  # ---- report ---------------------------------------------------------
  missing_cc <- .CIS_CONCEPTS[is.na(chosen)]
  if (verbose) {
    cat("\n=== study ", study, "  (", basename(file), ", ", n,
        " rows) ===\n", sep = "")
    cat("  fieldwork year: ", ifelse(is.na(year), "NOT FOUND", year),
        ifelse(is.na(year), "  -> 'generacion' left missing", ""), "\n", sep = "")
    for (cc in .CIS_CONCEPTS) {
      v <- chosen[cc]
      if (is.na(v)) {
        cat(sprintf("  %-12s --  NOT IN THE FILE -> empty column\n", cc))
      } else {
        cov <- 100 * mean(!is.na(out[[cc]]))
        cat(sprintf("  %-12s %-14s %5.1f%% valid   [%s] %s\n", cc, v, cov,
                    origin[cc], substr(.cis_label(d[[v]]), 1, 44)))
      }
    }
    if (!is.na(practice))
      cat("  (religious practice: ", practice, ")\n", sep = "")
    if (length(missing_cc))
      cat("  WARNING: no data for ", paste(missing_cc, collapse = ", "),
          ". The columns are created empty so that the file can still",
          " be stacked.\n", sep = "")
  }

  detected <- data.frame(
    concept  = .CIS_CONCEPTS,
    variable = ifelse(is.na(chosen), "", chosen),
    label    = vapply(.CIS_CONCEPTS, function(cc)
                 if (is.na(chosen[cc])) "" else .cis_label(d[[chosen[cc]]]),
                 character(1)),
    source   = origin,
    row.names = NULL, stringsAsFactors = FALSE)

  # ---- write ----------------------------------------------------------
  # 'dest' is taken as the path without extension: 'format' sets it, which
  # is the only way to ask for both files at once.
  if (write) {
    root <- if (is.null(dest))
              sub("[.](sav|dta)$", "_soc", file, ignore.case = TRUE)
            else
              sub("[.](sav|dta)$", "", dest, ignore.case = TRUE)
    w <- out
    if (writeAs == "as_R" && output == "full") {
      w <- .cis_for_saving(out, centered && likert == "numeric")
      ren <- attr(w, "renamed")
      attr(w, "renamed") <- NULL
      if (verbose && length(ren))
        cat("  note: in the file, ", paste(ren, collapse = ", "),
            " written with _ORIG so as not to clash in SPSS with its",
            " converted version.\n", sep = "")
    }
    w <- .cis_drop_decimal_labels(w)
    zap <- attr(w, "unlabelled")
    attr(w, "unlabelled") <- NULL
    if (verbose && length(zap))
      cat("  note: in the file, ", paste(zap, collapse = ", "),
          " written without value labels: decimal values, each one",
          " carrying its own label.\n", sep = "")
    for (e in .CIS_EXT[[format]]) {
      f_out <- paste0(root, ".", e)
      if (e == "sav") write_sav(w, f_out) else .cis_write_dta(w, f_out)
      if (verbose) cat("  -> ", f_out, "  (", ncol(w), " variables)\n", sep = "")
    }
  }

  # Factors and scales, only in what is returned: the files have already
  # been written labelled, with the codes of the common schema.
  if (output == "full") {
    out <- .cis_prepare_full(out, likert, scale, centered, factors, NAs,
                             raw, reasons)
    ord_exp <- attr(out, "ordered_explicit")
    attr(out, "ordered_explicit") <- NULL
    attr(out, "converted") <- NULL
  } else if (factors) {
    out <- .cis_to_factors(out, NAs, reasons)
    ord_exp <- .CIS_ORDINAL[vapply(.CIS_ORDINAL, .cis_na_explicit, logical(1),
                                   ordered = TRUE, NAs = NAs)]
  } else {
    ord_exp <- character(0)
  }

  if (is.character(NAs) && !identical(NAs, "auto")) {
    none <- setdiff(NAs, names(out))
    if (length(none))
      warning("'NAs' names variables that are not in what is returned: ",
              paste(none, collapse = ", "), call. = FALSE)
  }
  if (verbose && length(ord_exp))
    cat("  note: ", length(ord_exp), " ordered variable(s) carry the",
        " missing values as levels at the end, outside the order: ",
        paste(utils::head(ord_exp, 8), collapse = ", "),
        if (length(ord_exp) > 8) ", ...", ".\n", sep = "")
  # At the end, with the files already written and 'NAs' already checked
  # against the names
  if (labels) out <- .cis_label_names(out)
  attr(out, "detected") <- detected
  invisible(out)
}
