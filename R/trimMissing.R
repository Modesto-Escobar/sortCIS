# ---------------------------------------------------------------------
# Threshold for the explicit missing levels.
#
# An "N.C." with 0.4% of the cases is noise: it clutters the tables and
# in a model it is an almost empty category. An "N.S." of 13% in ideology
# is a finding. The threshold sends to NA the missing levels that do not
# exceed it and leaves the rest.
#
# Each level is looked at separately (a small N.C. is not saved because
# the N.S. is large) and over the cases with some value: the gaps of a
# filter do not count, or a filtered question would seem to have very
# few missing values. Unweighted.
# ---------------------------------------------------------------------

# Whether a column is one of the nine harmonized variables, by its name
# or, with cisConvert(labels = TRUE), by its label. The label is not
# mistaken for that of an original: if they match, the original carries
# its name in parentheses after it.
.cis_is_harmonized <- function(name)
  name %in% c(names(.CIS_SCHEMA), vapply(.CIS_SCHEMA, `[[`, character(1), "lab"))

# Which levels of a factor are missing. cisConvert() records them in the
# "missingLevels" attribute; if it got lost on the way (rbind, subset...),
# they are recognized by the label, as in the rest of cisConvert(). In the
# harmonized variables they are always "N.S." and "N.C.": their "N.P." is
# a category of the schema (profesional), not a missing value.
.cis_missing_levels_of <- function(f, name) {
  p <- attr(f, "missingLevels")
  if (.cis_is_harmonized(name)) return(intersect(levels(f), .CIS_MISSING_LEVELS))
  if (!is.null(p)) return(intersect(levels(f), p))
  et <- .cis_norm(levels(f))
  levels(f)[grepl(.CIS_MISSING_Q, et) & !grepl(.CIS_NOT_APPLICABLE, et)]
}

# Sends to NA the missing levels that do not exceed the threshold. In the
# harmonized variables the level stays even if it is emptied, so that they
# stack; in the others it is dropped.
.cis_trim <- function(f, name, threshold) {
  p <- .cis_missing_levels_of(f, name)
  n <- sum(!is.na(f))
  if (!length(p) || n == 0) return(f)
  share <- vapply(p, function(l) sum(f == l, na.rm = TRUE) / n, numeric(1))
  out <- p[share <= threshold]
  if (!length(out)) return(f)
  atr <- attributes(f)
  f[f %in% out] <- NA
  if (!.cis_is_harmonized(name)) {
    f <- droplevels(f, except = which(!levels(f) %in% out))
    atr$levels <- levels(f)
  }
  if (!is.null(atr$missingLevels)) atr$missingLevels <- setdiff(atr$missingLevels, out)
  attributes(f) <- atr
  f
}

#' Send the rare explicit missing levels to `NA`
#'
#' @description
#' In the factors that carry the missing values as explicit levels ("N.S.",
#' "N.C.", "No conoce"...; see the `NAs` argument of [cisConvert()]), sends
#' to `NA` the missing levels whose share does not exceed `threshold`, and
#' leaves those that do as they are. An "N.C." of 0.4% is noise; an "N.S."
#' of 13% in ideology is a finding.
#'
#' It works the same for one study as for several stacked ones. For
#' several, better apply it **after** stacking: applied to each study
#' separately, the "N.S." of a variable could be a category in one and `NA`
#' in another, and stacking them would mix two definitions. On the stacked
#' data the threshold is the same for all.
#'
#' @param d An output of [cisConvert()], or several stacked.
#' @param threshold Proportion, between 0 and 1. The missing levels that do
#'   not exceed it go to `NA`. By default, 0.10.
#' @param variables Names of the variables it is applied to. By default,
#'   all the factors of `d`.
#'
#' @details
#' Each missing level is looked at separately and over the cases that have
#' some value, unweighted: the `NA`s (the gaps of a filter) do not count in
#' the denominator.
#'
#' The missing levels are those that [cisConvert()] recorded in the
#' `"missingLevels"` attribute of the factor or, if it got lost when
#' stacking, those whose label says so. In the nine harmonized variables
#' they are always `"N.S."` and `"N.C."`, and the level is kept even if it
#' is emptied, so that the factors still match; the `"N.P."` of
#' `profesional` is a category and is not touched. In the others, the
#' emptied level is dropped.
#'
#' @return `d`, with the factors trimmed.
#'
#' @examples
#' \dontrun{
#' # One study
#' d <- trimMissing(cisConvert("datos/sav/3557.sav", write = FALSE))
#'
#' # Several: stack them first
#' a <- cisConvert("datos/sav/3528.sav", write = FALSE)
#' b <- cisConvert("datos/sav/3557.sav", write = FALSE)
#' both <- trimMissing(rbind(a, b), threshold = 0.10)
#'
#' # Only on some variables
#' d <- trimMissing(d, variables = c("religion", "laboral"))
#' }
#' @export
trimMissing <- function(d, threshold = 0.10, variables = names(d)) {
  if (!is.data.frame(d)) stop("'d' must be a data.frame.")
  if (!(is.numeric(threshold) && length(threshold) == 1 && !is.na(threshold) &&
        threshold > 0 && threshold < 1))
    stop("'threshold' must be a proportion between 0 and 1 (0.10 for 10%).")
  none <- setdiff(variables, names(d))
  if (length(none))
    warning("Not in 'd': ", paste(none, collapse = ", "), call. = FALSE)
  for (v in intersect(variables, names(d)))
    if (is.factor(d[[v]])) d[[v]] <- .cis_trim(d[[v]], v, threshold)
  d
}
