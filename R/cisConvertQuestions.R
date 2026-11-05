# ---------------------------------------------------------------------
# cisConvert(): scale and Likert questions of the rest of the file.
#
# They only count with output = "full", and only in what is returned in
# R: the .sav and .dta files are written before, labelled and untouched
# (unless writeAs = "as_R").
#
# As with the sociodemographic variables, the type is recognized by the
# SHAPE of the value labels, not by the variable name. Tested on the
# originals of 1181, 2207, 2798, 3557, 3567 and 3577. The patterns are in
# Spanish because they match the CIS labels.
# ---------------------------------------------------------------------

# The answers that are not answers. Wider than .CIS_MISSING: on the
# scales there are also "No conoce" (the leader), "No consta" and "No
# recuerda".
.CIS_MISSING_Q <- paste0(
  "^n[.]?s|^n[.]?c|^no sabe|^no contesta|^no procede|^no_procede|",
  "^no conoce|^no consta|^no recuerda|^n[.]?p[.]?$")

# The "No procede" of a filter is not a missing answer but a question that
# was not asked, the same as a blank: it always goes to NA, never to an
# explicit level. ("No procede (filtro)" gets here without the
# parentheses.)
.CIS_NOT_APPLICABLE <- "^no procede|^no_procede|^n[.]?p[.]?$"

# Words with which the categories of an intensity scale start.
.CIS_INTENSITY <- paste0(
  "^(muy|mucho|mucha|bastante|algo|poco|poca|nada|ninguna|ninguno|regular|",
  "ni |totalmente|completamente|siempre|casi|a menudo|algunas veces|a veces|",
  "raramente|nunca|mejor|igual|peor|buena|bueno|mala|malo|de acuerdo|",
  "en desacuerdo)")

# The low pole (bad, disagree, nothing). Tells which way the scale goes:
# the CIS almost always puts the high pole first ("Muy buena" ... "Muy
# mala"), but it is checked instead of taken for granted.
.CIS_LOW_POLE <- paste0(
  "mala|malo|^muy mal$|desacuerdo|^nada|^ninguna|^ninguno|^nunca|^casi nunca|",
  "^peor|^raramente|pesimist|^poco|^poca")

# The paradata variables (IA_) are counts of incidents, and some have the
# shape of a 0-10 scale. They are not questions.
.CIS_NOT_QUESTION <- "^IA_"

# Substantive value labels (without the missing ones), by code order.
.cis_substantive <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(setNames(character(0), character(0)))
  et <- .cis_norm(names(lv))
  cod <- as.numeric(lv)
  ok <- !grepl(.CIS_MISSING_Q, et)
  s <- setNames(et[ok], cod[ok])
  s[order(cod[ok])]
}

.cis_missing_codes <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(numeric(0))
  as.numeric(lv)[grepl(.CIS_MISSING_Q, .cis_norm(names(lv)))]
}

# "scale", "likert" or "".
#
# Scale (0-10, 1-10, 1-7). Two shapes, depending on the period:
#   - modern: six or more points with consecutive codes between 0 and 10,
#     and the intermediate ones labelled only with their number (2 =
#     "2"); the extremes may carry text ("1 Izquierda", "10 Muy bien").
#   - old: at most two substantive labels, and all the observed values
#     between 0 and 10, six or more different ones.
# Likert: from 3 to 7 categories with codes 1..k, and the first, the last
# and at least three out of four start with an intensity word.
.cis_question_type <- function(x, name = "") {
  if (!inherits(x, "haven_labelled") || grepl(.CIS_NOT_QUESTION, name)) return("")
  s <- .cis_substantive(x)
  cod <- as.numeric(names(s))
  v <- as.numeric(zap_missing(x))
  obs <- setdiff(unique(v[!is.na(v)]), .cis_missing_codes(x))

  if (length(cod) >= 6 && all(cod %in% 0:10) && all(diff(cod) == 1)) {
    mid <- s[-c(1, length(s))]
    if (all(mid == names(mid))) return("scale")
  }
  if (length(cod) <= 2 && length(obs) >= 6 && all(obs %in% 0:10)) return("scale")

  if (length(cod) >= 3 && length(cod) <= 7 && all(cod == seq_along(cod)) &&
      grepl(.CIS_INTENSITY, s[1]) && grepl(.CIS_INTENSITY, s[length(s)]) &&
      mean(grepl(.CIS_INTENSITY, s)) >= 0.75) return("likert")
  ""
}

# Scale to number: the value as it is (not rescaled: a 1-7 stays 1-7),
# with the missing values and what falls outside 0-10 to NA.
.cis_scale_numeric <- function(x) {
  v <- as.numeric(zap_missing(x))
  v[v %in% .cis_missing_codes(x) | v < 0 | v > 10] <- NA
  v
}

.cis_scale_factor <- function(x) {
  v <- .cis_scale_numeric(x)
  lv <- attr(x, "labels")
  points <- sort(unique(c(v[!is.na(v)],
                          as.numeric(names(.cis_substantive(x))))))
  nm <- as.character(points)
  if (!is.null(lv)) {
    m <- match(points, as.numeric(lv))
    nm[!is.na(m)] <- names(lv)[m[!is.na(m)]]
  }
  factor(v, levels = points, labels = nm, ordered = TRUE)
}

# Likert: 1 the low pole, k the high one. If the extremes do not say which
# way it goes, the CIS custom is assumed: the high pole first.
.cis_likert_reverse <- function(s) {
  low_first <- grepl(.CIS_LOW_POLE, s[1])
  low_last  <- grepl(.CIS_LOW_POLE, s[length(s)])
  !(low_first && !low_last)
}

.cis_likert_numeric <- function(x, centered = FALSE) {
  s <- .cis_substantive(x)
  k <- length(s)
  v <- as.numeric(zap_missing(x))
  v[!v %in% seq_len(k)] <- NA
  if (.cis_likert_reverse(s)) v <- k + 1 - v
  if (centered) {
    # With odd k the midpoint is 0; with even k there is no midpoint and 0
    # is skipped: 4 categories give -2, -1, 1, 2.
    z <- v - (k + 1) / 2
    v <- ifelse(z < 0, floor(z), ceiling(z))
  }
  v
}

.cis_likert_factor <- function(x) {
  s <- .cis_substantive(x)
  k <- length(s)
  lv <- attr(x, "labels")
  nm <- names(lv)[match(seq_len(k), as.numeric(lv))]
  v <- as.numeric(zap_missing(x))
  v[!v %in% seq_len(k)] <- NA
  ord <- if (.cis_likert_reverse(s)) rev(seq_len(k)) else seq_len(k)
  factor(v, levels = ord, labels = nm[ord], ordered = TRUE)
}

# Missing codes of a variable, with their label and by code order: those
# that say so in the label ("N.S.", "No procede"...) and those the file
# declares SPSS missing. 'raw' is the variable read with user_na = TRUE;
# in the normal read the latter are already NA.
.cis_labelled_missing <- function(raw) {
  lv <- attr(raw, "labels")
  if (is.null(lv)) return(setNames(numeric(0), character(0)))
  cod <- as.numeric(lv)
  spss <- cod %in% attr(raw, "na_values")
  rng <- attr(raw, "na_range")
  if (length(rng) == 2) spss <- spss | (cod >= rng[1] & cod <= rng[2])
  p <- setNames(cod, names(lv))[cod %in% .cis_missing_codes(raw) | spss]
  p[order(p)]
}

# Takes the missing values of a factor to explicit levels at the end, one
# for each missing label of the original ("N.S.", "N.C.", "No
# conoce"...), instead of lumping them together. The filter, with a code
# ("No procede") or without one (a blank), stays NA. If the factor is
# ordered it stays so, with the missing levels after the high pole.
.cis_explicit <- function(f, raw) {
  p <- .cis_labelled_missing(raw)
  p <- p[!grepl(.CIS_NOT_APPLICABLE, .cis_norm(names(p)))]
  if (!length(p)) return(f)
  new <- make.unique(c(levels(f), names(p)))[-seq_along(levels(f))]
  v <- as.numeric(unclass(raw))
  txt <- as.character(f)
  gap <- is.na(txt) & v %in% p
  txt[gap] <- new[match(v[gap], p)]
  g <- factor(txt, levels = c(levels(f), new), ordered = is.ordered(f))
  attr(g, "label") <- attr(f, "label", exact = TRUE)
  attr(g, "missingLevels") <- new
  g
}

# The rest of labelled variables. To factor if all their values have a
# label; if not, it is a quantity with some labelled value ("10 o mas",
# the weight) and it becomes a number, with the missing values to NA. In
# the factor, the missing values go to NA or to their explicit levels
# (see .cis_explicit).
.cis_other_to_factor <- function(x, explicit = FALSE, raw = x) {
  lv <- attr(x, "labels")
  v <- as.numeric(zap_missing(x))
  v[v %in% .cis_missing_codes(x)] <- NA
  if (is.null(lv) || !all(v[!is.na(v)] %in% as.numeric(lv))) {
    attr(v, "label") <- attr(x, "label", exact = TRUE)
    return(v)
  }
  ok <- !as.numeric(lv) %in% .cis_labelled_missing(raw)
  lv <- lv[ok][order(as.numeric(lv[ok]))]
  f <- factor(v, levels = as.numeric(lv), labels = make.unique(names(lv)))
  attr(f, "label") <- attr(x, "label", exact = TRUE)
  if (explicit) f <- .cis_explicit(f, raw)
  f
}

# Builds what is returned with output = "full": keys, the nine
# sociodemographic variables, the converted scales and Likert items (new
# columns, in lower case) and then the original file.
#
# 'raw' are the original variables read with user_na = TRUE, with the
# same names as in 'out'; they are only needed for the explicit missing
# levels. Returns, in the "ordered_explicit" attribute, the ordered
# variables left with the missing levels at the end, to give a note, and
# in "converted", the original of each converted variable (name =
# converted).
.cis_prepare_full <- function(out, likert, scale, centered, factors, NAs,
                              raw = list(), reasons = list()) {
  keys   <- intersect(c("orden", "numentr"), names(out))
  socio  <- names(.CIS_SCHEMA)
  orig   <- setdiff(names(out), c(keys, socio))
  raw_of <- function(v) if (is.null(raw[[v]])) out[[v]] else raw[[v]]
  notify <- character(0)

  type <- vapply(orig, function(v) .cis_question_type(out[[v]], v), character(1))
  added <- list()
  from <- character(0)
  for (v in orig[type != ""]) {
    x <- out[[v]]
    nm <- tolower(v)
    while (nm %in% c(names(out), names(added))) nm <- paste0(nm, "_r")
    nv <- if (type[v] == "scale") {
      if (scale == "numeric") .cis_scale_numeric(x) else .cis_scale_factor(x)
    } else {
      if (likert == "numeric") .cis_likert_numeric(x, centered) else .cis_likert_factor(x)
    }
    # A number cannot hold an "N.S.": the numeric ones, always NA
    if (is.factor(nv) && .cis_na_explicit(nm, TRUE, NAs)) {
      nv <- .cis_explicit(nv, raw_of(v))
      notify <- c(notify, nm)
    }
    attr(nv, "label") <- attr(x, "label", exact = TRUE)
    added[[nm]] <- nv
    from[nm] <- v
  }

  if (factors) {
    out <- .cis_to_factors(out, NAs, reasons)
    notify <- c(intersect(.CIS_ORDINAL, socio[vapply(socio, function(cc)
      .cis_na_explicit(cc, TRUE, NAs), logical(1))]), notify)
    for (v in orig) if (inherits(out[[v]], "haven_labelled"))
      out[[v]] <- .cis_other_to_factor(out[[v]], .cis_na_explicit(v, FALSE, NAs),
                                       raw_of(v))
  }

  for (nm in names(added)) out[[nm]] <- added[[nm]]
  out <- out[c(keys, socio, names(added), orig)]
  attr(out, "ordered_explicit") <- notify
  attr(out, "converted") <- from
  out
}

# What is written with writeAs = "as_R": the columns of output = "full",
# with the harmonized variables first (see below), but without factors (a
# written factor loses the codes: the 100..500 of estudios would come out
# 1..4) and with the converted variables always numeric and WITHOUT value
# labels, only the variable label: with value labels SPSS (and whatever
# reads the .sav, such as CARING) takes them as nominal. They are already
# oriented (the high value is "mucho", "muy bien", "de acuerdo"), so the
# code reads by itself. SPSS is not case sensitive, so the original that
# clashes with its converted version (ESCIDEOL with escideol) is written
# with _ORIG, like those that clash with the harmonized ones. The renamed
# ones are kept in the "renamed" attribute, to give a note.
.cis_for_saving <- function(out, centered) {
  g <- .cis_prepare_full(out, "numeric", "numeric", centered,
                         factors = FALSE, NAs = TRUE)
  from <- attr(g, "converted")
  attr(g, "ordered_explicit") <- NULL
  attr(g, "converted") <- NULL

  conv <- tolower(names(from))
  clashes <- names(g)[!names(g) %in% names(from) & tolower(names(g)) %in% conv]
  for (v in clashes) {
    nn <- paste0(v, "_ORIG")
    while (tolower(nn) %in% tolower(names(g))) nn <- paste0(nn, "1")
    names(g)[names(g) == v] <- nn
  }
  # In the file the harmonized variables go first. The header of the
  # original (study, record, territory, sex, age, nationality...:
  # everything before the first question, P1) goes to the end, with the
  # keys. Without P<n> questions the header is not recognized and stays.
  keys <- intersect(c("orden", "numentr"), names(g))
  orig <- setdiff(names(g), c(keys, names(.CIS_SCHEMA), names(from)))
  p1   <- match(TRUE, grepl("^P[0-9]", orig, ignore.case = TRUE))
  head <- if (is.na(p1)) character(0) else orig[seq_len(p1 - 1)]
  g <- g[c(names(.CIS_SCHEMA), names(from), setdiff(orig, head), head, keys)]
  attr(g, "renamed") <- clashes
  g
}
