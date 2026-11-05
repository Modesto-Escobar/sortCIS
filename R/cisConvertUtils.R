# ---------------------------------------------------------------------
# cisConvert(): text utilities, reading and writing.
#
# The CIS labels vary in accents, case, gender slashes and remarks in
# parentheses. They are always compared normalized.
# ---------------------------------------------------------------------
.cis_norm <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
              "aeiouunAEIOUUN", x)
  x <- tolower(x)
  # Remove the remarks in parentheses, but only them: 3577 says "Escala
  # (1-10) de autoubicacion ideologica", with the parentheses in the
  # middle, and cutting from the first "(" to the end left "escala". An
  # unclosed parenthesis does take the rest.
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- sub("\\(.*$", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.cis_label <- function(x) {
  L <- attr(x, "label", exact = TRUE)
  if (is.null(L) || length(L) != 1 || is.na(L)) return("")
  as.character(L)
}

# Names each column after its label (cisConvert(labels = TRUE)). Without
# a label, the name stays. If two match, the first keeps the label and
# the others get their name in parentheses: the harmonized variables come
# first, so the SEXO of the original, labelled like 'genero', is the one
# that gets the "(SEXO)". The previous names are kept in the "varnames"
# attribute, with the labels as names.
.cis_label_names <- function(d) {
  old <- names(d)
  new <- vapply(d, function(x) trimws(.cis_label(x)), character(1),
                USE.NAMES = FALSE)
  new[new == ""] <- old[new == ""]
  dup <- duplicated(new)
  new[dup] <- paste0(new[dup], " (", old[dup], ")")
  new <- make.unique(new)
  names(d) <- new
  attr(d, "varnames") <- setNames(old, new)
  d
}

# Value labels of a variable, already normalized and in the order of the
# codes.
.cis_values <- function(x) {
  lv <- attr(x, "labels")
  if (is.null(lv)) return(NULL)
  setNames(.cis_norm(names(lv)), as.character(as.numeric(lv)))
}

# Reads a .sav without the "Invalid date string" message of the reading
# library, which comes from the C code (it is not an R warning, so
# suppressWarnings does not silence it). It is caused by the creation
# date of the header (9 bytes from byte 92, "dd mmm yy") when it is
# malformed, as in 3577 ("22 028 26"). In that case a temporary copy with
# the date replaced is read; the data and the labels do not change.
.cis_read_sav <- function(file, ...) {
  hdr <- tryCatch(readBin(file, "raw", 101L), error = function(e) raw(0))
  if (length(hdr) == 101L && rawToChar(hdr[1:4]) == "$FL2") {
    date <- rawToChar(hdr[93:101])
    if (!grepl("^[0-9]{2} [A-Za-z]{3} [0-9]{2}$", date)) {
      tmp <- tempfile(fileext = ".sav")
      on.exit(unlink(tmp), add = TRUE)
      if (file.copy(file, tmp)) {
        con <- file(tmp, "r+b")
        seek(con, 92L, rw = "write")
        writeBin(charToRaw("01 Jan 70"), con)
        close(con)
        file <- tmp
      }
    }
  }
  read_sav(file, ...)
}

# ---------------------------------------------------------------------
# Output formats. 'both' writes the two files, with the same root and a
# different extension, in a single pass.
# ---------------------------------------------------------------------
.CIS_EXT <- list(sav = "sav", dta = "dta", both = c("sav", "dta"))

# Stata is stricter than SPSS with variable names: it only accepts
# letters, digits and underscores, they cannot start with a digit and
# cannot be longer than 32 characters. With output = "short" or "audit"
# the names are set by cisConvert() and are always valid; with "full"
# they come from the source file and may not be. Better to say so clearly
# than to let haven's error out, which does not say which variable fails.
.CIS_STATA_NAME <- "^[A-Za-z_][A-Za-z0-9_]{0,31}$"

.cis_write_dta <- function(d, path) {
  bad <- names(d)[!grepl(.CIS_STATA_NAME, names(d))]
  if (length(bad))
    stop("Stata does not accept these variable names: ",
         paste(bad, collapse = ", "),
         ". Write a .sav, or use output = 'short', or rename them first.")
  write_dta(d, path)
}

# The CIS sometimes labels variables with decimals: in 3577, each value of
# PESO and PESOCCAA carries the value itself written as its label
# ("2,87887"). Stata does not accept non-integer value labels, and in
# SPSS (and whatever reads the .sav) they make a weight look like a
# nominal variable with thousands of categories. Their value labels,
# which say nothing the number does not, are removed and the variable
# label is kept. The names of those affected are kept in the "unlabelled"
# attribute.
.cis_drop_decimal_labels <- function(d) {
  decimal <- vapply(d, function(x) {
    l <- attr(x, "labels", exact = TRUE)
    is.numeric(l) && any(l != round(l), na.rm = TRUE)
  }, logical(1))
  for (v in names(d)[decimal]) {
    lab <- attr(d[[v]], "label", exact = TRUE)
    d[[v]] <- as.vector(unclass(d[[v]]))
    attr(d[[v]], "label") <- lab
  }
  attr(d, "unlabelled") <- names(d)[decimal]
  d
}
