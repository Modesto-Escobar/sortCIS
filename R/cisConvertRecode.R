# ---------------------------------------------------------------------
# cisConvert(): recoders. Each one takes the raw vector and returns the
# code of the common schema. All of them work on the value LABEL; only
# when the variable has no labels do they fall back on the numeric code.
# The patterns are in Spanish because they match the CIS labels.
# ---------------------------------------------------------------------

# Applies a vector of rules c(pattern = target) to the labels. Returns NA
# for what does not match and for the rules whose target is NA: that is
# how N.C., N.S. and, depending on the variable, "Otros" or "Otra
# situacion" go to missing. What goes to missing is decided by the rules
# of each variable.
.cis_by_label <- function(x, rules, unlabelled = NULL) {
  v <- as.numeric(zap_missing(x))
  et <- .cis_values(x)
  if (is.null(et)) {
    if (is.null(unlabelled)) return(rep(NA_real_, length(v)))
    return(unlabelled(v))
  }
  txt <- et[as.character(v)]
  out <- rep(NA_real_, length(v))
  for (i in seq_along(rules)) {
    pending <- is.na(out) & !is.na(txt)
    if (!any(pending)) break
    hit <- pending & grepl(names(rules)[i], txt)
    out[hit] <- as.numeric(rules[i])
  }
  out
}

.cis_rec_gender <- function(x) {
  .cis_by_label(x, c("^hombre" = 1, "^mujer" = 2),
                unlabelled = function(v) ifelse(v %in% c(1, 2), v, NA_real_))
}

# Age travels in completed years. Missing are the N.C. declared as such
# (by label or by SPSS missing, which zap_missing already removes) and
# what falls outside a plausible range.
#
# 99 is not discarded blindly, only when the variable declares it N.C.
# That is what happens across the whole series, so the practical result
# is the same, with one difference: in 2798, 2815, 2847 and 3156 there are
# six respondents with a 99 labelled "N.C." that FID took as a real age
# and turned into people born in 1910. Here they are missing, which is
# what the label says.
.cis_rec_age_years <- function(x) {
  v <- as.numeric(zap_missing(x))
  et <- .cis_values(x)
  if (!is.null(et)) {
    nc <- as.numeric(names(et)[grepl("^n[.]?c|^n[.]?s", et)])
    v[v %in% nc] <- NA
  } else {
    v[v %in% c(99, 999)] <- NA   # without labels there is no other way to know
  }
  v[!is.na(v) & (v < 16 | v > 120)] <- NA
  v
}

.cis_rec_age_groups <- function(a) {
  cut(a, breaks = c(-Inf, 34, 49, 64, 79, Inf), labels = FALSE)
}

.cis_rec_generation <- function(a, year) {
  if (is.na(year)) return(rep(NA_real_, length(a)))
  born <- year - a
  cut(born, breaks = c(1881, 1897, 1913, 1930, 1949, 1965, 1980, 1996, 2012),
      labels = FALSE)
}

# Value labels that mean "no answer". Used where they have to be told
# apart by the text and not by the number.
.CIS_MISSING <- "^n[.]?s|^n[.]?c|^no sabe|^no contesta|^no procede"

# Length of the scale. The CIS states it in the variable label itself,
# "(1-10)" or "(1-7)", in the four studies checked (1181, 2207, 2798,
# 3557). Failing that, the largest labelled code that is not N.S./N.C. is
# taken; and if there are no labels either, 1-10 is assumed.
.cis_ideology_range <- function(x) {
  L <- .cis_label(x)
  m <- regmatches(L, regexpr("1[[:space:]]*-[[:space:]]*[0-9]+", L))
  if (length(m)) {
    n <- suppressWarnings(as.numeric(sub("^1[[:space:]]*-[[:space:]]*", "", m)))
    if (!is.na(n) && n >= 3 && n <= 11) return(n)
  }
  et <- .cis_values(x)
  if (!is.null(et)) {
    cod <- suppressWarnings(as.numeric(names(et)[!grepl(.CIS_MISSING, et)]))
    cod <- cod[!is.na(cod) & cod >= 1 & cod <= 11]
    if (length(cod)) return(max(cod))
  }
  10
}

# Self-placement scale, of variable length depending on the study: 1-10
# in modern ones, 1-7 in older ones (1181, from 1979, for instance).
#
# Two things have to be looked at, both by the value LABEL, not by the
# number, as in the rest of the function:
#
#   - On the 1-7 scale N.S. is 8 and N.C. is 9. As numbers they fall
#     inside the 1-10 range and cannot be told from an answer: taken as
#     valid they would all go to "Derecha". In 1181 that is 7,445 cases,
#     96% of the category.
#   - The centre of the scale depends on its length. 4 is left on a 1 to
#     10 scale and exactly the centre on a 1 to 7 one.
#
# Solved by rescaling linearly to 1-10 and always applying the same cuts.
# With a 1-10 scale the rescaling is the identity. With a 1-7 one it gives
# 1-3 left, 4 centre and 5-7 right.
.cis_rec_ideology <- function(x) {
  v <- as.numeric(zap_missing(x))
  et <- .cis_values(x)
  if (!is.null(et)) {
    nc <- suppressWarnings(as.numeric(names(et)[grepl(.CIS_MISSING, et)]))
    v[v %in% nc[!is.na(nc)]] <- NA
  }
  n <- .cis_ideology_range(x)
  if (n < 2) return(rep(NA_real_, length(v)))
  v[!is.na(v) & (v < 1 | v > n)] <- NA
  z <- if (n == 10) v else (v - 1) * 9 / (n - 1) + 1
  cut(z, breaks = c(0, 4, 6, 10), labels = FALSE)
}

# Two families of schemes:
#   modern : 1 Practising Catholic / 2 Non-practising Catholic / 3-6 others
#   old    : 1 Catholic / 2 Believer of another religion / 3 Non-believer
#            / 4 Atheist  -> practice is not distinguished, the frequency
#                            of attendance at services has to be looked at.
.cis_rec_religion <- function(x, practice = NULL) {
  et <- .cis_values(x)
  v  <- as.numeric(zap_missing(x))
  out <- rep(NA_real_, length(v))
  if (is.null(et)) return(out)
  txt <- et[as.character(v)]
  modern <- any(grepl("practicante", et))
  if (modern) {
    out[grepl("^catolico.* no practicante", txt)] <- 2
    out[is.na(out) & grepl("^catolico.* practicante", txt)] <- 1
    out[is.na(out) & !is.na(txt) & !grepl(.CIS_MISSING, txt)] <- 3
  } else {
    is_cat <- grepl("^catolico", txt)
    # By default a Catholic counts as practising; becomes non-practising
    # only if they say they almost never attend services.
    out[is_cat] <- 1
    if (!is.null(practice)) {
      pe <- .cis_values(practice)
      pv <- as.numeric(zap_missing(practice))
      if (!is.null(pe)) {
        ptxt <- pe[as.character(pv)]
        out[is_cat & !is.na(ptxt) & grepl("^casi nunca|^nunca", ptxt)] <- 2
      }
    }
    out[!is_cat & !is.na(txt) & !grepl(.CIS_MISSING, txt)] <- 3
  }
  out
}

# 100 No education / 200 Primary and lower secondary /
# 300 Upper secondary (including all vocational training) / 500 University.
# "Otros", N.S. and N.C. go to missing.
# The order of the rules is what resolves the overlaps: "Secundaria 1a
# etapa" before plain "Secundaria"; vocational training before anything
# else, because its labels mention degrees and cycles.
#
# The old wordings go with the modern ones in the same rule: the 1979 CIS
# says "Menos de estudios primarios", "Estudios primarios completos" and
# "Bachiller elemental/superior" where today's says "Educacion primaria"
# and "Bachillerato". Without them 1181 loses 89% of the variable, and not
# at random: precisely the low categories are left out, which there are
# four out of five respondents.
.CIS_RULES_EDUCATION <- c(
  "^n[.]?c|^n[.]?s"                                      = NA,
  "^otros"                                               = NA,
  # "Estudios primarios sin finalizar" has to go BEFORE plain "Estudios
  # primarios", or the latter takes it: they are 476 cases of 2207 that
  # are "Sin estudios", not "Primaria completa".
  "^sin estudios|menos de 5 anos|^no procede|^menos de estudios primarios|^estudios primarios sin" = 100,
  "^educacion primaria|^primaria$|^primaria[^a-z]|^estudios primarios" = 200,
  "1a etapa|^eso|^educacion secundaria|graduado escolar|^bachiller elemental" = 200,
  "f[.]?p|formacion profesional|cualificacion profesional" = 300,
  "^bachillerato|2a etapa|^secundaria|^bachiller superior" = 300,
  "diplomat|diplomad|licenciat|^grado|^estudios de grado|master|doctorado|postgrado|posgrado|arquitect|ingenier|universitario|^superiores" = 500
)

# The CIS asks the level attained (with all the detail of degrees) ONLY
# to those who have some education; those who have none are left without
# an answer. That is why the gaps of the detailed variable are filled
# with the grouped one, which does classify them. Without that filling
# some 125 respondents per study are lost, almost all "Sin estudios".
.cis_rec_education <- function(x, backup = NULL) {
  out <- .cis_by_label(x, .CIS_RULES_EDUCATION)
  if (!is.null(backup)) {
    alt <- .cis_by_label(backup, .CIS_RULES_EDUCATION)
    out[is.na(out)] <- alt[is.na(out)]
  }
  out
}

# "Sus labores" is how the CIS called unpaid domestic work until well into
# the 2000s, and "Esta parado" breaks the "^parado" anchor. Without both,
# 1181 loses 42% of the variable and 2207 22%, and what is lost are
# housewives and the unemployed: it is not noise, it is bias.
.CIS_RULES_EMPLOYMENT <- c(
  "^n[.]?c"                       = NA,
  "^otra situacion"               = NA,
  "^trabaja"                      = 1,
  "^jubilado|^pensionista"        = 2,
  "^parado|^en paro|^esta parado" = 3,
  "^estudiante"                   = 4,
  "domestico|^sus labores"        = 5
)
.cis_rec_employment <- function(x) .cis_by_label(x, .CIS_RULES_EMPLOYMENT)

# Beware: in FID 3 (self-employed without employees) is merged into 2 and
# 4/5 (family help, cooperative) into 6. The FID labels do not show it,
# the data do.
.CIS_RULES_OCCUPATION <- c(
  "^n[.]?c"                          = NA,
  "^no procede|^n[.]?p"              = 0,
  "^asalariado"                      = 1,
  "^empresario|autonomo"             = 2,
  "^ayuda familiar|cooperativa|^otra situacion" = 6
)

# 'profesional' is the CURRENT occupational status: whoever does not
# work, 0 N.P. Needed because the CIS changed the question:
#   - 2207, 2798: asked to everybody, about the current or last job.
#     Pensioners, the unemployed and housewives have a value.
#   - 3557 onwards: only to those who work. The rest have the 0 "No
#     procede", also declared SPSS missing, so the normal read leaves it
#     NA and the N.P. was lost.
# With the 'laboral' filter both forms say the same. In recent studies it
# matches case by case the 0 of the file (N.P. if and only if not working,
# without exceptions in 3557, 3567 and 3577); in older ones the last job
# of those not employed is lost, which remains in the raw variable
# (output = "audit").
#
# Any answer of 'laboral' that is neither "Trabaja" nor missing goes to
# N.P.; with 'laboral' missing, the question itself rules.
.cis_rec_occupation <- function(x, employment = NULL) {
  out <- .cis_by_label(x, .CIS_RULES_OCCUPATION)
  et <- if (is.null(employment)) NULL else .cis_values(employment)
  if (is.null(et)) return(out)
  txt <- et[as.character(as.numeric(zap_missing(employment)))]
  not_working <- !is.na(txt) & !grepl("^trabaja", txt) & !grepl(.CIS_MISSING, txt)
  out[not_working] <- 0
  out
}

.CIS_RULES_CLASS <- c(
  "^n[.]?c"                                   = NA,
  "^clase alta|media-alta"                    = 1,
  "media-media"                               = 2,
  "trabajadora|obrera"                        = 3,
  "media-baja|^clase baja"                    = 4,
  "^clase pobre|infraclase|proletariado|abajo|excluido|gente comun|^otras|no cree en las clases|^no sabe" = 9
)
.cis_rec_class <- function(x) .cis_by_label(x, .CIS_RULES_CLASS)
