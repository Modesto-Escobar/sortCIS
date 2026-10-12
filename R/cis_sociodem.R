#' Armoniza las variables sociodemográficas de un estudio del CIS
#'
#' @description
#' Toma un fichero de datos del CIS (`.sav` de SPSS o `.dta` de Stata) y
#' escribe otro `.sav` con las variables sociodemográficas recodificadas al
#' esquema común de `FID3978abrev.dta`, de modo que dos ficheros convertidos
#' con esta función se puedan apilar directamente.
#'
#' La variable de cada concepto se detecta por su **etiqueta**, y sus
#' categorías se recodifican por la **etiqueta del valor**, nunca por el
#' nombre ni por el código numérico: el CIS no mantiene estables ni unos ni
#' otros entre estudios. Véase [sortCIS-package] para el porqué y para la
#' procedencia de los mapas de recodificación.
#'
#' @param fichero Ruta del `.sav` (o `.dta`) de entrada.
#' @param destino Ruta del fichero de salida, **sin la extensión**: la pone
#'   `formato`. Si se da con extensión, se le quita. Por defecto, el mismo
#'   nombre del fichero de entrada con el sufijo `_soc`.
#' @param formato Formato del fichero de salida: `"sav"` de SPSS (por
#'   defecto), `"dta"` de Stata, o `"ambos"`, que escribe los dos con la
#'   misma raíz. Si no se da `formato` pero `destino` acaba en `.dta`, se
#'   entiende `"dta"`.
#'
#'   Stata sólo admite nombres de variable de letras, dígitos y guión bajo,
#'   que no empiecen por dígito y no pasen de 32 caracteres. Los que pone
#'   esta función los cumplen siempre; con `salida = "completo"` los nombres
#'   vienen del fichero de origen y, si alguno no vale, se avisa nombrándolo
#'   en vez de escribir un fichero a medias.
#' @param salida Qué columnas lleva el fichero de salida:
#'   \describe{
#'     \item{`"breve"`}{Claves más las nueve variables recodificadas. Siempre
#'       las mismas columnas en el mismo orden: es la que garantiza que dos
#'       ficheros se apilen. Es el valor por defecto.}
#'     \item{`"auditoria"`}{Lo anterior más las variables crudas de las que
#'       sale cada una, con prefijo `o_` (`o_sexo`, `o_escideol`...), para
#'       poder comprobar la recodificación. Las `o_` cambian de un estudio a
#'       otro; las doce primeras columnas no.}
#'     \item{`"completo"`}{El fichero de entrada (sin las `IA_`, salvo con
#'       `IAs = TRUE`) más las nueve nuevas
#'       al final, junto con `orden` y `numentr`. No se añaden `estudio` ni
#'       `cues`, porque el fichero ya trae sus propios identificadores. Si
#'       algún nombre del fichero original choca con uno armonizado, el
#'       original se conserva con el sufijo `_ORIG`: SPSS no distingue
#'       mayúsculas, así que `ESTUDIOS` chocaría con `estudios`.
#'       Lo que se devuelve en R va en otro orden: `orden` y `numentr`, las
#'       nueve armonizadas, las escalas y Likert convertidas (véase la
#'       sección «Escalas y Likert») y después el fichero original. Con
#'       `factores = TRUE` las variables etiquetadas del original pasan a
#'       factor si todos sus valores tienen etiqueta, y a número si no.}
#'   }
#' @param estudio Número de estudio. Por defecto se busca en las variables
#'   `ESTU`/`ESTUDIO` y, si no están, en el nombre del fichero.
#' @param anyo Año del trabajo de campo, necesario para `generacion`. Solo
#'   hace falta para un estudio que no sea de la serie: por defecto se busca,
#'   en este orden, en la variable `E103` del propio fichero, en
#'   `datos/estudios_fecha.csv` si está, y en la tabla interna que trae los
#'   36 estudios de la serie. Si no aparece en ninguno se avisa y
#'   `generacion` queda a perdido; las otras ocho variables no se resienten.
#' @param tabla Ruta del TSV de correspondencias revisable. Por defecto,
#'   `datos/sociodem_correspondencias.tsv` bajo la raíz del proyecto.
#' @param escribir Si es `FALSE`, devuelve el `data.frame` sin escribir el
#'   fichero de salida.
#' @param avisos Si es `FALSE`, silencia el informe por pantalla.
#' @param factores Si es `TRUE` (por defecto), las nueve variables
#'   armonizadas del `data.frame` que se devuelve son factores, con los
#'   niveles en el orden de los códigos: ordenados (`ordered`) `generacion`,
#'   `edad`, `ideologia`, `estudios` y `clase`, y sin orden `genero`,
#'   `religion`, `laboral` y `profesional`. Llevan siempre todos los niveles
#'   del esquema, aunque el estudio no tenga casos en alguno, para que los de
#'   dos estudios coincidan al apilarlos. Si es `FALSE`, se devuelven
#'   etiquetadas (`haven_labelled`), con los códigos del esquema común.
#'
#'   Sólo afecta a lo que se devuelve: los ficheros `.sav` y `.dta` se
#'   escriben siempre etiquetados y con los códigos del esquema común.
#'
#'   Ojo: `as.numeric()` sobre un factor da la posición del nivel, no el
#'   código. En `estudios` (100-500), `profesional` (0, 1, 2, 6) y `clase`
#'   (9 Otras) no coinciden; para los códigos, `factores = FALSE`.
#' @param NAs Sólo cuenta con `factores = TRUE`. Si es `TRUE` (por defecto),
#'   los perdidos quedan como `NA`. Si es `FALSE`, pasan a un nivel `"N.C."`
#'   explícito, el último.
#' @param likert Sólo cuenta con `salida = "completo"`. Cómo se devuelven las
#'   preguntas de tipo Likert ("Muy buena" ... "Muy mala", "Mucha" ...
#'   "Ninguna confianza", "Muy de acuerdo" ... "Muy en desacuerdo"):
#'   `"numerica"` (por defecto) o `"factor"` ordenado. En las dos, el valor
#'   más alto es el polo positivo o intenso, al revés que en los códigos del
#'   CIS, que ponen primero "Muy buena". Véase la sección «Escalas y Likert».
#' @param escala Sólo cuenta con `salida = "completo"`. Cómo se devuelven las
#'   escalas 0-10, 1-10 o 1-7 (ideología, valoración de líderes,
#'   probabilidad de voto...): `"numerica"` (por defecto), con el valor tal
#'   cual, o `"factor"` ordenado.
#' @param centrada Sólo cuenta con `likert = "numerica"`. Si es `TRUE`, el
#'   punto medio de la escala vale 0, el polo negativo es negativo y el
#'   positivo, positivo: con cinco categorías, -2, -1, 0, 1, 2; con cuatro,
#'   que no tienen punto medio, -2, -1, 1, 2. Por defecto, `FALSE`: de 1 a k.
#' @param IAs Sólo cuenta con `salida = "completo"`. Si es `FALSE` (por
#'   defecto), se quitan las variables de paradatos del CIS, las que empiezan
#'   por `IA_` (incidencias de contacto, supervisión de la entrevista...),
#'   tanto del fichero que se escribe como de lo que se devuelve. Con `TRUE`
#'   se conservan.
#'
#' @return Invisiblemente, el `data.frame` de salida. El efecto que interesa
#'   es el fichero escrito en `destino` (salvo con `escribir = FALSE`) y la
#'   actualización de la tabla de correspondencias.
#'
#' @section Variables de salida:
#' Con `salida = "breve"` el fichero lleva siempre estas trece columnas, en
#' este orden:
#'
#' \describe{
#'   \item{`estudio`}{Número de estudio del CIS.}
#'   \item{`cues`}{Número de cuestionario, tal cual viene en el fichero.}
#'   \item{`orden`}{Posición en el fichero de origen; es la clave que casa
#'     con `FID3978abrev`.}
#'   \item{`numentr`}{Identificador único de entrevistado,
#'     `estudio * 100000 + orden`.}
#'   \item{`genero`}{1 Hombre, 2 Mujer.}
#'   \item{`generacion`}{1 G. 98, 2 G. 14, 3 G. 27, 4 Postguerra, 5 Boomers,
#'     6 X, 7 Y, 8 Z. Se obtiene del año de nacimiento (año de campo menos
#'     edad), así que necesita `anyo`.}
#'   \item{`edad`}{Tramos: 1 Joven (hasta 34), 2 Adulto (35-49), 3 Maduro
#'     (50-64), 4 Mayor (65-79), 5 Vejez (80 y más).}
#'   \item{`ideologia`}{1 Izquierda, 2 Centro, 3 Derecha. En la escala 1-10
#'     de los estudios modernos son 1-4, 5-6 y 7-10. La longitud de la
#'     escala se lee de la etiqueta de la variable, porque los estudios
#'     antiguos usan otras: el 1181, de 1979, va de 1 a 7, y ahí el reparto
#'     es 1-3, 4 y 5-7. En esas escalas cortas el «N.S.» y el «N.C.» son el
#'     8 y el 9, que caen dentro del rango 1-10, así que se descartan por la
#'     etiqueta del valor y no por el número.}
#'   \item{`religion`}{1 Practicante, 2 Católico no practicante, 3 Otras.}
#'   \item{`estudios`}{100 Sin estudios, 200 Primaria y secundaria elemental,
#'     300 Secundaria superior (incluida toda la F.P.), 500 Universitarios.}
#'   \item{`laboral`}{1 Trabaja, 2 Pensionista, 3 Parado, 4 Estudiante,
#'     5 Doméstico.}
#'   \item{`profesional`}{0 N.P., 1 Asalariado por cuenta ajena,
#'     2 Empresario o autónomo, 6 Otra situación.}
#'   \item{`clase`}{1 Alta/media-alta, 2 Media, 3 Trabajador, 4 Baja,
#'     9 Otras.}
#' }
#'
#' Los "N.C." y los "N.S." van a perdido, y también "Otros" en `estudios` y
#' "Otra situación" en `laboral`. Hay dos excepciones, que reproducen lo que
#' hace el esquema común: en `profesional`, "Otra situación" (junto con la
#' ayuda familiar y la cooperativa) va al 6, y en `clase`, "Otras", "No sabe"
#' y las respuestas que no nombran una clase van al 9. Un concepto que no exista en el estudio se crea como
#' columna vacía, para que el fichero siga siendo apilable.
#'
#' @section Escalas y Likert:
#' Con `salida = "completo"`, las preguntas de escala y las de tipo Likert
#' del fichero se añaden convertidas, como columnas nuevas con el nombre en
#' minúsculas (`escideol`, `valoralideres_1`...), justo detrás de las nueve
#' armonizadas. Las originales se quedan al final con su nombre en
#' mayúsculas. Los "N.S.", "N.C.", "No procede" y "No conoce" van a `NA`.
#'
#' Se reconocen por la forma de las etiquetas de valor, no por el nombre:
#' \describe{
#'   \item{Escala}{Seis o más puntos seguidos entre 0 y 10 con los
#'     intermedios etiquetados sólo con su número (`2 = "2"`), como en los
#'     estudios recientes; o, como en los antiguos, sin etiquetas para los
#'     puntos y con todos los valores observados entre 0 y 10.}
#'   \item{Likert}{De tres a siete categorías con códigos 1..k cuyas
#'     etiquetas empiezan con palabras de intensidad ("muy", "mucho",
#'     "bastante", "poco", "nada", "ninguna", "regular", "mejor", "peor",
#'     "de acuerdo"...). Hacia dónde va la escala se mira en las etiquetas
#'     de los extremos.}
#' }
#' Las variables de paradatos (`IA_`) no se tocan.
#'
#' @section La tabla de correspondencias:
#' La función deja escrito en el TSV de `tabla` qué variable ha elegido para
#' cada concepto y con qué etiqueta. Las filas con `origen = "auto"` las
#' reescribe en cada pasada; las que se pongan a mano con `origen = "manual"`
#' **mandan** sobre la detección automática y no se tocan. Así, un estudio con
#' una etiqueta atípica se arregla editando una línea del TSV, sin tocar el
#' código, y queda constancia de la decisión.
#'
#' Para declarar que un concepto no existe en un estudio, se pone la variable
#' a `""` con `origen = "manual"`.
#'
#' Ojo: el TSV **no es una entrada** del proceso más que para eso. La función
#' lo escribe ella misma; no hay que llevarlo al lado del fichero de datos.
#'
#' @seealso [sortCIS-package] para la justificación del método.
#'
#' @examples
#' \dontrun{
#' # Lo habitual: escribe datos/sav/3557_soc.sav
#' cis_sociodem("datos/sav/3557.sav")
#'
#' # Comprobar la recodificación sin escribir nada
#' d <- cis_sociodem("datos/sav/3557.sav", salida = "auditoria",
#'                   escribir = FALSE)
#'
#' # Un estudio que no es de la serie y no trae la fecha de campo
#' cis_sociodem("otro.sav", estudio = 9999, anyo = 2021)
#'
#' # En Stata: escribe datos/sav/3557_soc.dta
#' cis_sociodem("datos/sav/3557.sav", formato = "dta")
#'
#' # Los dos a la vez: 3557_soc.sav y 3557_soc.dta
#' cis_sociodem("datos/sav/3557.sav", formato = "ambos")
#'
#' # Equivale a formato = "dta", por la extensión del destino
#' cis_sociodem("datos/sav/3557.sav", destino = "salida/3557.dta")
#'
#' # Apilar dos estudios ya convertidos
#' a <- cis_sociodem("datos/sav/3528.sav", escribir = FALSE)
#' b <- cis_sociodem("datos/sav/3557.sav", escribir = FALSE)
#' todo <- rbind(a, b)
#'
#' # Los perdidos como nivel "N.C." explícito
#' d <- cis_sociodem("datos/sav/3557.sav", escribir = FALSE, NAs = FALSE)
#'
#' # Etiquetadas con los códigos del esquema, como en el fichero
#' d <- cis_sociodem("datos/sav/3557.sav", escribir = FALSE, factores = FALSE)
#' }
#'
#' @export
cis_sociodem <- function(fichero,
                         destino  = NULL,
                         salida   = c("breve", "auditoria", "completo"),
                         estudio  = NULL,
                         anyo     = NULL,
                         tabla    = NULL,
                         escribir = TRUE,
                         avisos   = TRUE,
                         formato  = c("sav", "dta", "ambos"),
                         factores = TRUE,
                         NAs      = TRUE,
                         likert   = c("numerica", "factor"),
                         escala   = c("numerica", "factor"),
                         centrada = FALSE,
                         IAs      = FALSE) {

  salida <- match.arg(salida)
  if (!isTRUE(IAs) && !isFALSE(IAs))
    stop("'IAs' tiene que ser TRUE o FALSE.")
  likert <- match.arg(likert)
  escala <- match.arg(escala)
  if (!isTRUE(centrada) && !isFALSE(centrada))
    stop("'centrada' tiene que ser TRUE o FALSE.")
  if (!isTRUE(factores) && !isFALSE(factores))
    stop("'factores' tiene que ser TRUE o FALSE.")
  if (!isTRUE(NAs) && !isFALSE(NAs))
    stop("'NAs' tiene que ser TRUE o FALSE.")
  # Si no se pide formato pero el destino acaba en .dta, manda el destino:
  # cis_sociodem(f, destino = "x.dta") hace lo que parece que hace.
  auto_formato <- missing(formato)
  formato <- match.arg(formato)
  if (auto_formato && !is.null(destino) &&
      grepl("[.]dta$", destino, ignore.case = TRUE)) formato <- "dta"
  if (!file.exists(fichero)) stop("No existe el fichero: ", fichero)

  base <- .raiz_proyecto(fichero)
  if (is.null(tabla))
    tabla <- file.path(base, "datos", "sociodem_correspondencias.tsv")

  d <- suppressWarnings(
    if (grepl("[.]sav$", fichero, ignore.case = TRUE)) read_sav(fichero)
    else read_dta(fichero))

  if (is.null(estudio)) estudio <- .saca_estudio(d, fichero)
  if (is.na(estudio))
    stop("No he podido determinar el numero de estudio. Pasalo en 'estudio='.")
  if (is.null(anyo)) anyo <- .saca_anyo(d, estudio, base)

  # ---- que variable es cada concepto -------------------------------
  tab <- .lee_tabla(tabla)
  manual <- tab[!is.na(tab$estudio) & tab$estudio == estudio &
              tolower(tab$origen) == "manual", ]
  elegida <- setNames(rep(NA_character_, length(.CONCEPTOS)), .CONCEPTOS)
  fuente  <- setNames(rep("auto", length(.CONCEPTOS)), .CONCEPTOS)
  for (cc in .CONCEPTOS) {
    if (cc %in% manual$concepto) {
      v <- manual$variable[manual$concepto == cc][1]
      elegida[cc] <- if (is.na(v) || v == "") NA_character_ else v
      fuente[cc]  <- "manual"
      if (!is.na(elegida[cc]) && !elegida[cc] %in% names(d))
        stop("La tabla manda usar '", elegida[cc], "' para ", cc,
             " en el estudio ", estudio, ", y esa variable no esta en el fichero.")
    } else {
      elegida[cc] <- .detecta(d, .PATRON[[cc]])
    }
  }
  practica <- .detecta(d, .PAT_PRACTICA)

  # ---- recodificar --------------------------------------------------
  n <- nrow(d)
  vacio <- function() rep(NA_real_, n)
  g <- function(cc) if (is.na(elegida[cc])) NULL else d[[elegida[cc]]]

  out <- data.frame(estudio = rep(as.numeric(estudio), n))
  cues <- intersect(c("CUES", "CUEST", "cues"), names(d))
  out$cues    <- if (length(cues)) as.numeric(zap_missing(d[[cues[1]]])) else NA_real_
  out$orden   <- seq_len(n)
  out$numentr <- as.numeric(estudio) * 100000 + out$orden

  x <- g("genero");      out$genero    <- if (is.null(x)) vacio() else .rec_genero(x)
  x <- g("edad")
  anyos <- if (is.null(x)) vacio() else .rec_edad_anyos(x)
  out$generacion <- .rec_generacion(anyos, anyo)
  out$edad       <- .rec_edad_tramos(anyos)
  x <- g("ideologia");   out$ideologia <- if (is.null(x)) vacio() else .rec_ideologia(x)
  x <- g("religion")
  out$religion  <- if (is.null(x)) vacio() else
                   .rec_religion(x, if (is.na(practica)) NULL else d[[practica]])
  x <- g("estudios")
  respaldo_est <- setdiff(.detecta_todas(d, .PATRON$estudios), elegida["estudios"])
  respaldo_est <- if (length(respaldo_est)) d[[respaldo_est[1]]] else NULL
  out$estudios  <- if (is.null(x)) vacio() else .rec_estudios(x, respaldo_est)
  x <- g("laboral");     out$laboral   <- if (is.null(x)) vacio() else .rec_laboral(x)
  x <- g("profesional"); out$profesional <- if (is.null(x)) vacio() else .rec_profesional(x)
  x <- g("clase");       out$clase     <- if (is.null(x)) vacio() else .rec_clase(x)

  out <- out[, .SALIDA]

  # ---- etiquetas ----------------------------------------------------
  attr(out$estudio, "label") <- "Numero de estudio CIS"
  attr(out$cues,    "label") <- "Numero de cuestionario (tal cual viene en el fichero)"
  attr(out$orden,   "label") <- "Posicion en el fichero de origen (clave con FID3978abrev)"
  attr(out$numentr, "label") <- "N unico de entrevistado (estudio*100000+orden)"
  for (cc in names(.ETIQ)) {
    out[[cc]] <- labelled(as.numeric(out[[cc]]), labels = .ETIQ[[cc]]$val,
                          label = .ETIQ[[cc]]$lab)
  }

  # ---- las columnas crudas, si se piden -----------------------------
  if (salida == "auditoria") {
    for (cc in .CONCEPTOS) {
      if (is.na(elegida[cc])) next
      nm <- paste0("o_", tolower(elegida[cc]))
      out[[nm]] <- d[[elegida[cc]]]
    }
    if (!is.na(practica)) out[[paste0("o_", tolower(practica))]] <- d[[practica]]
  } else if (salida == "completo") {
    nuevas <- c(setdiff(names(out), c("estudio", "cues")), "orden", "numentr")
    nuevas <- unique(nuevas)
    d2 <- d
    # Los paradatos del CIS (IA_: incidencias de contacto, supervision...)
    # no son respuestas de la persona entrevistada. Fuera, salvo que se pidan.
    if (!IAs) d2 <- d2[!grepl("^IA_", names(d2), ignore.case = TRUE)]
    # SPSS no distingue mayusculas en los nombres, asi que ESTUDIOS del
    # fichero original chocaria con la 'estudios' armonizada. Manda la
    # armonizada, que es la que da la compatibilidad entre ficheros; la
    # original se conserva con el sufijo _ORIG.
    choques <- names(d2)[tolower(names(d2)) %in% tolower(nuevas)]
    if (length(choques)) {
      for (v in choques) {
        nn <- paste0(v, "_ORIG")
        while (tolower(nn) %in% tolower(c(names(d2), nuevas))) nn <- paste0(nn, "1")
        names(d2)[names(d2) == v] <- nn
      }
      if (avisos)
        cat("  nota: ", paste(choques, collapse = ", "),
            " del fichero original se renombra(n) con _ORIG para no chocar",
            " con las variables armonizadas.\n", sep = "")
    }
    for (v in nuevas) d2[[v]] <- out[[v]]
    out <- d2
  }

  # ---- informe y tabla ---------------------------------------------
  faltan <- .CONCEPTOS[is.na(elegida)]
  if (avisos) {
    cat("\n=== estudio ", estudio, "  (", basename(fichero), ", ", n,
        " filas) ===\n", sep = "")
    cat("  a\u00f1o de campo: ", ifelse(is.na(anyo), "NO DETERMINADO", anyo),
        ifelse(is.na(anyo), "  -> 'generacion' queda a perdido", ""), "\n", sep = "")
    for (cc in .CONCEPTOS) {
      v <- elegida[cc]
      if (is.na(v)) {
        cat(sprintf("  %-12s --  NO ESTA en el fichero -> columna vacia\n", cc))
      } else {
        cob <- 100 * mean(!is.na(out[[cc]]))
        cat(sprintf("  %-12s %-14s %5.1f%% valido   [%s] %s\n", cc, v, cob,
                    fuente[cc], substr(.etiqueta(d[[v]]), 1, 44)))
      }
    }
    if (!is.na(practica))
      cat("  (practica religiosa: ", practica, ")\n", sep = "")
    if (length(faltan))
      cat("  AVISO: sin datos para ", paste(faltan, collapse = ", "),
          ". Las columnas se crean vacias para que el fichero siga",
          " siendo apilable.\n", sep = "")
  }

  nuevas <- data.frame(
    estudio  = as.numeric(estudio),
    concepto = .CONCEPTOS,
    variable = ifelse(is.na(elegida), "", elegida),
    etiqueta = vapply(.CONCEPTOS, function(cc)
                 if (is.na(elegida[cc])) "(no esta en el fichero)"
                 else .etiqueta(d[[elegida[cc]]]), character(1)),
    origen   = fuente,
    stringsAsFactors = FALSE)
  tab <- tab[!(tab$estudio == estudio & tab$origen == "auto"), , drop = FALSE]
  ya <- paste(tab$estudio, tab$concepto)
  nuevas <- nuevas[!paste(nuevas$estudio, nuevas$concepto) %in% ya, , drop = FALSE]
  .guarda_tabla(rbind(tab, nuevas), tabla)

  # ---- escribir -----------------------------------------------------
  # 'destino' se toma como la ruta sin extension: la pone 'formato', que
  # es lo unico que permite pedir los dos ficheros de una vez.
  if (escribir) {
    raiz <- if (is.null(destino))
              sub("[.](sav|dta)$", "_soc", fichero, ignore.case = TRUE)
            else
              sub("[.](sav|dta)$", "", destino, ignore.case = TRUE)
    for (e in .EXT[[formato]]) {
      f_sal <- paste0(raiz, ".", e)
      if (e == "sav") write_sav(out, f_sal) else .escribe_dta(out, f_sal)
      if (avisos) cat("  -> ", f_sal, "  (", ncol(out), " variables)\n", sep = "")
    }
  }

  # Los factores y las escalas, solo en lo que se devuelve: los ficheros ya
  # se han escrito etiquetados, con los codigos del esquema comun.
  if (salida == "completo") {
    out <- .prepara_completo(out, likert, escala, centrada, factores, NAs)
  } else if (factores) {
    out <- .a_factores(out, NAs)
  }
  invisible(out)
}
