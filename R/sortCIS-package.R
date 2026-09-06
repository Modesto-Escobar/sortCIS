#' sortCIS: armonización de las variables sociodemográficas del CIS
#'
#' @description
#' El Centro de Investigaciones Sociológicas no nombra igual sus variables en
#' todos los estudios, ni codifica igual las categorías de una misma pregunta.
#' Este paquete convierte un fichero de datos del CIS a un esquema
#' sociodemográfico común, de modo que dos estudios convertidos con él se
#' puedan apilar directamente.
#'
#' @section Función principal:
#' [cis_sociodem()] es, de momento, la única función exportada. Toma un
#' fichero `.sav` o `.dta` del CIS y escribe otro con nueve variables
#' sociodemográficas armonizadas más cuatro claves de identificación.
#'
#' @section Por qué la detección va por etiqueta:
#' La misma pregunta se llama `SEXO` en los estudios recientes y `P23`,
#' `P24`, `P28`, `P29`, `P30`, `P31`, `P32`, `P40` o `P43` en los antiguos;
#' la ideología es `ESCIDEOL` o `P17`, `P20`, `P21`, `P22`, `P23`, `P25`,
#' `P28`, `P32` o `P34`. Comprobado sobre los 34 ficheros de la serie, ocho
#' conceptos dan sesenta nombres de variable distintos.
#'
#' Lo que sí es estable es la **etiqueta** de la variable ("Sexo de la persona
#' entrevistada", "Escala de autoubicación ideológica (1-10)"). Por eso la
#' detección va por etiqueta y no por nombre.
#'
#' Lo mismo ocurre un nivel más abajo, con los códigos: "Ayuda familiar" es el
#' código 5 en unos estudios y el 4 en otros; "Otra situación" es 7 u 8 según
#' el estudio. Recodificar por número da resultados silenciosamente
#' equivocados, así que la recodificación también va por la etiqueta del
#' valor, no por el valor.
#'
#' @section De dónde salen los mapas de recodificación:
#' No están inventados: se derivaron cruzando los 32 ficheros `.sav` que ya
#' están en `FID3978abrev.dta` contra el propio FID, fila a fila, y tabulando
#' categoría de origen contra categoría de destino. Es decir, reproducen lo
#' que de hecho hizo quien construyó el acumulado.
#'
#' El cruce es por **posición de fila** (`numentr = estudio * 100000 + orden`),
#' no por número de cuestionario: comprobado, por orden acierta el 100 % y por
#' `cues` entre el 54 % y el 62 % en los estudios antiguos.
#'
#' Las reglas coinciden con las de `Analiza.do`, con dos matices que se ven en
#' los datos de FID pero no en el texto de aquel do-file:
#'
#' - `estudios` llega a FID con la F.P. ya fundida en 300 (secundaria
#'   superior); el código 400 existe en las etiquetas pero no en los datos.
#' - `profesional` llega con el recode `(4/6 = 6) (3 = 2)` ya aplicado; las
#'   etiquetas de FID se quedaron sin actualizar y por eso engañan.
#'
#' En ambos casos manda el dato, no la etiqueta.
#'
#' @keywords internal
#' @importFrom haven read_sav read_dta write_sav write_dta zap_missing labelled
#' @importFrom stats setNames
"_PACKAGE"
