# sortCIS 0.4.0

* **Factores en lo que se devuelve.** `cis_sociodem()` gana el argumento
  `factores`, `TRUE` por defecto: las nueve variables armonizadas del
  `data.frame` devuelto son factores, con los niveles en el orden de los
  códigos y todos los niveles del esquema, aunque el estudio no tenga casos
  en alguno. `generacion`, `edad`, `ideologia`, `estudios` y `clase` son
  factores ordenados; `genero`, `religion`, `laboral` y `profesional`, sin
  orden. Los ficheros `.sav` y `.dta` se siguen escribiendo etiquetados, con
  los códigos del esquema común.

  **Cambia lo que devuelve la función.** Quien use los códigos del esquema
  común en R tiene que pasar `factores = FALSE`. `as.numeric()` sobre un
  factor da la posición del nivel y no el código, y en `estudios`,
  `profesional` y `clase` no coinciden.
* Nuevo argumento `NAs`, `TRUE` por defecto. Con `NAs = FALSE` los perdidos
  pasan a un nivel `"N.C."` explícito al final del factor.
* **Escalas y Likert con `salida = "completo"`.** Las preguntas de escala
  (0-10, 1-10, 1-7: ideología, valoración de líderes, probabilidad de
  voto...) y las de tipo Likert ("Muy buena" ... "Muy mala", "Mucha" ...
  "Ninguna confianza", "Mucho" ... "Nada"...) se añaden convertidas, como
  columnas nuevas en minúsculas, detrás de las nueve armonizadas. Las
  originales quedan al final, con su nombre en mayúsculas. Se reconocen por
  la forma de las etiquetas de valor; probado sobre el 1181, 2207, 2798,
  3557, 3567 y 3577.
  - `escala = c("numerica", "factor")`: el valor tal cual, o factor
    ordenado.
  - `likert = c("numerica", "factor")`: **invertidas**, de modo que el valor
    más alto es el polo positivo ("Muy buena" = 5, "Muy mala" = 1).
  - `centrada = TRUE`: las Likert con el punto medio en 0, de -2 a 2 con
    cinco categorías y -2, -1, 1, 2 con cuatro.
  - Con `factores = TRUE`, las demás variables etiquetadas del original
    pasan a factor si todos sus valores tienen etiqueta, y a número si no.
  - Los ficheros `.sav` y `.dta` no cambian: todo esto afecta sólo a lo que
    se devuelve en R.
* **Fuera los paradatos.** Con `salida = "completo"` se quitan las variables
  `IA_*` (incidencias de contacto, supervisión de la entrevista...), del
  fichero escrito y de lo que se devuelve. El nuevo argumento `IAs = TRUE`
  las conserva.

* **`profesional` es la situación profesional actual.** Quien no trabaja,
  según `laboral`, pasa a 0 N.P.; con `laboral` perdido manda la propia
  pregunta. **Cambia el resultado en todos los estudios que la tienen:**
  - En el 3557, 3567 y 3577 la pregunta sólo se hace a quien trabaja y el
    resto lleva el 0 «No procede», declarado missing de SPSS. La lectura lo
    dejaba en `NA` y la categoría N.P. salía vacía: unos 1.600 casos por
    estudio que eran N.P. quedaban como perdidos. Ahora son N.P., y
    coinciden caso por caso con el 0 del fichero.
  - En el 2207 y el 2798 la pregunta se hacía a todos, por el empleo actual
    o el último, así que pensionistas, parados y amas de casa tenían valor.
    Ahora son N.P. (4.162 casos en el 2207 y 1.347 en el 2798). Es lo que
    hace falta para que `profesional` signifique lo mismo en todos los
    estudios; el último empleo sigue en la variable cruda, con
    `salida = "auditoria"`.
  - Si el estudio no tiene la pregunta, como el 1181, `profesional` sigue
    vacía aunque haya `laboral`.
  - Las otras ocho variables no cambian en ninguno de los seis estudios.
* **`religion`**: un «N.S.» o un «No procede» en la religiosidad se contaba
  como «3 Otras», porque sólo se apartaba el «N.C.». Ahora van a perdido,
  como en las demás variables. En los seis estudios comprobados la
  religiosidad sólo trae «N.C.», así que el resultado no cambia.
* **`ideologia` en el 3577.** Su etiqueta es «Escala (1-10) de
  autoubicación ideológica», con el paréntesis en medio. Al normalizar las
  etiquetas se cortaba desde el primer paréntesis hasta el final, la
  etiqueta quedaba en «escala» y la variable no se encontraba: `ideologia`
  salía vacía. Ahora se quita sólo el contenido del paréntesis. En el 3577
  pasa del 0 % al 96,8 %; el 1181, el 2207, el 2798 y el 3557 dan un
  resultado idéntico al de antes.
* **Año de campo del 3567 y el 3577.** Ninguno de los dos trae `E103` y no
  estaban en la tabla interna, así que `generacion` salía vacía. Se añaden:
  junio de 2026 el 3567 y septiembre de 2026 el 3577.

# sortCIS 0.3.0

Tres correcciones en la recodificación, salidas de probar el paquete contra
los ficheros originales del banco de datos del CIS de los estudios 1181
(preelectoral de 1979), 2207 (1996), 2798 (2009) y 3557 (2026). Las tres
afectan sólo a estudios antiguos: se comprobó caso por caso que el 2798 y el
3557 dan un resultado **idéntico** al de la versión anterior.

* **`ideologia` con escalas que no son 1-10.** El 1181 usa una escala 1-7
  cuyos «No sabe» y «No contesta» son los códigos 8 y 9. Como caían dentro
  del rango 1-10 se tomaban por respuestas válidas y acababan en «Derecha»:
  7.445 de sus 7.745 casos, el 96 % de la categoría, y el informe decía
  `100.0% valido`. Además el 4, que es el centro exacto de una escala 1-7,
  se clasificaba como izquierda.

  Ahora los perdidos se identifican por la etiqueta del valor, como en el
  resto del paquete, y la longitud de la escala se lee de la etiqueta de la
  variable (`(1-7)`, `(1-10)`), con el mayor código no perdido como
  respaldo. La escala se reescala linealmente a 1-10 antes de aplicar los
  cortes de siempre, lo que para una 1-7 da 1-3 izquierda, 4 centro y 5-7
  derecha, y para una 1-10 es la identidad.

* **`estudios` con las formulaciones antiguas.** Se añaden «Menos de
  estudios primarios», «Estudios primarios», «Bachiller elemental» y
  «Bachiller superior». Sin ellas el 1181 perdía el 89 % de la variable, y
  no al azar: se quedaban fuera justo las categorías bajas. «Estudios
  primarios sin finalizar» va a «Sin estudios» y se comprueba antes que
  «Estudios primarios» a secas, o la segunda se lo llevaría.

* **`laboral`: «Sus labores» y «Está parado».** El CIS llamaba así al
  trabajo doméstico no remunerado y al desempleo hasta bien entrados los
  2000. La regla buscaba `domestico` y `^parado`. Se perdían el 42 % de la
  variable en el 1181 y el 22 % en el 2207, y lo perdido eran amas de casa y
  parados: sesgo, no ruido.

Cobertura antes y después, sobre los originales:

| | 1181 | 2207 | 2798 | 3557 |
|---|---|---|---|---|
| `ideologia` | 100 % → 70,8 % | 74,2 % | 84,3 % | 96,7 % |
| `estudios` | 11,0 % → 97,4 % | 98,3 % | 99,6 % | 99,9 % |
| `laboral` | 58,0 % → 99,1 % | 77,5 % → 98,9 % | 99,4 % | 98,0 % |

La bajada de `ideologia` en el 1181 es la corrección: antes contaba como
válidos 7.445 «No sabe» y «No contesta».

# sortCIS 0.2.0

* `cis_sociodem()` gana el argumento `formato`, que elige el formato del
  fichero de salida: `"sav"` de SPSS (como hasta ahora), `"dta"` de Stata, o
  `"ambos"`, que escribe los dos con la misma raíz en una sola pasada. Si no
  se da `formato` pero `destino` acaba en `.dta`, se entiende `"dta"`.
* `destino` pasa a interpretarse como la ruta **sin extensión**, que la pone
  `formato`. Si se da con extensión `.sav` o `.dta`, se le quita. Las
  llamadas de antes siguen dando el mismo fichero; sólo cambia el caso de un
  `destino` sin extensión, que antes se escribía tal cual y ahora recibe la
  que le toque.
* Al escribir en Stata se comprueban antes los nombres de variable, que allí
  sólo pueden llevar letras, dígitos y guión bajo, no empezar por dígito y no
  pasar de 32 caracteres. Si alguno no vale se avisa nombrándolo, en vez de
  dejar salir el error de **haven**, que no dice cuál falla. Sólo puede
  ocurrir con `salida = "completo"`.
* `formato` se añade al final de la lista de argumentos, así que las llamadas
  por posición que ya funcionaban siguen funcionando.

# sortCIS 0.1.0

Primera versión como paquete. Hasta aquí el código vivía en un único
fichero `cis_sociodem.R` que se cargaba con `source()`.

* `cis_sociodem()` pasa a ser la función exportada del paquete, con ayuda
  en español (`?cis_sociodem`) y una viñeta de conjunto en
  `?"sortCIS-package"`.
* La dependencia de **haven** se declara en `DESCRIPTION` e `NAMESPACE`, en
  vez de con un `library(haven)` dentro del propio fichero.
* Se añade una batería de pruebas con **testthat** que comprueba, sobre dos
  ficheros sintéticos con las mismas etiquetas y códigos distintos, que la
  recodificación por etiqueta hace apilables los dos estudios.
* Sin cambios de comportamiento. Los únicos retoques al código son internos:
  la variable local `T` pasa a llamarse `tab` (enmascaraba `TRUE`) y se
  elimina una línea muerta, `out$numertr <- NULL`.
