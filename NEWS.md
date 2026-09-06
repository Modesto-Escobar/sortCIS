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
