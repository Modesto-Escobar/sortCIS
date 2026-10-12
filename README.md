# sortCIS

Armonización de las variables sociodemográficas de los estudios del Centro
de Investigaciones Sociológicas (CIS).

`cis_sociodem()` toma un fichero de datos del CIS (`.sav` de SPSS o `.dta`
de Stata) y escribe otro con las variables sociodemográficas recodificadas
al esquema común de `FID3978abrev.dta`, de modo que dos ficheros convertidos
con la función se puedan apilar directamente.

## Instalación

Desde el directorio que contiene el paquete:

```r
# install.packages("remotes")
remotes::install_local("sortCIS")
```

O, con el repositorio clonado y **devtools** instalado:

```r
devtools::install()
```

La única dependencia es **haven**.

## Uso

```r
library(sortCIS)

# Escribe datos/sav/3557_soc.sav
cis_sociodem("datos/sav/3557.sav")

# Sin escribir nada, para inspeccionar el resultado
d <- cis_sociodem("datos/sav/3557.sav", escribir = FALSE)

# Con las variables crudas al lado, para comprobar la recodificación
d <- cis_sociodem("datos/sav/3557.sav", salida = "auditoria",
                  escribir = FALSE)

# Apilar dos estudios
a <- cis_sociodem("datos/sav/3528.sav", escribir = FALSE)
b <- cis_sociodem("datos/sav/3557.sav", escribir = FALSE)
todo <- rbind(a, b)
```

## Formato de salida

Por defecto escribe `.sav` de SPSS. El argumento `formato` permite pedir
Stata, o los dos ficheros a la vez:

```r
cis_sociodem("datos/sav/3557.sav")                     # 3557_soc.sav
cis_sociodem("datos/sav/3557.sav", formato = "dta")    # 3557_soc.dta
cis_sociodem("datos/sav/3557.sav", formato = "ambos")  # los dos

# La extensión del destino basta para pedir Stata
cis_sociodem("datos/sav/3557.sav", destino = "salida/3557.dta")
```

`destino` se interpreta como la ruta **sin extensión**: la pone `formato`.

Stata sólo admite nombres de variable de letras, dígitos y guión bajo, que no
empiecen por dígito y no pasen de 32 caracteres. Los que pone la función los
cumplen siempre; con `salida = "completo"` los nombres vienen del fichero de
origen y, si alguno no vale, se avisa nombrándolo.

## Por qué la detección va por etiqueta

El CIS no nombra igual las variables en todos los estudios. La misma
pregunta se llama `SEXO` en los recientes y `P23`, `P24`, `P28`, `P29`,
`P30`, `P31`, `P32`, `P40` o `P43` en los antiguos. Comprobado sobre los 34
ficheros de la serie, ocho conceptos dan sesenta nombres de variable
distintos.

Lo que sí es estable es la **etiqueta** de la variable. Por eso la detección
va por etiqueta y no por nombre.

Lo mismo ocurre un nivel más abajo, con los códigos: "Ayuda familiar" es el
código 5 en unos estudios y el 4 en otros; "Otra situación" es 7 u 8 según
el estudio. Recodificar por número da resultados silenciosamente
equivocados, así que la recodificación también va por la etiqueta del valor.

## Variables de salida

| Variable | Contenido |
|---|---|
| `estudio`, `cues`, `orden`, `numentr` | claves de identificación |
| `genero` | 1 Hombre, 2 Mujer |
| `generacion` | 1 G. 98 … 8 Z |
| `edad` | 1 Joven … 5 Vejez |
| `ideologia` | 1 Izquierda, 2 Centro, 3 Derecha |
| `religion` | 1 Practicante, 2 Católico no practicante, 3 Otras |
| `estudios` | 100 Sin estudios … 500 Universitarios |
| `laboral` | 1 Trabaja … 5 Doméstico |
| `profesional` | 0 N.P. (no trabaja), 1 Asalariado, 2 Empresario/autónomo, 6 Otra |
| `clase` | 1 Alta/media-alta … 9 Otras |

## La tabla de correspondencias

La función deja escrito en `datos/sociodem_correspondencias.tsv` qué variable
ha elegido para cada concepto. Las filas con `origen = "auto"` las reescribe
en cada pasada; las que se pongan a mano con `origen = "manual"` mandan sobre
la detección automática. Así, un estudio con una etiqueta atípica se arregla
editando una línea del TSV, sin tocar el código.

## Desarrollo

```r
devtools::document()   # regenera NAMESPACE y man/
devtools::test()       # pasa las pruebas
devtools::check()      # R CMD check completo
```
