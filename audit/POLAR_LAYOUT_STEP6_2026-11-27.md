# Paso 6 — Ejes y etiquetas de gráficos polares

Las capturas del usuario muestran ejes cartesianos y nombres internos (x, y,
labelPosition) en sectores y anillo. El anillo además superpone sus etiquetas.
No se considera superada esa comprobación visual, aunque los gráficos renderizan.

Los elementos del tema específicos de cada eje pueden sobrescribir la ocultación
general del tema. Se ocultan explícitamente títulos, textos, ticks y líneas x/y,
y se eliminan sus etiquetas. Se conserva el tema suministrado, con su fondo,
tipografía y paleta. El anillo usa una leyenda lateral con categoría, conteo y
porcentaje, retirando el texto interior superpuesto. Los datos y el orden de
categorías se conservan. Sectores mantiene las etiquetas con repelido.

Se añaden pruebas con un tema que define los elementos x/y explícitamente,
además del caso existente de 12 categorías, paleta y renderizado completo.
Las pruebas anteriores solo verificaban renderizado, fuentes y colores; no
verificaban la desaparición efectiva de ejes ni la legibilidad del anillo.

Revisión estática sin incidencias. R no está disponible aquí: testthat y la
inspección GUI de esta corrección están pendientes. El último resultado previo,
PASS 2064 sin fallos, advertencias ni omisiones (23.7 s), corresponde al paso 5.
El usuario confirmó las tres tablas de frecuencias del compilado anterior.

Aplicar archivos/ sobre el proyecto, reiniciar R y ejecutar testthat::test_local(".").
Si pasa, compilar e instalar con jmvtools::install(). Repetir sectores y anillo
con las 12 categorías, verificando ausencia de ejes/nombres internos y una leyenda
legible del anillo. Guardado, reapertura y exportación siguen pendientes.
