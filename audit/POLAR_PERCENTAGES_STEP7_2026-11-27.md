# Paso 7 — Paleta visible y porcentajes en el anillo

Petición del usuario: evitar sectores blancos o casi blancos; porcentajes dentro
del anillo y solo categorías en la leyenda.

El auxiliar de paleta usado por sectores y anillo tiñe los anclajes de luminancia
superior a .9 con el tono más oscuro de la paleta elegida. Conserva los demás
colores y amplía por interpolación cuando hay más categorías. Una paleta
completamente blanca usa anclajes grises visibles. No se añaden sectores blancos.

El anillo aumenta su grosor y centra los porcentajes en la banda (radio 3.25),
con la tipografía del tema a escala .75 y color de texto del tema. Se omiten
etiquetas para categorías de frecuencia cero. La leyenda contiene solo categorías.
Los ejes permanecen ocultos. Los datos y el orden se conservan.

Pruebas: paletas de 12 colores sin tonos casi blancos, conservación de colores
no claros, texto porcentual y posición en el anillo, leyenda de categorías,
renderizado y ocultación de ejes. Se incorpora la corrección de prueba del paso 6
(as.character en vez de unname para descartar el atributo pos de ggplot2).

Revisión estática sin incidencias; R no se ejecuta aquí. PASS 2088 sin fallos,
advertencias ni omisiones (21.6 s) corresponde al paso 6 anterior. Este paso
requiere testthat y comprobación visual del nuevo compilado.

Aplicar archivos/ sobre el proyecto. Reiniciar R y ejecutar testthat::test_local(".").
Si pasa, ejecutar jmvtools::install() y revisar sectores/anillo con 12 categorías,
las etiquetas porcentuales y las paletas seleccionadas. No publicar todavía.

## Corrección del ajuste de luminosidad

Primera ejecución: PASS 2100, FAIL 1, WARN 0, SKIP 0 (22.3 s).
La mezcla fija podía superar el límite .9 con un anclaje azul claro. Se sustituye
por una mezcla adaptativa con objetivo .85, dejando margen para el redondeo RGB.
Las pruebas permanecen sin cambios. Se verificó el cálculo e interpolación RGB
con Python en las tres paletas de prueba (12 colores distintos, luminosidad < .9).
Esta comprobación no sustituye ejecutar testthat con R. Nueva ejecución pendiente.
