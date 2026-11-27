# Paso 8 — Paleta multicolor jmv para sectores y anillo

El usuario solicita la paleta jmv Default mostrada en una captura de vijPlots.
Se revisó R/utils.R de https://github.com/vjalby/vijPlots y la implementación
pública R/themes.R de https://github.com/cran/jmvcore.

vijPlots obtiene la paleta mediante jmvcore::colorPalette(n, pal=theme$palette,
type="fill"). Para reproducir la selección jmv de la captura, saRa llama a esa
API con pal="jmv" en sectores y anillo. Usa los colores categóricos completos
y la ampliación de jmvcore para más categorías, sin interpolar los dos tonos
de theme$fill ni teñir manualmente los anclajes. Se retira el auxiliar .saraPalette,
que ya no se utiliza. No se añade una dependencia de vijPlots ni se copia su código.

Esta selección jmv es explícita para los dos gráficos, conforme a la petición;
la tipografía y el fondo siguen el tema de jamovi. Los porcentajes permanecen
centrados en la banda del anillo y la leyenda contiene solo categorías.

Las pruebas comprueban el mapa completo de las 12 categorías contra la API
pública, incluso con otros anclajes theme$fill, y conservan los casos de fuentes,
fondo, porcentajes, ejes ocultos y renderizado completo. Las pruebas de la mezcla
manual se sustituyen porque esa funcionalidad se retira por decisión del usuario.

Última suite anterior: PASS 2101, FAIL 0, WARN 0, SKIP 0, 21.4 s (paso 7).
Revisión estática sin incidencias. R y GUI pendientes para este cambio.

Aplicar archivos/ sobre el proyecto, reiniciar R y ejecutar testthat::test_local(".").
Si pasa, ejecutar jmvtools::install() y revisar sectores y anillo con 12 categorías.
