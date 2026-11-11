# saRa 1.1.0 — segundo lote y catálogo castellano

Fecha: 11 de noviembre de 2026. Rama prevista: feature/spanish-1.1.0.

## Contenido

- Textos dinámicos de resúmenes, tablas de asociación, frecuencias, distribuciones, normalidad y contrastes de rangos marcados para traducción.
- Plantillas completas con sprintf para títulos con porcentajes, percentiles, empates y mensajes con valores. Se conservan identificadores internos, fórmulas, nombres de variables y resultados numéricos.
- Catálogo oficial en jamovi/i18n/catalog.pot y primera propuesta castellana en jamovi/i18n/es.po: 569 claves extraídas, todas con traducción o conservación deliberada de nombres propios/símbolos.
- Las dos líneas explicativas del ACF se traducen por separado para evitar problemas de extracción y recuperación con saltos de línea escapados.
- DESCRIPTION y jamovi/0000.yaml pasan a 1.1.0; NEWS identifica este trabajo como candidato de desarrollo.
- Dos bloques adicionales de pruebas: títulos traducidos con cobertura porcentual y errores de validación de distribuciones. Total: 40 bloques.

## Aplicación

1. Confirmar que la rama activa es feature/spanish-1.1.0. Conservar el proyecto completo y copiar encima únicamente los archivos incluidos en el ZIP. No borrar R, tests, jamovi ni audit.
2. Ejecutar en consola R: testthat::test_local("."). La validación anterior de 294 comprobaciones corresponde al lote anterior, no a este.
3. Si no hay fallos: jmvtools::install(). Esto debe regenerar las definiciones y producir saRa_1.1.0.jmo.
4. Reiniciar jamovi, seleccionar castellano y abrir análisis nuevos para evitar evaluar únicamente resultados antiguos almacenados.
5. Revisar controles, tablas, notas y gráficos en todos los análisis. En Regresión comprobar especialmente títulos y notas de DW, normalidad, RESET, heterocedasticidad, ACF, residuos e influencia.
6. Cambiar a inglés y comprobar los mismos resultados. Revisar HTML/PDF y guardar/reabrir .omv. No marcar estas comprobaciones como superadas hasta realizarlas.
7. Registrar cualquier texto pendiente, recorte o término a cambiar. Mantener esta versión en la rama de traducción hasta completar la revisión.

Los catálogos ya están generados: no ejecutar i18nCreate('es') para esta entrega, porque podría reiniciar la traducción. Para futuras cadenas usar jmvtools::i18nUpdate() y revisar el diff del catálogo antes del commit.

## Terminología propuesta para revisión

- test: contraste; residuals: residuos; standardized: tipificado; studentized: estudentizado.
- standard deviation: desviación típica; lag: retardo; leverage: apalancamiento.
- ACF: FAC; heteroskedasticity: heterocedasticidad; confidence interval: intervalo de confianza.
- Odds se mantiene como odds; los nombres propios y los símbolos estadísticos se conservan.

Es una primera propuesta terminológica. La presencia de traducción en todas las claves del catálogo no demuestra por sí sola cobertura visual completa ni aprobación terminológica.

## Verificación realizada aquí

- Sintaxis R revisada con tree-sitter; sin errores de análisis sintáctico.
- Comparaciones e identificadores de selección existentes conservados; se añaden únicamente dos condiciones para etiquetar los ejes de frecuencias absolutas.
- Marcadores de interpolación conservados; ninguna traducción vacía.
- Extracción, actualización y carga del PO mediante jamovi-compiler, incluida comprobación de que la actualización conserva las traducciones.
- No hay ejecución de R ni compilación/GUI de jamovi en este entorno. Estas comprobaciones corresponden al desarrollador.

## Referencias de implementación

- https://dev.jamovi.org/tutorial/tuts0204-translation
- https://github.com/jamovi/jmv-i18n (es.po, referencia terminológica)
- https://github.com/jamovi/jamovi-compiler (extractor oficial)

La evidencia satisfactoria de 1.0.0 y del primer lote permanece histórica y no se sustituye por afirmaciones de ejecución del segundo lote.
