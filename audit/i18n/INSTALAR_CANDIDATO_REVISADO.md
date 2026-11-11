# saRa 1.1.0 — candidato con catálogo revisado

Las 569 entradas se han revisado con el desarrollador en 19 bloques. Se incluyen todas las correcciones acordadas y las correcciones inglesas de Wilcoxon y omega al cuadrado. El registro detallado está en REVISION_CASTELLANO.json.

## Aplicar

1. Confirmar feature/spanish-1.1.0 con git branch --show-current.
2. Copiar el contenido completo del ZIP sobre la raíz del proyecto, sustituyendo solo los archivos incluidos y conservando el resto. Este ZIP es acumulativo e incluye DESCRIPTION, NEWS y las carpetas R, jamovi, tests y audit.
3. En consola R ejecutar testthat::test_local("."). Son 40 bloques; el contador PASS corresponde a comprobaciones. Los 294 PASS previos corresponden al primer lote y no validan por sí solos este candidato.
4. Si pasa sin fallos, ejecutar jmvtools::install() y confirmar que se crea saRa_1.1.0.jmo y termina la instalación. No se incluye un binario compilado en esta entrega.
5. Reiniciar jamovi, seleccionar castellano y probar análisis nuevos: controles, tablas, notas, mensajes y gráficos. Especial atención a regresión, frecuencias, distribuciones y resúmenes ordinales/numéricos.
6. Comprobar √(|residuo tipificado|) en pantalla, HTML y PDF. Verificar guardar/reabrir .omv y funcionamiento en inglés. Anotar textos ingleses inesperados, cortes o errores.
7. Solo después de validar, registrar los resultados y hacer commit/push en la rama de traducción. Mantener main sin fusionar hasta completar las comprobaciones.

No ejecutar i18nCreate('es'): el catálogo ya está creado y revisado. Para nuevas cadenas usar i18nUpdate() y revisar el diff.

## Verificación aquí

La sintaxis R, los marcadores de interpolación y el procesamiento/actualización del catálogo oficial se han revisado satisfactoriamente. Se ha comprobado que cada traducción coincide con su aprobación registrada. R y jamovi no se han ejecutado en este entorno: faltan pruebas y compilación del candidato en el equipo del desarrollador.
