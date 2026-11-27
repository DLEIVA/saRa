# Registrar el candidato validado saRa 1.0.0

Rama: fix/audit-review-1.0.0. No cambiar a main ni limpiar el árbol.
Fuente reconciliada: 110 archivos. Última suite del usuario: 1878 comprobaciones
superadas, sin fallos/warnings/omisiones. JMO final identificado y comprobación
conjunta confirmada por el usuario. Ver FINAL_ACCEPTANCE_2026-11-27.json.

## Preparar staging desde la terminal del proyecto

Tras copiar los documentos de cierre:

```bash
git add -u
git add -- R/distribution-plots.R R/frequency-tables.R R/inequality.R R/messages.R R/normality-plot-messages.R
git add -- jamovi/js/catsummary.js jamovi/js/ordsummary.js tests/testthat tests/gui/frequency-selectors.cjs
git add -- audit/*.md audit/*.json audit/*.R audit/*.py audit/AUDIT_TRACKER.csv audit/REVIEWED_FILES_2026-11-27.csv audit/history
git diff --cached --stat

```

git add -u incluye las modificaciones y las tres eliminaciones revisadas del
add-on de regresión. Los comandos siguientes incorporan los auxiliares nuevos,
JS fuente, pruebas y registros. No usar git add . ni añadir el JMO con -f.
Los informes locales audit/LOCAL_FILE_COMPARISON.csv y LOCAL_GIT_STATE.txt
quedan sin añadir; sus copias históricas recibidas forman parte de la evidencia.
Revisar el resumen del staging antes de ejecutar el commit. Si hubiera algún
archivo añadido manualmente al staging antes de estos pasos, revisarlo también.
La salida proporcionada inicialmente no mostraba archivos ya staged.

## Crear el commit

```bash
git commit -m "Prepare validated saRa 1.0.0 audit candidate"
git rev-parse HEAD
git status --short
```

Conservar el hash que devuelve rev-parse junto al JMO. El documento de
aceptación se escribe antes del commit, por lo que no contiene su propio hash.
El commit que incluye ese documento y el hash comunicado después identifican
el candidato. No se hace otro commit solo para introducir un hash autorreferente.

Los espacios finales informados por diff --check son de formato, incluidos
archivos generados. Se conservan en este candidato para no alterar las fuentes
ya validadas. No hay cambios funcionales nuevos ni motivo para recompilar
por esta documentación. No se ha realizado push, merge, tag o publicación.
