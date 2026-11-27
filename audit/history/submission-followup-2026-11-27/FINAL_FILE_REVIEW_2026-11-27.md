# Revisión final reconciliada — saRa 1.0.0, 27 de noviembre de 2026

## Resultado

Cotejo completado para los 110 archivos de código, configuración, definiciones,
datos y pruebas del inventario del usuario. La primera comparación daba
102 archivos iguales, siete diferencias y un script adicional. Se recibieron
los archivos distintos, se revisaron y se verificaron las huellas de la copia
reconciliada frente a las suministradas por el usuario. No se ha cambiado su
código funcional ni sus pruebas durante este cotejo.

| Archivos distintos | Resultado de la revisión |
|---|---|
| R/bivnum.b.R | Conserva las notas de correlación con constantes y las orientaciones de marginales ya validadas; boxplots sin notches. Mantener versión del usuario. |
| R/numeric-statistics.R | Incluye el auxiliar de clasificación ordinal por puntuaciones almacenadas y sus pruebas. Mantener. |
| R/numsummary.h.R | Regeneración de jmvtools: orden, formato y documentación; conserva las opciones, valores por defecto y argumentos Gini/Lorenz. Mantener. |
| jamovi/0000.yaml | Registro explícito de los ocho datos; 12 análisis y metadatos del compilador. Mantener. |
| R/00jmv.R y jamovi/00refs.yaml | Cambios bibliográficos coherentes entre YAML y R generado; coinciden las 11 claves. Mantener. No se afirma una auditoría bibliográfica externa nueva. |
| tests/testthat/test-audit-review.R | Cuatro bloques ya validados: metadatos ordinales, notas de correlación, marginales y registro de datos. Mantener. |
| tests/gui/frequency-selectors.cjs | Pruebas unitarias de selectores nominal/ordinal y transición de Range. Ejecutadas con Node y controles simulados: PASS. Mantener. |
| audit/NORTEST_VALIDATION_2026-11-10.json | Registro general fechado 16-11-2026, 409 comprobaciones; se conserva original en history/cotejo-2026-11-27 y se anota como histórico sustituido. |

## Verificaciones

- Sintaxis de 58 archivos R y 38 YAML comprobada sin errores por el validador
  estático; opciones, dependencias de visibilidad y referencias sin incidencias.
- JavaScript fuente previamente comprobado con node --check; el script de
  selectores ahora se ha ejecutado con éxito.
- Los 12 análisis tienen definiciones A/R/U y clases B/H disponibles. No hay
  registro del add-on de regresión y se mantienen sus tres eliminaciones.
- Los ocho datos están inscritos explícitamente y sus ficheros existen.
  Su apertura/operatividad ya fue confirmada por el usuario.
- El código reconciliado contiene 112 bloques test_that. La última ejecución
  R del usuario pasó 1878 comprobaciones en 39.4 s, 0 fallos/warnings/omisiones.
- R y jamovi no se han ejecutado en el entorno del asistente.

Los registros anteriores se conservan; no deben confundirse con los datos
actuales. El JSON de validación runtime no se sobrescribe al ejecutar el
validador estático. Los espacios finales y alias lwd de mantenimiento no se
modifican en este cierre documental.

## Identidad y límites

Rama: fix/audit-review-1.0.0. HEAD comunicado:
2cfe7ee61c580950140c832a4b22163a90bba2f8 (base, no commit del candidato).
La huella del ZIP recibido y la clasificación de las diferencias están en
SOURCE_RECONCILIATION_2026-11-27.json. REVIEWED_FILES_2026-11-27.csv contiene
el inventario actualizado. El cotejo no identifica todavía el binario JMO
final ni cubre futuros cambios del árbol. No acredita por sí solo una
validación multiplataforma. No se ha hecho commit, push ni publicación.

## Cierre final confirmado por el usuario

saRa_1.0.0.jmo identificado con SHA-256 registrado en
FINAL_ACCEPTANCE_2026-11-27.json. Comprobación conjunta solicitada confirmada
por el usuario: reinicio/carga, OMV y HTML/PDF. No se ha recibido el binario
para inspección independiente. git status/stat revisados; diff --check solo
señala espacios finales, que se conservan sin modificar el candidato.

Queda preparar el staging de los archivos revisados y crear/registrar el
commit. El JMO y los informes LOCAL_FILE_COMPARISON.csv/LOCAL_GIT_STATE.txt
quedan fuera del staging. No hay razón para repetir las pruebas o recompilar
por esta actualización documental.
