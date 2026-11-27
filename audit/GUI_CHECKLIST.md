# Lista de aceptación saRa 1.0.0 — 27 de noviembre de 2026

Este documento sustituye la lista acumulativa anterior, archivada en
history/pre-consolidation-2026-11-27/GUI_CHECKLIST.md. No deben repetirse
comprobaciones ya aceptadas salvo cambios relevantes en el código.

## Bloques aceptados para los casos registrados

- [x] Resúmenes y frecuencias nominales, ordinales y numéricas.
- [x] Asociaciones descriptivas numéricas, ordinales y nominales; efectos mixtos.
- [x] Distribuciones continuas y discretas: resultados, gráficos y casos límite.
- [x] Normality Tests: mensajes individuales, ECDF/Q-Q agrupados y recuperación.
- [x] Correcciones de Mann-Whitney y Wilcoxon y casos límite registrados.
- [x] Gini, Gini normalizado y curva de Lorenz; controles, avisos y recuperación.
- [x] Visibilidad de las tres tablas nominales y selección individual de índices.
- [x] Nombres especiales en gráficos y tablas bivariantes nominales/ordinales/numéricas.
- [x] Apertura y operatividad de los ocho conjuntos de la biblioteca.
- [x] OMV/HTML/PDF para los bloques confirmados, incluidos Gini/Lorenz y notas ordinales.

## Cierre de validación y commit

- [x] Cotejados los 110 archivos del inventario de código/datos/pruebas del usuario.
- [x] Revisados tests/gui/frequency-selectors.cjs (ejecutado en Node) y el JSON histórico nortest.
- [x] Identificado saRa_1.0.0.jmo con SHA-256 del usuario; comprobado
      instalación tras reinicio, una reapertura OMV y exportación HTML/PDF conjunta.
      Confirmación conjunta final del usuario recibida; ver FINAL_ACCEPTANCE_2026-11-27.json.
- [ ] Registrar el nuevo commit del candidato; el HEAD comunicado es la base.

La prueba final conjunta ya fue confirmada por el usuario; no se exige
repetir las exportaciones anteriores por este cierre documental. No se afirma compatibilidad multiplataforma del último
candidato a partir de pruebas anteriores en Windows.

## Fuera del alcance actual

Traducciones 1.1.0, reorganización de menús y regresión independiente.
La revisión de una API privada del antiguo add-on de regresión no es un
requisito de este candidato: ese add-on está excluido.
