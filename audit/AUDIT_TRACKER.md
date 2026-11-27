# Registro consolidado de la auditoría — saRa 1.0.0

Fecha: 27 de noviembre de 2026. Rama del usuario: fix/audit-review-1.0.0.
Base: 2cfe7ee61c580950140c832a4b22163a90bba2f8. Candidato aún sin commit propio.

## Estado actual

La última suite del usuario pasa 1878 comprobaciones, sin fallos, advertencias
ni omisiones (39.4 s). Los bloques manuales registrados se han aceptado en
la instalación Linux indicada en USER_VALIDATION_LOG.md. El cotejo posterior de los 110 archivos y la identificación del JMO final
constan en los registros de cierre; no se afirma inspección independiente
del binario ni de las exportaciones.

Este archivo sustituye los pendientes acumulativos anteriores. Se conservan
intactos en history/pre-consolidation-2026-11-27/. El registro A01–A29 se
mantiene en AUDIT_TRACKER.csv; sus estados diferencian corrección funcional,
comprobación estática y tareas de mantenimiento.

| Bloque | Evidencia actual |
|---|---|
| Resúmenes y frecuencias | Casos y exportaciones confirmados en las revisiones previas |
| Distribuciones | Casos registrados, colas, cuantiles infinitos, soporte y recuperación aceptados |
| Normalidad | Tablas, Q-Q/ECDF, grupos válidos e inválidos, mensajes legibles y exportaciones aceptados |
| Mann-Whitney/Wilcoxon | Correcciones, intervalos y casos límite registrados aceptados |
| Asociaciones numéricas/ordinales | Contraste Rj, empates, dirección, constantes y recuperación aceptados |
| Notas ordinales | Pareja única y constantes; OMV/HTML/PDF aceptados |
| Gini y Lorenz | 97 comprobaciones automáticas; casos manuales y exportaciones aceptados |
| Visibilidad nominal | 108 comprobaciones; funcionamiento GUI confirmado |
| Nombres especiales | Gráficos bivariantes nominales, ordinales y numéricos confirmados |
| Biblioteca | Ocho conjuntos abiertos y operativos |

## Cotejo del código del usuario completado

Se recibieron y revisaron las siete diferencias y el script adicional. Los
110 archivos de código, configuración, pruebas y datos del inventario
reconciliado coinciden con las huellas suministradas por el usuario. Se
conservan todas sus versiones: las diferencias contienen correcciones ya
validadas, el manifiesto de los ocho datos, referencias y regeneración de
encabezados. Se ejecutó correctamente el script Node de selectores.
El registro nortest de nombre antiguo contiene validación general del
16 de noviembre (409 comprobaciones); se preserva y marca como histórico.
El código reconciliado contiene 112 bloques test_that; la ejecución del
usuario contabiliza 1878 comprobaciones. Son magnitudes distintas.

## Cierre de validación completado; commit pendiente

El usuario ha identificado saRa_1.0.0.jmo y confirmado la comprobación
conjunta final (reinicio/carga, OMV y HTML/PDF). SHA-256 registrado en
FINAL_ACCEPTANCE_2026-11-27.json. El binario no se ha inspeccionado aquí.
La revisión del último git status/stat no incorpora familias nuevas fuera
del alcance revisado; diff --check solo señala espacios finales.

Solo queda revisar el staging y crear/registrar el commit del candidato.
El binario JMO se conserva como entregable externo al commit de fuentes.
No se ha realizado push, merge ni publicación. Traducciones, menú y
regresión independiente continúan aplazados.

## Evidencia y revisión

- USER_VALIDATION_LOG.md y USER_VALIDATION_EVIDENCE.json: evidencia del usuario.
- VALIDATION_STATUS.json: estado consolidado, entorno y límites.
- SOURCE_REVIEW_2026-11-27.json: revisión estática de la copia disponible.
- FINAL_FILE_REVIEW_2026-11-27.md: revisión final de archivos y limitaciones.
- GUI_CHECKLIST.md: comprobaciones aceptadas y cierre pendiente.

VALIDATE_SOURCE.py genera un informe independiente. Nunca sustituye resultados
runtime ni vuelve a marcar como pendiente lo ya validado por el usuario.
