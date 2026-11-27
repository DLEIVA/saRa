# Revisión final de archivos — saRa 1.0.0

Fecha: 27 de noviembre de 2026. Revisión de la copia disponible en preparación,
no de una descarga íntegra del árbol actual del usuario. No se ha ejecutado
R ni jamovi aquí, ni se ha realizado commit, push o publicación.

## Resultado de la revisión disponible

| Comprobación | Resultado |
|---|---|
| Sintaxis R: fuentes, pruebas y scripts de auditoría | 58 archivos interpretados por tree-sitter sin errores de sintaxis |
| Definiciones YAML | 38 archivos interpretados; opciones, dependencias de visibilidad y referencias comprobadas |
| Controles de interfaz | 356 controles nominales comprobados contra opciones declaradas; sin nombres no declarados detectados |
| JavaScript fuente | catsummary.js y ordsummary.js pasan node --check |
| Registro del módulo | 12 análisis; cada uno tiene definiciones A/R/U y clases B/H disponibles |
| Versión | DESCRIPTION y manifiesto declaran 1.0.0 |
| Biblioteca | Ocho RData presentes; usuario confirma apertura y operatividad de los ocho |
| Regresión aplazada | No registrada; R/linreg.b.R, R/linreg.h.R y jamovi/linreg.a.yaml eliminados |
| Auxiliares de regresión | Conservados sin registrar, según decisión de alcance; no se añade un análisis de regresión |
| Dependencias | No se detectan usos de namespaces no declarados en el chequeo estático |
| Empaquetado | audit, img, logo SVG y elementos RStudio excluidos por .Rbuildignore; JMO/build ignorados por Git |
| Pruebas del usuario | 1878 comprobaciones pasan, 0 fallos/advertencias/omisiones |
| Revisión de formato Git | Quedan espacios al final de líneas en fuentes/encabezados ya existentes en el candidato; sin cambio funcional por esta consolidación |

No se detectaron incoherencias de registro, sintaxis o referencias en estas
comprobaciones. Esto no garantiza ausencia de defectos ni sustituye la
validación estadística/GUI. Se conservan alias lwd en dos curvas normales;
la modernización restante es mantenimiento de baja prioridad, no una nueva
corrección aplicada durante este cierre. La suite del usuario no reportó warnings.

## Archivos nuevos que deben conservarse al preparar el commit

- R/distribution-plots.R, R/frequency-tables.R, R/inequality.R,
  R/messages.R y R/normality-plot-messages.R son código fuente necesario.
- jamovi/js/catsummary.js y jamovi/js/ordsummary.js son manejadores fuente
  de la interfaz, no simples productos de compilación prescindibles.
- Conservar las pruebas nuevas de testthat, incluidas las correcciones más
  recientes de Gini/Lorenz y de visibilidad de tablas nominales.
- Conservar los cambios funcionales de YAML y sus encabezados generados
  correspondientes al código validado; no copiar encabezados antiguos.
- Conservar las tres eliminaciones aprobadas de entradas del add-on de regresión.

## Consolidación documental aplicada

Se han actualizado AUDIT_TRACKER.md/CSV, GUI_CHECKLIST.md, RELEASE_NOTES.md,
USER_VALIDATION_LOG.md, USER_VALIDATION_EVIDENCE.json y VALIDATION_STATUS.json.
Se mantiene el historial disponible en history/pre-consolidation-2026-11-27/.
El historial es la copia de los registros que estaban disponibles aquí;
no reemplaza los posibles cambios locales adicionales del usuario.
NEWS incorpora Gini/Lorenz, notas ordinales, mensajes agrupados legibles y
visibilidad nominal. Versión y fechas de compilación no se cambian en este paso.
VALIDATE_SOURCE.py escribe un informe propio y no destruye la evidencia runtime.
No se ha modificado ningún cálculo, definición funcional o gráfico en este paso.

## Diferencias de procedencia que impiden afirmar una revisión exacta completa

El git status suministrado por el usuario incluye tests/gui/ y una modificación
de audit/NORTEST_VALIDATION_2026-11-10.json. Sus contenidos actuales no están
en esta copia. Tampoco se ha cotejado byte a byte el resto del árbol del usuario;
los encabezados .h.R pueden diferir al regenerarse mediante jmvtools.
No se ha inspeccionado el JMO final ni se ha recibido su hash.

El commit 2cfe7ee61c580950140c832a4b22163a90bba2f8 es la base del trabajo.
No identifica el candidato con modificaciones y archivos nuevos pendientes.
No se recomienda cambiar de rama, limpiar el árbol o hacer staging global
sin revisar esos archivos.

## Cotejo para completar la revisión exacta

Desde la raíz del proyecto del usuario, tras aplicar estos documentos:

```r
source("audit/COMPARE_REVIEWED_FILES.R")
```

El script usa R base y tools: no instala dependencias, no altera las fuentes ni
realiza commits. Compara 109 archivos de fuentes, definiciones, JS, datos,
pruebas y configuración con REVIEWED_FILES_2026-11-27.csv. Normaliza CRLF a LF
solo para calcular huellas de texto; los binarios se comparan exactamente.
Guarda LOCAL_FILE_COMPARISON.csv y LOCAL_GIT_STATE.txt en audit/.
Las diferencias no se sustituyen automáticamente. LOCAL_ONLY_NOT_REVIEWED
identifica archivos adicionales (incluidos posibles productos generados).
Revisar además el JSON histórico nortest modificado. Compartir la salida y
los archivos distintos necesarios para cerrar el cotejo.

Tras resolver diferencias: congelar el código, identificar el JMO final y
hacer una prueba conjunta breve de instalación tras reinicio, reapertura OMV
y HTML/PDF. Los casos manuales ya aceptados no se repiten íntegramente.
Finalmente crear y registrar el nuevo commit del candidato.
