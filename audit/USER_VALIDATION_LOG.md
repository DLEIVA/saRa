# Evidencia de validación saRa 1.0.0 — 27 de noviembre de 2026

Fuente: salidas y confirmaciones del desarrollador durante esta conversación.
Las capturas son evidencia visual de los casos mostrados; los archivos OMV,
HTML y PDF han sido confirmados operativos por el usuario, no inspeccionados
independientemente por el asistente.

## Última ejecución completa

```text
> testthat::test_local(".")
Duration: 39.4 s
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1878 ]
```

PASS cuenta comprobaciones, no bloques de test_that. Contextos:

| Contexto | Comprobaciones |
|---|---:|
| audit-review | 190 |
| audit-statistics | 242 |
| categorical-plot-order | 96 |
| categorical-table-visibility | 108 |
| continuous-shading | 124 |
| distribution-display-window | 34 |
| frequency-range | 11 |
| frequency-tables | 45 |
| gini-lorenz | 97 |
| mann-whitney-median-intervals | 376 |
| mann-whitney-superiority | 12 |
| mixed-effect-footnotes | 36 |
| normality-footnotes | 36 |
| normality-histograms | 15 |
| normality-plot-messages | 62 |
| numeric-frequency-tables | 39 |
| numeric-qq-message | 35 |
| ordinal-association-notes | 63 |
| ordinal-plot-theme | 76 |
| uniform-plot-jumps | 55 |
| uniform-support | 52 |
| wilcoxon-hl-confidence | 74 |

Los mensajes stat_bin sobre bins=30 son informativos: testthat contabilizó
cero advertencias. Las ejecuciones intermedias con fallos están superadas por
esta ejecución: los últimos fallos de Gini/Lorenz y visibilidad procedían de
la preparación de pruebas y de atributos de nombre en valores numéricos.

## Confirmaciones manuales más recientes

- Notas ordinales: constantes, cambio de filas/columnas, recuperación y pareja
  única; OMV, HTML y PDF generados y operativos.
- Gini y Gini normalizado: 1:5 → 0.2667/0.3333; iguales positivos → 0/0;
  0,0,0,10 → .75/1. Curvas de Lorenz coherentes, paleta de jamovi.
- Suma cero y negativos: índices NaN con nota y explicación en el gráfico;
  otras estadísticas conservadas. Restauración elimina avisos.
- Ausentes excluidos sin cambiar los resultados; único 7 → Gini 0,
  normalizado NaN con nota y curva sobre la diagonal.
- Gini/Lorenz: archivos OMV, HTML y PDF confirmados operativos.
- Visibilidad nominal: las tres tablas responden a sus respectivas opciones;
  selección individual de Cramér V, tau de Goodman-Kruskal y Theil; usuario
  confirmó funcionamiento tras la suite de 1878.
- Nombres con espacios, comienzo numérico, acentos y acento grave: tablas y
  barras/mosaico nominales; barras/heatmap/alluvial ordinales; Pearson r=1,
  dispersión, recta y los tres marginales numéricos. Usuario confirmó todo.
- Biblioteca: los ocho conjuntos abren correctamente y son operativos.

## Entorno y código

R 4.6.2; Ubuntu 22.04.5 LTS x86_64; testthat 3.3.2; jmvtools 28.4;
jmvcore 28.3; ggplot2 4.0.3; DescTools 0.99.60. R es la sesión de pruebas
en RStudio; no se ha verificado por separado el R integrado.

```text
getOption("jamovi_home") → NULL
jmvtools::check() → jamovi 2.7.30 found at /usr/bin/flatpak
flatpak info → Version: 2.7.27; Subject: Version 2.7.30 (19f193ffc130)
branch → fix/audit-review-1.0.0
HEAD → 2cfe7ee61c580950140c832a4b22163a90bba2f8
```

Se conserva la discrepancia entre los metadatos Flatpak y la detección del
compilador. Las referencias generadas a jamovi 28 no acreditan la versión GUI.
HEAD es la base, no un commit del candidato: el usuario tiene modificaciones,
eliminaciones y archivos nuevos pendientes de commit. El estado local del
usuario incluye tests/gui/ y una modificación del JSON histórico de nortest
que no están disponibles en la copia revisada por el asistente.

Las validaciones anteriores de distribuciones, resúmenes, normalidad,
Mann-Whitney y Wilcoxon constan en los documentos históricos. El estado
consolidado y sus límites están en VALIDATION_STATUS.json.

## Cierre del cotejo del árbol local

El usuario remitió LOCAL_FILE_COMPARISON.csv: 102 MATCH, 7 DIFFERENT y un
LOCAL_ONLY_NOT_REVIEWED. Después remitió saRa-cotejo-final.zip con las ocho
rutas y el registro histórico nortest. Revisadas las diferencias, se conserva
el código del usuario sin modificarlo. Las 110 huellas del inventario
reconciliado coinciden con las suministradas. El script de selectores pasa
en Node con controles simulados; no se afirma ejecución browser end-to-end.
El archivo de pruebas audit-review incorpora cuatro bloques adicionales;
el total reconciliado es 112 test_that. Esto corrige el recuento de 108 en
la copia anterior incompleta, sin cambiar las 1878 comprobaciones del usuario.
El registro mal denominado nortest contiene evidencia general de 16-11-2026
con 409 comprobaciones; se archiva intacto y se marca como sustituido.
Detalles: SOURCE_RECONCILIATION_2026-11-27.json.

## Aceptación del artefacto final y cierre previo al commit

El usuario confirma: «jmo final identificado, comprobación conjunta realizada».
La comprobación solicitada incluía reinicio/carga, reapertura OMV y HTML/PDF.
Salida aportada desde su terminal:

```text
sha256sum "saRa_1.0.0.jmo"
163731acf8c0cee8f838c565e1da270178c35fdbada5ccd15a96fb5a16906b96  saRa_1.0.0.jmo
```

El binario no se ha recibido ni rehasheado aquí: se registra la evidencia del
usuario. git status y git diff --stat concuerdan con las familias de archivos
revisadas. git diff --check señala únicamente espacios finales. Se conservan
en este candidato sin alterar fuentes ni regenerar el binario.
Solo queda revisar/stagear el conjunto y crear/registrar su commit.
