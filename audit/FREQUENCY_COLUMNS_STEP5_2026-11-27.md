# Paso 5 — Simplificación de todas las frecuencias univariantes

Decisión del usuario (27 de noviembre de 2026): seguir la sugerencia del último
audit y retirar proporciones y proporciones acumuladas en los tres análisis.
Esta decisión sustituye la alternativa anterior de conservar ambas escalas.

| Análisis | Columnas conservadas |
|---|---|
| Categórico | Nivel, conteo, porcentaje, odds |
| Ordinal | Nivel, conteo, conteo acumulado, porcentaje, porcentaje acumulado |
| Numérico | Valor, conteo, conteo acumulado, porcentaje, porcentaje acumulado |

Se eliminan las columnas prop y cumprop del auxiliar compartido, tanto de los
datos de salida como de su inicialización. Los porcentajes conservan el formato
pc de jamovi (almacenamiento interno como fracción). Se actualizan las notas y
sus marcadores de traducción: ya no mencionan columnas retiradas.

Los porcentajes usan observaciones no perdidas. La fila Missing mantiene su
conteo y deja vacíos porcentajes y acumulados. Se conservan el orden definido
para ordinales, el orden numérico ascendente, las categorías sin observaciones,
los conteos acumulados y los odds categóricos. Las tablas bivariantes no cambian.

## Comprobaciones

Se adaptan los casos existentes de denominadores, orden, valores cercanos,
constantes, datos vacíos, datos perdidos y actualización de filas. Un nuevo
bloque verifica las columnas exactas y valores en las tres APIs públicas.
El caso de traducción comprueba también el formato de ambos porcentajes.

Revisión estática R/YAML sin incidencias. No hay R en este entorno: este cambio
requiere ejecutar testthat en RStudio. PASS 2054 corresponde exclusivamente al
paso 4 anterior, confirmado por el usuario, y no valida este cambio nuevo.
La compilación e inspección del nuevo JMO quedan para después de esta ejecución.

## Respuesta al punto de frecuencias del audit (inglés)

Removed the redundant proportion and cumulative proportion columns from every
univariate frequency table (categorical, ordinal and numerical). Percentages
use non-missing observations. Cumulative counts and percentages are retained
for ordinal and numerical data; nominal tables retain odds and omit cumulative
columns. The notes and translation markers have been updated accordingly.

## Aplicación

En la rama fix/audit-submission-followup, copiar el contenido de archivos/
sobre el proyecto, conservando sus rutas. Reiniciar R y ejecutar:

```r
testthat::test_local(".")
```

El archivo LEEME.txt del ZIP resume estos pasos. No hacer todavía el commit ni
el envío: continuamos tratando los puntos que queden antes del registro final.
