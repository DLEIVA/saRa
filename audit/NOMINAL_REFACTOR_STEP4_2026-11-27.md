# Paso 4 — Descomposición del análisis nominal

Aplicar después del paso 3, en `fix/audit-submission-followup`.
Base validada por el usuario: PASS 2005, FAIL 0, WARN 0, SKIP 0 (21.4 s),
incluidas 85 comprobaciones del contexto categorical-plot-theme.

## Cambio

La .run nominal pasa de 156 líneas a 16. Ahora orquesta métodos independientes:

| Método | Líneas |
|---|---:|
| .populateFrequencyRows | 12 |
| .frequencyRow | 29 |
| .frequencyTotal | 25 |
| .populateAssociationIndices | 33 |
| .addYuleNotes | 8 |
| .populatePredictionIndices | 6 |

Se prepara una sola tabla de contingencia por ejecución. Las expresiones que
calculan conteos, esperados, porcentajes e índices se conservan. Las tres ramas
idénticas de notas Yule se agrupan en un bucle sobre las opciones seleccionadas.
Se retiran variables locales sin uso y el bucle sobre una lista de una sola tabla.
El único archivo R de ejecución modificado es R/bivcat.b.R; las opciones, la GUI,
los gráficos, la inicialización de la estructura y las definiciones YAML se conservan.

Este cambio completa la descomposición de las tres rutinas citadas en el último
audit: el motor numérico ya tiene auxiliares a nivel superior y un despachador;
.computeIndicesOrd ya delega en el auxiliar ordinal; ahora .run nominal también
se descompone. No se afirma que toda función del paquete tenga pocas líneas.

## Comprobaciones

Cuatro bloques de pruebas nuevos mediante la API pública del análisis:

- Conteos, esperados, totales y porcentajes de una tabla con categorías vacías
  y una observación incompleta; nombres de variables con espacios.
- Ejemplo conocido [[10,0],[5,5]]: χ²=20/3, V=sqrt(1/3), λ direccionales y
  λ simétrica=1/3, contrastada además con DescTools.
- Invariancia de índices al retener categorías vacías y al transponer la tabla;
  intercambio de las direcciones de λ, τ y Theil; notas Yule fuera de 2×2.
- Datos degenerados: frecuencias conservadas, índices indefinidos y mensaje
  claro cuando no queda ninguna observación completa.

Sintaxis R/YAML y esquema revisados sin incidencias (61 R, 38 YAML).
No se ha ejecutado R/testthat aquí. Tras corregir el auxiliar de pruebas, el
usuario validó este paso con PASS 2054, FAIL 0, WARN 0, SKIP 0 (22.6 s).

## Aplicación

Copiar el contenido de archivos/ sobre el proyecto y reiniciar R. Ejecutar:

```r
testthat::test_local(".")
```

Comunicar la salida antes de seguir. Mantener la rama actual y no hacer todavía
commit ni recompilar/publicar el JMO. El siguiente bloque será el registro de
cierre y la respuesta completa al audit; después, compilación e inspección GUI.

## Primera ejecución y corrección del auxiliar de pruebas

Resultado del usuario: PASS 2005, FAIL 4, WARN 0, SKIP 0 (22.3 s).
Los cuatro errores se producen durante init, en jmvcore::select, antes de .run.
El auxiliar de pruebas pasa rows=rows y cols=cols; resolveQuo captura esos símbolos
como nombres de columnas. Al no existir, select intenta asignar los nombres de
fila originales a un data.frame de cero filas y falla.

Se sustituye esa llamada por do.call(saRa::bivcat, list(...)), resolviendo primero
las cadenas con los nombres. Se mantienen los cuatro casos, sus nombres con
espacios, todas las opciones y las expectativas estadísticas. Esta corrección
no modifica ningún archivo R de ejecución. El resultado de la nueva ejecución
se registra a continuación.

## Ejecución corregida validada

El usuario confirmó PASS 2054, FAIL 0, WARN 0, SKIP 0 (22.6 s),
incluidas 49 comprobaciones de nominal-refactor. Se conserva arriba el historial
del primer intento y su corrección. La compilación y comprobación del nuevo JMO
siguen pendientes; continuamos con la simplificación de frecuencias solicitada.
