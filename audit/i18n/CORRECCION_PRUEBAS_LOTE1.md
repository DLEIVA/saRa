# Corrección de las pruebas de traducción — 11 noviembre 2026

El resultado comunicado fue FAIL 5, WARN 0, SKIP 0, PASS 289. El primer fallo sucede en la inicialización de tablas de regresión: la simulación del análisis no proporcionaba self$options$translate, que utiliza jmvcore::.(). Los otros cuatro fallos corresponden a tablas que no llegaron a crearse.

Se añade ese contrato al contexto simulado, manteniendo los cinco controles de inicialización/filas. Las pruebas de traducción ahora llaman a la función real jmvcore::.() con un traductor proporcionado por options; ya no sustituyen la función punto. No se eliminan ni se omiten pruebas, y no se cambia el código del módulo.

Aplicación: sustituir únicamente los dos archivos incluidos dentro de tests/testthat. Copiar también esta nota a audit/i18n. Mantener todos los archivos R del primer lote. Ejecutar testthat::test_local(".") y facilitar el nuevo resultado antes de compilar. Se mantienen 38 bloques. La corrección pasa la revisión estática de sintaxis; no se ha ejecutado R aquí.
