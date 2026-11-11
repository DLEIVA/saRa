# Preparación de traducción — primer lote

Fecha: 11 de noviembre de 2026. Rama de trabajo prevista: feature/spanish-1.1.0.

## Cambios

- Nuevo R/i18n.R: diccionario de mensajes literales extraíbles por jmvtools; recibe self explícitamente. Sin contexto conserva el inglés, y los diagnósticos externos desconocidos se conservan.
- R/linreg.b.R: títulos, notas y encabezados dinámicos preparados con .(); títulos de modelos con plantilla completa.
- R/regression-diagnostics.R: etiquetas, notas, leyendas y errores visibles se resuelven al dibujar, usando el propietario saRa. No se guarda self dentro del estado de las imágenes.
- R/utils.R y llamadores: contexto explícito en Q-Q, errores de pruebas de rangos, errores de distribuciones y mensajes alternativos de gráficos de medianas.
- Tres nuevos bloques de prueba comprueban contexto explícito, conservación de diagnósticos externos y datos idénticos en gráficos con etiquetas diferentes.

## Estado

La revisión estática de sintaxis es satisfactoria y 31 definiciones de helpers se conservan idénticas, incluidos los cálculos de regresión, normalidad, rangos y la carga diferida de nortest. No se han ejecutado R ni jamovi aquí. La suite pasa de 35 a 38 bloques: la validación anterior de 274 comprobaciones NO valida estos cambios nuevos.

Este lote prepara textos; todavía no contiene traducción castellana ni catálogo es.po. Los restantes textos dinámicos de otros análisis siguen pendientes. La versión fuente continúa en 1.0.0 hasta preparar el candidato 1.1.0.

## Aplicación y comprobación

1. En el proyecto, ejecutar git branch --show-current y confirmar feature/spanish-1.1.0.
2. Copiar las carpetas R, tests y audit del ZIP sobre las del proyecto. Mantener todos los demás archivos. No borrar la carpeta audit ni sustituir el proyecto completo.
3. En consola R: testthat::test_local("."). Guardar la salida y revisar errores/advertencias/omisiones.
4. Si las pruebas terminan bien: jmvtools::install(). Reiniciar jamovi y comprobar normalidad, ACF, gráficos de residuos e influencia, Q-Q de resúmenes y gráficos de medianas.
5. Los textos deben seguir apareciendo en inglés: todavía no hay catálogo castellano. Comprobar que todos los resultados siguen generándose, incluido el funcionamiento sin moretests.
6. Después de validar, commit y push únicamente en la rama de traducción. No fusionar con main todavía.

## Próxima fase

Completar el marcado en los otros análisis, generar el catálogo oficial mediante jmvtools y traducir es.po; después comprobar ambos idiomas y las exportaciones.
