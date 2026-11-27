# Paso 1 — Marcado para traducción (saRa 1.0.0)

Rama local prevista: `fix/audit-submission-followup`.
Base: `main`, commit `a4f850848ccc76cb58f41bd714853f409de80de8`.

Este paso responde al apartado «User-visible strings aren't marked for translation»
del último audit. No cambia la versión ni añade catálogos de idiomas.

Cambios preparados:

- Marcadores literales en títulos y notas de tablas, ejes y mensajes de validación.
- Plantillas completas para porcentajes, percentiles, empates por variable y errores
  con valores; los datos y nombres del usuario quedan fuera de las claves.
- Registro extraíble de mensajes procedentes de auxiliares y callbacks que conservan
  la referencia al análisis, también en Q-Q, normalidad, Lorenz, distribuciones y rangos.
- Traducción de encabezados de frecuencias y de la fila de ausentes sin traducir una
  categoría del usuario que se llame literalmente «Missing».
- Errores de rangos: normalización antes de traducir, conservando las claves y NaN.
- Siete bloques nuevos de prueba con traductores simulados: propietario, etiquetas de
  datos, plantillas reordenables, Q-Q/normalidad, leyendas, validadores y errores de rangos.

Comprobación disponible aquí:

- Sintaxis de 59 archivos R y 38 YAML revisada, sin incidencias de esquema/dependencias.
- El extractor de jamovi reconoce los marcadores y las plantillas completas comprobadas.
  El catálogo temporal no forma parte del parche ni se instala.
- Los auxiliares de estadística numérica, desigualdad y regresión no cambian.
- No se ejecutó R/testthat/jamovi aquí. El anterior PASS 1878 corresponde al candidato
  anterior, no prueba este parche. Este paso queda preparado y pendiente de validación local.

Aplicación:

1. Mantener la rama `fix/audit-submission-followup` y copiar `archivos/` sobre el proyecto,
   conservando las subcarpetas R, tests y audit. No copiar el directorio archivos en sí.
2. En RStudio, reiniciar R (Session > Restart R).
3. Ejecutar desde la raíz del proyecto: `testthat::test_local(".")`.
4. Comunicar el resultado antes de avanzar al siguiente bloque. No hacer aún el commit
   ni recompilar/publicar el JMO en este paso.

Los cambios de rótulos/ortografía, estilos de gráficos, descomposición de funciones y
cierre documental se tratarán en los pasos siguientes; no se marcan aquí como resueltos.
La recompilación y validación final del nuevo JMO actualizarán su identidad al terminar
los cambios de código. El hash anterior no identifica un binario de este parche.

## Ejecución local aceptada

El usuario aporta la suite completa: PASS 1920, FAIL 0, WARN 0, SKIP 0, 19.7 s.
El contexto translation-output supera 40 comprobaciones. Este resultado corresponde
al árbol del paso 1 antes de los cambios de rótulos del paso 2 y sustituye la situación
pendiente descrita en la preparación anterior. No implica una ejecución independiente
ni la comprobación del JMO posterior.
