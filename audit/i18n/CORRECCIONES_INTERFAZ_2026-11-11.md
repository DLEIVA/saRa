# saRa 1.1.0 — correcciones tras la revisión visual

Este lote responde a los 11 problemas comunicados. La evidencia anterior de 305 comprobaciones superadas corresponde al candidato previo, no a este nuevo lote.

## Correcciones

- Regresión: un contexto de traducción ligado explícitamente al paquete saRa utiliza el idioma del análisis principal, obtenido mediante los métodos públicos de Options. Las tablas mantienen las opciones del padre para visibilidad/dependencias, pero sus textos se resuelven con saRa antes de insertarse. El renderizador recibe el mismo contexto.
- Barras bivariadas: se elimina la consulta a freqtype, variable inexistente en esos métodos, y se usa la opción real yaxis. Este error se introdujo al preparar la traducción.
- Callbacks bivariados: se dibuja dentro del callback. Un fallo de dibujo deja un mensaje visible en lugar de una imagen sin recurso. No se ocultan fallos en las pruebas: también se comprueba construir los objetos de los cuatro gráficos.
- Frecuencias: Fr. absolutas solo en los selectores univariados de gráficos; se conserva Frecuencias absolutas en los resultados mediante contexto de traducción.
- Mann-Whitney: etiquetas de interfaz IC de medianas y Hodges-Lehmann (dif. localización). El porcentaje sigue en el campo de nivel de confianza existente.
- Distribuciones: F. masa probabilidad, F. densidad, F. distribución, F. supervivencia, Inversa f. distribución. En continuas se conserva f(x), porque una densidad no es Pr(X=x).

## Aplicar y comprobar

1. Confirmar la rama feature/spanish-1.1.0.
2. Copiar el contenido del ZIP acumulativo sobre la raíz del proyecto, sustituyendo solo los archivos incluidos y conservando los restantes.
3. Ejecutar testthat::test_local("."). Hay 44 bloques. Si aparecen fallos, conservar la salida y no considerar validado el lote.
4. Si pasa, ejecutar jmvtools::install(), reiniciar jamovi y abrir análisis NUEVOS en castellano.
5. Revisar los 11 puntos del informe adjunto. En barras bivariadas comprobar frecuencias y porcentajes; en ordinales comprobar barras, Heatmap y aluvial. Si aparece un mensaje dentro de un gráfico, comunicar su texto.
6. Revisar Regresión: títulos, notas, ejes, leyendas y tabla de influencia. Cambiar a inglés y comprobar que no quedan textos castellanos.
7. Exportar HTML/PDF, comprobar √ y guardar/reabrir .omv antes de cerrar la versión.

No hace falta generar de nuevo los catálogos. No fusionar con main antes de la validación.

## Estado

La revisión estática pasa, hay 572 entradas de catálogo con traducción y se conservan los marcadores. Se añaden cuatro bloques de pruebas (40 anteriores + 4 = 44). No se ha ejecutado R ni jamovi en este entorno. El informe JSON mantiene la evidencia anterior separada de la validación pendiente de estas correcciones.
