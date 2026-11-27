# saRa 1.0.0 — candidato consolidado, 27 de noviembre de 2026

## Cambios incluidos

- Correcciones en resúmenes numéricos y ordinales, valores almacenados,
  medias/desviaciones geométricas, SP50/SP80 y tratamiento de ausentes.
- Frecuencias coherentes con el tipo de variable: acumuladas en ordinales y
  numéricas; tablas nominales sin acumuladas y opciones alineadas.
- Índices bivariantes revisados, empates y dirección ordinal, notas para
  índices no definidos y visibilidad de tablas nominales sin casillas activas.
- Gini empírico y normalizado con una casilla; curva de Lorenz independiente,
  ggplot2, tema/paleta de jamovi y explicaciones para datos no admisibles.
- Normality Tests ejecuta solo las pruebas seleccionadas; errores por celda,
  aviso de K-S sin corrección y Q-Q/ECDF por grupo sin bloquear grupos válidos.
- Ventanas de representación adaptadas, soporte uniforme, cuantiles infinitos,
  sombreado y marcadores discretos sin alterar probabilidades calculadas.
- Correcciones de Mann-Whitney y Wilcoxon: rangos, empates, dirección,
  superioridad, intervalos de medianas/HL y explicaciones con muestras mínimas.
- Gráficos y tablas compatibles con los nombres especiales comprobados.
- Biblioteca de ocho conjuntos; referencias y dependencias revisadas.

## Alcance y validación

No se añade inferencia a los módulos descriptivos univariantes/bivariantes.
El add-on de la regresión de jamovi está retirado del registro. Se conservan
auxiliares matemáticos sin registrar para desarrollo independiente futuro.
Traducciones 1.1.0 y reorganización del menú aplazadas.

Última suite del usuario: 1878 comprobaciones, 0 fallos/advertencias/omisiones.
GUI y exportaciones aceptadas para los casos registrados; los nombres
especiales y visibilidad final se confirmaron en GUI. Ver los límites y el
entorno en VALIDATION_STATUS.json. No es todavía una publicación: queda crear el commit del candidato. El
cotejo de los 110 archivos, la identificación del binario y la comprobación
conjunta final están completados.

## Cierre previo al commit

JMO final identificado y comprobación conjunta final confirmada por el usuario.
Nombre/hash y límites de evidencia en FINAL_ACCEPTANCE_2026-11-27.json.
Candidato listo para commit revisado; no publicado ni fusionado.
