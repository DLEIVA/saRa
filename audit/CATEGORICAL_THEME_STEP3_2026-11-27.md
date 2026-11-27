# Paso 3 — Tema de los gráficos categóricos

Aplicar después de los pasos 1 y 2, en `fix/audit-submission-followup`.
Base local validada por el usuario: paso 2, PASS 1920, FAIL 0, WARN 0, SKIP 0,
19.8 s. Ese resultado no se atribuye a este parche, aún pendiente de ejecución R.

## Cambios

- Sectores y anillos: sustitución de theme_void completo por ajustes parciales
  para ocultar solo ejes, ticks y rejillas que no tienen sentido en coordenadas
  polares. Se conservan fondo, fuentes de título/leyenda y restantes opciones.
- Etiquetas de datos: tamaño, familia y estilo resueltos desde el tema recibido,
  con la API pública calc_element y conversión de puntos a milímetros. No se
  cambian los valores por defecto globales de ggplot2.
- Paletas: el tema se aplica antes de la escala manual ampliable. Así las escalas
  incluidas en un tema real de jamovi no sustituyen después la paleta de sectores
  y anillos, también con más de nueve categorías.
- Pareto: retirada de theme_classic y de fuentes fijas; las capas acumuladas usan
  el segundo color del tema. Conteos, orden y porcentajes acumulados se conservan.

## Correcciones detectadas al revisar el renderizado

El marcado del paso 1 introdujo dos nombres de función incorrectos:
`ggplot2::ylab.` y `jmvcore::createError.`. La sintaxis R acepta esos nombres y
la suite anterior no alcanzaba ambas rutas. Este parche restaura ylab y
createError con sus marcadores de traducción dentro del argumento.

La corrección de createError se limita a esa errata; no modifica la política
ni la preparación de datos del contraste Wilcoxon.

## Pruebas

Cuatro bloques nuevos:

1. Fuentes, tamaños y fondo del tema en los cinco gráficos categóricos, con
   construcción del grob completo y conservación de los datos preparados.
2. Paleta con doce categorías y un tema real de jamovi que aporta escalas;
   etiquetas de sectores/anillos oscuras y tamaño procedente del tema.
3. Colores y conteos acumulados del Pareto.
4. Resolución pública de llamadas pkg::función en Pareto y Wilcoxon, incluidas
   ramas que no se ejercitan con pares numéricos válidos.

Comprobación estática local: 60 archivos R y 38 YAML, sin incidencias. R/jamovi
no se ejecutan aquí. El paso está preparado, no validado mediante testthat.

API consultada: https://ggplot2.tidyverse.org/reference/calc_element.html

## Aplicación y comprobación

Copiar el contenido de archivos/ sobre el proyecto. Reiniciar R y ejecutar:

```r
testthat::test_local(".")
```

Pegar el resultado antes de continuar. No hacer aún commit ni recompilar el JMO.
Al compilar el conjunto final, revisar sectores, anillos y Pareto en dos temas
(incluido uno con fondo/rejilla diferentes) y paletas, también en HTML/PDF.

Se actualizan los registros del paso 2 con su ejecución local aceptada. Quedan
para los pasos posteriores la descomposición de funciones y el cierre documental.

## Ejecución local aceptada

El usuario aporta PASS 2005, FAIL 0, WARN 0, SKIP 0, duración 21.4 s.
El contexto categorical-plot-theme supera 85 comprobaciones. Corresponde al
paso 3 antes del refactor del paso 4. La revisión visual final queda pendiente.
