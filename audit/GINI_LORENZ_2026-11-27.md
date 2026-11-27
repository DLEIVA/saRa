# Gini y curva de Lorenz — 27 de noviembre de 2026

## Alcance
- Univariante numérica: una casilla Gini and normalized Gini entre CV y Range.
- Produce Gini empírico y normalizado G*n/(n-1) con n no perdidos.
- Lorenz curve: control independiente bajo Q-Q Plots, columna izquierda.
- Curva ggplot2 con paleta y tema de jamovi, diagonal de igualdad y ejes 0–1.
- Misma muestra y orden para ambos índices y la curva. No incorpora inferencia.

## Definiciones y casos límite
G = sum((2*i-n-1)*x_ordenada)/(n*sum(x)), i=1,...,n.
La curva incluye (0,0), con p=i/n y L=cumsum(x)/sum(x).
G equivale a 1 menos dos veces el área trapezoidal bajo la curva.
El normalizado multiplica G por n/(n-1).
Se escala por el máximo antes de sumar para evitar desbordamiento.
Valores perdidos se excluyen; ceros individuales se conservan.
Negativos, ausencia de observaciones y suma cero llevan explicación por salida.
La interfaz jmvcore rechaza infinitos antes de ejecutar el análisis; el auxiliar
también los rechaza. No se promete aislamiento entre variables para infinitos.
Con una observación positiva, G=0 y el normalizado es indefinido con nota.
Una variable no válida no bloquea los índices de las otras variables.

## Validación
- Comprobadas sintaxis de los 5 archivos R nuevos/modificados mediante parser
  tree-sitter y las 3 definiciones YAML con PyYAML.
- Comprobada posición de los controles y alineación de 37 metadatos estadísticos.
- Referencias numéricas comprobadas independientemente mediante diferencias
  absolutas por pares y fracciones exactas en Python.
- Pruebas R incluidas: comparación con DescTools::Gini(unbiased=FALSE/TRUE)
  y DescTools::Lc; área, extremos, transformación de escala, datos inválidos,
  casillas independientes, callbacks de gráficos, paleta, notas y recuperación.
- R no está disponible en el entorno de preparación. Ejecución R, compilación,
  GUI y exportaciones pendientes para este cambio. La suite de 1673 corresponde
  al candidato anterior, no valida este desarrollo.

## Fuentes de contraste
https://andrisignorell.github.io/DescTools/reference/Gini.html
https://andrisignorell.github.io/DescTools/reference/Lc.html

## Cierre de validación (sustituye los pendientes anteriores)

La suite completa del usuario pasa 1878 comprobaciones, incluidas las 97 de
Gini/Lorenz. Casos de referencia, igualdad, concentración, ceros, negativos,
ausentes, observación única, recuperación, paleta, disposición de la GUI y
OMV/HTML/PDF se confirmaron en la conversación. Lorenz Plots aparece como
encabezado sobre la casilla indentada bajo Q-Q Plots. No se ejecutó R aquí.
