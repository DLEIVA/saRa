# Inventario de traducción de saRa 1.1.0

Código de referencia: `2cfe7ee61c580950140c832a4b22163a90bba2f8`. Fecha: 11 de noviembre de 2026.

Inventario de preparación; todavía no es una traducción ni un catálogo validado por jmvtools. No cambia el código del módulo ni su versión.

| Archivo | YAML | R marcado con .() | R candidato | R interno/etiqueta corta por revisar |
|---|---:|---:|---:|---:|
| `R/bivcat.b.R` | 0 | 0 | 19 | 110 |
| `R/bivmix.b.R` | 0 | 3 | 26 | 263 |
| `R/bivnum.b.R` | 0 | 0 | 0 | 42 |
| `R/bivord.b.R` | 0 | 0 | 22 | 111 |
| `R/catsummary.b.R` | 0 | 0 | 8 | 190 |
| `R/contvars.b.R` | 0 | 0 | 36 | 449 |
| `R/discvars.b.R` | 0 | 0 | 35 | 289 |
| `R/linreg.b.R` | 0 | 0 | 24 | 139 |
| `R/numeric-statistics.R` | 0 | 0 | 0 | 37 |
| `R/numsummary.b.R` | 0 | 0 | 20 | 183 |
| `R/ordsummary.b.R` | 0 | 0 | 8 | 177 |
| `R/regression-diagnostics.R` | 0 | 0 | 23 | 42 |
| `R/testnorm.b.R` | 0 | 0 | 4 | 55 |
| `R/umw.b.R` | 0 | 3 | 9 | 56 |
| `R/utils.R` | 0 | 0 | 32 | 43 |
| `R/wilcoxt.b.R` | 0 | 11 | 11 | 51 |
| `jamovi/0000.yaml` | 60 | 0 | 0 | 0 |
| `jamovi/bivcat.a.yaml` | 42 | 0 | 0 | 0 |
| `jamovi/bivcat.r.yaml` | 47 | 0 | 0 | 0 |
| `jamovi/bivcat.u.yaml` | 14 | 0 | 0 | 0 |
| `jamovi/bivmix.a.yaml` | 62 | 0 | 0 | 0 |
| `jamovi/bivmix.r.yaml` | 8 | 0 | 0 | 0 |
| `jamovi/bivmix.u.yaml` | 17 | 0 | 0 | 0 |
| `jamovi/bivnum.a.yaml` | 22 | 0 | 0 | 0 |
| `jamovi/bivnum.r.yaml` | 18 | 0 | 0 | 0 |
| `jamovi/bivnum.u.yaml` | 8 | 0 | 0 | 0 |
| `jamovi/bivord.a.yaml` | 31 | 0 | 0 | 0 |
| `jamovi/bivord.r.yaml` | 27 | 0 | 0 | 0 |
| `jamovi/bivord.u.yaml` | 12 | 0 | 0 | 0 |
| `jamovi/catsummary.a.yaml` | 21 | 0 | 0 | 0 |
| `jamovi/catsummary.r.yaml` | 9 | 0 | 0 | 0 |
| `jamovi/catsummary.u.yaml` | 7 | 0 | 0 | 0 |
| `jamovi/contvars.a.yaml` | 37 | 0 | 0 | 0 |
| `jamovi/contvars.r.yaml` | 20 | 0 | 0 | 0 |
| `jamovi/contvars.u.yaml` | 13 | 0 | 0 | 0 |
| `jamovi/discvars.a.yaml` | 36 | 0 | 0 | 0 |
| `jamovi/discvars.r.yaml` | 20 | 0 | 0 | 0 |
| `jamovi/discvars.u.yaml` | 13 | 0 | 0 | 0 |
| `jamovi/linreg.a.yaml` | 2 | 0 | 0 | 0 |
| `jamovi/numsummary.a.yaml` | 55 | 0 | 0 | 0 |
| `jamovi/numsummary.r.yaml` | 6 | 0 | 0 | 0 |
| `jamovi/numsummary.u.yaml` | 16 | 0 | 0 | 0 |
| `jamovi/ordsummary.a.yaml` | 39 | 0 | 0 | 0 |
| `jamovi/ordsummary.r.yaml` | 6 | 0 | 0 | 0 |
| `jamovi/ordsummary.u.yaml` | 13 | 0 | 0 | 0 |
| `jamovi/testnorm.a.yaml` | 23 | 0 | 0 | 0 |
| `jamovi/testnorm.r.yaml` | 29 | 0 | 0 | 0 |
| `jamovi/testnorm.u.yaml` | 9 | 0 | 0 | 0 |
| `jamovi/umw.a.yaml` | 38 | 0 | 0 | 0 |
| `jamovi/umw.r.yaml` | 56 | 0 | 0 | 0 |
| `jamovi/umw.u.yaml` | 13 | 0 | 0 | 0 |
| `jamovi/wilcoxt.a.yaml` | 28 | 0 | 0 | 0 |
| `jamovi/wilcoxt.r.yaml` | 39 | 0 | 0 | 0 |
| `jamovi/wilcoxt.u.yaml` | 14 | 0 | 0 | 0 |

## Cómo interpretar el inventario

- `yaml_catalog_candidate`: textos de campos visibles; la extracción oficial decidirá su incorporación al catálogo.
- `already_marked`: literal ya envuelto en `.()`; todavía hay que comprobar el contexto de traducción.
- `candidate_review`: posible texto visible de R que requiere revisión y marcado.
- `internal_or_short_label_review`: códigos, claves, formatos, nombres de funciones y etiquetas cortas; no traducir automáticamente.
- Los recuentos representan apariciones, no mensajes únicos ni porcentaje de cobertura.

## Preparación del código

1. Conservar las cadenas originales en inglés y las claves internas. No traducir `name`, nombres de opciones, columnas internas, métodos ni identificadores de estado.
2. En métodos R6, marcar los literales visibles con `.()`; el NAMESPACE ya importa selectivamente esta función.
3. En helpers de `utils.R`, `numeric-statistics.R` y `regression-diagnostics.R`, pasar contexto explícito o recibir etiquetas traducidas desde el análisis. No añadir `.()` sin `self`.
4. Para mensajes dinámicos, traducir frases completas con marcadores, no concatenar fragmentos traducidos. Conservar nombres de variables aportados por el usuario.
5. En el add-on de Regresión, resolver las traducciones en el contexto de saRa antes de insertar resultados en el árbol de jmv; comprobar ambos idiomas en el renderizado.
6. Las excepciones de paquetes externos pueden conservar mensajes originales; traducir las explicaciones propias de saRa y no ocultar el diagnóstico técnico.
7. Mantener los cambios de presentación separados de los cálculos y de las claves usadas por las pruebas.

## Archivos prioritarios

- `R/regression-diagnostics.R`: ejes ACF, residuos, escala-localización, Q–Q e influencia; leyendas, notas y errores del renderizado.
- `R/linreg.b.R`: títulos y notas de tablas creadas dinámicamente, etiquetas de diagnósticos y títulos de gráficos.
- `R/utils.R`: validación, errores compartidos y etiquetas de gráficos descriptivos.
- `R/numeric-statistics.R`: textos generados por las rutinas compartidas de resúmenes.
- Restantes `R/*.b.R`: textos dinámicos y llamadas `.()` existentes.
- `jamovi/*.a.yaml`, `*.r.yaml`, `*.u.yaml`: catálogos de controles y resultados.

## Comprobaciones necesarias

- Generar y comparar el catálogo con `jmvtools::i18nCreate("catalog")` en RStudio después del marcado.
- Crear `es` y traducir durante el siguiente paso, no mediante sustitución de los originales.
- Comprobar acentos, símbolos, interpolación, espacios y texto largo.
- Ejecutar la suite estadística y verificar visualmente ambos idiomas, exportaciones y reapertura de .omv.

## Literales R candidatos, por archivo

### R/bivcat.b.R

- Línea 30: "No complete observations are available for the selected variables"
- Línea 129: "Association indices require at least two observed levels in each variable and complete observations"
- Línea 149: "Indices can only be computed for 2x2 tables."
- Línea 154: "Indices can only be computed for 2x2 tables."
- Línea 159: "Indices can only be computed for 2x2 tables."
- Línea 205: "% within row"
- Línea 205: "% within column"
- Línea 205: "% of total"
- Línea 229: "(obs && (exp || pcRow || pcCol || pcTot))"
- Línea 290: "(obs && (exp || pcRow || pcCol || pcTot))"
- Línea 323: "Total"
- Línea 329: "Total"
- Línea 336: "Total"
- Línea 344: "Total"
- Línea 352: "Total"
- Línea 372: "Total"
- Línea 502: "Percentages of total"
- Línea 504: "Percentages within "
- Línea 550: "Total"

### R/bivmix.b.R

- Línea 25: "Mean"
- Línea 25: "Median"
- Línea 25: "Mode"
- Línea 25: "Geometric mean"
- Línea 28: "% trimmed mean"
- Línea 30: "% winsorized mean"
- Línea 31: "Variance"
- Línea 31: "Standard deviation"
- Línea 31: "Geometric standard deviation"
- Línea 32: "Mean deviation"
- Línea 33: "Quartile dispersion"
- Línea 34: "% Inner range"
- Línea 35: "Pearson's \\u03B2\\u2081"
- Línea 36: "Fisher's \\u03B3\\u2081"
- Línea 36: "Pearson's \\u03B2\\u2082"
- Línea 37: "Fisher's \\u03B3\\u2082"
- Línea 55: "Cohen's d"
- Línea 55: "Point-biserial Correlation"
- Línea 55: "Probability of superiority"
- Línea 56: "η²_H (Kruskal–Wallis)"
- Línea 77: "Select a Group by variable"
- Línea 684: "More than one mode exists, only the first is reported"
- Línea 692: "Geometric mean can only be computed with positive values (>0)"
- Línea 700: "Geometric std. deviation can only be computed with positive values (>0)"
- Línea 745: "More than two levels, effect size indicator is not reported"
- Línea 834: "Percentile "

### R/bivord.b.R

- Línea 31: "Select ordinal variables with ordered levels"
- Línea 33: "Select ordinal variables with ordered levels"
- Línea 37: "No complete observations are available for the selected variables"
- Línea 132: "Concordant pairs"
- Línea 132: "Discordant pairs"
- Línea 132: "Ties in "
- Línea 133: "Ties in "
- Línea 133: "Ties in "
- Línea 133: " and "
- Línea 170: "% within row"
- Línea 170: "% within column"
- Línea 170: "% of total"
- Línea 193: "(obs && (pcRow || pcCol || pcTot))"
- Línea 254: "(obs && (pcRow || pcCol || pcTot))"
- Línea 287: "Total"
- Línea 293: "Total"
- Línea 301: "Total"
- Línea 309: "Total"
- Línea 329: "Total"
- Línea 463: "Percentages of total"
- Línea 465: "Percentages within "
- Línea 553: "Total"

### R/catsummary.b.R

- Línea 18: "Mode"
- Línea 18: "Variation ratio"
- Línea 84: "Cumulative counts"
- Línea 90: "Cumulative proportions"
- Línea 93: "% of Total"
- Línea 96: "Cumulative %"
- Línea 328: "More than one mode exists, only the first is reported"
- Línea 596: "Cumulative percent (%)"

### R/contvars.b.R

- Línea 30: "Scale, rate and degrees of freedom must be positive"
- Línea 34: "Uniform minimum must be smaller than maximum"
- Línea 136: "Prob = "
- Línea 238: "Normal: "
- Línea 240: "Exponential: "
- Línea 240: "Uniform: "
- Línea 291: "Normal: "
- Línea 293: "Exponential: "
- Línea 293: "Uniform: "
- Línea 315: "Pr(X \\u2264 "
- Línea 352: "Normal: "
- Línea 354: "Exponential: "
- Línea 354: "Uniform: "
- Línea 377: "Pr(X > "
- Línea 405: "Normal: "
- Línea 407: "Exponential: "
- Línea 407: "Uniform: "
- Línea 475: "Normal: "
- Línea 477: "Exponential: "
- Línea 477: "Uniform: "
- Línea 573: " \\\\phantom{xx} "
- Línea 590: "Random variable values must be a real number ("
- Línea 594: "Random variable values must be positive ("
- Línea 600: "Random variable values must be between "
- Línea 600: " and "
- Línea 612: "Random variable values must be a real number between 0 and 1("
- Línea 616: "Quantile values must be between 0 and 1 ("
- Línea 628: "Random variable value must be a positive number ("
- Línea 632: "Random variable value must be between "
- Línea 632: " and "
- Línea 644: "X1 must be a positive number ("
- Línea 648: "X1 must be between "
- Línea 648: " and "
- Línea 660: "X2 must be a positive number ("
- Línea 664: "X2 must be between "
- Línea 664: " and "

### R/discvars.b.R

- Línea 35: "Negative Binomial"
- Línea 116: "Prob = "
- Línea 204: "Binomial: "
- Línea 205: "Poisson: "
- Línea 205: "Negative Binomial: "
- Línea 206: "Geometric: "
- Línea 206: "Hypergeometric: "
- Línea 227: "Pr(X = "
- Línea 252: "Binomial: "
- Línea 253: "Poisson: "
- Línea 253: "Negative Binomial: "
- Línea 254: "Geometric: "
- Línea 254: "Hypergeometric: "
- Línea 275: "Pr(X \\u2264 "
- Línea 300: "Binomial: "
- Línea 301: "Poisson: "
- Línea 301: "Negative Binomial: "
- Línea 302: "Geometric: "
- Línea 302: "Hypergeometric: "
- Línea 323: "Pr(X > "
- Línea 347: "Binomial: "
- Línea 348: "Poisson: "
- Línea 348: "Negative Binomial: "
- Línea 349: "Geometric: "
- Línea 349: "Hypergeometric: "
- Línea 407: "Binomial: "
- Línea 408: "Poisson: "
- Línea 408: "Negative Binomial: "
- Línea 409: "Geometric: "
- Línea 409: "Hypergeometric: "
- Línea 472: " \\\\phantom{xx} "
- Línea 488: "Random variable values must be positive integers ("
- Línea 492: "Random variable values must be positive integers ("
- Línea 504: "Random variable values must be a real number between 0 and 1("
- Línea 508: "Quantile values must be real numbers between 0 and 1 ("

### R/linreg.b.R

- Línea 15: "Statistic"
- Línea 23: "Table obtained with the saRa module."
- Línea 30: "- Model"
- Línea 49: "Specification Test"
- Línea 53: "Ramsey RESET"
- Línea 54: "Table obtained with the saRa module. Tests squared and cubed fitted values for functional-form misspecification."
- Línea 56: "Normality Tests"
- Línea 57: "Kolmogorov-Smirnov (Lilliefors)"
- Línea 58: "Kolmogorov-Smirnov uses the Lilliefors correction for estimated mean and variance. Residual normality diagnostics."
- Línea 60: "Statistic"
- Línea 64: "Heteroskedasticity Tests"
- Línea 65: "Koenker (studentized Breusch-Pagan)"
- Línea 66: "Null hypothesis: constant error variance. GQ: two-sided, ordered by fitted values, split at 50%, no central omission. HMC: ordered by fitted values, split at 50%; 5,000 simulations, seed 123456. BP statistic is chi-square, GQ is F; HMC is a residual sum-of-squares fraction."
- Línea 71: "Durbin-Watson Tests by Lag"
- Línea 72: "Lags 1-10, capped at N-1. Two-sided p-values from residual bootstrapping with 5,000 resamples; seed = 123456. Complete-case row order; p-values are not adjusted across lags."
- Línea 73: "Lag"
- Línea 74: "Autocorrelation"
- Línea 77: "Residual ACF"
- Línea 80: "Residuals vs Fitted"
- Línea 81: "Normal Q-Q with Residual Histogram"
- Línea 81: "Influence Plot"
- Línea 83: "Influence Indices (Noteworthy Cases)"
- Línea 84: "Same noteworthy rule as car::influencePlot(): union of the two largest absolute studentized residuals, leverage values and Cook's distances. Case labels identify original fitted rows; selection is diagnostic, not an exclusion rule."
- Línea 85: "Case"

### R/numsummary.b.R

- Línea 20: "Mean"
- Línea 20: "Median"
- Línea 20: "Mode"
- Línea 20: "Geometric mean"
- Línea 23: "% trimmed mean"
- Línea 25: "% winsorized mean"
- Línea 26: "Variance"
- Línea 26: "Standard deviation"
- Línea 26: "Geometric standard deviation"
- Línea 27: "Mean deviation"
- Línea 28: "Quartile dispersion"
- Línea 29: "% inner range"
- Línea 30: "Pearson's \\u03B2\\u2081"
- Línea 31: "Fisher's \\u03B3\\u2081"
- Línea 31: "Pearson's \\u03B2\\u2082"
- Línea 32: "Fisher's \\u03B3\\u2082"
- Línea 436: "More than one mode exists, only the first is reported"
- Línea 444: "Geometric mean can only be computed with positive values (>0)"
- Línea 452: "Geometric std. deviation can only be computed with positive values (>0)"
- Línea 483: "Percentile "

### R/ordsummary.b.R

- Línea 19: "Median"
- Línea 19: "Mode"
- Línea 20: "Quartile dispersion"
- Línea 181: "More than one mode exists, only the first is reported"
- Línea 373: "Percentiles must be numbers between 0 and 100"
- Línea 388: "Percentile "
- Línea 401: "% Inner range"
- Línea 432: "Ordinal summaries require numeric stored values."

### R/regression-diagnostics.R

- Línea 25: "This diagnostic is available for unweighted OLS models only."
- Línea 47: "Durbin-Watson tests require a fitted model with finite, non-zero residuals."
- Línea 49: "Bootstrap Durbin-Watson tests are available for unweighted OLS models only."
- Línea 61: "Residual ACF requires at least two finite, non-constant residuals."
- Línea 90: "Note. Plot obtained with the saRa module."
- Línea 102: "\\nRow order; "
- Línea 102: " automatic lags.\\nApproximate 95% white-noise bounds."
- Línea 107: "Lag"
- Línea 107: "Autocorrelation"
- Línea 119: "Fitted values"
- Línea 119: "Residuals"
- Línea 127: "Fitted values"
- Línea 127: "Sqrt(|standardized residual|)"
- Línea 133: "Theoretical normal quantiles"
- Línea 133: "Standardized residual quantiles"
- Línea 144: "Cook's D"
- Línea 146: "Leverage (hat value)"
- Línea 146: "Studentized residuals"
- Línea 147: "Unknown regression plot type"
- Línea 152: "Cook's D"
- Línea 160: "Plot data unavailable. Reselect Residual plots to refresh."
- Línea 165: "Plot could not be drawn:"
- Línea 168: "Note. Plot obtained with the saRa module."

### R/testnorm.b.R

- Línea 16: "Grouping variable must not also be a dependent variable"
- Línea 46: "Normal parameters are estimated from these data; this p-value is uncorrected. Use the Lilliefors test for corrected inference."
- Línea 259: "Standardised values"
- Línea 260: "Cumulative probability"

### R/umw.b.R

- Línea 31: "Grouping variable '{a}' must not also be a dependent variable"
- Línea 40: "Grouping variable '{a}' must have exactly 2 levels"
- Línea 46: "Grouping variable '{a}' has less than 2 levels after missing values are excluded"
- Línea 175: "Group 1"
- Línea 175: "Group 2"
- Línea 192: "H\\u2090 \\u03BE\\u2009<sub>{}</sub> > \\u03BE\\u2009<sub>{}</sub>"
- Línea 194: "H\\u2090 \\u03BE\\u2009<sub>{}</sub> < \\u03BE\\u2009<sub>{}</sub>"
- Línea 196: "H\\u2090 \\u03BE\\u2009<sub>{}</sub> \\u2260 \\u03BE\\u2009<sub>{}</sub>"
- Línea 199: "Number of replicates in Bootstrap CI(s): "

### R/utils.R

- Línea 10: "Binomial size must be a non-negative integer and probability between 0 and 1"
- Línea 15: "Poisson rate must be non-negative"
- Línea 21: "Negative binomial size must be positive and probability greater than 0 and at most 1"
- Línea 26: "Geometric probability must be greater than 0 and at most 1"
- Línea 32: "Hypergeometric counts must be non-negative integers, the population positive, and draws no larger than the population"
- Línea 35: "Unknown discrete distribution"
- Línea 38: "These parameters require too many plotting points; use less extreme values"
- Línea 50: "Infinite paired differences are not supported"
- Línea 67: "Requires two observed groups and finite values"
- Línea 80: "Requires finite observations"
- Línea 170: "Confidence level must be between zero and one"
- Línea 205: "Theoretical quantiles"
- Línea 205: "Standardised values"
- Línea 212: "grouping factor must have exactly 2 levels"
- Línea 212: "One or both groups do not contain enough observations"
- Línea 213: "not enough observations"
- Línea 213: "One or both groups do not contain enough observations"
- Línea 214: "data are essentially constant"
- Línea 214: "All observations are tied"
- Línea 215: "cannot compute confidence interval when all observations are tied"
- Línea 215: "All observations are tied"
- Línea 226: "Unknown median confidence-interval method"
- Línea 291: "Requires at least two non-missing observations"
- Línea 292: "Infinite values are not supported"
- Línea 293: "Requires a non-constant variable"
- Línea 304: "Unknown normality test"
- Línea 311: "Variable is not numeric"
- Línea 312: "Variable contains infinite values"
- Línea 319: "Unknown Mann-Whitney output"
- Línea 340: "Median plot values and confidence intervals are not aligned"
- Línea 348: "No finite medians are available for this plot"
- Línea 371: "Plot could not be drawn:"

### R/wilcoxt.b.R

- Línea 115: "not enough 'x' observations"
- Línea 117: "missing value where TRUE/FALSE needed"
- Línea 119: "cannot compute confidence interval when all observations are tied"
- Línea 121: "'y' must be numeric"
- Línea 151: "Positive differences"
- Línea 151: "Negative differences"
- Línea 151: "Tied pairs"
- Línea 201: "H\\u2090 \\u03BE\\u2009<sub>{}</sub> > 0"
- Línea 203: "H\\u2090 \\u03BE\\u2009<sub>{}</sub> < 0"
- Línea 205: "H\\u2090 \\u03BE\\u2009<sub>{}</sub> \\u2260 0"
- Línea 208: "Number of replicates in Bootstrap CI(s): "

