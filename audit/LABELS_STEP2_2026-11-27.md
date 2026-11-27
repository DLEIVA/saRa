# Paso 2 — Rótulos y ortografía

Base: paso 1, validado por el usuario con PASS 1920, FAIL 0, WARN 0, SKIP 0 (19.7 s).
Rama prevista: `fix/audit-submission-followup`.

| Antes | Después |
|---|---|
| Blau | Blau index |
| Teachman | Teachman index |
| Variables Median confidence intervals | Median confidence intervals |
| Hodges-Lehmann Estimator CIs | Hodges-Lehmann confidence intervals |
| Descriptives Table / Descriptives | Descriptive statistics |
| Descriptives Plots / Descriptives plots | Descriptive plots |
| Z statistic | z statistic |
| Mann-Whitney U Test and effect sizes | Mann-Whitney U test and effect sizes |
| Wilcoxon T Test and effect sizes | Wilcoxon T test and effect sizes |
| Standardised values | Standardized values |

Se armonizan también las descripciones de Blau, Teachman y del rango interno mixto.
Se conserva title case en los encabezados de grupos de la GUI. Los símbolos, las
etiquetas y las direcciones ya corregidos en la revisión anterior se mantienen.
La clave de traducción Standardized values se reutiliza; se retira su duplicado
con la grafía Standardised. Se actualiza únicamente la expectativa existente del
rótulo Q-Q: no se añaden pruebas que se limiten a repetir los textos del código.

Verificación aquí: sintaxis R/YAML sin incidencias; comprobación de que el YAML
solo cambia títulos, etiquetas y descripciones. Identificadores, valores por
defecto, validadores, visibilidad y cálculos se conservan. R/testthat no se ejecuta
localmente aquí; la validación de este paso sigue pendiente en RStudio.

Aplicar el contenido de archivos/ sobre el proyecto que ya contiene el paso 1.
Reiniciar R y ejecutar `testthat::test_local(".")`. No hacer commit ni recompilar
el JMO hasta terminar y validar los siguientes bloques. La regeneración de .h.R
y la inspección final de los rótulos en jamovi se harán con la compilación final.

Incluye la actualización de los dos registros del paso 1 con el resultado real
aportado por el usuario. Este resultado no se atribuye al árbol nuevo del paso 2.

## Ejecución local aceptada

El usuario aporta PASS 1920, FAIL 0, WARN 0, SKIP 0, duración 19.8 s.
Corresponde al paso 2 antes de los cambios de tema del paso 3. La inspección
visual final de los rótulos continúa ligada a la recompilación final.
