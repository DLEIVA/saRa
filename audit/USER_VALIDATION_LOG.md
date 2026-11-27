# Registro de validación — seguimiento del último audit

Fecha: 27 de noviembre de 2026. Evidencia aportada por el usuario desde RStudio
y jamovi en Ubuntu. Las confirmaciones no son una ejecución independiente aquí.

El candidato inicial se comprometió como cc15a0d y se integró en main; el
seguimiento parte de a4f8508 en la rama prevista fix/audit-submission-followup.
La rama y el HEAD locales finales aún deben cotejarse.

| Paso | Cambio | PASS | Duración (s) |
|---|---|---:|---:|
| 1 | Translation marking | 1920 | 19.7 |
| 2 | Labels and spelling | 1920 | 19.8 |
| 3 | Categorical plot themes | 2005 | 21.4 |
| 4 | Nominal refactor after test-helper correction | 2054 | 22.6 |
| 5 | Remove redundant frequency proportions | 2064 | 23.7 |
| 6 | Polar axes and legend after test metadata correction | 2088 | 21.6 |
| 7 | Internal doughnut percentages and adaptive tint correction | 2101 | 21.4 |
| 8 | User-requested multicolor jmv palette | 2107 | 22.5 |

Todos los resultados aceptados de la tabla tienen FAIL 0, WARN 0, SKIP 0.
Los avisos informativos de stat_bin sobre bins no aparecen como WARN de testthat.

## Intentos fallidos y correcciones conservados

- Paso 4: FAIL 4 / PASS 2005. El auxiliar de pruebas enviaba símbolos capturados
  como nombres de columnas; se corrigió con do.call. Después PASS 2054.
- Paso 6: FAIL 1 / PASS 2087. get_labels conservaba un atributo pos tras unname;
  la prueba se corrigió a as.character, conservando texto y orden. Después PASS 2088.
- Paso 7: FAIL 1 / PASS 2100. La mezcla fija dejaba colores demasiado claros;
  se corrigió el cálculo sin modificar la prueba. Después PASS 2101.
- Paso 8: la petición de la paleta multicolor jmv sustituye por completo el ajuste
  manual de luminosidad. Se retira el auxiliar y se verifica la API de jmvcore.

## Confirmaciones manuales del nuevo compilado

- Compilación e instalación confirmadas por el usuario.
- Frecuencias categóricas, ordinales y numéricas sin proporciones redundantes;
  conteos, porcentajes, acumulados y tratamiento de Missing aceptados.
- Gráficos con 12 categorías: capturas detectaron ejes internos y texto del anillo
  superpuesto; se corrigieron. La última captura muestra paleta multicolor,
  porcentajes dentro de la banda y leyenda solo de categorías.
- Tras solicitar guardado OMV, cierre/reapertura y exportación HTML/PDF con los
  cinco gráficos, el usuario confirmó: «Archivos generados y operativos».
- No se inspeccionaron aquí los archivos OMV/HTML/PDF completos.

## Identidad y cotejo final completados

La rama local es fix/audit-submission-followup y HEAD antes del nuevo commit es
a4f850848ccc76cb58f41bd714853f409de80de8.

El informe del usuario contiene 109 MATCH y cuatro DIFFERENT, sin ausentes ni
rutas adicionales. Se recibieron los cuatro .h.R y se cotejaron íntegramente:
solo cambian tres líneas de documentación de parámetros y cinco títulos de tablas,
coherentes con los YAML. Se conservan los encabezados regenerados del usuario y
se actualizan sus huellas; las 113 rutas quedan reconciliadas con la evidencia
aportada. Detalles: FOLLOWUP_SOURCE_RECONCILIATION_2026-11-27.json.

Nuevo JMO identificado por salida sha256sum del usuario:

```text
20bdc32ce37742b1b2e77e2735573381e5e23bc8819bbe804a5ded09b69fb865  saRa_1.0.0.jmo
```

El binario no se ha recibido ni rehasheado aquí. El hash anterior pertenece al
candidato anterior y se mantiene exclusivamente como histórico. Resta revisar
el staging, crear el commit del seguimiento y registrar su hash. No se declara
publicación o envío realizado.
