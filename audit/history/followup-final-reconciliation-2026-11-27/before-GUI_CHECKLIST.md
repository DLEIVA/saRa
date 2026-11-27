# Validación GUI — seguimiento del último audit

Evidencia: confirmaciones y capturas del usuario en Ubuntu; no ejecución GUI independiente.

| Comprobación del nuevo compilado | Estado |
|---|---|
| Compilación e instalación | Confirmadas |
| Frecuencias categóricas, ordinales y numéricas sin proporciones redundantes | Confirmadas |
| Porcentajes válidos, acumulados y Missing en el ejemplo 1,1,2,3,NA | Confirmados |
| Sectores y anillo con 12 categorías y paleta multicolor jmv | Captura aportada |
| Ejes y nombres internos ocultos en ambos gráficos | Captura aportada |
| Porcentajes dentro del anillo y leyenda solo de categorías | Captura aportada |
| Conjunto de cinco gráficos: guardado OMV y cierre/reapertura | Usuario confirma archivos operativos tras la solicitud |
| Exportaciones HTML y PDF | Usuario confirma archivos operativos tras la solicitud |
| Identidad SHA-256 del nuevo JMO | Pendiente de salida local |
| Cotejo exacto de fuentes locales finales | Pendiente |

La última captura no prueba por sí sola el cambio entre dos temas o paletas.
Los casos automatizados comprueban fuentes, fondo y escalas, y la selección fija
jmv es deliberada en sectores/anillo. No se declara inspección independiente de
los archivos OMV/HTML/PDF ni validación multiplataforma del nuevo compilado.
Los casos manuales previos permanecen en history/submission-followup-2026-11-27/GUI_CHECKLIST.md.
