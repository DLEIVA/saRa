# Revisión del candidato saRa 1.0.0 — 16 de noviembre de 2026

Base exacta: `2cfe7ee61c580950140c832a4b22163a90bba2f8`.
Rama de trabajo de preparación: `audit-review-1.0.0-2026-11-16`.
No se ha modificado ni publicado main. Las traducciones de 1.1.0 quedan aparte.

## Cambios preparados para comprobar

| Punto | Implementación | Validación pendiente |
|---|---|---|
| λ simétrica | Cociente de errores combinados; caso conocido 1/3 | testthat y tabla |
| τ simétrica | Eliminada de opciones, interfaz, resultados y cálculo | Interfaz y reapertura |
| H1/H3 | SP50 y SP80, cuantiles tipo 7 y denominadores de amplitud | Traslación, reflexión y amplitud cero |
| Libro | Solanas, Salafranca, Fauquet y Núñez (2005), siete análisis descriptivos | Referencias en resultados |
| Regresión | Retirada completa del add-on; eliminada la clase privada de imágenes | Confirmar que Regresión de jamovi permanece sin extensiones |
| Ordinales | Categorías ordenadas de texto mediante inversa de FDEA; puntuaciones numéricas conservadas; notas de no aplicabilidad | Texto, números, combinación y cambio de tipo |
| Interfaz ordinal | Controles numéricos habilitados según metadatos del proveedor; se filtran variables no ordinales | Comprobar en jamovi instalado |
| Datos numéricos/mixtos | Conversión de valores almacenados antes de operar; errores de estadísticos aislados | Factores numéricos, ceros, huecos y grupos vacíos |
| Contingencia | Tabla completa para frecuencias y tabla sin márgenes vacíos para índices | Niveles definidos vacíos y datos degenerados |
| Porcentajes y cercas | Límites coherentes; recorte y winsorización por cola; validación en R | Valores límite y restauración |
| Distribuciones | Ventanas por cuantiles, títulos calculados directamente, tres cifras significativas, cuantiles infinitos explícitos | Las seis continuas y cinco discretas, colas y extremos 0/1 |
| Contrastes de rangos | Empates totales y exclusión completa con mensajes claros; nota de dirección | Pares y grupos, datos escasos y empates |
| Gráficos | Colores del tema; paletas ampliables; etiquetas oscuras; renderizado de bivariados | Pantalla, HTML y PDF, nombres con espacios |
| Frecuencias | Denominador sin ausentes; proporciones y porcentajes conservados; acumuladas solo en ordenadas | Comparar casos válidos y ausentes |
| Presentación | Direcciones de índices, SP50/SP80, ω², subíndice H, ejes, nombres y notas | Inspección de interfaz y tablas |
| Mantenimiento | Funciones numéricas separadas; eliminación de preparación de datos no utilizada y llamadas obsoletas | testthat completo |
| Empaquetado | audit, img y logo excluidos del paquete R | Compilación final |

## Evidencia disponible aquí

- Definiciones R e interfaz de los 12 análisis aceptadas por el compilador oficial de jamovi.
- Análisis de sintaxis de R y YAML, opciones, dependencias y referencias sin incidencias.
- Comprobación aislada del manejador JavaScript ordinal: selección textual, mixta y cambio de metadatos.
- 40 bloques testthat preparados, **sin ejecutar en este entorno**.
- No hay R/Rscript ni jamovi disponibles aquí: no se afirma que el paquete compile, que los contrastes pasen o que la interfaz funcione ya en la aplicación.

## Comprobación local

1. Ejecutar `testthat::test_local(".")` desde una sesión R limpia.
2. Si pasa: compilar e instalar con `jmvtools::install()`.
3. Reiniciar jamovi. Comprobar ordinales de texto y numéricas, mezclas y cambios de tipo; índices con categorías vacías; gráficos bivariados con nombres que contengan espacios; distribuciones, extremos y valores pequeños; contrastes con todos los valores empatados.
4. Confirmar que los diagnósticos añadidos ya no aparecen en la Regresión base de jamovi.
5. Exportar HTML/PDF y guardar, cerrar y reabrir un `.omv`, especialmente con Datos mixtos.
6. Registrar las salidas y la versión de R/jamovi/Windows o Linux antes de marcar este candidato como validado o hacer el commit.

El futuro constructor de regresión independiente, polinomios y splines no forma parte de este candidato. Los auxiliares numéricos de diagnóstico y sus pruebas se conservan como código sin registrar para ese desarrollo; ya no insertan resultados en otra biblioteca.

## Seguimiento 1: errores detectados en la primera ejecución local

El usuario ejecutó el primer candidato en Linux y comunicó errores; no se considera validado.
Se corrigen cuatro causas:

- jmvcore instalado no exporta `.`, de modo que la importación impedía resolver el marcador. El módulo define ahora su propio marcador con la API pública `Options$translate`, manteniendo la carga diferida y sin importar paquetes completos.
- `expand.grid` no admite `check.names`: se interpretaba como otra dimensión y producía una tabla incompatible. Las barras usan ahora directamente el data.frame de la tabla de contingencia y restauran sus nombres.
- `factor(x)` conserva por defecto el carácter ordenado de un factor ordenado. La prueba de rechazo nominal declara ahora `ordered=FALSE`, sin alterar la política de aceptar solo ordinales.
- Se eliminan tres pruebas de renderizado de regresión cuyo código ya había sido retirado según lo acordado. Se conservan las pruebas de auxiliares numéricos y de ausencia del add-on.

La batería preparada queda en **40 bloques**, incluida una prueba del marcador de traducción. La nueva ejecución en R y la compilación siguen pendientes. No se rebajan expectativas estadísticas ni se marcan como aprobadas las pruebas fallidas anteriores.

## Seguimiento 2: jmvcore 2.2 en RStudio

Entorno comunicado: R 4.5.1; jmvcore 2.2 instalado en /usr/local/lib/R/site-library/jmvcore; jmvtools no está instalado en ese R.
La ejecución del seguimiento 1 terminó con FAIL 3, WARN 0, SKIP 0, PASS 316.
Los tres fallos proceden de consultar un miembro inexistente, options$translate.
Ahora se protege únicamente la consulta del traductor con tryCatch; si no existe,
se conserva el texto original. Un error dentro de un traductor existente no se oculta.
Se añade una prueba con un miembro que lanza error y con las Options realmente instaladas.
La batería preparada queda en 41 bloques. Nueva ejecución y compilación pendientes.
La ausencia de jmvtools se resolverá antes de compilar; no explica estos fallos de testthat.

## Seguimiento 3: Range en ordinales de texto

Evidencia comunicada por el usuario antes de este cambio:
- 41 bloques testthat: FAIL 0, WARN 0, SKIP 0, PASS 326 (6,3 s).
- Compilación e instalación correctas en Linux/Flatpak, jamovi 28.17.0.
- SP50/SP80 correctos en datos continuos y ordinales numéricos, con desplazamiento,
  reflexión, mediana cero y datos constantes; notas de no definición visibles.
- Lambda simétrica 0,333; direccionales 0,500 y 0; tau simétrica ausente.
- Niveles vacíos: índices invariables; frecuencias y nota dependen correctamente
  de Retain unused levels in analyses.
- Ordinal textual Satisfacción: N=7; mediana Media; moda Alta; min Baja y max Alta;
  cuartiles y percentiles como categorías; controles numéricos deshabilitados;
  gráfico de barras con frecuencias 2,2,3. El Range seleccionado por defecto
  seguía apareciendo con NaN y nota.

A petición del usuario, Range se desmarca para selecciones solo textuales y el
backend omite su fila incluso al ejecutar desde R o reabrir opciones anteriores.
Con selección mixta sigue disponible, con nota en la variable textual. Al pasar
de una selección textual a numérica/mixta se activa de nuevo. Una selección
numérica guardada con Range desmarcado se respeta al abrirla. Se espera a disponer
de todos los metadatos antes de modificar la selección del control.

Se han comprobado aisladamente las transiciones del manejador JavaScript.
La batería preparada pasa a 42 bloques. Los 326 resultados anteriores certifican
el candidato previo; este ajuste requiere una nueva ejecución y comprobación GUI.

## Seguimiento 4: cabecera de datos en la prueba de Range

La ejecución local de la corrección 3 terminó con FAIL 1, WARN 0, SKIP 0,
PASS 331. Falló únicamente la expectativa de clasificación de una cabecera
creada con data[FALSE,,drop=FALSE]: el subconjunto de factor había perdido el
atributo values con sus puntuaciones. Las demás comprobaciones, incluidas las
de omisión de Range para texto y conservación para selección mixta, pasaron.

Se corrige la preparación de esa cabecera para conservar explícitamente values.
Se comprueban tres casos: cabecera con puntuaciones numéricas (TRUE), columna de
texto (FALSE) y cabecera tras retirar sus puntuaciones (FALSE). No se cambia el
clasificador ni se infieren puntuaciones desde etiquetas textuales. La batería
sigue teniendo 42 bloques. Nueva ejecución local y comprobación GUI pendientes.
