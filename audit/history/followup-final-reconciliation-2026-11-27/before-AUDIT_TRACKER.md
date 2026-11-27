# Último audit de jamovi — estado del seguimiento

27 de noviembre de 2026. Este registro se refiere a los **22 puntos del último
informe**, no a los A01–A29 del audit inicial. La lista inicial y los registros
previos se conservan como históricos.

| Punto | Hallazgo | Resultado |
|---|---|---|
| 1 | Symmetric Goodman-Kruskal lambda | Corrected |
| 2 | H1/H3 percentile skewness | Corrected |
| 3 | Private image bridge in the regression add-on | Removed |
| 4 | Changes to native regression checkboxes and overlap with moretests | Removed |
| 5 | Text-coded ordinal variables | Corrected |
| 6 | Numeric values stored as nominal/ordinal factors | Corrected |
| 7 | Empty categories blank all association indices | Corrected |
| 8 | Percentage options and fence bounds | Corrected |
| 9 | Default exponential and F plotting windows | Corrected |
| 10 | Seven small code slips | Corrected |
| 11 | Numeric mode stored as text | Corrected |
| 12 | Distribution titles and endpoint quantiles | Corrected |
| 13 | Rank-test edge cases and direction | Corrected |
| 14 | Plot themes and palettes | Addressed with a declared design exception |
| 15 | Geometric and negative-binomial count convention | Corrected |
| 16 | Redundant frequency columns and missing denominators | Corrected |
| 17 | Labels, casing and spelling | Corrected |
| 18 | Enabled bar-only controls | Corrected |
| 19 | Translation marking | Corrected at source-marking scope |
| 20 | Long functions | Corrected in the named routines |
| 21 | Dead code and deprecations | Corrected in the cited locations |
| 22 | Working notes bundled into the R package | Corrected |

Resultado: 19 puntos corregidos en el alcance citado, 2 resueltos por retirada del
add-on y 1 atendido con la excepción de diseño declarada para la paleta de sectores
/anillo. No se han identificado defectos estadísticos del informe aún sin resolver.
Esto no equivale a aceptar todas las recomendaciones literalmente.

- Paso 8 validado: PASS 2107, FAIL 0, WARN 0, SKIP 0; 22.5 s.
- Compilado instalado y frecuencias, gráficos y OMV/HTML/PDF confirmados por el usuario.
- Excepción: sectores y anillo usan jmv fija; otras paletas globales no cambian sus rellenos.
- Catálogos de idiomas no incluidos; marcado para traducción realizado.
- Feature request de imágenes en add-ons no presentado; el add-on se ha retirado.
- Pendientes de cierre: cotejo local de archivos, nuevo hash JMO y revisión del commit.

Respuesta completa: JAMOVI_AUDIT_RESPONSE_2026-11-27.md.
Estado operativo: VALIDATION_STATUS.json. Historial: USER_VALIDATION_LOG.md.
