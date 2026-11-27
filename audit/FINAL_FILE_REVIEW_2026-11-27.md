# Cotejo final del seguimiento — 27 de noviembre de 2026

Estado: 113 rutas reconciliadas; nuevo hash JMO registrado; listo para revisar y crear commit.
Rama: fix/audit-submission-followup. HEAD previo al nuevo commit:
a4f850848ccc76cb58f41bd714853f409de80de8.

El usuario aportó 109 MATCH y cuatro DIFFERENT, más los cuatro encabezados .h.R.
Las huellas de referencia del CSV coinciden con el inventario anterior y los
encabezados suministrados coinciden con sus huellas locales declaradas.

| Archivo | Única diferencia aceptada |
|---|---|
| R/bivmix.h.R | Descripción inner range, coherente con bivmix.a.yaml |
| R/catsummary.h.R | Descripciones Blau index y Teachman index, coherentes con catsummary.a.yaml |
| R/umw.h.R | Dos títulos de tablas, coherentes con umw.r.yaml |
| R/wilcoxt.h.R | Tres títulos de tablas, coherentes con wilcoxt.r.yaml |

Se conservan los encabezados del usuario. No hay cambios de cálculos, condiciones,
opciones ni estructura en sus diferencias. El inventario se actualiza a los bytes
aceptados. Los 109 restantes se cotejan mediante el informe del usuario; no se ha
recibido otra descarga íntegra de todas sus fuentes. MD5 normaliza únicamente CRLF.

Nuevo JMO: SHA-256 20bdc32ce37742b1b2e77e2735573381e5e23bc8819bbe804a5ded09b69fb865. Evidencia: sha256sum del usuario; binario no recibido ni rehasheado aquí.
La suite PASS 2107 y las confirmaciones GUI/OMV/HTML/PDF siguen vigentes.

Este paquete solo modifica documentos e inventario. No sobreescribe encabezados
ni modifica R/, YAML, datos o pruebas; no requiere recompilar. Los informes originales,
el inventario previo y los diffs aceptados se conservan bajo
history/followup-final-reconciliation-2026-11-27/.

La respuesta mantiene explícita la excepción de paleta jmv fija en sectores/anillo.
Resta revisar staging, crear el commit y registrar su hash. Publicación/envío no realizados.
