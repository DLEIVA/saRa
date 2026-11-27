# Revisión de archivos del seguimiento — 27 de noviembre de 2026

Estado: fuentes del seguimiento revisadas; cotejo del árbol local final pendiente.
Referencia: main a4f850848ccc76cb58f41bd714853f409de80de8, tras el candidato
cc15a0dcbad46671f8d8f1a74197252d1ba0de3f. Rama prevista: fix/audit-submission-followup.

El inventario REVIEWED_FILES_2026-11-27.csv contiene 113 rutas de fuentes,
configuración, datos y pruebas de la copia revisada. Los archivos .h.R son referencias
anteriores a la regeneración local del usuario: pueden diferir y deben cotejarse
contra sus YAML. La huella MD5 normaliza únicamente CRLF a LF. El SHA-256 y tamaño
registran los bytes de la referencia, no del JMO.

No se ha recibido una copia completa de las fuentes locales posteriores a la última
compilación. Por ello, no se declara coincidencia exacta final todavía. El informe
SOURCE_REVIEW_2026-11-27.json y SOURCE_RECONCILIATION_2026-11-27.json previos son
históricos y no deben interpretarse como cotejo de estos nuevos cambios.

Los registros de validación ahora distinguen el JMO anterior del nuevo: el hash
anterior se conserva como histórico y el campo actual permanece pendiente hasta
recibir sha256sum. La suite actual aceptada es PASS 2107 sin fallos/advertencias/
omisiones; las comprobaciones manuales se atribuyen al usuario.

Esta actualización documental no modifica R/, jamovi/, datos ni pruebas, ni requiere
recompilar. El script de comparación solo lee fuentes y escribe sus dos informes.
Los documentos anteriores a la consolidación se conservan intactos bajo
history/submission-followup-2026-11-27/.
