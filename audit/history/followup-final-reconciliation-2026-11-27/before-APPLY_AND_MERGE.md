# Cotejo y commit del seguimiento

## Aplicar el registro

En la rama actual de seguimiento, copiar archivos/ sobre la raíz de saRa.
Solo se actualizan documentos y el script de cotejo; no hace falta recompilar
ni repetir toda la suite por esta actualización documental.

## Cotejar las fuentes locales (consola de RStudio)

```r
source("audit/COMPARE_REVIEWED_FILES.R")
```

Compartir el resumen y las rutas diferentes. Los .h.R pueden haber sido regenerados:
no deben descartarse ni darse por correctos automáticamente. Cualquier diferencia
de R/, YAML o pruebas se cotejará antes de cerrar el inventario. Los dos informes
LOCAL_FILE_COMPARISON.csv y LOCAL_GIT_STATE.txt son locales; no incorporarlos al commit.

## Identificar el árbol y el nuevo binario (Terminal de RStudio)

```bash
git branch --show-current
git rev-parse HEAD
git status --short
git --no-pager diff --stat
sha256sum "saRa_1.0.0.jmo"
```

Compartir estas salidas. El JMO ha cambiado: no reutilizar el hash anterior.
No hacer todavía commit o push. Tras revisar las diferencias se actualizará la
recepción del nuevo hash, se revisará el staging y se indicará el commit concreto.

## Integración y publicación posteriores

El commit anterior cc15a0d ya se integró; este es un seguimiento distinto.
Tras cotejo y commit, preparar una nueva PR contra main y asociar el nuevo binario
al commit revisado. Antes de actualizar la release se comprobará si v1.0.0 ya está
publicada: no se debe mover una etiqueta publicada ni sustituir un binario sin
registrar la decisión y su identidad. La respuesta a los 22 puntos está en
JAMOVI_AUDIT_RESPONSE_2026-11-27.md e incluye la excepción de la paleta fija.
