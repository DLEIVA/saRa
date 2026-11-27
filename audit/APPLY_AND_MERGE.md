# Cotejo cerrado y commit del seguimiento

Copiar archivos/ sobre la raíz del proyecto. Solo se actualizan documentos y
huellas: conservar los encabezados locales regenerados. No recompilar ni repetir
toda la suite por esta actualización documental.

Opcionalmente, verificar el inventario desde la consola de RStudio:

```r
source("audit/COMPARE_REVIEWED_FILES.R")
```

Se esperan 113 MATCH. Si aparece otra diferencia, cotejarla antes de crear commit.

## Revisar y crear el commit (Terminal de RStudio)

La rama verificada es fix/audit-submission-followup. Revisar primero su nombre y
el estado, sin cambiar de rama ni descartar archivos:

```bash
git branch --show-current
git status --short
git add -- NEWS R jamovi tests audit
git --no-pager diff --cached --stat
```

El staging debe contener los cambios del seguimiento, las tres pruebas nuevas,
los cuatro encabezados regenerados y los registros históricos/documentales.
No incluir el JMO ni los informes locales LOCAL_FILE_COMPARISON.csv y
LOCAL_GIT_STATE.txt de la raíz de audit; estos dos informes ya estaban excluidos
localmente. Sus copias históricas documentales sí se incluyen.

Si la revisión del staging corresponde a ese conjunto, crear el commit:

```bash
git commit -m "Address jamovi follow-up audit for saRa 1.0.0"
git rev-parse HEAD
git status --short
```

Compartir la salida para registrar el commit y continuar con el push y la PR.
Si quedan archivos imprevistos, revisar su origen antes de incluirlos o borrarlos.
No se ha realizado ningún commit/push desde esta actualización.

## Binario validado

saRa_1.0.0.jmo; SHA-256 aportado por el usuario:

```text
20bdc32ce37742b1b2e77e2735573381e5e23bc8819bbe804a5ded09b69fb865
```

No confundir con el hash histórico del candidato anterior. No recompilar entre
la identificación del binario y su publicación sin actualizar su hash y validación.

Tras commit y PR, comprobar el estado real de la release v1.0.0 antes de actualizar
sus archivos. No mover una etiqueta publicada sin revisar la decisión. La respuesta
a los 22 puntos declara la excepción de paleta jmv fija y la retirada del add-on.
