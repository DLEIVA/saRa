# Validación final — saRa 1.1.0: catalán y castellano

El desarrollador confirma 48 bloques, 372 comprobaciones superadas, FAIL/WARN/SKIP 0; duración 15,7 s. También confirma interfaz, tablas, notas, gráficos y símbolos en catalán; exportaciones HTML/PDF; guardado y recuperación .omv en la rama catalana; y funcionamiento al cambiar a castellano e inglés.

La evidencia actual está en VALIDACION_FINAL_1.1.0.json y VALIDACION_CATALAN_1.1.0.json. La validación anterior del castellano (345 comprobaciones) se conserva en VALIDACION_CASTELLANO_1.1.0_HISTORICA.json. No se ha registrado el hash exacto probado ni las versiones exactas de jamovi, R y jmvtools.

Las pruebas del catálogo usan el JSON del candidato con el traductor real de jmvcore y una caché temporal restaurada al terminar. La comprobación de la instalación procede de la confirmación de GUI del desarrollador. Esta documentación no equivale a aceptación de la biblioteca ni a validación de todas las plataformas. Revisión de APIs privadas de regresión: pendiente de los mantenedores.

## Guardar en GitHub

Copiar NEWS y audit a la raíz del proyecto, combinando carpetas. Esta actualización es documental: no requiere repetir las pruebas ni recompilar. Mantener feature/catalan-1.1.0. No hacer merge con main mientras jamovi revisa la 1.0.0.

Terminal:

```bash
git branch --show-current
git status --short
git add -- NEWS R jamovi inst/i18n tests audit
git diff --cached --stat
git -c commit.gpgsign=false commit -m "Add Catalan translation and record validation"
git push -u origin feature/catalan-1.1.0
git status --short --branch
```

Antes del commit, revisar que el resumen incluye los catálogos ca.po/ca.json, las correcciones de regresión, las pruebas y NEWS/audit. Se añaden las carpetas de código/catálogos/pruebas y audit; no añadir build/, ZIP ni .jmo. Un commit nuevo identifica el código guardado, pero no proporciona retroactivamente el hash exacto de la ejecución anterior.
