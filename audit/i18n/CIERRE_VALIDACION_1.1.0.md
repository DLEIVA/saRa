# Cierre de validación — saRa 1.1.0

El desarrollador confirma: 44 bloques, 345 comprobaciones superadas, cero fallos, advertencias u omisiones; funcionamiento de la interfaz y los gráficos; exportaciones HTML/PDF correctas incluida √; guardado y reapertura .omv; funcionamiento al cambiar al inglés.

La evidencia se conserva en VALIDACION_FINAL_1.1.0.json. Las notas anteriores de pruebas pendientes describen fases previas y quedan superadas por este registro. La aceptación en la biblioteca de jamovi y la revisión del uso de APIs privadas siguen correspondiendo a sus mantenedores.

## Guardar en GitHub

Copiar el contenido del ZIP sobre la raíz del proyecto, sustituyendo únicamente los archivos incluidos. No hace falta recompilar por estos cambios documentales.

En Terminal, confirmar feature/spanish-1.1.0 con git branch --show-current. Revisar git status --short. Añadir los archivos fuente, catálogos, pruebas y auditoría mediante git add -- DESCRIPTION NEWS R jamovi tests audit. Revisar git diff --cached --stat y hacer commit/push. No añadir ZIP, .jmo ni build/.

El commit identifica la revisión del repositorio, pero no debe presentarse como el hash exacto probado: no se registró el hash local de aquella ejecución. Una vez subido, se puede abrir una pull request hacia main y revisar el diff antes de fusionar.
