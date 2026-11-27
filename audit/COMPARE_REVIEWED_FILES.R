# Run from the package root. Read-only comparison of source; writes audit reports only.
if (!file.exists("DESCRIPTION") || !file.exists("audit/REVIEWED_FILES_2026-11-27.csv"))
  stop("Ejecutar desde la raíz de saRa, después de copiar la actualización documental")
reference <- read.csv("audit/REVIEWED_FILES_2026-11-27.csv", stringsAsFactors=FALSE,
  check.names=FALSE, fileEncoding="UTF-8")
# Compare text using LF line endings, so CRLF alone is not a source difference.
canonical_md5 <- function(path, text) {
  if (!file.exists(path)) return(NA_character_)
  if (!text) return(unname(tools::md5sum(path)))
  bytes <- readBin(path, what="raw", n=file.info(path)$size)
  value <- rawToChar(bytes)
  value <- gsub("\r\n", "\n", value, fixed=TRUE)
  temporary <- tempfile()
  on.exit(unlink(temporary))
  writeBin(charToRaw(value), temporary)
  unname(tools::md5sum(temporary))
}
actual <- vapply(seq_len(nrow(reference)), function(i)
  canonical_md5(reference$path[i], reference$text[i]), character(1))
comparison <- data.frame(path=reference$path, reviewed_md5=reference$md5_lf,
  local_md5=actual, status=ifelse(is.na(actual), "MISSING",
    ifelse(actual==reference$md5_lf,"MATCH","DIFFERENT")))
roots <- c("R","jamovi","tests","data")
roots <- roots[dir.exists(roots)]
local <- unique(unlist(lapply(roots, list.files, recursive=TRUE,
  full.names=TRUE, all.files=FALSE), use.names=FALSE))
local <- local[!dir.exists(local)]
extra <- setdiff(local, reference$path)
if (length(extra)) comparison <- rbind(comparison, data.frame(path=extra,
  reviewed_md5=NA_character_, local_md5=unname(tools::md5sum(extra)),
  status="LOCAL_ONLY_NOT_REVIEWED"))
write.csv(comparison,"audit/LOCAL_FILE_COMPARISON.csv",row.names=FALSE,na="",
  fileEncoding="UTF-8")
cat("Comparación de archivos (no modifica código ni realiza commits):\n")
print(table(comparison$status))
if (any(comparison$status!="MATCH"))
  print(comparison[comparison$status!="MATCH",c("path","status")],row.names=FALSE)
commands <- list(branch=c("branch","--show-current"), head=c("rev-parse","HEAD"),
  status=c("status","--short"))
lines <- unlist(lapply(names(commands),function(name) c(paste0("## ",name),
  tryCatch(system2("git",commands[[name]],stdout=TRUE,stderr=TRUE),
    error=function(e) conditionMessage(e)))),use.names=FALSE)
writeLines(lines,"audit/LOCAL_GIT_STATE.txt",useBytes=TRUE)
cat("Informes: audit/LOCAL_FILE_COMPARISON.csv y audit/LOCAL_GIT_STATE.txt\n")
cat("Las diferencias requieren cotejo; los encabezados .h.R pueden haber sido regenerados.\n")
cat("Referencia reconciliada: 113 rutas del seguimiento; ver FOLLOWUP_SOURCE_RECONCILIATION_2026-11-27.json.\n")
