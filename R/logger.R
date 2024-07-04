logger <- function(type, txt, ...){
  log_txt <- sprintf(txt, ...)
  cat(sprintf("[artfishr][%s] %s \n", type, log_txt), file = stderr())
}
INFO <- function(txt, ...){logger("INFO", txt, ...)}
WARN <- function(txt, ...){logger("WARN", txt, ...)}
ERROR <- function(txt, ...){logger("ERROR", txt, ...)}
