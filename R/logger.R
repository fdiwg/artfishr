#'@name logger
#'@title util generic logger
#'@description Generic logger
#'@param type type either INFO, WARN or DEBUG
#'@param txt a character vector of format strings, each of up to 8192 bytes.
#'@param ... any values to be passed into \code{txt}. See \link{sprintf}
#'@export
logger <- function(type = c("INFO","WARN","ERROR"), txt, ...){
  log_txt <- sprintf(txt, ...)
  cat(sprintf("[artfishr][%s] %s \n", type, log_txt), file = stderr())
}

#'@name INFO
#'@title util INFO logger
#'@description INFO logger
#'@param txt a character vector of format strings, each of up to 8192 bytes.
#'@param ... any values to be passed into \code{txt}. See \link{sprintf}
#'@export
INFO <- function(txt, ...){logger("INFO", txt, ...)}

#'@name WARN
#'@title util WARN logger
#'@description WARN logger
#'@param txt a character vector of format strings, each of up to 8192 bytes.
#'@param ... any values to be passed into \code{txt}. See \link{sprintf}
#'@export
WARN <- function(txt, ...){logger("WARN", txt, ...)}

#'@name ERROR
#'@title util ERROR logger
#'@description ERROR logger
#'@param txt a character vector of format strings, each of up to 8192 bytes.
#'@param ... any values to be passed into \code{txt}. See \link{sprintf}
#'@export
ERROR <- function(txt, ...){logger("ERROR", txt, ...)}
