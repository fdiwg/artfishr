#'@name set_translation_language
#'@title Set translation language
#'@description set_translation_language
#'@param lang lang
#'@export
set_translation_language <- function(lang){
  .artfishr$translator$set_translation_language(lang)
  fdishinyr::set_translation_language(lang)
}

#'@name translator
#'@title Get translator
#'@description Get translator
#'@return the translator, object of class \link[shiny.18n]{Translator}
#'@export
translator <- function(){
  return(.artfishr$translator)
}