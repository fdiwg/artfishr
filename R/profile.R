.onLoad <- function (libname, pkgname) {
  
  assign(".artfishr", new.env(), envir= asNamespace(pkgname))
  .artfishr$format_specs = list()
  set_vrule_validators()

  .artfishr$translator <- shiny.i18n::Translator$new(
    translation_csvs_path = system.file("extdata", "i18n/translations", package = "artfishr"),
    translation_csv_config = system.file("extdata", "i18n/config.yml", package = "artfishr")
  )
  .artfishr$translator$set_translation_language("en")
   
}