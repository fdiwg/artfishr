.onLoad <- function (libname, pkgname) {
  
  assign(".artfish", new.env(), envir= asNamespace(pkgname))
  .artfish$format_specs = list()
  set_vrule_validators()
  
}