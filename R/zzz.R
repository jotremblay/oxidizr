#' Package Load Functions
#'
#' @name oxidizr-hooks
#' @keywords internal
NULL

.onLoad <- function(libname, pkgname) {
  # Register S7 methods
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "oxidizr: Substrate Oxidation Analysis\n",
    "Version ", utils::packageVersion(pkgname)
  )
}
