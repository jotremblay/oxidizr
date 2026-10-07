#' Package Load Functions
#'
#' @name oxidizr-hooks
#' @keywords internal
NULL

.onLoad <- function(libname, pkgname) {
  # Register S7 methods
  S7::methods_register()
  # The S7 method(print, ...) calls in R/generics.R create a `print` object in
  # this namespace, so R registers the plain S3 print methods in the package's
  # own table only. Register them with base::print as well.
  registerS3method("print", "oxidation_constants", print.oxidation_constants, envir = baseenv())
  registerS3method("print", "oxidation_uncertainty", print.oxidation_uncertainty, envir = baseenv())
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "oxidizr: Substrate Oxidation Analysis\n",
    "Version ", utils::packageVersion(pkgname)
  )
}
