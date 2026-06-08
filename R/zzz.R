.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Circadian Dynamics ", utils::packageVersion(pkgname))
}
