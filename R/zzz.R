.onAttach <- function(libname, pkgname) {
  capture.output(lapply(as.list(list.files(path = "R", pattern = "filtering_.*R$", full.names = T)), source))
  packageStartupMessage("(TEST) nimbleSMC is loaded!")
}