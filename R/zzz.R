.onAttach <- function(libname, pkgname) {
  ignored_nimbleSMC_loading_message <- 
    lapply(as.list(list.files(path = "R", pattern = "filtering_.*R$", full.names = T)), source)
  rm(ignored_nimbleSMC_loading_message)
  packageStartupMessage("(TEST) nimbleSMC is loaded!")
}