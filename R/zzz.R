.onAttach <- function(libname, pkgname) {

  packageStartupMessage("(TEST) nimbleSMC")
  sourceFiles <- as.list(list.files(path = "R", pattern = ".*R$", full.names = T))
  ignored_nimbleSMC_loading_message <- lapply(sourceFiles[!grepl("zzz", sourceFiles)], source)
  rm(ignored_nimbleSMC_loading_message)
  packageStartupMessage("Source files loaded.")

}