#!/usr/bin/Rscript

# this script creates the Rd files from the roxygen info in the R files
# and creates the NAMESPACE file

library(roxygen2)
library(methods)

### 1. Create Rd files and first-pass NAMESPACE

out <- file.remove(file.path('nimbleSMC', 'NAMESPACE'))  ## roxygen2 doesn't want to overwrite if file not created by roxygen2; assignment to out prevents printing of TRUE/FALSE
roxygenize('nimbleSMC', c('namespace','rd'))

