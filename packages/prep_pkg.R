#!/usr/bin/Rscript

# this script creates the Rd files from the roxygen info in the R files
# and creates the NAMESPACE file

library(roxygen2)
if(packageVersion('roxygen2') > "5.0.1")
    stop("some issue with roxygen2 6.0.1 -- see https://github.com/klutometis/roxygen/issues/568 and https://github.com/klutometis/roxygen/issues/595; use version 5.0.1 of royxgen2")
library(methods)

### 1. Create Rd files and first-pass NAMESPACE

out <- file.remove(file.path('nimbleSMC', 'NAMESPACE'))  ## roxygen2 doesn't want to overwrite if file not created by roxygen2; assignment to out prevents printing of TRUE/FALSE
roxygenize('nimbleSMC', c('namespace','rd'))

