#!/usr/bin/env Rscript

requirements <- c(
    'igraph',
    'coda',
    'testthat',
    'R6',
    'nimble'
    )     

for (package in requirements) {
    if (!suppressPackageStartupMessages(require(package,
                                                character.only = TRUE))) {
        install.packages(package, repos = 'https://cran.us.r-project.org')
    }
}

