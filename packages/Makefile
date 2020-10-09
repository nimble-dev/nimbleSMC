.PHONY: FORCE

VERSION=$(shell grep Version: nimbleSMC/DESCRIPTION | grep -o '[^ ]*$$')

version: FORCE
	@echo $(VERSION)

# this rebuilds Rd files in man/ and rebuilds NAMESPACE
man: FORCE
	./prep_pkg.R

nimbleSMC_$(VERSION).tar.gz: FORCE
	@echo "Building nimble package in this directory"
	(R CMD build nimbleSMC)

build: nimbleSMC_$(VERSION).tar.gz

install: nimbleSMC_$(VERSION).tar.gz
	R CMD INSTALL ${NDIR} nimbleSMC_$(VERSION).tar.gz

check: build FORCE
	R CMD check --as-cran nimbleSMC_$(VERSION).tar.gz

rhub: build FORCE
	Rscript -e "library(rhub); check(platform = c('ubuntu-gcc-devel','windows-x86_64-devel','linux-x86_64-rocker-gcc-san','linux-x86_64-centos6-epel','fedora-clang-devel'))"

test: FORCE
	cd .. ; ./install_requirements.R
	cd .. ; ./run_tests.R --parallel

# Keeps .Rproj.user/ and .Rhistory.
clean: FORCE
	git clean -dfx -e .R*

mrproper: FORCE
	git clean -dfx

FORCE:

