# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Rweixin Version:", packageDescription("Rweixin", fields = "Version")) )
}

