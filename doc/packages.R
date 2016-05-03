## import packages til DESCRIPTION import

pkg.impt <- function(pkg){
    ny.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(ny.pkg)) 
        devtools::use_package(ny.pkg, pkg = "nbReg")
}

pakke <- c("ggplot2", "foreign", "dplyr", "tidyr" , "lazyeval")

pkg.impt(pakke)

