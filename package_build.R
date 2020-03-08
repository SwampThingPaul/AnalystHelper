#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
library(roxygen2)

roxygen2::roxygenise()

# once all files are complete
devtools::document()

#then ctrl+shift+b to build

devtools::check()


