
library(devtools)
library(roxygen2)

document('coevo')
build('coevo')
install.packages('coevo_0.1.tar.gz')
