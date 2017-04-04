#!/bin/bash

echo '***** removing existing packages, if they exist on the server already *****'

Rscript -e 'remove.packages("backports")' || true
Rscript -e 'remove.packages("base64enc")' || true
Rscript -e 'remove.packages("bitops")' || true
Rscript -e 'remove.packages("caTools")' || true
Rscript -e 'remove.packages("colorspace")' || true
Rscript -e 'remove.packages("crayon")' || true
Rscript -e 'remove.packages("curl")' || true
Rscript -e 'remove.packages("dichromat")' || true
Rscript -e 'remove.packages("digest")' || true
Rscript -e 'remove.packages("evaluate")' || true
Rscript -e 'remove.packages("hazardItems")' || true
Rscript -e 'remove.packages("highr")' || true
Rscript -e 'remove.packages("htmltools")' || true
Rscript -e 'remove.packages("httr")' || true
Rscript -e 'remove.packages("jsonlite")' || true
Rscript -e 'remove.packages("knitr")' || true
Rscript -e 'remove.packages("labeling")' || true
Rscript -e 'remove.packages("magrittr")' || true
Rscript -e 'remove.packages("mapdata")' || true
Rscript -e 'remove.packages("maps")' || true
Rscript -e 'remove.packages("markdown")' || true
Rscript -e 'remove.packages("mime")' || true
Rscript -e 'remove.packages("munsell")' || true
Rscript -e 'remove.packages("openssl")' || true
Rscript -e 'remove.packages("plyr")' || true
Rscript -e 'remove.packages("png")' || true
Rscript -e 'remove.packages("praise")' || true
Rscript -e 'remove.packages("R6")' || true
Rscript -e 'remove.packages("R.cache")' || true
Rscript -e 'remove.packages("RColorBrewer")' || true
Rscript -e 'remove.packages("Rcpp")' || true
Rscript -e 'remove.packages("RCurl")' || true
Rscript -e 'remove.packages("rmarkdown")' || true
Rscript -e 'remove.packages("R.methodsS3")' || true
Rscript -e 'remove.packages("R.oo")' || true
Rscript -e 'remove.packages("rprojroot")' || true
Rscript -e 'remove.packages("R.rsp")' || true
Rscript -e 'remove.packages("RSclient")' || true
Rscript -e 'remove.packages("Rserve")' || true
Rscript -e 'remove.packages("R.utils")' || true
Rscript -e 'remove.packages("scales")' || true
Rscript -e 'remove.packages("stringi")' || true
Rscript -e 'remove.packages("stringr")' || true
Rscript -e 'remove.packages("testthat")' || true
Rscript -e 'remove.packages("XML")' || true
Rscript -e 'remove.packages("yaml")' || true

echo '***** install required packages for hazardItems dependencies *****'

Rscript -e 'install.packages(c("Rserve","jsonlite","httr","RCurl", "devtools"), repos = c("https://owi.usgs.gov/R", "https://cran.rstudio.com/"), dependencies = TRUE)'
