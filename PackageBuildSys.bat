R CMD build BuildSys
R CMD check --as-cran BuildSys_*.tar.gz
R CMD INSTALL --build BuildSys
