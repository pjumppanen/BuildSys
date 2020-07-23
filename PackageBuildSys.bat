R CMD build BuildSys
R CMD check --as-cran BuildSys_1.0.tar.gz
R CMD INSTALL --build BuildSys
