jvamisc
=======

**jvamisc** is a package of miscellaneous utility functions for the [R programming language](http://www.r-project.org/).

You should be able to access the functions by installing them directly from within R.

	library("devtools")
	devtools::install_github("JVAdams/jvamisc")
	library(jvamisc)

If you don't already have `devtools` and `Rtools`, you will need to download and install (as administrator, if using a PC) Rtools 3.1 from [CRAN](http://cran.r-project.org/bin/windows/Rtools/), 
	the Comprehensive R Archive Network, then run the following lines of code before submitting the code above:

	install.packages("devtools")
	library(devtools)
	find_rtools()

An alternative approach for Windows users is to download this 
[zip file](https://github.com/JVAdams/jvamisc/raw/master/jvamisc.zip)
and install the package from the R menu:
- Packages
- Install package(s) from local zip files...
	
_Thanks to Hilary Parker whose blog post [Writing an R package from scratch](http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
encouraged me to create my first R package._
