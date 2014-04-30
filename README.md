jvamisc
=======

**jvamisc** is an [R](http://www.r-project.org/) package of miscellaneous functions.

Once I get this working (and I haven't tested it yet) anyone should be able to access the functions by installing them directly from within R.
First, download and install Rtools 3.1 from [CRAN](http://cran.r-project.org/bin/windows/Rtools/), then run the following lines of code:

	install.packages("devtools")
	find_rtools()
	library("devtools")
	devtools::install_github("jvadams/jvamisc")
	library(jvamisc)
