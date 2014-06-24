#' Cheat Sheet
#'
#' A non-functioning function, which allows me to create a cheat sheet collection of examples.
#' @export
#' @examples
#' \dontrun{
#'
#' # read in xls files
#' library(XLConnect)
#' wb <- loadWorkbook("c:/temp/junk.xlsx")
#' dat <- readWorksheet(wb, sheet=getSheets(wb)[1], startRow=1)
#'
#' ### Greek and math symbols ###
#' # http://www.decodeunicode.org/
#' plot(1, 1, xlab="Length  (\U03BCm)", ylab="Temperature  (\U00b0 C)", main="Lambda squared = \\U03BB\\U00B2 = \U03BB\U00B2")
#'
#' ### update package ###
#' library(devtools)
#' setwd("C:/JVA/GitHub/jvamisc")
#' document()
#' # quit and restart R
#' library(devtools)
#' library(roxygen2)
#' # install from local folder
#' setwd("C:/JVA/GitHub")
#' install("jvamisc")
#' setwd("C:/JVA/R/Working Directory")
#' library(jvamisc)
#' # OR from shared GitHub site
#' library(devtools)
#' devtools::install_github("JVAdams/jvamisc")
#' library(jvamisc)
#'
#' ### error bars ###
#' x <- 1:10
#' y <- sample(10)
#' noise <- abs(rnorm(10))
#' plot(x, y, ylim=range(y-noise, y+noise))
#' arrows(x, lo, x, hi, length=0.1, angle=90, code=3)
#'
#' ### error shading ###
#' x <- 1:10
#' y <- sample(10)
#' noise <- abs(rnorm(10))
#' plot(x, y, ylim=range(y-noise, y+noise), type="n")
#' shadepoly(x, y, y-noise, y+noise)
#'
#' ### confidence limits ###
#' # of the mean
#' p <- predict(fit, interval="confidence")
#' # of a new observation
#' p <- predict(fit, interval="prediction")
#' # if you have a gam ...
#' p <- predict(fit, se.fit=TRUE)
#' pe <- p$fit
#' tt <- qt(1 - 0.05/2, fit$df.residual)
#' pl <- pe + tt*p$se.fit
#' pu <- pe - tt*p$se.fit
#'
#' ### mulitple comparison Tukey test approach 1 ###
#' amod <- aov(breaks ~ tension, data = warpbreaks)
#' TukeyHSD(amod)
#'
#' ### mulitple comparison Tukey test approach 2 ###
#' library(multcomp)
#' amod <- aov(breaks ~ tension, data = warpbreaks)
#' mc <- glht(amod, linfct = mcp(tension = "Tukey"))
#' summary(amod)
#' summary(mc)
#' confint(mc)
#' windows()
#' plot(mc)
#'
#' ### set up two-way ANOVA with interactions ###
#' fit <- aov(y ~ f1 + f2 + f1:f2)
#' # set up linear hypotheses for all-pairs of both factors
#' wht <- glht(fit, linfct = mcp(f1 = "Tukey", f2 = "Tukey"))
#' # cf. Westfall et al. (1999, page 181)
#' summary(wht, test = adjusted("Shaffer"))
#'
#' ### label months on jday axis ###
#' plot(101:200, rnorm(100), axes=FALSE)
#' axis(1, at=julian(as.Date(paste(2005, 1:12, 1, sep="-")), origin=as.Date("2004-12-31"))-0.5, labels=F)
#' axis(1, at=julian(as.Date(paste(2005, 1:12, 15, sep="-")), origin=as.Date("2004-12-31")), labels=month.abb, tick=F)
#' axis(2)
#' box()
#'
#' ### contour plot ###
#' library(akima)
#' y <- rnorm(50)
#' x <- runif(50)
#' z <- 2*x^2 - y^2 + 4
#' contour(interp(x, y, z, duplicate="mean"))
#'
#' ### get lat longs for locations ###
#' library(dismo)
#' geocode(c("1600 Pennsylvania Ave NW, Washington DC", "Luca, Italy", "Kampala", "Antigo, WI"))
#'
#' ### get a lat long for a location ###
#' # from R-help posting by Phil Spector, UC Berkeley, Mar 16, 2010; 3:57pm 
#' library(XML) 
#' root <- xmlRoot(xmlTreeParse(paste0("http://maps.google.com/maps/api/geocode/xml?address=", "Antigo, WI", "&sensor=false"))) 
#' lat <- xmlValue(root[["result"]][["geometry"]][["location"]][["lat"]]) 
#' long <- xmlValue(root[["result"]][["geometry"]][["location"]][["lng"]]) 
#'
#' ### plot points on a map ###
#' library(RgoogleMaps) 
#' MyMap <- GetMap.bbox(c(-80, -79), c(45, 46), maptype="terrain", destfile="junk.png", zoom=8)
#' PlotOnStaticMap(MyMap, lat=seq(45, 46, 0.1), lon=seq(-80, -79, 0.1), col="red", pch=16)
#'
#' ### make a quick map ###
#' library(ggmap)
#' qmap("Antigo, Wisconsin", zoom=14)
#'
#' ### convert between lat/long and projections ###
#' library(proj4)
#' project(xy, proj, inverse=FALSE, degrees=TRUE, silent=FALSE, ellps.default="sphere")
#'
#' ### other map stuff of interest ###
#' # http://cartodb.com/
#' # https://github.com/Vizzuality/cartodb-r
#'
#' ### dendrogram reorder ###
#' x <- sample(100, 10)
#' names(x) <- x
#' hc <- hclust(dist(x))
#' dd <- as.dendrogram(hc)
#' dd.reorder <- reorder(dd, x, mean)
#' par(mfcol = 1:2)
#' plot(dd, main="default dendrogram")
#' plot(dd.reorder, main="reordered")
#'
#' ### background jpeg image in plot ###
#' library(jpeg)
#' x <- rnorm(20)
#' y <- rnorm(20)
#' img <- readJPEG("C:/Users/Public/Pictures/Sample Pictures/Chrysanthemum.jpg")
#' plot(x, y, type="n")
#' pusr <- par("usr")
#' rasterImage(img, pusr[1], pusr[3], pusr[2], pusr[4])
#' points(x, y, pch=16, cex=3)
#'
#' ### fit all subsets models ###
#' # select all possible combinations of 8 independent variables
#' var.names <- names(train)[1:8]
#' comb <- as.data.frame(all.combs(8))
#' dimnames(comb)[[2]] <- var.names
#' fits <- vector("list", dim(comb)[1])
#' fits[[1]] <- lm(lpsa ~ 1, dat=train)
#' for(i in 2:length(fits)) {
#' 	fits[[i]] <- lm(formula=paste("lpsa ~", paste(var.names[comb[i, ]==1], collapse=" + ")), dat=train)
#' 	}
#' comb2 <- comb
#' comb2$nx <- apply(comb, 1, sum)
#' # AIC
#' aic <- AICc(fits)
#' aic <- aic[order(as.numeric(row.names(aic))), -1]
#' comb2 <- data.frame(comb2, aic)
#' comb2 <- comb2[order(comb2$aicc), ]
#' comb2[comb2$daicc <= 2, ]
#' fits[[as.numeric(row.names(comb2)[1])]]
#'
#' ### regular expression examples ###
#' # get rid of spaces before commas or periods
#' t2 <- gsub("[[:space:]]\\.", "\\.", charvec)
#' gsub("[[:space:]]\\,", "\\,", t2)
#' # insert a space between all punctuation and letters
#' gsub("([[:punct:]])([[:alpha:]])", "\\1 \\2", charvec)
#' # add a period to any single alpha character
#' t2 <- gsub("([[:space:]][[:alpha:]])([[:space:]])", "\\1\\.\\2", charvec)
#' gsub("([[:space:]].)$", "\\1\\.", t2)
#' # insert a space before and after an equal sign
#' t2 <- gsub("([[:alpha:]]|[[:punct:]])=", "\\1 =", charvec)
#' gsub("=([[:alpha:]]|[[:punct:]])", "= \\1", t2)
#' # make sure Jr has a period after it
#' gsub("Jr$", "Jr.", t2)
#' # remove all apostrophes (and any surrounding spaces)
#' t2 <- gsub('[[:space:]]\\"[[:space:]]', "", charvec)
#' t2 <- gsub('[[:space:]]\\"', "", t2)
#' gsub('\\"[[:space:]]', "", t2)
#' # cut off equal sign and everything after
#' gsub("=.*", "", charvec)
#' # replace all punctuation marks with spaces
#' gsub("[[:punct:]]", " ", charvec)
#' # get rid of leading and trailing white space
#' gsub("^[ \t]+|[ \t]+$", "", charvec)
#' # change double spaces to single spaces
#' gsub("[ \t]+", " ", charvec)
#' 
#' ### convert a data frame to json ###
#' library(RJSONIO)
#' data <- toJSON(y)
#' cat(data, file="data.json")
#'
#' ### read in internet table ###
#' library(XML)
#' allTables <- readHTMLTable("http://en.wikipedia.org/wiki/United_States_presidential_election,_2012")
#' # Look at the allTables object to find the specific table we want
#' str(allTables)  
#' # if you have problems reading the URL, you could try this ...
#' mylines <- readLines(url("http://en.wikipedia.org/wiki/United_States_presidential_election,_2012"))
#' closeAllConnections() 
#' mylist <- readHTMLTable(mylines, asText=TRUE) 
#' mytable <- mylist1$xTable
#'
#' ### allow users to browse to a file ###
#' myfile <- file.choose()
#'
#' ### one slider ###
#' library(rpanel)
#' density.draw <- function(panel) {
#' 	plot(density(panel$x, bw = panel$h))
#' 	panel
#' 	}
#' panel <- rp.control(x = rnorm(50))
#' rp.slider(panel, h, 0.5, 5, log = TRUE, action = density.draw)
#'
#' ### two sliders ###
#' library(rpanel)
#' loess.draw <- function(panel) {
#' 	plot(panel$x, panel$y)
#' 	lines(loess.smooth(panel$x, panel$y, span=panel$s, degree=panel$d))
#' 	panel
#' 	}
#' panel <- rp.control(x=rnorm(50), y=rnorm(50), s=rep(3, 50), d=1)
#' rp.slider(panel, s, 0.1, 10, showvalue=T, action=loess.draw)
#' rp.slider(panel, d, 1, 2, showvalue=T, action=loess.draw)
#'
#' ### animation ###
#' for(i in 1:10) {
#' 	windows()
#' 	plot(1:10, 1:10, type="l")
#' 	points(i, i, pch=16, cex=2)
#' 	savePlot(filename=paste("Rplot", i), type="bmp")
#' }
#' cat(paste("Plots saved to", getwd()), "\n")
#' # GIF Construction Set
#' # Animation wizard
#' # add the bitmaps and save as gif animation file
#' # GIMP file open as layers ... save as animated gif
#' }

cheat <- function() {
	cat("Use    ?cheat    to see the examples.\n")
	}