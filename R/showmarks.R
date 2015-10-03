#' Show Marks
#'
#' Show marks used in graphing, including line types, plotting symbols,
#' default colors, color blind friendly colors (`blindcolz`), and fonts.
#' @export
#' @examples
#' showmarks()

showmarks <- function() {
  cat("For more color info see\nhttp://research.stowers-institute.org/efg/R/Color/Chart/index.htm\n")
  par(mar=rep(1, 4))
  # number of marks in each group
  n <- c(6, 25, 8, length(blindcolz), 20)
  # number of rows for the marks in each group
  rowsn <- ceiling(n/10)
  rowsn[1] <- n[1]
  groupn <- rep(1:length(rowsn), rowsn)
  rowz <- 1:length(groupn)
  plot(c(1, 10), -range(rowz), type="n", xlab="", ylab="", axes=F)

  for(i in 1:n[1]) lines(c(1, 10), -rep(i, 2), lty=i, cex=3, lwd=3)

  x <- rep(1:10, max(rowsn[-1]))
  y <- rep(rowz[groupn==2], rep(10, rowsn[2]))
  for(i in 1:n[2]) points(x[i], -y[i], pch=i,  cex=3)

  y <- rep(rowz[groupn==3], rep(10, rowsn[3]))
  for(i in 1:n[3]) points(x[i], -y[i], pch=22, cex=5, bg=i)

  y <- rep(rowz[groupn==4], rep(10, rowsn[4]))
  for(i in 1:n[4]) points(x[i], -y[i], pch=22, cex=5, bg=blindcolz[i])

  y <- rep(rowz[groupn==5], rep(10, rowsn[5]))
  for(i in 1:n[5]) text(x[i], -y[i], "Font", font=i)
  }
