#' Create an RTF Document
#'
#' Create an rtf (rich text format) document.
#' @param file
#'   Character scalar name of document, default "RGeneratedDocument" with
#'   \code{\link{Sys.Date}} suffix.
#' @param dir
#'   Character scalar name of directory where document should be stored,
#'   default \code{\link{getwd}()}.
#' @param width
#'   Numeric scalar width of document page in inches, default 8.5.
#' @param height
#'   Numeric scalar height of document page in inches, default 11.
#' @param omi
#'   Numeric vector, length 4, width of document page margins in inches
#'   (bottom, left, top, right), default c(1, 1, 1, 1).
#' @param quiet
#'   Logical scalar indicating if name of new rtf document should be printed to
#'   command line, default FALSE.
#' @return
#'   An rtf file is created in the specified directory.
#'   An object of class rtf is created.  This object is referred to in
#'   other function to write to the file.
#'   In addition, two numeric vectors of length 1, \code{tabcount} and
#'   \code{figcount}, are written to the working directory to keep track
#'   of the number of tables and figures written to the rtf document, and
#'   label the captions accordingly.
#' @details
#'   The rtf file may be written to until the \code{\link{endrtf}()} function
#'   is run.
#'   If you assign your rtf file to an object called \code{doc},
#'   you can use the default settings in other \pkg{\link{jvamisc}}
#'   rtf functions.
#' @seealso
#'   \code{\link[jvamisc]{heading}}, \code{\link[jvamisc]{para}},
#'   \code{\link[jvamisc]{tabl}}, \code{\link[jvamisc]{figu}},
#'   \code{\link[jvamisc]{figbig}}, \code{\link[jvamisc]{endrtf}},
#'   \code{\link[rtf]{RTF}}.
#' @import
#'  rtf
#' @export
#' @examples
#' \dontrun{
#' doc <- startrtf()
#' heading("Heading 1")
#' para("First paragraph.")
#' tab <- head(cars)
#' tabl("First few rows of cars data.", row.names=FALSE)
#' heading("Heading 2", 2)
#' para("Second paragraph.")
#' fig <- function() {
#' 	plot(cars)
#' 	lo <- loess(cars$dist ~ cars$speed)
#' 	lines(lo$x, lo$fitted)
#' }
#' figu("Speed vs. distance from the cars data.")
#' endrtf()
#' }

startrtf <- function(file=NULL, dir=getwd(), width=8.5, height=11,
    omi=c(1, 1, 1, 1), quiet=FALSE) {
	# create a new RTF file readable by Word
	# create two new variables to keep count of tables and figures
	jvamiscenv$tabcount <- 1
	jvamiscenv$figcount <- 1
	if (is.null(file)) {
    file <- paste0("RGeneratedDocument", Sys.Date())
	}
	dirfiledoc <- if (length(grep(".doc", file))>0) {
    paste(dir, file, sep="/")
  } else {
    paste(dir, paste0(file, ".doc"), sep="/")
  }
	if (!quiet) {
    cat(paste0("New RTF document created, ", dirfiledoc, "\n"))
	}
	RTF(dirfiledoc, width=width, height=height, omi=omi)
}
