#' Add a Paragraph to an RTF Document
#'
#' Add a paragraph to an rtf (rich text format) document.
#' @param ... 		One or more character scalars (separated by commas) of text to add to document as a single paragraph.
#' @param rtf		An rtf object, default \code{doc}.
#' @param bold 		Logical scalar indicating if paragraph should use bold font, default FALSE.
#' @param italic 	Logical scalar indicating if paragraph should use italic font, default FALSE.
#' @details 		The specified heading is written to the rtf file.
#' @seealso			\code{\link[jvamisc]{startrtf}}, \code{\link[jvamisc]{heading}}, \code{\link[jvamisc]{tabl}}, \code{\link[jvamisc]{figu}}, 
#' \code{\link[jvamisc]{endrtf}}, \code{\link[rtf]{RTF}}.
#' @import 			rtf
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
#' 	}
#' figu("Speed vs. distance from the cars data.")
#' endrtf()
#' }

para <- function(..., rtf=doc, bold=FALSE, italic=FALSE) {
	startParagraph(this=rtf)
	addText(this=rtf, ..., bold=bold, italic=italic)
	endParagraph(this=rtf)
	addNewLine(this=rtf)
	}
