# C:\JVA\R\My R functions\myrtf.r - my functions to create RTF files

# if you call your rtf file "doc", your table "tab", and your figure "fig", you can use the default settings
if(FALSE) {
	doc <- start.rtf(file="Junk.doc", dir="C:/JVA/")
	head1("Heading")
	para("Paragraph.")
	tabl("Caption.")
	figu("Caption.")
	end.rtf()
	}

start.rtf <- function(file=NULL, dir=getwd(), width=8.5, height=11, fsize=12, omi=c(1, 1, 1, 1), quiet=FALSE) {
	# create a new RTF file readable by Word
	# create two new variables to keep count of tables and figures
	tabcount <<- 1
	figcount <<- 1
	if(is.null(file)) file <- paste0("RGeneratedDocument", Sys.Date())
	dirfiledoc <- if(length(grep(".doc", file))>0) paste(dir, file, sep="/") else paste(dir, paste0(file, ".doc"), sep="/")
	if(!quiet) cat(paste0("New RTF document created, ", dirfiledoc, "\n"))
	RTF(dirfiledoc, width=width, height=height, font.size=fsize, omi=omi)
	}

head1 <- function(words, rtf=doc, fsize=12) {
	addHeader(this=rtf, title=words, font.size=fsize)
	}

head2 <- function(words, rtf=doc, bold=TRUE, italic=FALSE) {
	startParagraph(this=rtf)
	addText(this=rtf, words, bold=bold, italic=italic)
	endParagraph(this=rtf)
	addNewLine(this=rtf)
	}

head3 <- function(..., rtf=doc, bold=FALSE, italic=TRUE) {
	startParagraph(this=rtf)
	addText(this=rtf, ..., bold=bold, italic=italic)
	endParagraph(this=rtf)
	addNewLine(this=rtf)
	}

para <- function(..., rtf=doc, bold=FALSE, italic=FALSE) {
	startParagraph(this=rtf)
	addText(this=rtf, ..., bold=bold, italic=italic)
	endParagraph(this=rtf)
	addNewLine(this=rtf)
	}

tabl <- function(..., TAB=tab, rtf=doc, fontt=8, row.names=TRUE, tabc=tabcount, boldt=TRUE, newpage=c("none", "port", "land")[1], omi=c(1, 1, 1, 1)) {
	if(newpage=="none") addNewLine(this=rtf)
	if(newpage=="port") addPageBreak(this=rtf, width=8.5, height=11, omi=omi)
	if(newpage=="land") addPageBreak(this=rtf, width=11, height=8.5, omi=omi)
	startParagraph(this=rtf)
	addText(this=rtf, paste0("Table ", tabc, ".  "), bold=boldt)
	addText(this=rtf, ...)
	addNewLine(this=rtf)
	endParagraph(this=rtf)
	addTable(this=rtf, TAB, font.size=fontt, row.names=row.names)
	addNewLine(this=rtf)
	addNewLine(this=rtf)
	tabcount <<- tabc + 1
	}

figu <- function(..., FIG=fig, rtf=doc, figc=figcount, boldt=TRUE, w=NULL, h=NULL, rf=300, newpage=c("none", "port", "land")[1], omi=c(1, 1, 1, 1)) {
	wf <- if(is.null(w)) 6.5 else w
	hf <- if(is.null(h)) 8 else h
	if(newpage=="none") addNewLine(this=rtf)
	if(newpage=="port") addPageBreak(this=rtf, width=8.5, height=11, omi=omi)
	if(newpage=="land") {
		wf <- if(is.null(w)) 9 else w
		hf <- if(is.null(h)) 5.5 else h
		addPageBreak(this=rtf, width=11, height=8.5, omi=omi)
		}
	addPlot(this=rtf, plot.fun=FIG, width=wf, height=hf, res=rf)
	addNewLine(this=rtf)
	addNewLine(this=rtf)
	startParagraph(this=rtf)
	addText(this=rtf, paste0("Figure ", figc, ".  "), bold=boldt)
	addText(this=rtf, ...)
	endParagraph(this=rtf)
	addNewLine(this=rtf)
	addNewLine(this=rtf)
	figcount <<- figc + 1
	}

end.rtf <- function(rtf=doc, details=FALSE, ...) {
	if(details==TRUE) {
		addPageBreak(rtf, ...)
		addSessionInfo(rtf)
		}
	done(rtf)
	}

if(FALSE) {

x <- sample(5, 100, T)
y <- sample(5, 100, T)
tab <- table(x, y)
fig <- function() plot(x, y)

doc <- start.rtf(file="Junk.doc", dir="C:/JVA/")

head1("Document title")
para("This document is an example of how to create a Word document in R.")
para("This document is an example of how to create a Word document in R.")
head2("Section 1")
para("Some pretty important stuff.")
para("This document is an example of how to create a Word document in R.")
tabl("Nothing too exciting.")
para("This document is an example of how to create a Word document in R.")
para("This document is an example of how to create a Word document in R.")
head3("Section 1.1")
para("Some less important stuff.")
para("This document is an example of how to create a Word document in R.")
figu("Here's a plot at resolution=300.", wf=4, hf=4)
para("How does this text look afterwards?")
para("How does a second paragraph look?")
head3("Section 1.2")
para("Some pretty boring stuff.")
para("This document is an example of how to create a Word document in R.")
figu("Here's the same plot at resolution=100.", wf=4, hf=4, rf=100)
para("This document is an example of how to create a Word document in R.")
para("This document is an example of how to create a Word document in R.")
head2("Section 2")
para("And now", " for some fun", " ... tables!")
para("This document is an example of how to create a Word document in R.")
figu("And again at resolution=600.", wf=4, hf=4, rf=600)
para("This document is an example of how to create a Word document in R.")
para("This document is an example of how to create a Word document in R.")

end.rtf()

}
