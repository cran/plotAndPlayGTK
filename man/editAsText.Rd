\name{editAsText}
\alias{editAsText}
\alias{guiTextInput}
\title{Edit a data frame in spreadsheet-compatible format}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
editAsText(x, title = NULL, edit.row.names = any(row.names(x) != 1:nrow(x)))

guiTextInput(text = "", title = "Text Input", prompt = "")
}
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{title}{ ~~Describe \code{title} here~~ }
  \item{edit.row.names}{ ~~Describe \code{edit.row.names} here~~ }
  \item{text}{ ~~Describe \code{text} here~~ }
  \item{prompt}{ ~~Describe \code{prompt} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, title=NULL, edit.row.names=any(row.names(x) != 1:nrow(x))) {
	if (!is.data.frame(x)) { stop("'x' must be a data frame") }
	if (is.null(title)) {
		title <- paste("Editing", deparse(substitute(x)))
	}
	# make table text block from data frame 'x'
	foo <- capture.output(
		write.table(x, sep="\t", quote=F, row.names=edit.row.names,
		col.names=if (edit.row.names) {NA} else {T})
	)
	tableTxt <- paste(paste(foo, collapse="\n"), "\n", sep='')
	if (edit.row.names) { tableTxt <- paste("row.names", tableTxt, sep='') }
	# show text box and repeat if there was an error
	readOK <- F
	while (!readOK) {
		newTableTxt <- guiTextInput(text=tableTxt, title=title, 
			prompt=paste("Copy and paste to/from a spreadsheet,",
				"or edit the text here (in tab-separated format).\n",
				"Do not move the columns around,",
				"they must stay in this order."))
		if (is.null(newTableTxt)) { return(x) }
		# convert table text block back to data frame
		zz <- textConnection(newTableTxt)
		newData <- tryCatch(
			read.delim(file=zz, header=T, colClasses=sapply(x, class),
			row.names=if (edit.row.names) {1} else {NULL}),
			error=function(e)e)
		close(zz)
		# check whether there was an error in reading the table
		if (inherits(newData, "error")) {
			errorDialog("Error reading table: ", conditionMessage(newData))
			tableTxt <- newTableTxt
		} else {
			readOK <- T
		}
	}
	# warn if the number of rows has changed
	if (nrow(newData) != nrow(x)) {
		warning("Number of rows changed from ",
			nrow(x), " to ", nrow(newData))
	} else if (!edit.row.names) {
		# keep original row names 
		row.names(newData) <- attr(x, "row.names")
	}
	# ensure factor levels are the same
	for (i in which(sapply(x, class) == "factor")) {
		newData[[i]] <- factor(newData[[i]], levels=levels(x[[i]]))
	}
	newData
  }
}

\keyword{ utilities }
\keyword{ manip }
