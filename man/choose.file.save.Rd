\name{choose.file.save}
\alias{choose.file.save}
\alias{get.extension}
\title{GTK file save dialog}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
choose.file.save(default = "", caption = "Save File", filters = Filters[c("All"), ], index = 0)

get.extension(path)
}
\arguments{
  \item{default}{ ~~Describe \code{default} here~~ }
  \item{caption}{ ~~Describe \code{caption} here~~ }
  \item{filters}{ ~~Describe \code{filters} here~~ }
  \item{index}{ ~~Describe \code{index} here~~ }
  \item{path}{ ~~Describe \code{path} here~~ }
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
function(default="", caption="Save File", filters=Filters[c("All"),], index=0) {
	dialog <- gtkFileChooserDialog(caption, NULL, "save",
		"gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
	dialog$setCurrentName(default)
	
	if (length(filters)==2) {
		filters <- matrix(filters, nrow=1, ncol=2)
	}
	
	for (i in seq(1, nrow(filters))) {
		ff <- gtkFileFilterNew()
		ff$setName(filters[i,1])
		for (x in strsplit(filters[i,2], ';')[[1]]) {
			ff$addPattern(x)
		}
		dialog$addFilter(ff)
		if (i == index) { dialog$setFilter(ff) }
	}
	
	#dialog$setDoOverwriteConfirmation(T) crap, appears behind filechooser
	if (dialog$run() == GtkResponseType["accept"]) {
		filename <- dialog$getFilename()
		if (file.exists(filename)) {
			if (is.null(questionDialog("Replace existing file?"))) {
				filename <- NA
			}
		}
		dialog$destroy()
		return(filename)
	} else {
		dialog$destroy()
		return(NA)
	}
  }
}

\keyword{ file }
