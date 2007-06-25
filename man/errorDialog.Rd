\name{errorDialog}
\alias{errorDialog}
\alias{infoDialog}
\alias{questionDialog}
\title{Quick GTK message dialogs}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
errorDialog(..., isMarkup=F)
infoDialog(..., isMarkup=F)
questionDialog(..., isMarkup=F)
}
\arguments{
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
  \item{isMarkup}{ ~~Describe \code{isMarkup} here~~ }
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
function(...) {
	guiMessageDialog(type="error", ...)
  }
}

\keyword{ utilities }
