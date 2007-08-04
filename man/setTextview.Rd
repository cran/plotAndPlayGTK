\name{setTextview}
\alias{setTextview}
\alias{addTextview}
\alias{getTextviewText}
\alias{setTextviewMonospace}
\title{Convenience functions for gtkTextView}
\description{
  Convenience functions for \code{\link[RGtk2]{gtkTextView}}.
}
\usage{
setTextview(tv, ..., sep = "")
addTextview(tv, ..., sep = "")
getTextviewText(tv)
setTextviewMonospace(tv)
}
\arguments{
  \item{tv}{ a \code{\link[RGtk2]{gtkTextView}}. }
  \item{\dots}{ character objects, passed to \code{\link{paste}}. }
  \item{sep}{ passed to \code{\link{paste}}. }
}
\details{
  
}
\value{
	\code{getTextviewText} returns the text from the given text box.
}
\author{ Graham Williams }
\seealso{  }
\examples{
	
}

\keyword{ utilities }
