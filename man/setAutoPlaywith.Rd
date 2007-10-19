\name{setAutoPlaywith}
\alias{setAutoPlaywith}
\title{ Set playwith to run automatically with lattice plots. }
\description{
	Set \code{playwith} to run automatically with lattice plots.
}
\usage{
setAutoPlaywith(on = TRUE)
}
\arguments{
	\item{on}{ \code{TRUE} to set \code{\link{playwith}} to run automatically 
		with lattice plots; \code{FALSE} to revert to the usual plot device. }
}
\details{
	When enabled, the \code{\link[lattice]{print.trellis}} function, which is typically
	called implicitly to create lattice plots, will trigger 
	\code{\link{playwith}}, passing the original high-level call. 
	So for lattice plots only, this is like changing your default plot device.
	However, it only replaces screen devices: plotting to a file device will work as normal.
	
	Note that this automatic behaviour is not a full replacement for calling
	\code{\link{playwith}} directly, since it does not allow to you specify any of the
	optional arguments.
	
	This feature requires \code{lattice} package version \code{0.17-1} or later.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}}, \code{\link[lattice]{lattice.options}} }
\examples{
\dontrun{
	
## appears in your usual screen device:
xyplot(Sepal.Length ~ Sepal.Width | Species, data=iris)
setAutoPlaywith(TRUE)
## appears in the playwith interface:
xyplot(Sepal.Length ~ Sepal.Width | Species, data=iris)
setAutoPlaywith(FALSE)

}
}
\keyword{ iplot }
\keyword{ device }

