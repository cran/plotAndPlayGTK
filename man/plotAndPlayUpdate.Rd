\name{plotAndPlayUpdate}
\alias{plotAndPlayUpdate}
\alias{plotAndPlayGetToolbar}
\alias{plotAndPlayGetDA}
\alias{plotAndPlayMakePrompt}
\alias{plotAndPlayUnmakePrompt}
\alias{plotAndPlaySetPrompt}
\title{Utilities for plotAndPlayGTK button callback functions}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotAndPlayUpdate(name)
plotAndPlayGetToolbar(name)
plotAndPlayGetDA(name)
plotAndPlayMakePrompt(name)
plotAndPlayUnmakePrompt(name)
plotAndPlaySetPrompt(name, text)
}
\arguments{
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{text}{ ~~Describe \code{text} here~~ }
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
function(name) {
	result <- eval(.StateEnv$call[[name]])
	if ("trellis" \%in\% class(result)) {
		print(result)
		if (any(.StateEnv$focus[[name]])) {
			with(.StateEnv$focus[[name]], 
				trellis.focus("panel", col, row))
		}
	}
  }
}

\keyword{ utilities }
