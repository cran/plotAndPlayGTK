\name{guiDo}
\alias{guiDo}
\title{Wrapper for running commands from a GUI}
\description{
  Execute a command, log it, and show error dialog on failure.
}
\usage{
guiDo(expr, call, string, doLog = T, doFailureLog = doLog, logFunction = addToLog, 
      doFailureDialog = T, doStop = T, envir = if (doLog) .GlobalEnv else parent.frame(),
      ...)
}
\arguments{
  \item{expr}{ an expression to run (\code{\link{eval}}) in the given environment.
  To run multiple commands, wrap them in \{braces\}.}
  \item{call}{ a \code{\link{call}} object. If given this is used instead of \code{expr}. }
  \item{string}{ a string to parse and run. If given this is used instead of \code{expr}. }
  \item{doLog}{ whether to write the command to a log: see \code{logFunction}. }
  \item{doFailureLog}{ whether to log \code{"# FAILED"} if an error occurs. }
  \item{logFunction}{ a function to write text to the log, passed as the first argument. 
  If the default \code{addToLog} is not defined then \code{function(x) cat(x,"\n")} is used.}
  \item{doFailureDialog}{ Whether to show a dialog box if an error occurs. }
  \item{doStop}{ Whether to stop execution if an error occurs.
  If false, the function can return an object of class \code{error}: see \code{\link{tryCatch}}.}
  \item{envir}{ \code{\link{environment}} to evaluate the command in. }
  \item{\dots}{ further arguments passed to \code{\link{deparse}}. }
}
\details{
  This is for use in a GUI based on RGtk2. It runs the given command 
  (which can be given as a literal expression, a string to parse, or a call object)
  in the specified environment, writes the command to a log, and shows an error dialog on failure.
  The error dialog contains the given call, the frame where the error occurred, the error message,
  and a suggestion to send in a bug report.
}
\value{
	\code{guiDo} returns the result of the given call, or an object of class \code{error}
	if there was an error and \code{doStop=F}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\examples{
\dontrun{

x <- "hello"
guiDo(y <- paste(x, "world"))

# error dialog:
guiDo( print(paste(y + 1, "is not a number")) )

}
}

\keyword{ error }
\keyword{ programming }
