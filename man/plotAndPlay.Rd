\name{plotAndPlay}
\alias{plotAndPlay}
\alias{plotAndPlayButtons}
\title{A GUI for interactive plots}
\description{
  Open a GTK+ window to view and interact with a plot.
}
\usage{
plotAndPlay(expr, name = "plot", plot.call, nav.scales = c("x","y"), trans.scales = c("y"), buttons = plotAndPlayButtons[c("identify", "zoomin", "zoomout", "zoomfit", "centre")], extra.buttons = plotAndPlayButtons[c("zero")], labels = NULL, identify.call = NULL, is.lattice = (callName \%in\% latticeNames), eval.args = F, envir = parent.frame(), restore.on.close = NULL)

plotAndPlayButtons
}
\arguments{
  \item{expr}{ an expression to create a plot, like \code{plot(1:10)} (see examples). }
  \item{name}{ character value to identify the plot window. }
  \item{plot.call}{ a plot call ( \code{\link{call}} object), if given this is used instead of \code{expr}. }
  \item{nav.scales}{ one or more of \code{c("x","y")} defining which scale(s) to navigate along (e.g. zoom in). }
  \item{trans.scales}{ one or more of \code{c("x","y")} defining which scale(s) to transform (e.g. log). }
  \item{buttons}{ a list of button definitions for the toolbar. 
  Normally this is a subset of the predefined \code{plotAndPlayButtons}. 
  Note that buttons to choose Lattice panels and pages are added automatically if relevant.
  See the \code{Details} section if you want custom buttons. }
  \item{extra.buttons}{ same as \code{buttons}, provided for convenient additions to the default set. }
  \item{labels}{ a character vector of labels for data points, for use in \code{\link{identify}}. If missing, it will be guessed from the plot call. If \code{identify.call} is given, it is ignored. }
  \item{identify.call}{ a \code{\link{call}} to be evaluated when the identify button is clicked. If missing, it will be constructed from the plot call. Generally this argument should not be needed. }
  \item{is.lattice}{ whether the plot is a \link{Lattice} plot, or a traditional graphics plot. If the called function is in the \code{lattice} package it will be recognised automatically. }
  \item{eval.args}{ \code{TRUE}, \code{FALSE}, or a \link[=regex]{regular expression} matching the argument values to evaluate in the plot call. See below. }
  \item{envir}{ \code{\link{environment}} to use in evaluating the call arguments (see \code{eval.args}) }
  \item{restore.on.close}{ a \code{gtkWindow} to bring to front when the plot window is closed. }
}
\details{
  \code{plotAndPlay} works best with Lattice plots. There is only basic support for traditional graphics plots.
  
  Each element of \code{buttons} is a named list passed as arguments to 
  \code{function(label, icon.name=NULL, f, data=list(), isToggle=F)}, with:
  \describe{
  \item{label}{ the button label. }
  \item{icon.name}{ name of the GTK icon to use, starting with "gtk-". See \url{http://developer.gnome.org/doc/API/2.0/gtk/gtk-Stock-Items.html\#GTK-STOCK-ABOUT:CAPS} for a list. }
  \item{f}{ function to be called when the button is clicked ("callback function"). }
  \item{data}{ list of any extra data to be passed to the callback function. 
  Note that the data (list) passed to callback functions always includes an item \code{name} giving the plot identifier.}
  \item{isToggle}{ whether the button should be a \code{gtkToggleToolButton}. }
  }
  
  For example, one element of \code{plotAndPlayButtons} is
  \code{zoomout=list("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event)}.
  New callback functions should be adapted from the pre-defined ones.
  
  If all arguments are evaluated (\code{eval.args=T}) then the PlotAndPlay window will be independent of the R session and any changes to the original data. But automatic axis labelling will get screwed up.
  If arguments are not evaluated until plot time (\code{eval.args=F}) then less memory will be used to store the call, but the plot will depend on all the variables used as arguments remaining accessible (i.e. in the global environment). Also the plot will change along with the original data (and may cause confusion, e.g. \code{labels} may be wrong).
  So if \code{plotAndPlay} is called inside a function, any local variables will need to be evaluated as they will be inaccessible to callback functions (so interaction will fail).
  Setting \code{eval.args="^tmp"} will evaluate any variables appearing in the plot call whose names begin with "tmp".
}
\value{
	\code{plotAndPlay} invisibly returns the value from the plot call
	(which for lattice plots is a "trellis" object).
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \link{Lattice}, \code{\link{xyplot}} }
\examples{
\dontrun{
require(lattice)

plotAndPlay(xyplot(Income ~ Population / Area | state.region, 
	data=data.frame(state.x77)))

## same plot with two panels per page
plotAndPlay(xyplot(Income ~ Population / Area | state.region, 
	data=data.frame(state.x77), layout=c(2,1)), name="other")

## time series plot: navigate x-axis only, transform y-axis
treering2 <- window(treering, 0, 1979)
plotAndPlay(plot(treering2), nav.scales="x", trans.scales="y", 
	labels=paste(time(treering2),"CE"), 
	extra.buttons=plotAndPlayButtons['logscale'])

# see what the current call is
str(as.list(plotAndPlayGTK:::StateEnv[["plot"]]$call))

# simple brushing is possible, thanks to Deepayan Sarkar
plotAndPlay(splom(environmental))

# simple spin and zoom for 3D plots
plotAndPlay(wireframe(volcano))

}
}

\keyword{ iplot }
\keyword{ dynamic }
