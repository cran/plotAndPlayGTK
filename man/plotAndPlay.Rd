\name{plotAndPlay}
\alias{plotAndPlay}
\title{An interactive plot GUI}
\description{
  Open a GTK+ window to view and interact with a plot.
}
\usage{
plotAndPlay(expr, name = "plot", nav.scales = c("x","y"), trans.scales = c("y"), 
	    buttons = list("identify", "zoomin", "zoomout", "zoomfit", "centre"), 
	    extra.buttons = list("zero"), basic.buttons=plotAndPlayBasicButtons, 
	    labels = NULL, label.args = list(cex = 0.7), identify.call = NULL, 
	    plot.call, is.lattice = (callName \%in\% latticeNames), 
	    eval.args = F, envir = parent.frame(), restore.on.close = NULL)
}
\arguments{
  \item{expr}{ an expression to create a plot, like \code{plot(mydata)}. Note, this can have lots of arguments, just like a normal plot call (see examples). }
  \item{name}{ character value to identify the plot window. }
  \item{nav.scales}{ one or more of \code{c("x","y")} defining which scale(s) to navigate along (e.g. zoom in). }
  \item{trans.scales}{ one or more of \code{c("x","y")} defining which scale(s) to transform (e.g. log). }
  \item{buttons}{ a list of buttons for the toolbar.
	Each element should be a call to generate a \code{\link[RGtk2]{gtkToolItem}}, or a character string. 
	If it is a character string, that element is taken from the pre-defined \code{plotAndPlayButtons}.
  	See the \code{Details} and \code{Examples} sections. }
  \item{extra.buttons}{ same as \code{buttons}, provided for convenient additions to the default set. }
  \item{basic.buttons}{ same as \code{buttons}, the default here is almost always useful, so you probably do not want to change this. }
  \item{labels}{ a character vector of labels for data points, for use in \code{\link{identify}}. 
  	If missing, it will be guessed from the plot call. If \code{identify.call} is given, this is ignored. }
  \item{label.args}{ a list of arguments passed to \code{\link{identify}} and \code{\link{text}}: can include \code{cex}, \code{col} and \code{font}. 
	Will also be added to \code{identify.call} if that is given. }
  \item{identify.call}{ a \code{\link{call}} to be evaluated when the identify button is clicked. 
  	If missing, it will be constructed from the plot call. Generally this argument should not be needed. }
  \item{plot.call}{ a plot call ( \code{\link{call}} object), if given this is used instead of \code{expr}. }
  \item{is.lattice}{ whether the plot is a \link{Lattice} plot, i.e. returns a trellis object.
  	If the called function is in the \code{lattice} package it will be recognised automatically. }
  \item{eval.args}{ \code{TRUE}, \code{FALSE}, or a \link[=regex]{regular expression} matching the argument values to evaluate in the plot call. See below. }
  \item{envir}{ \code{\link{environment}} to use in evaluating the call arguments (see \code{eval.args}) }
  \item{restore.on.close}{ a \code{gtkWindow} to bring to front when the plot window is closed. }
}
\details{
  This function (or rather, the default set of button handlers) works best with Lattice plots. 
  There is some support for traditional graphics plots, but this has not been well tested.
  
  Each element of \code{buttons} should be a call to generate a \code{\link[RGtk2]{gtkToolItem}} (typically a \code{\link[RGtk2]{gtkToolButton}}),
  or text, in which case it is taken from the pre-defined set.
  Type \code{str(plotAndPlayButtons)} to see the pre-defined set of button handlers.
  However, you can usually ignore this, as most buttons will be automatically added to the toolbar for relevant plots. 
  Specifically: \itemize{
	  \item \code{"expand"} will be added to lattice plots with more than one panel per page. 
	  An alternative is \code{"focus"}, which is the same but does not expand the chosen panel to the full window.
	  \item \code{c("prev.page", "next.page")} will be added to lattice plots with more than one page.
	  \item \code{c("zoomin.3d", "zoomout.3d", "fly.left.3d", "fly.right.3d")} will be added to \code{\link{wireframe}} and \code{\link{cloud}} 
	  (note, these do not yet work with non-Lattice plots like \code{\link{persp}}).
	  \item \code{c("brush", "brush.region", "brush.drag", "clear")} will be added to \code{\link{splom}} 
	  (note, these do not yet work with non-Lattice plots like \code{\link{pairs}}).
  }
  See \code{\link{plotAndPlayButtons}} if you want to define new buttons.
  
  If arguments to the plot call are not evaluated until plot time (\code{eval.args=F}, the default) then automatic axis labelling will work correctly, 
  but the plot will depend on all the variables used as arguments remaining accessible (i.e. in the global environment). 
  Arguments to the plot call will be evaluated each time the plot is updated -- so random data as in \code{plot(rnorm(100))} will keep changing, with confusing consequences!
  You should either generate random data first (in the global environment) or set \code{eval.args=T}.
  Similarly, changes to the original data will cause inconsistencies in previously generated plots (e.g. \code{labels} on data points may be wrong).
  If \code{plotAndPlay} is called inside a function, any local variables will need to be evaluated, otherwise they will be inaccessible to callback functions (so interaction will fail).
  
  If all plot arguments are evaluated (\code{eval.args=T}) then the PlotAndPlay window will be able to function independently. 
  But automatic axis labelling will get screwed up (because the symbols are destroyed by evaluation). And it will use more memory to store the data.
  A compromise may be reached by using regular expressions. Setting \code{eval.args="^tmp"} will evaluate any variables appearing in the plot call whose names begin with "tmp".
}
\value{
	\code{plotAndPlay} invisibly returns the value from the plot call
	(which for lattice plots is a \code{\link[=print.trellis]{trellis}} object).
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{plotAndPlayButtons}}, \code{\link{xyplot}}, \link{Lattice} }
\examples{
\dontrun{
## from example(plot.default)
plotAndPlay(plot(cars$speed, cars$dist, xlab="Speed", ylab="Distance",
     panel.first = lines(lowess(cars$speed, cars$dist), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue"))

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
	extra.buttons=list("logscale"))

## see what the current call is (for last focused plot window)
str(as.list(plotAndPlayGetCurrState()$call))

## simple brushing is possible, based on lattice::panel.brush.splom
plotAndPlay(splom(environmental))

## simple spin and zoom for 3D plots
plotAndPlay(wireframe(volcano))

## see help(plotAndPlayButtons) for examples of defining new buttons!

}
}

\keyword{ iplot }
\keyword{ dynamic }
