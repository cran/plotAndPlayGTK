\name{plotAndPlayButtons}
\alias{plotAndPlayButtons}
\alias{plotAndPlayBasicButtons}
\alias{quickTool}
\alias{plotAndPlayGetCurrState}
\alias{plotAndPlaySetCurrState}
\alias{plotAndPlayUpdate}
\alias{plotAndPlayDoFocus}
\alias{plotAndPlayMakePrompt}
\alias{plotAndPlayUnmakePrompt}
\alias{plotAndPlaySetPrompt}
\alias{plotAndPlayGetToolbar}
\alias{plotAndPlayGetDA}
\title{Utilities for plotAndPlayGTK buttons}
\description{
  Make new buttons for a \code{\link{plotAndPlay}} plot window.
}
\usage{
plotAndPlayButtons
plotAndPlayBasicButtons

quickTool(label, icon.name = NULL, tooltip = NULL, f, data = NULL, isToggle = F)

plotAndPlayGetCurrState()
plotAndPlaySetCurrState(state)

plotAndPlayUpdate()

plotAndPlayDoFocus(highlight = T, ...)
plotAndPlayMakePrompt()
plotAndPlayUnmakePrompt()
plotAndPlaySetPrompt(text)

plotAndPlayGetToolbar()
plotAndPlayGetDA()
}
\arguments{
  \item{label}{ the button label. }
  \item{icon.name}{ name of the GTK icon to use, starting with "gtk-". 
  See \url{http://developer.gnome.org/doc/API/2.0/gtk/gtk-Stock-Items.html\#GTK-STOCK-ABOUT:CAPS} for a list. }
  \item{tooltip}{ the button tooltip. }
  \item{f}{ function to be called when the button is clicked ("callback function"). }
  \item{data}{ extra data to be passed to the callback function. }
  \item{isToggle}{ whether the button should be a \code{gtkToggleToolButton}. }
  \item{state}{ a list containing state information for the plot window. See below. }
  \item{highlight, \dots}{ passed to \code{\link{trellis.focus}}. }
  \item{text}{ text to display in the plot window prompt. }
}
\details{
  \code{plotAndPlayButtons} and \code{plotAndPlayBasicButtons} are lists of calls to generate \code{\link[RGtk2]{gtkToolItem}}s.
  These are used as buttons in a \code{\link{plotAndPlay}} plot window.
  
  New buttons can be defined: see the \code{Examples} section of this page.
  
  \code{quickTool} is a convenience function to create a \code{\link[RGtk2]{gtkToolButton}} with the given label, icon, tooltip and click handler.
  For example, one element of \code{plotAndPlayButtons} is
  \code{zoomout=quickTool("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event)}.
  
  \code{plotAndPlayGetCurrState} can be used by callback functions to get access to the plot window and plot state. 
  It returns a list (see below for details), which can be modified and passed back to \code{plotAndPlaySetCurrState}.
  
  \code{plotAndPlayUpdate} re-generates the current plot display (e.g. after updating the call).
  
  \code{plotAndPlayDoFocus} brings one lattice panel into focus. 
  If there is only one panel it is chosen automatically and without highlighting.
  Otherwise the user is prompted to choose one. 
  It returns the new focus (see \code{focus} item below) or NULL if the user cancelled.
  
  The plotAndPlay \code{\link{plotAndPlay}} window can show a simple prompt to the user.
  This must be initialised with \code{plotAndPlayMakePrompt}, then set with \code{plotAndPlaySetPrompt}, 
  and finally removed with \code{plotAndPlayUnmakePrompt}.
  
  \code{plotAndPlayGetToolbar} returns the \code{gtkToolbar}, 
  and \code{plotAndPlayGetDA} returns the \code{gtkDrawingArea}.
  
}
\value{
  \code{plotAndPlayGetCurrState} returns a list containing at least:
  \item{win}{a \code{gtkWindow} (the plot window).}
  \item{dev}{the plot device number.}
  \item{call}{the current plot call.}
  \item{id.call}{the call to \code{\link{identify}} or similar, used to label data points.}
  \item{label.args}{style arguments for labelling points.}
  \item{nav.scales}{see \code{\link{plotAndPlay}}.}
  \item{trans.scales}{see \code{\link{plotAndPlay}}.}
  \item{ids}{a list, each item contains a vector of subscripts for the points currently labelled in the named lattice packet. For non-lattice plots, there is only one item \code{"all"}.}
  \item{brushed}{a list, each item contains a vector of subscripts for the points currently brushed in the named lattice packet. }
  \item{is.lattice}{logical: is it a lattice plot?}
  \item{focus}{a list with items \code{col} and \code{row} for a lattice panel if one is in focus. Otherwise it is \code{list(col=0, row=0)}.}
  \item{page}{current lattice page number.}
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{plotAndPlay}} }
\examples{
\dontrun{

## defining new buttons (and other widgets) for the toolbar...

require(lattice)

## A kind of "OK" button: It gets the subscripts of currently labelled points 
## (from 'identify') and passes them to a function: in this case, just 'print'.
showids_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	ids <- unique(unlist(tmp.state$ids))
	print(ids)
}
showids_button <- quote(quickTool("Show IDs", "gtk-yes", 
	tooltip="Print out indices of the selected points", f=showids_handler))
plotAndPlay(xyplot(Income ~ Population / Area, data=data.frame(state.x77)), 
	extra.buttons=list(showids_button))

## A toggle button to add a smoothing line to xyplot
## (assumes the 'type' argument will be passed to panel.xyplot)
smooth_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# get the current 'type' argument
	plotType <- eval(tmp.state$call$type)
	if (is.null(plotType)) plotType <- "p"
	# remove "smooth" type if it is already there
	if (!is.na(i <- match("smooth", plotType))) plotType <- plotType[-i]
	# add "smooth" type if the toggle button is active
	if (widget[["active"]]) plotType <- c(plotType, "smooth")
	# update state
	tmp.state$call$type <- plotType
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}
smooth_button <- quote(quickTool("Smooth", "gtk-add", f=smooth_handler, 
	tooltip="Overlay loess smooth (with default span=2/3)", isToggle=T))
plotAndPlay(xyplot(sunspot.year ~ 1700:1988, type="l"), 
	extra.buttons=list(smooth_button))

## Get the current plot limits and print them (works with lattice / traditional)
## It would be simpler to grab the xlim/ylim arguments, but they might be NULL.
showlims_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	# get current plot limits
	if (tmp.state$is.lattice) {
		# lattice plot
		if (!any(tmp.state$focus)) {
			trellis.focus("panel", 1, 1, highlight=F)
		}
		require(grid)
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		if (!any(tmp.state$focus)) { trellis.unfocus() }
		# convert back from log scale if required
		newlims <- plotAndPlayGTK:::latticeUnLog(xlim, ylim, 
			tmp.state$call$scales)
		xlim <- newlims$x
		ylim <- newlims$y
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		if (par("xlog")) xlim <- 10 ^ xlim
		if (par("ylog")) ylim <- 10 ^ ylim
	}
	print(list(x=signif(xlim, 3), y=signif(ylim, 3)))
}
showlims_button <- quote(quickTool("Limits", "gtk-yes",
	tooltip="Print out current plot limits", f=showlims_handler))
plotAndPlay(xyplot(Income ~ Population / Area, data=data.frame(state.x77)), 
	extra.buttons=c(list("logscale", "zero"), showlims_button))

## A more complex toolbar item:
## A "spinbutton" to choose a number 'n', then group the data into 'n' clusters.
my_cluster_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetCurrState()
	n <- widget[["value"]]
	xy <- xy.coords(eval(tmp.state$call$x), eval(tmp.state$call$y), recycle=T)
	groups <- NULL
	if (n > 1) {
		clusts <- kmeans(cbind(xy$x,xy$y), n)
		labels <- paste("#", 1:n, " (n = ", clusts$size, ")", sep="")
		groups <- factor(clusts$cluster, labels=labels)
	}
	# update state
	tmp.state$call$groups <- groups
	plotAndPlaySetCurrState(tmp.state)
	plotAndPlayUpdate()
}
my_cluster_spinner <- quote({
	spinner <- gtkSpinButton(min=1, max=10, step=1)
	spinner[["value"]] <- 1
	gSignalConnect(spinner, "value-changed", my_cluster_handler)
	vbox <- gtkVBox()
	vbox$packStart(gtkLabel("Clusters:"))
	vbox$packStart(spinner)
	foo <- gtkToolItem()
	foo$add(vbox)
	foo
})
# need to generate random data first (or use eval.args=T), otherwise it changes!
xdata <- rnorm(100)
ydata <- rnorm(100) * xdata / 2
plotAndPlay(xyplot(ydata ~ xdata, aspect="iso", auto.key=list(space="right")), 
	extra.buttons=list(my_cluster_spinner))

}
}

\keyword{ utilities }