## plotAndPlayGTK: interactive plots in R using GTK
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>, GPL

MAJOR <- "0"
MINOR <- "7"
REVISION <- unlist(strsplit("$Revision: 16 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- "(c) 2007 Felix Andrews <felix@nfrac.org>, GPL"
WEBSITE <- "http://code.google.com/p/plotandplay-gtk/"

## LICENSE
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version. See the file gpl-license.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

.StateEnv <- new.env()
.StateEnv$win <- list()
.StateEnv$dev <- list()
.StateEnv$call <- list()
.StateEnv$id.call <- list()
.StateEnv$focus <- list()
.StateEnv$nav.scales <- list()
.StateEnv$trans.scales <- list()
.StateEnv$is.lattice <- list()
.StateEnv$page <- list()

plotAndPlayButtons <- alist(
	identify=list("Identify data", "gtk-info", f=.plotAndPlay_identify_event),
	zoomin=list("Zoom in", "gtk-zoom-in", f=.plotAndPlay_zoomin_event),
	zoomout=list("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event),
	centre=list("Re-centre", "gtk-jump-to-ltr", f=.plotAndPlay_centre_event),
	zero=list("Full scale", "gtk-goto-bottom", f=.plotAndPlay_zero_event, isToggle=T),
	logscale=list("Log scale", "gtk-goto-top", f=.plotAndPlay_logscale_event, isToggle=T),
	focus=list("Choose panel", "gtk-select-color", f=.plotAndPlay_focus_event),
	prev.page=list("Prev page", "gtk-go-back-ltr", f=.plotAndPlay_prevpage_event),
	next.page=list("Next page", "gtk-go-forward-ltr", f=.plotAndPlay_nextpage_event)
)
#"gtk-index" / "gtk-preferences"
#"gtk-zoom-100" (relation="iso")

latticeNames <- c("barchart", "bwplot", "cloud", "contourplot", "densityplot", 
	"dotplot", "histogram", "levelplot", "parallel", "qq", "qqmath", "rfs", 
	"splom", "stripplot", "tmd", "wireframe", "xyplot")

plotAndPlay <- function(expr, name="plot", plot.call, nav.scales=c("x","y"), trans.scales=c("y"), buttons=plotAndPlayButtons[c("identify", "zoomin", "zoomout", "centre")], extra.buttons=plotAndPlayButtons[c("zero")], labels=NULL, identify.call=NULL, is.lattice=(callName %in% latticeNames), eval.args.pattern=".", envir=parent.frame()) {
	if (!require("cairoDevice", quietly=TRUE)) {
		stop("Require package 'cairoDevice'")
	}
	if (missing(plot.call)) {
		plot.call <- substitute(expr)
	}
	# check types
	if (!is.call(plot.call)) { stop("'plot.call' should be a call object") }
	if (!is.character(name)) { stop("'name' should be character") }
	nav.scales <- match.arg(nav.scales, c("x","y"), several.ok=T)
	trans.scales <- match.arg(trans.scales, c("x","y"), several.ok=T)
	if (!is.null(buttons) && !is.list(buttons)) {
		stop("'buttons' should be a list (like 'plotAndPlayButtons') or NULL")
	}
	if (!is.null(extra.buttons) && !is.list(extra.buttons)) {
		stop("'extra.buttons' should be a list (like 'plotAndPlayButtons') or NULL")
	}
	# put call into canonical form and evaluate its arguments
	plot.call <- match.call(eval(plot.call[[1]]), plot.call)
	plot.call <- evalCallArgs(plot.call, envir=envir, pattern=eval.args.pattern)
	callName <- deparse(plot.call[[1]])
	if (is.lattice) {
		# need this for correctly identifying points
		plot.call$subscripts <- TRUE
	}
	# try to construct a call to identify() if it was not supplied
	if (missing(identify.call)) {
		if (is.lattice) {
			# lattice plot
			identify.call <- call('panel.identify')
			# try to guess labels
			if (missing(labels)) {
				if ('data' %in% names(plot.call)) {
					labels <- row.names(plot.call$data)
				}
			}
			if (!is.null(labels)) {
				identify.call$labels <- bquote(
					.(labels)[trellis.panelArgs()$subscripts]
				)
			}
		} else {
			# traditional graphics plot
			identify.call <- call('identify')
			callArgNames <- names(plot.call)
			if ('x' %in% names(plot.call)) {
				identify.call$x <- plot.call$x
			}
			if ('y' %in% names(plot.call)) {
				identify.call$y <- plot.call$y
			}
			# try to guess labels
			if (missing(labels)) {
				if ('x' %in% names(plot.call)) {
					labels <- row.names(plot.call$x)
					if (is.null(labels)) {
						if (is.ts(plot.call$x)) {
							labels <- time(x)
						}
					}
				}
			}
			if (!is.null(labels)) {
				identify.call$labels <- labels
			}
		}
		identify.call$cex <- 0.75
	}
	# check whether the window already exists
	if (is.null(.StateEnv$win[[name]])) {
		# create a new GTK window and set up cairoDevice for plotting
		myWin <- gtkWindow(show=FALSE)
		myWin[["default-width"]] <- 600
		myWin[["default-height"]] <- 400
		myWin[["title"]] <- paste("plotAndPlay:", name)
		gSignalConnect(myWin, "delete-event", .plotAndPlay_close_event, 
			data=list(name=name))
		myWin$show()
		myVBox <- gtkVBox()
		myWin$add(myVBox)
		myDA <- gtkDrawingArea()
		myVBox$packStart(myDA)
		asCairoDevice(myDA)
		trellis.device(new=F)
	} else {
		# window exists
		myWin <- .StateEnv$win[[name]]
		myVBox <- myWin$getChildren()[[1]]
		# switch to the device
		dev.set(.StateEnv$dev[[name]])
		# blank the plot
		.StateEnv$call[[name]] <- NULL
		plot.new()
		# remove the toolbar, it will be recreated
		plotAndPlayGetToolbar(name)$destroy()
	}
	# set which buttons are visible
	if (is.lattice) {
		result <- eval(plot.call)
		nPanels <- prod(dim(result))
		nPerPage <- nPanels
		myLayout <- plot.call$layout
		if (!is.null(myLayout)) {
			nPerPage <- myLayout[1] * myLayout[2]
			if (myLayout[1] == 0) { nPerPage <- myLayout[2] }
			if (nPanels > nPerPage) {
				buttons$prev.page <- plotAndPlayButtons$prev.page
				buttons$next.page <- plotAndPlayButtons$next.page
			}
			# needed for page navigation (packet.panel.page)
			plot.call$layout[3] <- 1
		}
		if (nPerPage > 1) {
			buttons$focus <- plotAndPlayButtons$focus
		}
	}
	buttons <- c(buttons, extra.buttons)
	theToolbar <- gtkToolbar()
	theToolbar[["toolbar-style"]] <- GtkToolbarStyle['both']
	myVBox$packStart(theToolbar, expand=FALSE)
	gtkToolButtonWithCallback <- function(label, icon.name=NULL, f, data=NULL, isToggle=F) {
		x <- if (isToggle) { gtkToggleToolButton() } else { gtkToolButton() }
		x[["label"]] <- label
		x[["icon-name"]] <- icon.name
		gSignalConnect(x, "clicked", f, data=data)
		x
	}
	for (buttonSpec in buttons) {
		xargs <- eval(buttonSpec)
		xargs$data$name <- name
		newButton <- do.call(gtkToolButtonWithCallback, xargs)
		theToolbar$insert(newButton, -1)
	}
	# insert standard toolbar items: save etc
	theToolbar$insert(gtkSeparatorToolItem(), -1)
	saveButton <- gtkMenuToolButton(gtkImageNewFromStock('gtk-save-as', 
		size=GtkIconSize['small-toolbar']), label="Save as")
	gSignalConnect(saveButton, "clicked", .plotAndPlay_save_event, data=list(name=name))
	theToolbar$insert(saveButton, -1)
	saveMenu <- gtkMenu()
	saveItemPDF <- gtkMenuItem("PDF")
	saveItemPNG <- gtkMenuItem("PNG (bitmap)")
	saveItemPS <- gtkMenuItem("PostScript")
	saveItemSVG <- gtkMenuItem("SVG")
	saveMenu$append(saveItemPDF)
	saveMenu$append(saveItemPNG)
	saveMenu$append(saveItemPS)
	saveMenu$append(saveItemSVG)
	saveButton$setMenu(saveMenu)
	gSignalConnect(saveItemPDF, "activate", .plotAndPlay_save_event, 
		data=list(name=name, ext="pdf"))
	gSignalConnect(saveItemPNG, "activate", .plotAndPlay_save_event, 
		data=list(name=name, ext="png"))
	gSignalConnect(saveItemPS, "activate", .plotAndPlay_save_event, 
		data=list(name=name, ext="ps"))
	gSignalConnect(saveItemSVG, "activate", .plotAndPlay_save_event, 
		data=list(name=name, ext="svg"))
	if (is.lattice) {
		greyButton <- gtkToolButtonWithCallback("Greyscale", "gtk-print-preview", 
			f=.plotAndPlay_greyscale_event, data=list(name=name), isToggle=T)
		theToolbar$insert(greyButton, -1)
	}
	# bring window to front
	myWin$present()
	# store the state of this plot window
	.StateEnv$win[[name]] <- myWin
	.StateEnv$dev[[name]] <- dev.cur()
	.StateEnv$call[[name]] <- plot.call
	.StateEnv$id.call[[name]] <- identify.call
	.StateEnv$focus[[name]] <- list(col=0, row=0)
	.StateEnv$nav.scales[[name]] <- nav.scales
	.StateEnv$trans.scales[[name]] <- trans.scales
	.StateEnv$is.lattice[[name]] <- is.lattice
	.StateEnv$page[[name]] <- 1
	# do the plot
	result <- eval(plot.call)
	if (identical(class(result), "trellis")) { print(result) }
	invisible(result)
}

.plotAndPlay_close_event <- function(widget, event, user.data) {
	name <- user.data$name
	if (!is.null(.StateEnv$win[[name]])) {
		.StateEnv$win[[name]]$destroy()
	}
	.StateEnv$win[[name]] <- NULL
	.StateEnv$dev[[name]] <- NULL
	.StateEnv$call[[name]] <- NULL
	.StateEnv$id.call[[name]] <- NULL
	.StateEnv$focus[[name]] <- NULL
}

.plotAndPlay_save_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# get filename
	myExt <- if (!is.null(user.data$ext)) { user.data$ext } else { 'pdf' }
	myDefault <- paste(name, '.', myExt, sep='')
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/png/ps/svg)", 
		filters=Filters[c("pdf","png","ps","All"),],
		index=match(myExt, c("pdf","png","ps","svg"))
	)
	.StateEnv$win[[name]]$present()
	if (is.na(filename)) { return() }
	ext <- tolower(get.extension(filename))
	if (ext == "") {
		filename <- paste(filename, '.pdf', sep='')
		ext <- 'pdf'
	}
	# save plot to file
	mySize <- plotAndPlayGetDA(name)$getAllocation()
	myWidth <- mySize$width
	myHeight <- mySize$height
	myScale <- 1/72
	myWidth <- myWidth * myScale
	myHeight <- myHeight * myScale
	if (ext %in% "pdf") {
		dev.copy(pdf, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% c("ps", "eps")) {
		dev.copy(postscript, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "png") {
		dev.copy(Cairo_png, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "svg") {
		dev.copy(Cairo_svg, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else {
		errorDialog("Unrecognised filename extension")
		return()
	}
}

.plotAndPlay_greyscale_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# get new greyscale setting
	greyscale <- widget$getActive()
	# make change and re-draw plot
	if (greyscale) {
		trellis.device(new=F, color=F)
	} else {
		trellis.device(new=F)
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_zoomin_event <- function(widget, user.data) {
	name <- user.data$name
	nav.x <- ("x" %in% .StateEnv$nav.scales[[name]])
	nav.y <- ("y" %in% .StateEnv$nav.scales[[name]])
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	lowEdge <- "bottom-left corner"
	if (!nav.y) { lowEdge <- "left edge" }
	if (!nav.x) { lowEdge <- "bottom edge" }
	highEdge <- "top-right corner"
	if (!nav.y) { highEdge <- "right edge" }
	if (!nav.x) { highEdge <- "top edge" }
	plotAndPlaySetPrompt(name, paste("Click at the", lowEdge, 
		"of the region to zoom in to"))
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.5)), name="tmp.mask")
	# get new scales interactively
	if (.StateEnv$is.lattice[[name]]) {
		# lattice plot
		if (!any(.StateEnv$focus[[name]])) { trellis.focus("panel", 1, 1) }
		xlim <- as.numeric(convertX(unit(0:1, "npc"), "native"))
		ylim <- as.numeric(convertY(unit(0:1, "npc"), "native"))
		# get lower limits
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(.StateEnv$focus[[name]])) { trellis.unfocus() }
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new <- if (nav.x) { clickLoc$x } else { xlim[1] }
		ylim.new <- if (nav.y) { clickLoc$y } else { ylim[1] }
		# mask out lower regions
		grid.draw(editGrob(maskGrob, 
			x=unit(0,"npc"), width=unit(xlim.new[1] - xlim[1],"native"), 
			just="left"))
		grid.draw(editGrob(maskGrob,
			y=unit(0,"npc"), height=unit(ylim.new[1] - ylim[1],"native"),
			x=unit(1,"npc"), width=unit(xlim[2] - xlim.new[1],"native"),
			just=c("right", "bottom")))
		# get upper limits
		plotAndPlaySetPrompt(name, paste("OK, now click at the", highEdge))
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			grid.remove("tmp.mask", grep=T, global=T, strict=T, redraw=F)
			plotAndPlayUpdate(name)
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new[2] <- if (nav.x) { clickLoc$x } else { xlim[2] }
		ylim.new[2] <- if (nav.y) { clickLoc$y } else { ylim[2] }
		# mask out upper regions
		grid.draw(editGrob(maskGrob, 
			x=unit(1,"npc"), width=unit(xlim[2] - xlim.new[2],"native"), 
			y=unit(1,"npc"), height=unit(ylim[2] - ylim.new[1],"native"),
			just=c("right", "top")))
		grid.draw(editGrob(maskGrob, 
			y=unit(1,"npc"),
			height=unit(ylim[2] - ylim.new[2],"native"),
			x=unit(xlim.new[2],"native"), 
			width=unit(xlim.new[2] - xlim.new[1],"native"),
			just=c("right", "top")))
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim.new, ylim.new, 
			.StateEnv$call[[name]]$scales)
		if (nav.x) { .StateEnv$call[[name]]$xlim <- newlims$x }
		if (nav.y) { .StateEnv$call[[name]]$ylim <- newlims$y }
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# get lower limits
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		xlim.new <- if (nav.x) { clickLoc$x } else { xlim[1] }
		ylim.new <- if (nav.y) { clickLoc$y } else { ylim[1] }
		if (nav.x) { abline(v=xlim.new, col="red") }
		if (nav.y) { abline(h=ylim.new, col="red") }
		# get upper limits
		plotAndPlaySetPrompt(name, paste("OK, now click at the", highEdge))
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		xlim.new[2] <- if (nav.x) { clickLoc$x } else { xlim[2] }
		ylim.new[2] <- if (nav.y) { clickLoc$y } else { ylim[2] }
		if (nav.x) { abline(v=xlim.new[2], col="red") }
		if (nav.y) { abline(h=ylim.new[2], col="red") }
		# convert back from log scale if required (TODO)
		if (nav.x) { .StateEnv$call[[name]]$xlim <- xlim.new }
		if (nav.y) { .StateEnv$call[[name]]$ylim <- ylim.new }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_zoomout_event <- function(widget, user.data) {
	name <- user.data$name
	nav.x <- ("x" %in% .StateEnv$nav.scales[[name]])
	nav.y <- ("y" %in% .StateEnv$nav.scales[[name]])
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# find existing scales and update call
	if (.StateEnv$is.lattice[[name]]) {
		# lattice plot
		if (!any(.StateEnv$focus[[name]])) {
			trellis.focus("panel", 1, 1, highlight=F)
		}
		xlim <- as.numeric(convertX(unit(0:1, "npc"), "native"))
		ylim <- as.numeric(convertY(unit(0:1, "npc"), "native"))
		# zoom out: make range twice the size
		if (nav.x) { xlim <- xlim + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- ylim + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim, ylim, .StateEnv$call[[name]]$scales)
		if (nav.x) { .StateEnv$call[[name]]$xlim <- newlims$x }
		if (nav.y) { .StateEnv$call[[name]]$ylim <- newlims$y }
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# zoom out: make range twice the size
		if (nav.x) { xlim <- xlim + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- ylim + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required (TODO)
		if (nav.x) { .StateEnv$call[[name]]$xlim <- xlim }
		if (nav.y) { .StateEnv$call[[name]]$ylim <- ylim }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_centre_event <- function(widget, user.data) {
	name <- user.data$name
	nav.x <- ("x" %in% .StateEnv$nav.scales[[name]])
	nav.y <- ("y" %in% .StateEnv$nav.scales[[name]])
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	plotAndPlaySetPrompt(name, "Click to re-centre the plot")
	# get new scales interactively
	if (.StateEnv$is.lattice[[name]]) {
		# lattice plot
		if (!any(.StateEnv$focus[[name]])) { trellis.focus("panel", 1, 1) }
		xlim <- as.numeric(convertX(unit(0:1, "npc"), "native"))
		ylim <- as.numeric(convertY(unit(0:1, "npc"), "native"))
		# get new centre point
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(.StateEnv$focus[[name]])) { trellis.unfocus() }
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		if (nav.x) { xlim <- clickLoc$x + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- clickLoc$y + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim, ylim, .StateEnv$call[[name]]$scales)
		if (nav.x) { .StateEnv$call[[name]]$xlim <- newlims$x }
		if (nav.y) { .StateEnv$call[[name]]$ylim <- newlims$y }
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# get new centre point
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		if (nav.x) { xlim <- clickLoc$x + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- clickLoc$y + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required (TODO)
		if (nav.x) { .StateEnv$call[[name]]$xlim <- xlim }
		if (nav.y) { .StateEnv$call[[name]]$ylim <- ylim }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_focus_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	plotAndPlaySetPrompt(name, "Click on a panel to focus on (for further interaction)")
	newFocus <- trellis.clickFocus()
	plotAndPlaySetPrompt(name, "")
	if (!is.null(newFocus)) {
		.StateEnv$focus[[name]] <- newFocus
	}
}

.plotAndPlay_identify_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	plotAndPlaySetPrompt(name, paste("Identifying data points...",
		"Click the right mouse button to finish."))
	# do identify
	idCall <- .StateEnv$id.call[[name]]
	if (.StateEnv$is.lattice[[name]]) {
		# lattice plot
		if (!any(.StateEnv$focus[[name]])) {
			if (length(trellis.currentLayout()) == 1) {
				trellis.focus("panel", 1, 1)
			} else {
				plotAndPlaySetPrompt(name, "First, choose a panel")
				trellis.clickFocus()
			}
			plotAndPlaySetPrompt(name, paste("Identifying data points...",
				"Click the right mouse button to finish."))
		}
		ids <- eval(idCall)
		# TODO: store this to maintain state?
		if (!any(.StateEnv$focus[[name]])) { trellis.unfocus() }
	} else {
		# traditional graphics plot
		ids <- eval(idCall)
	}
}

.plotAndPlay_zero_event <- function(widget, user.data) {
	name <- user.data$name
	trans.x <- ("x" %in% .StateEnv$trans.scales[[name]])
	trans.y <- ("y" %in% .StateEnv$trans.scales[[name]])
	isX <- isTRUE(user.data$x)
	isY <- isTRUE(user.data$y)
	if (!isX && !isY) { isX <- isY <- T }
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(.StateEnv$dev[[name]])
	on.exit(dev.set(oldDev), add=T)
	# get new zero scale setting
	zeroScale <- widget$getActive()
	# make change and re-draw plot
	if (zeroScale) {
		if (.StateEnv$is.lattice[[name]]) {
			# lattice plot
			if (!any(.StateEnv$focus[[name]])) {
				trellis.focus("panel", 1, 1, highlight=F)
			}
			xlim <- as.numeric(convertX(unit(0:1, "npc"), "native"))
			ylim <- as.numeric(convertY(unit(0:1, "npc"), "native"))
		} else {
			# traditional graphics plot
			xlim <- par("usr")[1:2]
			ylim <- par("usr")[3:4]
		}
		if (trans.x) {
			if (min(xlim) > 0) {
				xlim[which.min(xlim)] <- 0
			} else if (max(xlim) < 0) {
				xlim[which.max(xlim)] <- 0
			}
			.StateEnv$call[[name]]$xlim <- xlim
		}
		if (trans.y) {
			if (min(ylim) > 0) {
				ylim[which.min(ylim)] <- 0
			} else if (max(ylim) < 0) {
				ylim[which.max(ylim)] <- 0
			}
			.StateEnv$call[[name]]$ylim <- ylim
		}
	} else {
		if (trans.x) { .StateEnv$call[[name]]$xlim <- NULL }
		if (trans.y) { .StateEnv$call[[name]]$ylim <- NULL }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_logscale_event <- function(widget, user.data) {
	name <- user.data$name
	trans.x <- ("x" %in% .StateEnv$trans.scales[[name]])
	trans.y <- ("y" %in% .StateEnv$trans.scales[[name]])
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# get new log scale setting
	logScale <- widget$getActive()
	# make change and re-draw plot
	if (.StateEnv$is.lattice[[name]]) {
		if (trans.x && trans.y) {
			# apply to both scales
			.StateEnv$call[[name]]$scales$log <- logScale
		} else {
			if (trans.x) { .StateEnv$call[[name]]$scales$x$log <- logScale }
			if (trans.y) { .StateEnv$call[[name]]$scales$y$log <- logScale }
		}
	} else {
		logSpec <- "xy"
		if (!trans.y) { logSpec <- "x" }
		if (!trans.x) { logSpec <- "y" }
		.StateEnv$call[[name]]$log <- if (logScale) { logSpec }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_prevpage_event <- function(widget, user.data) {
	name <- user.data$name
	if (.StateEnv$page[[name]] == 1) { return() }
	.StateEnv$page[[name]] <- .StateEnv$page[[name]] - 1
	plotAndPlayUpdate(name)
}

.plotAndPlay_nextpage_event <- function(widget, user.data) {
	name <- user.data$name
	.StateEnv$page[[name]] <- .StateEnv$page[[name]] + 1
	plotAndPlayUpdate(name)
}

plotAndPlayUpdate <- function(name) {
	result <- eval(.StateEnv$call[[name]])
	if ("trellis" %in% class(result)) {
		curPage <- .StateEnv$page[[name]]
		print(result, packet.panel=packet.panel.page(curPage))
		if (any(.StateEnv$focus[[name]])) {
			with(.StateEnv$focus[[name]], 
				trellis.focus("panel", col, row))
		}
	}
}

plotAndPlayGetDA <- function(name) {
	.StateEnv$win[[name]]$getChildren()[[1]]$getChildren()[[1]]
}

plotAndPlayGetToolbar <- function(name) {
	.StateEnv$win[[name]]$getChildren()[[1]]$getChildren()[[2]]
}

plotAndPlayMakePrompt <- function(name) {
	theToolbar <- plotAndPlayGetToolbar(name)
	# first freeze the toolbar size so plot doesn't resize
	toolbarSize <- theToolbar$getAllocation()
	theToolbar$setSizeRequest(toolbarSize$width, toolbarSize$height)
	toolItems <- theToolbar$getChildren()
	for (x in rev(toolItems)) { x$hide() }
	myLabel <- gtkLabel()
	promptItem <- gtkToolItem()
	promptItem$setExpand(T)
	myEventBox <- gtkEventBox()
	myEventBox$add(myLabel)
	promptItem$add(myEventBox)
	theToolbar$insert(promptItem, 0)
	myEventBox$modifyBg(GtkStateType['insensitive'], "yellow")
	myLabel$modifyFg(GtkStateType['insensitive'], "black")
}

plotAndPlayUnmakePrompt <- function(name) {
	theToolbar <- plotAndPlayGetToolbar(name)
	theToolbar$getChildren()[[1]]$destroy()
	toolItems <- theToolbar$getChildren()
	for (x in toolItems) { x$show() }
	# unfreeze the toolbar size
	theToolbar$setSizeRequest(-1, -1)
}

plotAndPlaySetPrompt <- function(name, text) {
	if (!is.character(text)) { stop("'text' must be character") }
	theToolbar <- plotAndPlayGetToolbar(name)
	myLabel <- theToolbar$getChildren()[[1]]$getChildren()[[1]]$getChildren()[[1]]
	myLabel$setMarkup(paste(sep='','<big><b>', text, '</b></big>'))
}

trellis.clickFocus <- function() {
	layoutMatrix <- trellis.currentLayout()
	currVpp <- current.vpPath()
	if (!is.null(currVpp)) { upViewport(currVpp$n) }
	depth <- downViewport(trellis.vpname("panel", 1, 1))
	colRange <- current.viewport()$layout.pos.col[1]
	rowRange <- current.viewport()$layout.pos.row[1]
	upViewport()
	downViewport(trellis.vpname("panel", ncol(layoutMatrix), nrow(layoutMatrix)))
	colRange[2] <- current.viewport()$layout.pos.col[1]
	rowRange[2] <- current.viewport()$layout.pos.row[1]
	upViewport()
	layCols <- current.viewport()$layout$ncol
	layRows <- current.viewport()$layout$nrow
	leftPad <- sum(sapply(current.viewport()$layout$widths[1:(min(colRange)-1)], convertX, "npc"))
	rightPad <- sum(sapply(current.viewport()$layout$widths[(max(colRange)+1):layCols], convertX, "npc"))
	topPad <- sum(sapply(current.viewport()$layout$heights[1:(min(rowRange)-1)], convertY, "npc"))
	botPad <- sum(sapply(current.viewport()$layout$heights[(max(rowRange)+1):layRows], convertY, "npc"))
	clickLoc <- grid.locator("npc")
	# reset current viewport so lattice doesn't get confused
	upViewport(depth-1)
	if (!is.null(currVpp)) { downViewport(currVpp) }
	if (is.null(clickLoc)) {
		return(NULL)
	}
	clickLoc <- lapply(clickLoc, as.numeric)
	clickXScaled <- (clickLoc$x - leftPad) / (1 - leftPad - rightPad)
	focusCol <- ceiling(clickXScaled * ncol(layoutMatrix))
	clickYScaled <- (clickLoc$y - botPad) / (1 - botPad - topPad)
	focusRow <- ceiling(clickYScaled * nrow(layoutMatrix))
	if ((focusCol < 1) || (focusCol > ncol(layoutMatrix))
	 || (focusRow < 1) || (focusRow > nrow(layoutMatrix))) {
		focusCol <- focusRow <- 0
		trellis.unfocus()
	} else {
		trellis.focus("panel", focusCol, focusRow)
	}
	invisible(list(col=focusCol, row=focusRow))
}

# this function by Deepayan Sarkar
packet.panel.page <- function(n)
{
   ## returns a function that when used as the 'packet.panel'
   ## argument in print.trellis plots page number 'n' only
   function(layout, page, ...) {
       stopifnot(layout[3] == 1)
       packet.panel.default(layout = layout,
                            page = page + n - 1,
                            ...)
   }
}

latticeUnLog <- function(xlim, ylim, scalesArg) {
	# x scale
	if (!is.null(scalesArg$x$log)) {
		logBase <- latticeLogBase(scalesArg$x$log)
		if (!is.null(logBase)) {
			xlim <- logBase ^ xlim
		}
	} else {
		logBase <- latticeLogBase(scalesArg$log)
		if (!is.null(logBase)) {
			xlim <- logBase ^ xlim
		}
	}
	# y scale
	if (!is.null(scalesArg$y$log)) {
		logBase <- latticeLogBase(scalesArg$y$log)
		if (!is.null(logBase)) {
			ylim <- logBase ^ ylim
		}
	} else {
		logBase <- latticeLogBase(scalesArg$log)
		if (!is.null(logBase)) {
			ylim <- logBase ^ ylim
		}
	}
	return(list(x=xlim, y=ylim))
}

latticeLogBase <- function(x) {
	if (is.null(x) || identical(x, FALSE)) {
		return(NULL)
	}
	logBase <- x
	if (isTRUE(x)) { return(10) }
	if (identical(x, "e")) { return(exp(1)) }
	x
}

as.POSIXct.numeric <- function(x) {
	structure(as.numeric(x), class = c("POSIXt", "POSIXct"))
}

evalCallArgs <- function(myCall, envir=parent.frame(2), pattern=".*") {
	for (i in seq(along=myCall)) {
		if ((mode(myCall) %in% "call") && (i == 1)) {
			next # don't eval function itself
		}
		if (length(grep(pattern, deparse(myCall[[i]])))>0) {
			myCall[[i]] <- eval(myCall[[i]], envir)
		} else if (mode(myCall[[i]]) %in% c("call","list")) {
			myCall[[i]] <- evalCallArgs(myCall[[i]], pattern)
		}
	}
	return(myCall)
}

