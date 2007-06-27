## plotAndPlayGTK: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org> and others
## GPL version 2 or newer

MAJOR <- "0"
MINOR <- "7"
REVISION <- unlist(strsplit("$Revision: 19 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- paste("(c) 2007 Felix Andrews <felix@nfrac.org>",
	"with contributions from Graham Williams and Deepayan Sarkar")
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

StateEnv <- new.env()

plotAndPlayButtons <- alist(
	identify=list("Identify data", "gtk-info", f=.plotAndPlay_identify_event),
	zoomin=list("Zoom", "gtk-zoom-in", f=.plotAndPlay_zoomin_event),
	zoomout=list("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event),
	zoomfit=list("Zoom to fit", "gtk-zoom-fit", f=.plotAndPlay_zoomfit_event),
	centre=list("Re-centre", "gtk-jump-to-ltr", f=.plotAndPlay_centre_event),
	zero=list("Full scale", "gtk-goto-bottom", f=.plotAndPlay_zero_event, isToggle=T),
	logscale=list("Log scale", "gtk-goto-top", f=.plotAndPlay_logscale_event, isToggle=T),
	focus=list("Choose panel", "gtk-select-color", f=.plotAndPlay_focus_event),
	expand=list("Expand panel", "gtk-fullscreen", f=.plotAndPlay_expand_event, isToggle=T),
	prev.page=list("Prev page", "gtk-go-back-ltr", f=.plotAndPlay_prevpage_event),
	next.page=list("Next page", "gtk-go-forward-ltr", f=.plotAndPlay_nextpage_event),
	brush=list("Brush", "gtk-media-record", f=.plotAndPlay_brush_event)
)

#gdkPixbufGetFromDrawable(dest = NULL, src, cmap = NULL, src.x, src.y, dest.x, dest.y, width, height)
#"gtk-index" / "gtk-preferences"
#"gtk-find"
#"gtk-italic" (edit labels/titles)
#"gtk-undo-ltr"

latticeNames <- c("barchart", "bwplot", "cloud", "contourplot", "densityplot", 
	"dotplot", "histogram", "levelplot", "parallel", "qq", "qqmath", "rfs", 
	"splom", "stripplot", "tmd", "wireframe", "xyplot")


plotAndPlay <- function(expr, name="plot", plot.call, nav.scales=c("x","y"), trans.scales=c("y"), buttons=plotAndPlayButtons[c("identify", "zoomin", "zoomout", "zoomfit", "centre")], extra.buttons=plotAndPlayButtons[c("zero")], labels=NULL, identify.call=NULL, is.lattice=(callName %in% latticeNames), eval.args=F, envir=parent.frame()) {
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
	if (!identical(eval.args, FALSE)) {
		plot.call <- evalCallArgs(plot.call, envir=envir, pattern=eval.args)
	}
	callName <- deparse(plot.call[[1]])
	if (is.lattice &&
		!(callName %in% c("splom", "cloud", "levelplot",
			"contourplot", "wireframe", "parallel")) ) {
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
					labels <- row.names(eval(plot.call$data))
				}
				# TODO: try to get row names from formula
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
					tmp.x <- eval(plot.call$x)
					labels <- row.names(tmp.x)
					if (is.null(labels)) {
						if (is.ts(tmp.x)) {
							labels <- time(tmp.x)
						}
					}
				}
			}
			if (!is.null(labels)) {
				identify.call$labels <- labels
			}
		}
		identify.call$cex <- 0.7
	}
	# check whether the window already exists
	if (is.null(StateEnv[[name]])) {
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
		myWin <- StateEnv[[name]]$win
		myVBox <- myWin$getChildren()[[1]]
		# switch to the device
		dev.set(StateEnv[[name]]$dev)
		# blank the plot
		StateEnv[[name]]$call <- NULL
		plot.new()
		# remove the toolbar, it will be recreated
		plotAndPlayGetToolbar(name)$destroy()
	}
	# set which buttons are visible
	if (is.lattice &&
		(callName %in% c("cloud", "wireframe")) ) {
		if (missing(buttons)) {
			buttons <- list()
		}
	}
	if (is.lattice &&
		(callName %in% c("splom")) ) {
		if (missing(buttons)) {
			buttons <- plotAndPlayButtons[c("brush")]
		}
		if (missing(extra.buttons)) {
			extra.buttons <- list()
		}
	}
	extra.unique <- !(names(extra.buttons) %in% names(buttons))
	buttons <- c(buttons, extra.buttons[extra.unique])
	if (is.lattice) {
		result <- eval(plot.call)
		nPanels <- prod(dim(result))
		nPerPage <- nPanels
		myLayout <- eval(plot.call$layout)
		if (!is.null(myLayout)) {
			nPerPage <- myLayout[1] * myLayout[2]
			if (myLayout[1] == 0) { nPerPage <- myLayout[2] }
			if (nPanels > nPerPage) {
				buttons$prev.page <- plotAndPlayButtons$prev.page
				buttons$next.page <- plotAndPlayButtons$next.page
			}
			# needed for page navigation (packet.panel.page)
			myLayout[3] <- 1
			plot.call$layout <- myLayout
		}
		if (nPerPage > 1) {
			buttons$expand <- plotAndPlayButtons$expand
		}
	}
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
	# copy button
	copyButton <- gtkToolButtonWithCallback("Copy", "gtk-copy", 
		f=.plotAndPlay_copy_event, data=list(name=name))
	theToolbar$insert(copyButton, -1)
	if (is.lattice) {
		greyButton <- gtkToolButtonWithCallback("Greyscale", "gtk-print-preview", 
			f=.plotAndPlay_greyscale_event, data=list(name=name), isToggle=T)
		theToolbar$insert(greyButton, -1)
	}
	# bring window to front
	myWin$present()
	# store the state of this plot window
	StateEnv[[name]] <- list(
		win=myWin,
		dev=dev.cur(),
		call=plot.call,
		id.call=identify.call,
		focus=list(col=0, row=0),
		nav.scales=nav.scales,
		trans.scales=trans.scales,
		is.lattice=is.lattice,
		page=1
	)
	# do the plot
	invisible(plotAndPlayUpdate(name))
}

.plotAndPlay_close_event <- function(widget, event, user.data) {
	name <- user.data$name
	if (StateEnv[[name]]$dev %in% dev.list()) {
		dev.off(StateEnv[[name]]$dev)
	}
	if (!is.null(StateEnv[[name]]$win)) {
		StateEnv[[name]]$win$destroy()
	}
	StateEnv[[name]] <- NULL
}

.plotAndPlay_save_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# get filename
	myExt <- if (!is.null(user.data$ext)) { user.data$ext } else { 'pdf' }
	myDefault <- paste(name, '.', myExt, sep='')
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/png/ps/svg)", 
		filters=Filters[c("pdf","png","ps","All"),],
		index=match(myExt, c("pdf","png","ps","svg"))
	)
	StateEnv[[name]]$win$present()
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

.plotAndPlay_copy_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# save plot to file
	filename <- paste(tempfile(), ".png", sep="")
	mySize <- plotAndPlayGetDA(name)$getAllocation()
	myWidth <- mySize$width
	myHeight <- mySize$height
	myScale <- 1/72
	myWidth <- myWidth * myScale
	myHeight <- myHeight * myScale
	dev.copy(Cairo_png, file=filename, width=myWidth, height=myHeight)
	dev.off()
	im <- gdkPixbufNewFromFile(filename)$retval
	gtkClipboardGet("CLIPBOARD")$setImage(im)
	file.remove(filename)
}

.plotAndPlay_greyscale_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
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
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
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
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.5)), name="tmp.mask")
	# get new scales interactively
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		plotAndPlayDoFocus(name)
		plotAndPlaySetPrompt(name, paste("Click at the", lowEdge, 
			"of the region to zoom in to"))
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# get lower limits
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(StateEnv[[name]]$focus)) { trellis.unfocus() }
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
			StateEnv[[name]]$call$scales)
		if (nav.x) { StateEnv[[name]]$call$xlim <- newlims$x }
		if (nav.y) { StateEnv[[name]]$call$ylim <- newlims$y }
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt(name, paste("Click at the", lowEdge, 
			"of the region to zoom in to"))
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
		if (nav.x) { StateEnv[[name]]$call$xlim <- xlim.new }
		if (nav.y) { StateEnv[[name]]$call$ylim <- ylim.new }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_zoomout_event <- function(widget, user.data) {
	name <- user.data$name
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# find existing scales and update call
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		if (!any(StateEnv[[name]]$focus)) {
			trellis.focus("panel", 1, 1, highlight=F)
		}
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# zoom out: make range twice the size
		if (nav.x) { xlim <- xlim + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- ylim + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim, ylim, StateEnv[[name]]$call$scales)
		if (nav.x) { StateEnv[[name]]$call$xlim <- newlims$x }
		if (nav.y) { StateEnv[[name]]$call$ylim <- newlims$y }
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# zoom out: make range twice the size
		if (nav.x) { xlim <- xlim + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- ylim + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required (TODO)
		if (nav.x) { StateEnv[[name]]$call$xlim <- xlim }
		if (nav.y) { StateEnv[[name]]$call$ylim <- ylim }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_zoomfit_event <- function(widget, user.data) {
	name <- user.data$name
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# find existing scales and update call
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		if (nav.x) { StateEnv[[name]]$call$xlim <- NULL }
		if (nav.y) { StateEnv[[name]]$call$ylim <- NULL }
	} else {
		# traditional graphics plot
		if (nav.x) { StateEnv[[name]]$call$xlim <- NULL }
		if (nav.y) { StateEnv[[name]]$call$ylim <- NULL }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_centre_event <- function(widget, user.data) {
	name <- user.data$name
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	# get new scales interactively
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		plotAndPlayDoFocus(name)
		plotAndPlaySetPrompt(name, "Click to re-centre the plot")
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# get new centre point
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(StateEnv[[name]]$focus)) { trellis.unfocus() }
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		if (nav.x) { xlim <- clickLoc$x + diff(xlim) * c(-0.5, 0.5) }
		if (nav.y) { ylim <- clickLoc$y + diff(ylim) * c(-0.5, 0.5) }
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim, ylim, StateEnv[[name]]$call$scales)
		if (nav.x) { StateEnv[[name]]$call$xlim <- newlims$x }
		if (nav.y) { StateEnv[[name]]$call$ylim <- newlims$y }
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt(name, "Click to re-centre the plot")
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
		if (nav.x) { StateEnv[[name]]$call$xlim <- xlim }
		if (nav.y) { StateEnv[[name]]$call$ylim <- ylim }
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
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	plotAndPlaySetPrompt(name, "Click on a panel to focus on (for further interaction)")
	newFocus <- trellis.clickFocus()
	if (!is.null(newFocus)) {
		StateEnv[[name]]$focus <- newFocus
	}
}

.plotAndPlay_expand_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# get new expanded setting
	doExpand <- widget$getActive()
	if (doExpand) {
		# set up prompt
		plotAndPlayMakePrompt(name)
		on.exit(plotAndPlayUnmakePrompt(name), add=T)
		plotAndPlaySetPrompt(name, "Click on a panel to expand (for further interaction)")
		newFocus <- trellis.clickFocus(highlight=F)
		if (is.null(newFocus)) {
			return()
		}
		StateEnv[[name]]$focus <- list(col=0, row=0)
		StateEnv[[name]]$old.call.layout <- StateEnv[[name]]$call$layout
		StateEnv[[name]]$call$layout <- c(0,1,1)
		StateEnv[[name]]$old.page <- StateEnv[[name]]$page
		StateEnv[[name]]$page <- packet.number()
	} else {
		StateEnv[[name]]$focus <- list(col=0, row=0)
		StateEnv[[name]]$call$layout <- StateEnv[[name]]$old.call.layout
		StateEnv[[name]]$page <- StateEnv[[name]]$old.page
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_identify_event <- function(widget, user.data) {
	name <- user.data$name
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	# do identify
	idCall <- StateEnv[[name]]$id.call
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		plotAndPlayDoFocus(name, clip.off=T) #, highlight=F
		plotAndPlaySetPrompt(name, paste("Identifying data points...",
			"Click the right mouse button to finish."))
		ids <- eval(idCall)
		# TODO: store this to maintain state?
		if (!any(StateEnv[[name]]$focus)) { trellis.unfocus() }
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt(name, paste("Identifying data points...",
			"Click the right mouse button to finish."))
		ids <- eval(idCall)
	}
}

.plotAndPlay_brush_event <- function(widget, user.data) {
	name <- user.data$name
	if (!StateEnv[[name]]$is.lattice) {
		errorDialog("Brushing only works for Lattice plots (splom).")
		return()
	}
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# set up prompt
	plotAndPlayMakePrompt(name)
	on.exit(plotAndPlayUnmakePrompt(name), add=T)
	# do brushing
	plotAndPlayDoFocus(name) #, highlight=F
	plotAndPlaySetPrompt(name, paste("Brushing data points...",
		"Click the right mouse button to finish."))
	while(highlightOne()) {} # right click to exit
	if (!any(StateEnv[[name]]$focus)) { trellis.unfocus() }
}

.plotAndPlay_zero_event <- function(widget, user.data) {
	name <- user.data$name
	trans.x <- ("x" %in% StateEnv[[name]]$trans.scales)
	trans.y <- ("y" %in% StateEnv[[name]]$trans.scales)
	isX <- isTRUE(user.data$x)
	isY <- isTRUE(user.data$y)
	if (!isX && !isY) { isX <- isY <- T }
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# switch to this device
	oldDev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	on.exit(dev.set(oldDev), add=T)
	# get new zero scale setting
	zeroScale <- widget$getActive()
	# make change and re-draw plot
	if (zeroScale) {
		if (StateEnv[[name]]$is.lattice) {
			# lattice plot
			if (!any(StateEnv[[name]]$focus)) {
				trellis.focus("panel", 1, 1, highlight=F)
			}
			xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
			ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
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
			StateEnv[[name]]$call$xlim <- xlim
		}
		if (trans.y) {
			if (min(ylim) > 0) {
				ylim[which.min(ylim)] <- 0
			} else if (max(ylim) < 0) {
				ylim[which.max(ylim)] <- 0
			}
			StateEnv[[name]]$call$ylim <- ylim
		}
	} else {
		if (trans.x) { StateEnv[[name]]$call$xlim <- NULL }
		if (trans.y) { StateEnv[[name]]$call$ylim <- NULL }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_logscale_event <- function(widget, user.data) {
	name <- user.data$name
	trans.x <- ("x" %in% StateEnv[[name]]$trans.scales)
	trans.y <- ("y" %in% StateEnv[[name]]$trans.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar(name)$setSensitive(F)
	on.exit(plotAndPlayGetToolbar(name)$setSensitive(T))
	# get new log scale setting
	logScale <- widget$getActive()
	# make change and re-draw plot
	if (StateEnv[[name]]$is.lattice) {
		if (trans.x && trans.y) {
			# apply to both scales
			StateEnv[[name]]$call$scales$log <- logScale
		} else {
			if (trans.x) { StateEnv[[name]]$call$scales$x$log <- logScale }
			if (trans.y) { StateEnv[[name]]$call$scales$y$log <- logScale }
		}
	} else {
		logSpec <- "xy"
		if (!trans.y) { logSpec <- "x" }
		if (!trans.x) { logSpec <- "y" }
		StateEnv[[name]]$call$log <- if (logScale) { logSpec }
	}
	plotAndPlayUpdate(name)
}

.plotAndPlay_prevpage_event <- function(widget, user.data) {
	name <- user.data$name
	if (StateEnv[[name]]$page == 1) { return() }
	StateEnv[[name]]$page <- StateEnv[[name]]$page - 1
	plotAndPlayUpdate(name)
}

.plotAndPlay_nextpage_event <- function(widget, user.data) {
	name <- user.data$name
	StateEnv[[name]]$page <- StateEnv[[name]]$page + 1
	plotAndPlayUpdate(name)
}

plotAndPlayDoFocus <- function(name, highlight=T, ...) {
	if (!any(StateEnv[[name]]$focus)) {
		if (length(trellis.currentLayout()) == 1) {
			trellis.focus("panel", 1, 1, highlight=F, ...)
		} else {
			plotAndPlaySetPrompt(name, "First, choose a panel")
			trellis.clickFocus(highlight=highlight, ...)
		}
	}
}

plotAndPlayUpdate <- function(name) {
	result <- eval(StateEnv[[name]]$call)
	if (inherits(result, "trellis")) {
		curPage <- StateEnv[[name]]$page
		print(result, packet.panel=packet.panel.page(curPage))
		if (any(StateEnv[[name]]$focus)) {
			with(StateEnv[[name]]$focus, 
				trellis.focus("panel", col, row))
		}
	}
	invisible(result)
}

plotAndPlayGetDA <- function(name) {
	StateEnv[[name]]$win$getChildren()[[1]]$getChildren()[[1]]
}

plotAndPlayGetToolbar <- function(name) {
	StateEnv[[name]]$win$getChildren()[[1]]$getChildren()[[2]]
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

trellis.clickFocus <- function(...) {
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
		trellis.focus("panel", focusCol, focusRow, ...)
	}
	invisible(list(col=focusCol, row=focusRow))
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
	x <- eval(x)
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

evalCallArgs <- function(myCall, envir=parent.frame(), pattern=T) {
	for (i in seq(along=myCall)) {
		if ((mode(myCall) %in% "call") && (i == 1)) {
			next # don't eval function itself
		}
		if (isTRUE(pattern)) {
			myCall[[i]] <- eval(myCall[[i]], envir)
		} else if (any(grep(pattern, deparse(myCall[[i]])))) {
			myCall[[i]] <- eval(myCall[[i]], envir)
		} else if (mode(myCall[[i]]) %in% c("call","list")) {
			myCall[[i]] <- evalCallArgs(myCall[[i]], envir=envir,
				pattern=pattern)
		}
	}
	return(myCall)
}

## The following functions by Deepayan Sarkar Deepayan.Sarkar@R-project.org

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

highlightOne <-
   function(pargs = trellis.panelArgs(),
            threshold = 18,
            col = 1, pch = 16, ...)
{
   #cat("click to choose one point to highlight", fill = TRUE)

   ll <- grid.locator(unit = "npc")
   if (is.null(ll)) return(FALSE)

   nvars <- length(pargs$z)

   ## which subpanel
   colpos <- ceiling(convertUnit(ll$x, "npc", valueOnly = TRUE) * nvars)
   rowpos <- ceiling(convertUnit(ll$y, "npc", valueOnly = TRUE) * nvars)
   if (rowpos == colpos) return(TRUE)
   subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")

   ## coordinates of click in subpanel
   ll$x <- nvars * (ll$x - unit((colpos-1) / nvars, "npc"))
   ll$y <- nvars * (ll$y - unit((rowpos-1) / nvars, "npc"))

   ## get to that viewport, so we can convert units
   depth <- downViewport(subpanel.name)
   xnative <- convertX(ll$x, "native", TRUE)
   ynative <- convertY(ll$y, "native", TRUE)

   ## find nearest point in data (replicate steps in panel.identify)

   xpoints <- convertX(unit(xnative, "native"), "points", TRUE)
   ypoints <- convertY(unit(ynative, "native"), "points", TRUE)

   data.xp <- convertX(unit(pargs$z[, colpos], "native"), "points", TRUE)
   data.yp <- convertY(unit(pargs$z[, rowpos], "native"), "points", TRUE)

   pdists <- sqrt((data.xp - xpoints)^2 + (data.yp - ypoints)^2)

   if (min(pdists, na.rm = TRUE) > threshold)
   {
       warning("no points within ", threshold, " points of click")
       upViewport(depth)
   }
  else
   {
       w <- which.min(pdists)
       ## print(pargs$z[w,])
       upViewport(depth)
       for (row in 1:nvars)
       for (column in 1:nvars)
           if (row != column)
           {
               subpanel.name <-
                   paste("subpanel",
                         column, row, sep = ".")
               depth <- downViewport(subpanel.name)
               panel.points(x = pargs$z[w, column],
                            y = pargs$z[w, row],
                            pch = pch, col = col,
                            ...)
               upViewport(depth)
           }
   }
   return(TRUE)
}
