## plotAndPlayGTK: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## with contributions from Deepayan Sarkar and Graham Williams
## GPL version 2 or newer

MAJOR <- "0"
MINOR <- "8"
REVISION <- unlist(strsplit("$Revision: 46 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- paste("(c) 2007 Felix Andrews <felix@nfrac.org>",
	"with contributions from Deepayan Sarkar and Graham Williams")
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

if (!exists("StateEnv", environment(), inherits=FALSE)) {
	StateEnv <- new.env()
}

plotAndPlayButtons <- alist(
	identify=makeIdentifyMenuButton(),
	identify.simple=quickTool("Label points", "gtk-info", tooltip="Identify data points by clicking on them", f=.plotAndPlay_identify_event),
	zoomin=quickTool("Zoom to...", "gtk-zoom-in", tooltip="Select plot region with the mouse", f=.plotAndPlay_zoomin_event),
	zoomout=quickTool("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event),
	zoomfit=quickTool("Fit data", "gtk-zoom-fit", f=.plotAndPlay_zoomfit_event),
	centre=quickTool("Re-centre", "gtk-jump-to-ltr", f=.plotAndPlay_centre_event),
	zero=quickTool("Full scale", "gtk-goto-bottom", tooltip="Show the full scale starting from zero", f=.plotAndPlay_zero_event, isToggle=T),
	logscale=quickTool("Log scale", "gtk-goto-top", tooltip="Use a logarithmic scale (base 10)", f=.plotAndPlay_logscale_event, isToggle=T),
	brush=quickTool("Brush points", "gtk-media-record", f=.plotAndPlay_brush_event),
	brush.region=quickTool("Brush region", "gtk-media-record", f=.plotAndPlay_brush_region_event),
	brush.drag=quickTool("Brush region (drag)", "gtk-media-record", f=.plotAndPlay_brush_drag_event),
	clear=quickTool("Clear", "gtk-clear", tooltip="Remove labels and annotations", f=.plotAndPlay_clear_event),
	zoomin.3d=quickTool("Zoom in", "gtk-zoom-in", f=.plotAndPlay_zoomin3d_event),
	zoomout.3d=quickTool("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout3d_event),
	fly.left.3d=quickTool("Fly left", "gtk-media-rewind-ltr", f=.plotAndPlay_flyleft3d_event),
	fly.right.3d=quickTool("Fly right", "gtk-media-rewind-rtl", f=.plotAndPlay_flyright3d_event),
	#fly.up.3d=quickTool("Fly up", "gtk-go-up", f=.plotAndPlay_flyup3d_event),
	#fly.down.3d=quickTool("Fly down", "gtk-go-down", f=.plotAndPlay_flydown3d_event),
	focus=quickTool("Choose panel", "gtk-select-color", tooltip="Choose a panel to focus on (for further interaction)", f=.plotAndPlay_focus_event),
	expand=quickTool("Expand panel", "gtk-fullscreen", tooltip="Choose a panel to expand and focus (for further interaction)", f=.plotAndPlay_expand_event, isToggle=T),
	prev.page=quickTool("Prev page", "gtk-go-back-ltr", f=.plotAndPlay_prevpage_event),
	next.page=quickTool("Next page", "gtk-go-forward-ltr", f=.plotAndPlay_nextpage_event)
)

plotAndPlayBasicButtons <- alist(
	gtkSeparatorToolItem(),
	edit=quickTool("Edit call", "gtk-italic", tooltip="Edit the plot call as text", f=.plotAndPlay_edit_event),
	save=makeSaveMenuButton(),
	copy=quickTool("Copy", "gtk-copy", tooltip="Copy this plot to the clipboard (as a bitmap)", f=.plotAndPlay_copy_event),
	greyscale=quickTool("Greyscale", "gtk-print-preview", tooltip="Switch to the greyscale lattice theme (then save the plot for printing)", f=.plotAndPlay_greyscale_event, isToggle=T)
)

quickTool <- function(label, icon.name=NULL, tooltip=NULL, f, data=NULL, isToggle=F) {
	x <- if (isToggle) gtkToggleToolButton() else gtkToolButton()
	x[["label"]] <- label
	x[["icon-name"]] <- icon.name
	if (!is.null(tooltip)) {
		thisTips <- gtkTooltips()
		thisTips$setTip(x, tooltip)
	}
	gSignalConnect(x, "clicked", f, data=data)
	x
}

makeIdentifyMenuButton <- function() {
	idButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-info", 
		size=GtkIconSize['small-toolbar']), label="Label points")
	thisTips <- gtkTooltips()
	thisTips$setTip(idButton, "Identify data points by clicking on them")
	gSignalConnect(idButton, "clicked", .plotAndPlay_identify_event)
	idMenu <- gtkMenu()
	idItemRegion <- gtkMenuItem("Label all points in a region")
	idItemClear <- gtkMenuItem("Clear labels")
	idMenu$append(idItemRegion)
	idMenu$append(idItemClear)
	idButton$setMenu(idMenu)
	gSignalConnect(idItemRegion, "activate", .plotAndPlay_identify_region_event)
	gSignalConnect(idItemClear, "activate", .plotAndPlay_clear_event)
	idButton
}

makeSaveMenuButton <- function() {
	saveButton <- gtkMenuToolButton(gtkImageNewFromStock('gtk-save-as', 
		size=GtkIconSize['small-toolbar']), label="Save as")
	gSignalConnect(saveButton, "clicked", .plotAndPlay_save_event)
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
		data=list(ext="pdf"))
	gSignalConnect(saveItemPNG, "activate", .plotAndPlay_save_event, 
		data=list(ext="png"))
	gSignalConnect(saveItemPS, "activate", .plotAndPlay_save_event, 
		data=list(ext="ps"))
	gSignalConnect(saveItemSVG, "activate", .plotAndPlay_save_event, 
		data=list(ext="svg"))
	saveButton
}

# look at rpanel and tkwidgets and GeoXP and iplots etc

# get labels from formula rownames

# superpose button:
# off = ~ data | which
# on = ~ data,  groups=which
# hmm = data ~ which

#gdkPixbufGetFromDrawable(dest = NULL, src, cmap = NULL, src.x, src.y, dest.x, dest.y, width, height)
#"gtk-index" (layers)
# TODO: xlim / ylim list if scales$relation %in% c("free", "sliced")

latticeNames <- c("barchart", "bwplot", "cloud", "contourplot", "densityplot", 
	"dotplot", "histogram", "levelplot", "parallel", "qq", "qqmath", "rfs", 
	"splom", "stripplot", "tmd", "wireframe", "xyplot")

playwith <- function(expr, name="plot", nav.scales=c("x","y"), trans.scales=c("y"), 
	buttons=list("identify", "zoomin", "zoomout", "zoomfit", "centre"), 
	extra.buttons=list("zero"), basic.buttons=plotAndPlayBasicButtons, 
	labels=NULL, label.args=list(cex=0.7), identify.call=NULL, plot.call, 
	is.lattice=(callName %in% latticeNames), eval.args=NA, invert.match=F,
	envir=parent.frame(), restore.on.close=NULL) {
	
	if (missing(plot.call) == missing(expr)) stop("give only one of 'expr' and 'plot.call'")
	if (missing(plot.call)) plot.call <- substitute(expr)
	# check types
	if (!is.call(plot.call)) stop("'plot.call' should be a call object")
	if (!is.character(name)) stop("'name' should be character")
	nav.scales <- match.arg(nav.scales, c("x","y"), several.ok=T)
	trans.scales <- match.arg(trans.scales, c("x","y"), several.ok=T)
	if (!is.null(buttons) && !is.list(buttons)) {
		if (is.character(buttons)) buttons <- as.list(buttons) else
		stop("'buttons' should be a list (of character or calls) or NULL")
	}
	if (!is.null(extra.buttons) && !is.list(extra.buttons)) {
		if (is.character(extra.buttons)) extra.buttons <- as.list(extra.buttons) else
		stop("'extra.buttons' should be a list (of character or calls) or NULL")
	}
	if (!is.null(basic.buttons) && !is.list(basic.buttons)) {
		if (is.character(basic.buttons)) basic.buttons <- as.list(basic.buttons) else
		stop("'basic.buttons' should be a list (of character or calls) or NULL")
	}
	# put call into canonical form and maybe evaluate its arguments
	plot.call <- match.call(eval(plot.call[[1]], envir), plot.call)
	env <- new.env()
	# work out evalulation rules
	inherits <- !is.na(eval.args)
	if (is.na(eval.args)) eval.args <- (environmentName(envir) != "R_GlobalEnv")
	if (!identical(eval.args, FALSE)) {
		copyArgsIntoEnv(plot.call, envir=envir, newEnv=env, inherits=inherits, 
			pattern=eval.args, invert.match=invert.match)
	}
	callName <- deparse(plot.call[[1]])
	# lattice check
	if (is.lattice && !exists("panel.brush.splom")) {
		stop("Need version >= 0.16 of the lattice package.")
	}
	if (is.lattice &&
		!(callName %in% c("splom", "cloud", "levelplot",
			"contourplot", "wireframe", "parallel")) ) {
		# need this for correctly identifying points
		plot.call$subscripts <- TRUE
	}
	# try to construct a call to identify() if it was not supplied
	if (is.null(identify.call)) {
		if (is.lattice) {
			# lattice plot
			identify.call <- call('panel.identify')
			if (callName %in% c("qqmath")) 
				identify.call <- call('panel.identify.qqmath')
			# try to guess labels
			if (is.null(labels)) {
				if ('data' %in% names(plot.call)) {
					labels <- row.names(eval(plot.call$data, env))
				}
				# TODO: try to get row names from formula
			}
			if (!is.null(labels)) identify.call$labels <- labels
		} else {
			# traditional graphics plot
			identify.call <- call('identify')
			callArgNames <- names(plot.call)
			if ('x' %in% names(plot.call)) identify.call$x <- plot.call$x
			if ('y' %in% names(plot.call)) identify.call$y <- plot.call$y
			# try to guess labels
			if (is.null(labels)) {
				if ('x' %in% names(plot.call)) {
					tmp.x <- eval(plot.call$x, env)
					labels <- row.names(tmp.x)
					if (is.null(labels)) {
						if (is.ts(tmp.x)) {
							labels <- time(tmp.x)
						}
					}
				}
			}
			if (!is.null(labels)) identify.call$labels <- labels
		}
	}
	identify.call <- as.call(c(as.list(identify.call), label.args))
	# put identify call into canonical form
	identify.call <- match.call(eval(identify.call[[1]], envir), identify.call)
	# check whether the window already exists
	if (is.null(StateEnv[[name]]$win) 
	|| (inherits(StateEnv[[name]]$win, "<invalid>"))) {
		# create a new GTK window and set up cairoDevice for plotting
		myWin <- gtkWindow(show=FALSE)
		if (!inherits(myWin, "GtkWindow")) stop(paste(
			"Could not create the GTK window.",
			"Make sure you have recent versions of",
			"RGtk2 and the GTK+ libraries.",
			"See http://www.ggobi.org/rgtk2/"))
		myWin[["default-width"]] <- 640
		myWin[["default-height"]] <- 480
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
		plotAndPlayGetToolbar()$destroy()
	}
	
	# set which buttons are visible
	if (is.lattice &&
		(callName %in% c("cloud", "wireframe")) ) {
		if (is.null(plot.call$zoom)) plot.call$zoom <- 1
		if (is.null(plot.call$screen)) plot.call$screen <- list(z = 40, x = -60)
		if (missing(buttons)) buttons <- list("zoomin.3d", "zoomout.3d", 
			"fly.left.3d", "fly.right.3d")
		if (missing(extra.buttons)) extra.buttons <- list()
	}
	if (is.lattice &&
	(callName %in% c("splom")) ) {
		if (missing(buttons)) buttons <- list("brush", "brush.region", 
			"brush.drag", "clear")
		if (missing(extra.buttons)) extra.buttons <- list()
	}
	buttons <- c(buttons, extra.buttons)
	if (is.lattice) {
		result <- eval(plot.call, env)
		nPanels <- prod(dim(result))
		nPerPage <- nPanels
		myLayout <- eval(plot.call$layout, env)
		if (!is.null(myLayout)) {
			nPerPage <- myLayout[1] * myLayout[2]
			if (myLayout[1] == 0) nPerPage <- myLayout[2]
			if (nPanels > nPerPage) {
				buttons <- c(buttons, "prev.page", "next.page")
			}
			# needed for page navigation (packet.panel.page)
			myLayout[3] <- 1
			plot.call$layout <- myLayout
		}
		if (nPerPage > 1) buttons <- c(buttons, "expand")
	}
	if (!is.lattice) basic.buttons$greyscale <- NULL
	buttons <- c(buttons, basic.buttons)
	# create the toolbar
	theToolbar <- gtkToolbar()
	theToolbar[["toolbar-style"]] <- GtkToolbarStyle['both']
	myVBox$packStart(theToolbar, expand=FALSE)
	# add buttons
	for (i in seq_along(buttons)) {
		newButton <- eval(buttons[[i]])
		if (is.character(newButton)) {
			newButton <- eval(plotAndPlayButtons[[newButton]])
		}
		if (is.null(newButton)) {
			stop("Unrecognised (NULL) button at position ", i)
		}
		theToolbar$insert(newButton, -1)
	}
	# set StateEnv$.current to 'name' whenever this window comes to front
	gSignalConnect(myWin, "focus-in-event", 
		.plotAndPlay_window_focus_in_event, data=list(name=name))
	gSignalConnect(myWin, "focus-out-event", 
		.plotAndPlay_window_focus_out_event, data=list(name=name))
	StateEnv$.current <- name
	# bring window to front
	myWin$present()
	# store the state of this plot window
	StateEnv[[name]] <- list(
		win=myWin,
		dev=dev.cur(),
		old.dev=dev.cur(),
		call=plot.call,
		id.call=identify.call,
		env=env,
		label.args=label.args,
		nav.scales=nav.scales,
		trans.scales=trans.scales,
		ids=list(),
		brushed=list(),
		is.lattice=is.lattice,
		focus=list(col=0, row=0),
		page=1,
		restore.on.close=restore.on.close
	)
	# do the plot
	invisible(plotAndPlayUpdate())
}

.plotAndPlay_window_focus_in_event <- function(widget, event, user.data) {
	name <- user.data$name
	StateEnv$.current <- name
	# switch to this device
	StateEnv[[name]]$old.dev <- dev.cur()
	dev.set(StateEnv[[name]]$dev)
	return(FALSE)
}

.plotAndPlay_window_focus_out_event <- function(widget, event, user.data) {
	name <- user.data$name
	# revert to previous device
	dev.set(StateEnv[[name]]$old.dev)
	return(FALSE)
}

.plotAndPlay_close_event <- function(widget, event, user.data) {
	name <- StateEnv$.current
	if (is.null(StateEnv[[name]])) return()
	if (StateEnv[[name]]$dev %in% dev.list()) {
		try(dev.off(StateEnv[[name]]$dev))
	}
	if (!is.null(StateEnv[[name]]$win)) {
		try(StateEnv[[name]]$win$destroy())
	}
	if (!is.null(StateEnv[[name]]$restore.on.close) 
	&& (!inherits(StateEnv[[name]]$restore.on.close, "<invalid>"))) {
		try(StateEnv[[name]]$restore.on.close$present())
	}
	rm(list=name, envir=StateEnv)
}

.plotAndPlay_save_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get filename
	myExt <- if (!missing(user.data) && !is.null(user.data$ext)) 
		user.data$ext else 'pdf'
	myDefault <- paste(name, '.', myExt, sep='')
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/png/ps/svg)", 
		filters=Filters[c("pdf","png","ps","All"),],
		index=match(myExt, c("pdf","png","ps","svg"))
	)
	StateEnv[[name]]$win$present()
	if (is.na(filename)) return()
	ext <- tolower(get.extension(filename))
	if (ext == "") {
		filename <- paste(filename, '.pdf', sep='')
		ext <- 'pdf'
	}
	# save plot to file
	mySize <- plotAndPlayGetDA()$getAllocation()
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
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# save plot to file
	filename <- paste(tempfile(), ".png", sep="")
	mySize <- plotAndPlayGetDA()$getAllocation()
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
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get new greyscale setting
	greyscale <- widget$getActive()
	# make change and re-draw plot
	if (greyscale) {
		trellis.device(new=F, color=F)
	} else {
		trellis.device(new=F)
	}
	plotAndPlayUpdate()
}

.plotAndPlay_zoomin_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	lowEdge <- "bottom-left corner"
	if (!nav.y) lowEdge <- "left edge"
	if (!nav.x) lowEdge <- "bottom edge"
	highEdge <- "top-right corner"
	if (!nav.y) highEdge <- "right edge"
	if (!nav.x) highEdge <- "top edge"
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.25)), name="tmp.mask")
	# get new scales interactively
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		newFocus <- plotAndPlayDoFocus()
		if (!any(newFocus)) return()
		plotAndPlaySetPrompt(paste("Zooming to selected region...",
			"click at the", lowEdge))
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# get lower limits
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		# mask out lower regions
		grid.draw(editGrob(maskGrob, 
			x=unit(0,"npc"), width=unit(xlim.new[1] - xlim[1],"native"), 
			just="left"))
		grid.draw(editGrob(maskGrob,
			y=unit(0,"npc"), height=unit(ylim.new[1] - ylim[1],"native"),
			x=unit(1,"npc"), width=unit(xlim[2] - xlim.new[1],"native"),
			just=c("right", "bottom")))
		# get upper limits
		plotAndPlaySetPrompt(paste("OK, now click at the", highEdge))
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			plotAndPlayUpdate()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
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
		# convert new scale to factor levels if required
		if (nav.x && is.factor(datax <- trellis.panelArgs()$x)) {
			if (is.null(StateEnv[[name]]$call$scales$x$labels)) {
				StateEnv[[name]]$call$scales$x$labels <- levels(datax)
				StateEnv[[name]]$call$scales$x$at <- 1:nlevels(datax)
			}
			#newlims$x <- levels(datax)[
			#	seq(ceiling(min(newlims$x)), max(newlims$x))]
		}
		if (nav.y && is.factor(datay <- trellis.panelArgs()$y)) {
			if (is.null(StateEnv[[name]]$call$scales$y$labels)) {
				StateEnv[[name]]$call$scales$y$labels <- levels(datay)
				StateEnv[[name]]$call$scales$y$at <- 1:nlevels(datay)
			}
			#newlims$y <- levels(trellis.panelArgs()$y)[
			#	seq(ceiling(min(newlims$y)), max(newlims$y))]
		}
		if (nav.x) StateEnv[[name]]$call$xlim <- newlims$x
		if (nav.y) StateEnv[[name]]$call$ylim <- newlims$y
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt(paste("Zooming to selected region...",
			"click at the", lowEdge))
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# get lower limits
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) return()
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		if (nav.x) abline(v=xlim.new, col="red")
		if (nav.y) abline(h=ylim.new, col="red")
		# get upper limits
		plotAndPlaySetPrompt(paste("OK, now click at the", highEdge))
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		if (nav.x) abline(v=xlim.new[2], col="red")
		if (nav.y) abline(h=ylim.new[2], col="red")
		# convert back from log scale if required
		if (par("xlog")) xlim <- 10 ^ xlim
		if (par("ylog")) ylim <- 10 ^ ylim
		if (nav.x) StateEnv[[name]]$call$xlim <- xlim.new
		if (nav.y) StateEnv[[name]]$call$ylim <- ylim.new
	}
	plotAndPlayUpdate()
}

.plotAndPlay_zoomout_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# find existing scales and update call
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		if (!any(StateEnv[[name]]$focus)) {
			trellis.focus("panel", 1, 1, highlight=F)
		}
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# zoom out: make range twice the size
		if (nav.x) xlim <- xlim + diff(xlim) * c(-0.5, 0.5)
		if (nav.y) ylim <- ylim + diff(ylim) * c(-0.5, 0.5)
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim, ylim, StateEnv[[name]]$call$scales)
		if (nav.x) StateEnv[[name]]$call$xlim <- newlims$x
		if (nav.y) StateEnv[[name]]$call$ylim <- newlims$y
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# zoom out: make range twice the size
		if (nav.x) xlim <- xlim + diff(xlim) * c(-0.5, 0.5)
		if (nav.y) ylim <- ylim + diff(ylim) * c(-0.5, 0.5)
		# convert back from log scale if required
		if (par("xlog")) xlim <- 10 ^ xlim
		if (par("ylog")) ylim <- 10 ^ ylim
		if (nav.x) StateEnv[[name]]$call$xlim <- xlim
		if (nav.y) StateEnv[[name]]$call$ylim <- ylim
	}
	plotAndPlayUpdate()
}

.plotAndPlay_zoomfit_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# find existing scales and update call
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		if (nav.x) StateEnv[[name]]$call$xlim <- NULL
		if (nav.y) StateEnv[[name]]$call$ylim <- NULL
	} else {
		# traditional graphics plot
		if (nav.x) StateEnv[[name]]$call$xlim <- NULL
		if (nav.y) StateEnv[[name]]$call$ylim <- NULL
	}
	plotAndPlayUpdate()
}

.plotAndPlay_centre_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# get new scales interactively
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		newFocus <- plotAndPlayDoFocus()
		if (!any(newFocus)) return()
		plotAndPlaySetPrompt("Click to re-centre the plot")
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# get new centre point
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		if (nav.x) xlim <- clickLoc$x + diff(xlim) * c(-0.5, 0.5)
		if (nav.y) ylim <- clickLoc$y + diff(ylim) * c(-0.5, 0.5)
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim, ylim, StateEnv[[name]]$call$scales)
		# label factor levels if required
		if (nav.x && is.factor(datax <- trellis.panelArgs()$x)) {
			if (is.null(StateEnv[[name]]$call$scales$x$labels)) {
				StateEnv[[name]]$call$scales$x$labels <- levels(datax)
				StateEnv[[name]]$call$scales$x$at <- 1:nlevels(datax)
			}
		}
		if (nav.y && is.factor(datay <- trellis.panelArgs()$y)) {
			if (is.null(StateEnv[[name]]$call$scales$y$labels)) {
				StateEnv[[name]]$call$scales$y$labels <- levels(datay)
				StateEnv[[name]]$call$scales$y$at <- 1:nlevels(datay)
			}
		}
		if (nav.x) StateEnv[[name]]$call$xlim <- newlims$x
		if (nav.y) StateEnv[[name]]$call$ylim <- newlims$y
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt("Click to re-centre the plot")
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# get new centre point
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		if (nav.x) xlim <- clickLoc$x + diff(xlim) * c(-0.5, 0.5)
		if (nav.y) ylim <- clickLoc$y + diff(ylim) * c(-0.5, 0.5)
		# convert back from log scale if required
		if (par("xlog")) xlim <- 10 ^ xlim
		if (par("ylog")) ylim <- 10 ^ ylim
		if (nav.x) StateEnv[[name]]$call$xlim <- xlim
		if (nav.y) StateEnv[[name]]$call$ylim <- ylim
	}
	plotAndPlayUpdate()
}

.plotAndPlay_focus_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	plotAndPlaySetPrompt("Click on a panel to focus on (for further interaction)")
	newFocus <- trellis.focus()
	if (!is.null(newFocus)) {
		StateEnv[[name]]$focus <- newFocus
	}
}

.plotAndPlay_expand_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get new expanded setting
	doExpand <- widget$getActive()
	if (doExpand) {
		# set up prompt
		plotAndPlayMakePrompt()
		on.exit(plotAndPlayUnmakePrompt(), add=T)
		plotAndPlaySetPrompt("Click on a panel to expand (for further interaction)")
		newFocus <- trellis.focus()
		if (!any(newFocus)) return()
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
	plotAndPlayUpdate()
}

.plotAndPlay_identify_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# do identify
	idCall <- StateEnv[[name]]$id.call
	if (!StateEnv[[name]]$is.lattice) {
		# traditional graphics plot
		plotAndPlaySetPrompt(paste("Identifying data points...",
			"Click the right mouse button to finish."))
		ids.new <- eval(idCall, StateEnv[[name]]$env)
		# set identified points
		ids.old <- StateEnv[[name]]$ids$all # may be NULL
		StateEnv[[name]]$ids$all <- union(ids.old, ids.new)
		return()
	}
	# lattice plot
	newFocus <- plotAndPlayDoFocus(clip.off=T)
	if (!any(newFocus)) return()
	plotAndPlaySetPrompt(paste("Identifying data points...",
		"Click the right mouse button to finish."))
	ids.new <- eval(idCall, StateEnv[[name]]$env)
	# set identified points
	myPacket <- as.character(packet.number())
	ids.old <- StateEnv[[name]]$ids[[myPacket]] # may be NULL
	StateEnv[[name]]$ids[[myPacket]] <- union(ids.old, ids.new)
	if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
}

.plotAndPlay_identify_region_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- T
	nav.y <- T
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	lowEdge <- "bottom-left corner"
	if (!nav.y) lowEdge <- "left edge"
	if (!nav.x) lowEdge <- "bottom edge"
	highEdge <- "top-right corner"
	if (!nav.y) highEdge <- "right edge"
	if (!nav.x) highEdge <- "top edge"
	# get region
	if (StateEnv[[name]]$is.lattice) {
		newFocus <- plotAndPlayDoFocus(clip.off=T)
		if (!any(newFocus)) return()
		plotAndPlaySetPrompt(paste("Identifying data points in a region...",
			"click at the", lowEdge))
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# get lower limits
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		# draw lower bounds
		if (nav.x) panel.abline(v=xlim.new)
		if (nav.y) panel.abline(h=ylim.new)
		# get upper limits
		plotAndPlaySetPrompt(paste("OK, now click at the", highEdge))
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			plotAndPlayUpdate()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		# draw upper bounds
		if (nav.x) panel.abline(v=xlim.new[2])
		if (nav.y) panel.abline(h=ylim.new[2])
		# convert back from log scale if required
		newlims <- latticeUnLog(xlim.new, ylim.new, 
			StateEnv[[name]]$call$scales)
		# set identified points
		idCall <- StateEnv[[name]]$id.call
		if ('x' %in% names(idCall)) {
			xy <- xy.coords.call(idCall, StateEnv[[name]]$env)
			subscripts <- seq_along(xy$x)
		} else {
			pargs <- trellis.panelArgs()
			xy <- xy.coords(pargs, recycle=T)
			# convert back from log scale if required
			xy <- latticeUnLog(xy$x, xy$y, StateEnv[[name]]$call$scales)
			subscripts <- pargs$subscripts
		}
		ids.new <- which(
			(min(xlim.new) < xy$x) & (xy$x < max(xlim.new)) &
			(min(ylim.new) < xy$y) & (xy$y < max(ylim.new))
		)
		ids.new <- subscripts[ids.new]
		myPacket <- as.character(packet.number())
		ids.old <- StateEnv[[name]]$ids[[myPacket]] # may be NULL
		StateEnv[[name]]$ids[[myPacket]] <- union(ids.old, ids.new)
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt(paste("Identifying data points in a region...",
			"click at the", lowEdge))
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# get lower limits
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) return()
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		if (nav.x) abline(v=xlim.new, col="red")
		if (nav.y) abline(h=ylim.new, col="red")
		# get upper limits
		plotAndPlaySetPrompt(paste("OK, now click at the", highEdge))
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		if (nav.x) abline(v=xlim.new[2], col="red")
		if (nav.y) abline(h=ylim.new[2], col="red")
		# convert back from log scale if required
		if (par("xlog")) xlim.new <- 10 ^ xlim.new
		if (par("ylog")) ylim.new <- 10 ^ ylim.new
		xy <- xy.coords.call(StateEnv[[name]]$call, StateEnv[[name]]$env)
		ids.new <- which(
			(min(xlim.new) < xy$x) & (xy$x < max(xlim.new)) &
			(min(ylim.new) < xy$y) & (xy$y < max(ylim.new))
		)
		# set identified points
		ids.old <- StateEnv[[name]]$ids$all # may be NULL
		StateEnv[[name]]$ids$all <- union(ids.old, ids.new)
	}
	plotAndPlayUpdate()
}

.plotAndPlay_brush_event <- function(widget, user.data) {
	name <- StateEnv$.current
	if (!StateEnv[[name]]$is.lattice) {
		errorDialog("Brushing only works for Lattice plots (splom).")
		return()
	}
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# do brushing
	newFocus <- plotAndPlayDoFocus()
	if (!any(newFocus)) return()
	plotAndPlaySetPrompt(paste("Brushing data points...",
		"Click the right mouse button to finish."))
	brushed.new <- panel.brush.splom()
	myPacket <- as.character(packet.number())
	brushed.old <- StateEnv[[name]]$brushed[[myPacket]]
	if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
	StateEnv[[name]]$brushed[[myPacket]] <- brushed.new
	if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
}

.plotAndPlay_brush_drag_event <- function(widget, user.data) {
	name <- StateEnv$.current
	if (!StateEnv[[name]]$is.lattice) {
		errorDialog("Brushing only works for Lattice plots (splom).")
		return()
	}
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# do brushing
	newFocus <- plotAndPlayDoFocus()
	if (!any(newFocus)) return()
	plotAndPlaySetPrompt(paste("Brushing data points in a region...",
		"click and drag!"))
	pargs <- trellis.panelArgs()
	nvars <- length(pargs$z)
	devicePos <- getGraphicsEvent(prompt="",
		onMouseDown=function(buttons, x, y) {
			list(x=x, y=y)
		}
	)
	## which subpanel
	panelPos <- deviceNPCToVp(devicePos, unit="npc", valueOnly=T)
	colpos <- ceiling(panelPos$x * nvars)
	rowpos <- ceiling(panelPos$y * nvars)
	if (rowpos == colpos) {
		if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
		return()
	}
	subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")
	## get to that viewport, so we can convert units
	depth <- downViewport(subpanel.name)
	## coordinates of click in subpanel
	startPos <- deviceNPCToVp(devicePos, unit="native", valueOnly=T)
	datax <- pargs$z[, colpos]
	datay <- pargs$z[, rowpos]
	
	StateEnv[[name]]$tmp.brushed <- F
	getGraphicsEvent(prompt="",
		onMouseMove=function(buttons, x, y) {
			#if (length(buttons)==0) return(TRUE) # 'buttons' unimplemented?
			nowPos <- deviceNPCToVp(c(x, y), unit="native", valueOnly=T)
			xx <- c(startPos$x, nowPos$x)
			yy <- c(startPos$y, nowPos$y)
			brushed <- (
				(min(xx) < datax) & (datax < max(xx)) &
				(min(yy) < datay) & (datay < max(yy))
			)
			brushed.new <- brushed & !StateEnv[[name]]$tmp.brushed
			StateEnv[[name]]$tmp.brushed <- brushed |
				StateEnv[[name]]$tmp.brushed
			panel.points(datax[brushed.new], datay[brushed.new], 
				col='black', pch=16)
			NULL
		},
		onMouseUp=function(buttons, x, y) TRUE,
		onMouseDown=function(buttons, x, y) TRUE
	)
	upViewport(depth)
	brushed.new <- which(StateEnv[[name]]$tmp.brushed)
	splom.drawBrushed(brushed.new)
	StateEnv[[name]]$tmp.brushed <- NULL
	if (!is.null(pargs$subscripts)) {
		brushed.new <- pargs$subscripts[brushed.new]
	}
	myPacket <- as.character(packet.number())
	brushed.old <- StateEnv[[name]]$brushed[[myPacket]]
	if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
	StateEnv[[name]]$brushed[[myPacket]] <- brushed.new
	if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
}

.plotAndPlay_brush_region_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	if (!StateEnv[[name]]$is.lattice) {
		errorDialog("Brushing only works for Lattice plots (splom).")
		return()
	}
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	lowEdge <- "bottom-left corner"
	if (!nav.y) lowEdge <- "left edge"
	if (!nav.x) lowEdge <- "bottom edge"
	highEdge <- "top-right corner"
	if (!nav.y) highEdge <- "right edge"
	if (!nav.x) highEdge <- "top edge"
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.5)), name="tmp.mask")
	# do brushing
	newFocus <- plotAndPlayDoFocus()
	if (!any(newFocus)) return()
	plotAndPlaySetPrompt(paste("Brushing data points in a region...",
		"click at the", lowEdge))
	ll <- grid.locator(unit = "npc")
	if (is.null(ll)) {
		if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
		return()
	}
	pargs <- trellis.panelArgs()
	nvars <- length(pargs$z)
	## which subpanel
	colpos <- ceiling(convertUnit(ll$x, "npc", valueOnly = TRUE) * nvars)
	rowpos <- ceiling(convertUnit(ll$y, "npc", valueOnly = TRUE) * nvars)
	if (rowpos == colpos) {
		if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
		return()
	}
	subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")
	## coordinates of click in subpanel
	ll$x <- nvars * (ll$x - unit((colpos-1) / nvars, "npc"))
	ll$y <- nvars * (ll$y - unit((rowpos-1) / nvars, "npc"))
	## get to that viewport, so we can convert units
	depth <- downViewport(subpanel.name)
	xlim.new <- convertX(ll$x, "native", TRUE)
	ylim.new <- convertY(ll$y, "native", TRUE)
	# draw lower bounds
	if (nav.x) panel.abline(v=xlim.new)
	if (nav.y) panel.abline(h=ylim.new)
	# get upper bounds
	plotAndPlaySetPrompt(paste("OK, now click at the", highEdge))
	ll <- grid.locator(unit = "npc")
	if (is.null(ll)) {
		plotAndPlayUpdate()
		return()
	}
	ll$x <- nvars * (ll$x - unit((colpos-1) / nvars, "npc"))
	ll$y <- nvars * (ll$y - unit((rowpos-1) / nvars, "npc"))
	xlim.new[2] <- convertX(ll$x, "native", TRUE)
	ylim.new[2] <- convertY(ll$y, "native", TRUE)
	# draw upper bounds
	if (nav.x) panel.abline(v=xlim.new[2])
	if (nav.y) panel.abline(h=ylim.new[2])
	datax <- pargs$z[, colpos]
	datay <- pargs$z[, rowpos]
	brushed.new <- which(
		(min(xlim.new) < datax) & (datax < max(xlim.new)) &
		(min(ylim.new) < datay) & (datay < max(ylim.new))
	)
	if (!is.null(pargs$subscripts)) {
		brushed.new <- pargs$subscripts[brushed.new]
	}
	myPacket <- as.character(packet.number())
	brushed.old <- StateEnv[[name]]$brushed[[myPacket]]
	if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
	StateEnv[[name]]$brushed[[myPacket]] <- brushed.new
	#splom.drawBrushed(brushed.new)
	#if (!any(StateEnv[[name]]$focus)) trellis.unfocus()
	plotAndPlayUpdate()
}

.plotAndPlay_clear_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$ids <- list()
	StateEnv[[name]]$brushed <- list()
	plotAndPlayUpdate()
}

.plotAndPlay_zero_event <- function(widget, user.data) {
	name <- StateEnv$.current
	trans.x <- ("x" %in% StateEnv[[name]]$trans.scales)
	trans.y <- ("y" %in% StateEnv[[name]]$trans.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
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
				xlim[which.min(xlim)] <- 0 - 0.07 * max(abs(xlim))
			} else if (max(xlim) < 0) {
				xlim[which.max(xlim)] <- 0 + 0.07 * max(abs(xlim))
			}
			StateEnv[[name]]$call$xlim <- xlim
		}
		if (trans.y) {
			if (min(ylim) > 0) {
				ylim[which.min(ylim)] <- 0 - 0.07 * max(abs(ylim))
			} else if (max(ylim) < 0) {
				ylim[which.max(ylim)] <- 0 + 0.07 * max(abs(ylim))
			}
			StateEnv[[name]]$call$ylim <- ylim
		}
	} else {
		if (trans.x) StateEnv[[name]]$call$xlim <- NULL
		if (trans.y) StateEnv[[name]]$call$ylim <- NULL
	}
	plotAndPlayUpdate()
}

.plotAndPlay_logscale_event <- function(widget, user.data) {
	name <- StateEnv$.current
	trans.x <- ("x" %in% StateEnv[[name]]$trans.scales)
	trans.y <- ("y" %in% StateEnv[[name]]$trans.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get new log scale setting
	logScale <- widget$getActive()
	# make change and re-draw plot
	if (StateEnv[[name]]$is.lattice) {
		if (trans.x && trans.y) {
			# apply to both scales
			StateEnv[[name]]$call$scales$log <- logScale
		} else {
			if (trans.x) StateEnv[[name]]$call$scales$x$log <- logScale
			if (trans.y) StateEnv[[name]]$call$scales$y$log <- logScale
		}
	} else {
		logSpec <- "xy"
		if (!trans.y) logSpec <- "x"
		if (!trans.x) logSpec <- "y"
		StateEnv[[name]]$call$log <- if (logScale) logSpec
	}
	plotAndPlayUpdate()
}

.plotAndPlay_edit_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	theCall <- StateEnv[[name]]$call
	callTxt <- paste(deparse(theCall, control=c("showAttributes"),
		width=54), collapse="\n")
	repeat {
		newTxt <- guiTextInput(callTxt, title="Edit plot call", 
			prompt="", accepts.tab=F)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt)[[1]], error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			errorDialog(paste("Error:", conditionMessage(tmp)))
		} else {
			StateEnv[[name]]$call <- tmp
			plotAndPlayUpdate()
			break
		}
	}
	StateEnv[[name]]$win$present()
}

.plotAndPlay_zoomin3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$call$zoom <- StateEnv[[name]]$call$zoom * 1.5
	plotAndPlayUpdate()
}

.plotAndPlay_zoomout3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$call$zoom <- StateEnv[[name]]$call$zoom / 1.5
	plotAndPlayUpdate()
}

.plotAndPlay_flyleft3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$call$screen <- c(z=45, StateEnv[[name]]$call$screen)
	plotAndPlayUpdate()
}

.plotAndPlay_flyright3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$call$screen <- c(z=-45, StateEnv[[name]]$call$screen)
	plotAndPlayUpdate()
}

.plotAndPlay_flyup3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$call$screen <- c(z=45, StateEnv[[name]]$call$screen)
	plotAndPlayUpdate()
}

.plotAndPlay_flydown3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$call$screen <- c(z=45, StateEnv[[name]]$call$screen)
	plotAndPlayUpdate()
}

.plotAndPlay_prevpage_event <- function(widget, user.data) {
	name <- StateEnv$.current
	if (StateEnv[[name]]$page == 1) return()
	StateEnv[[name]]$page <- StateEnv[[name]]$page - 1
	plotAndPlayUpdate()
}

.plotAndPlay_nextpage_event <- function(widget, user.data) {
	name <- StateEnv$.current
	StateEnv[[name]]$page <- StateEnv[[name]]$page + 1
	plotAndPlayUpdate()
}

plotAndPlayGetCurrState <- function() {
	name <- StateEnv$.current
	StateEnv[[name]]
}

plotAndPlaySetCurrState <- function(state) {
	name <- StateEnv$.current
	StateEnv[[name]] <- state
	invisible(NULL)
}

# returns the new focus (col, row), which may be (0, 0), NULL if user cancelled
plotAndPlayDoFocus <- function(highlight=T, ...) {
	name <- StateEnv$.current
	if (any(StateEnv[[name]]$focus)) {
		return(StateEnv[[name]]$focus)
	} else {
		if (sum(trellis.currentLayout() > 0) == 1) {
			highlight <- F
		} else {
			plotAndPlaySetPrompt("First, choose a panel")
		}
		tmp <- trellis.focus(highlight=highlight, ...)
		return(tmp)
	}
}

plotAndPlayUpdate <- function() {
	name <- StateEnv$.current
	result <- eval(StateEnv[[name]]$call, StateEnv[[name]]$env)
	if (inherits(result, "trellis")) {
		# plot trellis object
		curPage <- StateEnv[[name]]$page
		print(result, packet.panel=packet.panel.page(curPage))
		# draw persistent labels
		packets <- trellis.currentLayout(which="packet")
		for (myPacket in names(StateEnv[[name]]$ids)) {
			whichOne <- which(packets == as.numeric(myPacket))
			if (length(whichOne) == 0) next
			myCol <- col(packets)[whichOne]
			myRow <- row(packets)[whichOne]
			trellis.focus("panel", myCol, myRow, highlight=F)
			# find which points are identified
			ids <- StateEnv[[name]]$ids[[myPacket]]
			idCall <- StateEnv[[name]]$id.call
			labels <- eval(idCall$labels, StateEnv[[name]]$env)
			if ('x' %in% names(idCall)) {
				xy <- xy.coords.call(idCall, StateEnv[[name]]$env)
			} else {
				pargs <- trellis.panelArgs()
				xy <- xy.coords(pargs, recycle=T)
				subscripts <- pargs$subscripts
				if (length(labels) == 0) {
					labels <- subscripts
				}
				if (length(labels) > length(subscripts)) {
					labels <- labels[subscripts]
				}
				# next line same as: which(subscripts %in% ids)
				ids <- findInterval(ids, subscripts)
			}
			if (length(labels) > 0) {
				label.args <- StateEnv[[name]]$label.args
				do.call(panel.text, c(list(xy$x[ids], xy$y[ids], 
					labels=labels[ids], pos=1), label.args))
			}
			trellis.unfocus()
		}
		# draw persistent brushing
		for (myPacket in names(StateEnv[[name]]$brushed)) {
			whichOne <- which(packets == as.numeric(myPacket))
			if (length(whichOne) == 0) next
			myCol <- col(packets)[whichOne]
			myRow <- row(packets)[whichOne]
			trellis.focus("panel", myCol, myRow, highlight=F)
			# find which points are identified
			pargs <- trellis.panelArgs()
			ids <- StateEnv[[name]]$brushed[[myPacket]]
			# next line same as: which(pargs$subscripts %in% ids)
			ids.sub <- findInterval(ids, pargs$subscripts)
			splom.drawBrushed(ids.sub)
			trellis.unfocus()
		}
		# set focus
		if (any(StateEnv[[name]]$focus)) {
			with(StateEnv[[name]]$focus, 
				trellis.focus("panel", col, row))
		}
	} else {
		# draw persistent labels
		ids <- StateEnv[[name]]$ids$all
		if (any(ids)) {
			idCall <- StateEnv[[name]]$id.call
			labels <- eval(idCall$labels, StateEnv[[name]]$env)
			xy <- xy.coords.call(idCall, StateEnv[[name]]$env)
			if (length(labels) == 0) labels <- seq_along(xy$x)
			if (length(labels) > 0) {
				label.args <- StateEnv[[name]]$label.args
				do.call(text, c(list(xy$x[ids], xy$y[ids], 
					labels=labels[ids], pos=1), label.args))
			}
		}
	}
	invisible(result)
}

plotAndPlayGetDA <- function() {
	name <- StateEnv$.current
	StateEnv[[name]]$win$getChildren()[[1]]$getChildren()[[1]]
}

plotAndPlayGetToolbar <- function() {
	name <- StateEnv$.current
	StateEnv[[name]]$win$getChildren()[[1]]$getChildren()[[2]]
}

plotAndPlayMakePrompt <- function() {
	name <- StateEnv$.current
	theToolbar <- plotAndPlayGetToolbar()
	# first freeze the toolbar size so plot doesn't resize
	toolbarSize <- theToolbar$getAllocation()
	theToolbar$setSizeRequest(toolbarSize$width, toolbarSize$height)
	toolItems <- theToolbar$getChildren()
	for (x in rev(toolItems)) x$hide()
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

plotAndPlayUnmakePrompt <- function() {
	name <- StateEnv$.current
	theToolbar <- plotAndPlayGetToolbar()
	theToolbar$getChildren()[[1]]$destroy()
	toolItems <- theToolbar$getChildren()
	for (x in toolItems) x$show()
	# unfreeze the toolbar size
	theToolbar$setSizeRequest(-1, -1)
}

plotAndPlaySetPrompt <- function(text) {
	name <- StateEnv$.current
	if (!is.character(text)) stop("'text' must be character")
	theToolbar <- plotAndPlayGetToolbar()
	myLabel <- theToolbar$getChildren()[[1]]$getChildren()[[1]]$getChildren()[[1]]
	myLabel$setMarkup(paste(sep='','<big><b>', text, '</b></big>'))
}

latticeUnLog <- function(x, y, scalesArg) {
	# x scale
	if (!is.null(scalesArg$x$log)) {
		logBase <- latticeLogBase(scalesArg$x$log)
		if (!is.null(logBase)) x <- logBase ^ x
	} else {
		logBase <- latticeLogBase(scalesArg$log)
		if (!is.null(logBase)) x <- logBase ^ x
	}
	# y scale
	if (!is.null(scalesArg$y$log)) {
		logBase <- latticeLogBase(scalesArg$y$log)
		if (!is.null(logBase)) y <- logBase ^ y
	} else {
		logBase <- latticeLogBase(scalesArg$log)
		if (!is.null(logBase)) y <- logBase ^ y
	}
	return(list(x=x, y=y))
}

latticeLogBase <- function(x) {
	x <- eval(x)
	if (is.null(x) || identical(x, FALSE)) return(NULL)
	if (isTRUE(x)) return(10)
	if (identical(x, "e")) return(exp(1))
	x
}

xy.coords.call <- function(the.call, envir=parent.frame(), log=NULL, recycle=TRUE) {
	stopifnot(is.call(the.call))
	# put call into canonical form
	the.call <- match.call(eval(the.call[[1]], envir=envir), the.call)
	xy.coords(eval(the.call$x, envir),
		if ('y' %in% names(the.call)) eval(the.call$y, envir),
		log=log, recycle=recycle)
}

substitute.call <- function(the.call, ...) 
	do.call(substitute, list(the.call, ...))

copyArgsIntoEnv <- function(the.call, envir=parent.frame(), newEnv, inherits=F, pattern=T, invert.match=F) {
	stopifnot(is.call(the.call) || is.list(the.call))
	isMatch <- !invert.match
	for (i in seq_along(the.call)) {
		if (is.call(the.call) && (i == 1)) next
		this.arg <- the.call[[i]]
		# skip literal symbol in "$" extractor
		if (is.call(this.arg) && this.arg[[1]] == as.symbol("$"))
			this.arg <- this.arg[[2]]
		
		if (mode(this.arg) %in% c("call", "(", "list")) {
			# call recursively...
			copyArgsIntoEnv(this.arg, envir=envir, newEnv=newEnv,
				inherits=inherits, pattern=pattern, 
				invert.match=invert.match)
		} else if (mode(this.arg) %in% "name") {
			this.name <- as.character(this.arg)
			if (!isTRUE(pattern) && 
				(any(grep(pattern, this.name))) != isMatch)
				next
			if (exists(this.name, envir=envir, inherits=inherits)
			&& !exists(this.name, envir=newEnv, inherits=F)) {
				assign(this.name, eval(this.arg, envir=envir),
					envir=newEnv)
			}
		}
		# leave constants and expressions alone
	}
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

# based on grid::locator
deviceNPCToVp <- function(pos, unit="native", valueOnly=FALSE) {
	stopifnot(length(pos) == 2)
	# first find device size in inches
	# (note: par("din") is wrong, at least in my cairoDevice window)
	din <- grid:::grid.Call("L_currentViewport")[c("devwidthcm","devheightcm")]
	din <- convertX(unit(unlist(din),"cm"), "inches", valueOnly=T)
	location <- c(din * as.numeric(unlist(pos)), 1)
        transform <- solve(grid::current.transform())
        location <- (location %*% transform)
	location <- unit(location/location[3], "inches")
	list(x=convertX(location[1], unit, valueOnly=valueOnly), 
		y=convertY(location[2], unit, valueOnly=valueOnly))
}

## The following functions by Deepayan Sarkar <Deepayan.Sarkar@R-project.org>

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

splom.drawBrushed <- function(ids, pargs=trellis.panelArgs(), threshold=18, col='black', pch=16, cex=1, ...) {
	nvars <- length(pargs$z)
	for (row in 1:nvars)
        for (column in 1:nvars)
            if (row != column)
            {
                subpanel.name <-
                    paste("subpanel",
                          column, row, sep = ".")
                depth <- downViewport(subpanel.name)
                panel.points(x = pargs$z[ids, column],
                             y = pargs$z[ids, row],
                             pch = pch, col = col, cex = cex,
                             ...)
                upViewport(depth)
            }
}


