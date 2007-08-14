## plotAndPlayGTK: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## with contributions from Deepayan Sarkar and Graham Williams
## GPL version 2 or newer

plotAndPlayButtons <- alist(
	annotate=makeAnnotateMenuButton(),
	identify=makeIdentifyMenuButton(),
	zoom=makeZoomMenuButton(),
	identify.simple=quickTool("Identify points", "gtk-info", tooltip="Identify data points by clicking on them", f=.plotAndPlay_identify_event),
	zoomin=quickTool("Zoom to...", "gtk-zoom-in", tooltip="Select plot region with the mouse", f=.plotAndPlay_zoomin_event),
	zoomout=quickTool("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event),
	zoomfit=quickTool("Fit data", "gtk-zoom-fit", f=.plotAndPlay_zoomfit_event),
	centre=quickTool("Re-centre", "gtk-jump-to-ltr", f=.plotAndPlay_centre_event),
	zero=quickTool("Full scale", "gtk-goto-bottom", tooltip="Show the full scale starting from zero", f=.plotAndPlay_zero_event, isToggle=T),
	logscale=quickTool("Log scale", "gtk-goto-top", tooltip="Use a logarithmic scale (base 10)", f=.plotAndPlay_logscale_event, isToggle=T),
	layers=makeLayersMenuButton(),
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
	next.page=quickTool("Next page", "gtk-go-forward-ltr", f=.plotAndPlay_nextpage_event),
	edit=quickTool("Edit call", "gtk-italic", tooltip="Edit the plot call as text", f=.plotAndPlay_edit_event)
)

plotAndPlayBasicButtons <- alist(
	save=makeSaveMenuButton(),
	copy=quickTool("Copy", "gtk-copy", tooltip="Copy this plot to the clipboard (as a bitmap)", f=.plotAndPlay_copy_event),
	print=quickTool("Print", "gtk-print", f=.plotAndPlay_print_event),
	greyscale=quickTool("Greyscale", "gtk-print-preview", tooltip="Switch to the greyscale lattice theme (then save the plot for printing)", f=.plotAndPlay_greyscale_event, isToggle=T)
)

quickTool <- function(label, icon.name=NULL, tooltip=NULL, f, data=NULL, isToggle=F) {
	x <- if (isToggle) gtkToggleToolButton() else gtkToolButton()
	x["label"] <- label
	x["icon-name"] <- icon.name
	if (!is.null(tooltip)) {
		thisTips <- gtkTooltips()
		thisTips$setTip(x, tooltip)
		thisTips$enable()
	}
	gSignalConnect(x, "clicked", f, data=data)
	x
}

makeAnnotateMenuButton <- function() {
	annButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-edit", 
		size=GtkIconSize['small-toolbar']), label="Annotate")
	thisTips <- gtkTooltips()
	thisTips$setTip(annButton, "Add your own labels to the plot")
	gSignalConnect(annButton, "clicked", .plotAndPlay_annotate_event,
		data=list(figure=TRUE))
	annMenu <- gtkMenu()
	itemFigLabel <- gtkMenuItem("Add label to figure")
	itemPlotLabel <- gtkMenuItem("Add label inside plot region")
	itemFigArrow <- gtkMenuItem("Add arrow to figure")
	itemPlotArrow <- gtkMenuItem("Add arrow inside plot region")
	itemEdit <- gtkMenuItem("Edit annotations")
	itemClear <- gtkMenuItem("Clear annotations")
	annMenu$append(itemFigLabel)
	annMenu$append(itemFigArrow)
	annMenu$append(gtkSeparatorMenuItem())
	annMenu$append(itemPlotLabel)
	annMenu$append(itemPlotArrow)
	annMenu$append(gtkSeparatorMenuItem())
	annMenu$append(itemEdit)
	annMenu$append(itemClear)
	annButton$setMenu(annMenu)
	gSignalConnect(itemFigLabel, "activate", .plotAndPlay_annotate_event, 
		data=list(figure=TRUE))
	gSignalConnect(itemFigArrow, "activate", .plotAndPlay_annotate_event, 
		data=list(figure=TRUE, arrow=TRUE))
	gSignalConnect(itemPlotLabel, "activate", .plotAndPlay_annotate_event)
	gSignalConnect(itemPlotArrow, "activate", .plotAndPlay_annotate_event, 
		data=list(arrow=TRUE))
	gSignalConnect(itemEdit, "activate", .plotAndPlay_edit_annotations_event)
	gSignalConnect(itemClear, "activate", .plotAndPlay_clear_event,
		data=list(types="annotations"))
	annButton
}

makeIdentifyMenuButton <- function() {
	idButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-info", 
		size=GtkIconSize['small-toolbar']), label="Identify points")
	thisTips <- gtkTooltips()
	thisTips$setTip(idButton, "Identify data points by clicking on them")
	gSignalConnect(idButton, "clicked", .plotAndPlay_identify_event)
	idMenu <- gtkMenu()
	itemRegion <- gtkMenuItem("Identify all points in a region")
	itemClear <- gtkMenuItem("Clear labels")
	idMenu$append(itemRegion)
	idMenu$append(itemClear)
	idButton$setMenu(idMenu)
	gSignalConnect(itemRegion, "activate", .plotAndPlay_identify_region_event)
	gSignalConnect(itemClear, "activate", .plotAndPlay_clear_event,
		data=list(types="ids"))
	idButton
}

makeZoomMenuButton <- function() {
	zoomButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-zoom-in", 
		size=GtkIconSize['small-toolbar']), label="Zoom to...")
	thisTips <- gtkTooltips()
	thisTips$setTip(zoomButton, "Select new plot region with the mouse")
	gSignalConnect(zoomButton, "clicked", .plotAndPlay_zoomin_event)
	zMenu <- gtkMenu()
	itemZoomout <- gtkMenuItem("Zoom out")
	itemZoomfit <- gtkMenuItem("Zoom to fit")
	itemCentre <- gtkMenuItem("Re-centre")
	zMenu$append(itemZoomout)
	zMenu$append(itemZoomfit)
	zMenu$append(itemCentre)
	zoomButton$setMenu(zMenu)
	gSignalConnect(itemZoomout, "activate", .plotAndPlay_zoomout_event)
	gSignalConnect(itemZoomfit, "activate", .plotAndPlay_zoomfit_event)
	gSignalConnect(itemCentre, "activate", .plotAndPlay_centre_event)
	zoomButton
}

makeSaveMenuButton <- function() {
	saveButton <- gtkMenuToolButton(gtkImageNewFromStock('gtk-save-as', 
		size=GtkIconSize['small-toolbar']), label="Save as")
	gSignalConnect(saveButton, "clicked", .plotAndPlay_save_event)
	saveMenu <- gtkMenu()
	saveItemPDF <- gtkMenuItem("PDF")
	saveItemPNG <- gtkMenuItem("PNG (bitmap)")
	saveItemPS <- gtkMenuItem("PostScript")
	saveItemEPS <- gtkMenuItem("EPS")
	saveItemSVG <- gtkMenuItem("SVG")
	saveItemWMF <- gtkMenuItem("WMF")
	saveItemXfig <- gtkMenuItem("xfig")
	saveMenu$append(saveItemPDF)
	saveMenu$append(saveItemPNG)
	saveMenu$append(saveItemPS)
	saveMenu$append(saveItemEPS)
	saveMenu$append(saveItemSVG)
	saveMenu$append(saveItemWMF)
	saveMenu$append(saveItemXfig)
	saveButton$setMenu(saveMenu)
	gSignalConnect(saveItemPDF, "activate", .plotAndPlay_save_event, data=list(ext="pdf"))
	gSignalConnect(saveItemPNG, "activate", .plotAndPlay_save_event, data=list(ext="png"))
	gSignalConnect(saveItemPS, "activate", .plotAndPlay_save_event, data=list(ext="ps"))
	gSignalConnect(saveItemEPS, "activate", .plotAndPlay_save_event, data=list(ext="eps"))
	gSignalConnect(saveItemSVG, "activate", .plotAndPlay_save_event, data=list(ext="svg"))
	gSignalConnect(saveItemWMF, "activate", .plotAndPlay_save_event, data=list(ext="wmf"))
	gSignalConnect(saveItemXfig, "activate", .plotAndPlay_save_event, data=list(ext="fig"))
	saveButton
}

makeLayersMenuButton <- function() {
	name <- StateEnv$.current
	layersButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-index", 
		size=GtkIconSize['small-toolbar']), label="Layers...")
	thisTips <- gtkTooltips()
	thisTips$setTip(layersButton, "Toggle visible layers")
	itemNames <- NULL
	itemIDs <- NULL
	itemStates <- NULL
	the.call <- StateEnv[[name]]$call
	layerType <- NA
	if ('layers' %in% names(the.call)) {
		layerType <- "layers"
		# store evaluated list in call
		StateEnv[[name]]$call$layers <- eval(the.call$layers, StateEnv[[name]]$env)
		the.call <- StateEnv[[name]]$call
		for (i in seq_along(the.call$layers)) {
			itemName <- names(the.call$layers)[i]
			if (is.null(itemName) || (nchar(itemName) == 0)) {
				if (is.expression(the.call$layers[[i]])) {
					itemName <- deparse(the.call$layers[[i]][[1]])[1]
				} else itemName <- deparse(the.call$layers[[i]])[1]
			}
			itemNames[i] <- itemName
			itemIDs[i] <- i
			itemStates[i] <- !any(grep("\\.off$", itemName))
		}
	} else if ('sp.layout' %in% names(the.call)) {
		layerType <- "sp.layout"
		for (i in seq_along(the.call$sp.layout)) {
			itemName <- names(the.call$sp.layout)[i]
			if (is.null(itemName) || (nchar(itemName) == 0)) {
				itemName <- deparse(the.call$sp.layout[[i]])[1]
				# TODO: make lists pretty
			}
			itemNames[i] <- itemName
			itemIDs[i] <- i
			itemStates[i] <- !identical(the.call$sp.layout[[i]]$which, 0)
		}
	} else if ('panel' %in% names(the.call)) {
		layerType <- "panel"
		# store evaluated panel function in call
		StateEnv[[name]]$call$panel <- eval(the.call$panel, StateEnv[[name]]$env)
		panelBody <- body(StateEnv[[name]]$call$panel)
		# treat any call to panel.* or grid.* as a layer
		r.grep.call <- function(x, pattern="^panel\\.|^grid\\.", indexPath=NULL) {
			stopifnot(is.call(x))
			if (any(grep(pattern, deparse(x[[1]])[1]))) {
				return(bquote(c(.(indexPath))))
			}
			unlist(lapply(seq_along(x)[-1], function(i) 
				if (is.call(x[[i]])) 
					r.grep.call(x[[i]], pattern=pattern,
						indexPath=c(indexPath, i))))
		}
		itemIDs <- r.grep.call(panelBody)
		itemIDs <- lapply(itemIDs, eval) # convert from `call` to vector
		itemIDsStr <- sapply(itemIDs, toIndexStr)
		itemNames <- sapply(paste('panelBody',itemIDsStr,sep=''), 
			function(s) deparse(eval(parse(text=s)))[1] )
		# work out whether each item is quoted
		itemStates <- rep(T, length(itemIDs))
		drop_last <- function(xx) lapply(xx, function(x) 
			if (length(x) > 1) x[-length(x)] else x)
		itemParentIDsStr <- sapply(drop_last(itemIDs), toIndexStr)
		itemStates <- sapply(paste('panelBody',itemParentIDsStr,sep=''), 
			function(s) eval(parse(text=s))[[1]] != as.symbol("quote") )
		itemIDs[itemStates==F] <- drop_last(itemIDs[itemStates==F])
		# tmp <- quote({ print(x); while (x) { print(panel.list('a','b')); x <- something(grid.lines()) } })
	}
	itemNames <- sapply(itemNames, toString, width=34)
	# store layers info
	StateEnv[[name]]$layers.names <- itemNames
	StateEnv[[name]]$layers.ids <- itemIDs
	# make button menu
	layersMenu <- gtkMenu()
	for (i in seq_along(itemIDs)) {
		menuItem <- gtkCheckMenuItem(itemNames[i]) #gtkMenuItem(itemName)
		menuItem['active'] <- itemStates[i]
		layersMenu$append(menuItem)
		gSignalConnect(menuItem, "activate", .plotAndPlay_layers_event, 
			data=list(index=i, ID=itemIDs[[i]], layerType=layerType))
	}
	layersButton$setMenu(layersMenu)
	# set main button handler
	gSignalConnect(layersButton, "clicked", .plotAndPlay_layers_event,
		data=list(menu=layersMenu, layerType=layerType))
	layersButton
}

##### BUTTON HANDLERS #####

.plotAndPlay_save_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# get filename
	myExt <- if (!missing(user.data) && !is.null(user.data$ext)) 
		user.data$ext else 'pdf'
	myDefault <- paste(name, '.', myExt, sep='')
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/png/ps/etc)", 
		filters=Filters[c("pdf","png","ps","eps","svg","wmf","fig"),],
		index=match(myExt, c("pdf","png","ps","eps","svg","wmf","fig"))
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
	else if (ext %in% "ps") {
		dev.copy(postscript, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "eps") {
		dev.copy(postscript, file=filename, width=myWidth, height=myHeight,
			horizontal=FALSE, onefile=FALSE, paper="special")
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
	else if (ext %in% c("wmf", "emf")) {
		dev.copy(win.metafile, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "fig") {
		dev.copy(xfig, file=filename, width=myWidth, height=myHeight)
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

.plotAndPlay_print_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# print plot
	isWindows <- (.Platform$OS.type == "windows")
	if (isWindows) dev.print(win.print)
	else dev.print()
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
	maskGrob <- rectGrob(gp=gpar(col="red", 
		fill=rgb(0.5,0.5,0.5, alpha=0.25)), name="tmp.mask")
	# get new scales interactively
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		newFocus <- plotAndPlayDoFocus(clip.off=T)
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
	}
	# this converts from raw numeric to original format (including unlog)
	if (nav.x) plotAndPlaySetRawXLim(xlim.new)
	if (nav.y) plotAndPlaySetRawYLim(ylim.new)
	plotAndPlayUpdate()
}

.plotAndPlay_zoomout_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# find existing scales
	if (StateEnv[[name]]$is.lattice) {
		if (!any(StateEnv[[name]]$focus)) {
			okPnl <- which(trellis.currentLayout() > 0, arr=T)[1,]
			trellis.focus("panel", okPnl['col'], okPnl['row'], highlight=F)
		}
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
	}
	# zoom out: make range twice the size
	if (nav.x) xlim <- xlim + diff(xlim) * c(-0.5, 0.5)
	if (nav.y) ylim <- ylim + diff(ylim) * c(-0.5, 0.5)
	# this converts from raw numeric to original format (including unlog)
	if (nav.x) plotAndPlaySetRawXLim(xlim)
	if (nav.y) plotAndPlaySetRawYLim(ylim)
	plotAndPlayUpdate()
}

.plotAndPlay_zoomfit_event <- function(widget, user.data) {
	name <- StateEnv$.current
	nav.x <- ("x" %in% StateEnv[[name]]$nav.scales)
	nav.y <- ("y" %in% StateEnv[[name]]$nav.scales)
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# update scales
	if (nav.x) StateEnv[[name]]$call$xlim <- NULL
	if (nav.y) StateEnv[[name]]$call$ylim <- NULL
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
	}
	if (nav.x) xlim <- clickLoc$x + diff(xlim) * c(-0.5, 0.5)
	if (nav.y) ylim <- clickLoc$y + diff(ylim) * c(-0.5, 0.5)
	# this converts from raw numeric to original format (including unlog)
	if (nav.x) plotAndPlaySetRawXLim(xlim)
	if (nav.y) plotAndPlaySetRawYLim(ylim)
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
		xlim <- unlogX(xlim.new, StateEnv[[name]]$call)
		ylim <- unlogY(ylim.new, StateEnv[[name]]$call)
		# set identified points
		if ('x' %in% names(idCall <- StateEnv[[name]]$id.call)) {
			xy <- xy.coords.call(idCall, StateEnv[[name]]$env)
			subscripts <- seq_along(xy$x)
		} else {
			pargs <- trellis.panelArgs()
			xy <- xy.coords(pargs, recycle=T)
			# convert back from log scale if required
			xy$x <- unlogX(xy$x, StateEnv[[name]]$call)
			xy$y <- unlogY(xy$y, StateEnv[[name]]$call)
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
		xlim <- unlogX(xlim.new, StateEnv[[name]]$call, is.lattice=F)
		ylim <- unlogY(ylim.new, StateEnv[[name]]$call, is.lattice=F)
		xy <- xy.coords.call(StateEnv[[name]]$id.call, StateEnv[[name]]$env)
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

.plotAndPlay_clear_event <- function(widget, user.data=NULL) {
	name <- StateEnv$.current
	types <- c("ids", "annotations", "brushed")
	if (!is.null(user.data$types)) types <- user.data$types
	for (type in types)
		StateEnv[[name]][[type]] <- list()
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
	if (zeroScale == FALSE) {
		if (trans.x) StateEnv[[name]]$call$xlim <- NULL
		if (trans.y) StateEnv[[name]]$call$ylim <- NULL
		plotAndPlayUpdate()
		return()
	}
	# else... (zeroScale == TRUE)
	if (StateEnv[[name]]$is.lattice) {
		# lattice plot
		if (!any(StateEnv[[name]]$focus)) {
			okPnl <- which(trellis.currentLayout() > 0, arr=T)[1,]
			trellis.focus("panel", okPnl['col'], okPnl['row'], highlight=F)
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
		StateEnv[[name]]$call$xlim <- signif(xlim, 4)
	}
	if (trans.y) {
		if (min(ylim) > 0) {
			ylim[which.min(ylim)] <- 0 - 0.07 * max(abs(ylim))
		} else if (max(ylim) < 0) {
			ylim[which.max(ylim)] <- 0 + 0.07 * max(abs(ylim))
		}
		StateEnv[[name]]$call$ylim <- signif(ylim, 4)
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

.plotAndPlay_annotate_event <- function(widget, user.data) {
	name <- StateEnv$.current
	isFigure <- (!missing(user.data) && isTRUE(user.data$figure))
	isArrow <- (!missing(user.data) && isTRUE(user.data$arrow))
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# set up prompt
	plotAndPlayMakePrompt()
	on.exit(plotAndPlayUnmakePrompt(), add=T)
	# get location
	myPacket <- "all"
	myPrompt <- "Click to place a label inside the plot region"
	if (isFigure) myPrompt <- "Click to place a label on the figure"
	if (isArrow) myPrompt <- "Click at the start of the arrow (\"from\")"
	nextPrompt <- "OK, now click at the end of the arrow (\"to\")"
	
	if (StateEnv[[name]]$is.lattice) {
		if (isFigure) {
			trellis.focus("toplevel", highlight=F)
			plotAndPlaySetPrompt(myPrompt)
		} else {
			newFocus <- plotAndPlayDoFocus()
			if (!any(newFocus)) return()
			myPacket <- as.character(packet.number())
			plotAndPlaySetPrompt(myPrompt)
		}
		on.exit(trellis.unfocus(), add=T)
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) return()
		clickLoc <- lapply(clickLoc, as.numeric)
		clickLoc <- lapply(clickLoc, signif, 4)
		if (isArrow) {
			plotAndPlaySetPrompt(nextPrompt)
			clickLoc1 <- grid.locator()
			if (is.null(clickLoc1)) return()
			clickLoc1 <- lapply(clickLoc1, as.numeric)
			clickLoc1 <- lapply(clickLoc1, signif, 4)
			theCall <- call('panel.arrows', x0=clickLoc$x, y0=clickLoc$y,
				x1=clickLoc1$x, y1=clickLoc1$y, length=0.2)
		} else {
			myLabel <- placeLabelDialog()
			if (is.null(myLabel)) return()
			myAdj <- switch(as.character(myLabel$align[1]),
				`0`="left", `0.5`="centre", `1`="right")
			myAdj[2] <- switch(as.character(myLabel$align[2]),
				`0`="bottom", `0.5`="centre", `1`="top")
			theCall <- call('panel.text', myLabel$text, 
				x=clickLoc$x, y=clickLoc$y, adj=myAdj)
		}
		# add user-specified default style
		theCall <- as.call(c(as.list(theCall), StateEnv[[name]]$label.args))
		
	} else {
		# traditional graphics plot
		plotAndPlaySetPrompt(myPrompt)
		if (isFigure) op <- par(usr=rep(0:1,2), xpd=NA, xlog=F, ylog=F)
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) return()
		clickLoc <- lapply(clickLoc, signif, 4)
		if (isArrow) {
			plotAndPlaySetPrompt(nextPrompt)
			clickLoc1 <- locator(n=1)
			if (is.null(clickLoc1)) return()
			clickLoc1 <- lapply(clickLoc1, signif, 4)
			theCall <- call('arrows', x0=clickLoc$x, y0=clickLoc$y,
				x1=clickLoc1$x, y1=clickLoc1$y, length=0.2)
		} else {
			myLabel <- placeLabelDialog()
			if (is.null(myLabel)) return()
			theCall <- call('text', myLabel$text, 
				x=clickLoc$x, y=clickLoc$y, adj=myLabel$align)
		}
		# revert graphical settings
		if (isFigure) par(op)
		# add user-specified default style
		theCall <- as.call(c(as.list(theCall), StateEnv[[name]]$label.args))
		if (isFigure) {
			theCall <- bquote({
				op <- par(usr=rep(0:1,2), xpd=NA, xlog=F, ylog=F)
				.(theCall)
				par(op)
			})
		}
	}
	# add the annotation
	eval(theCall, StateEnv[[name]]$env)
	StateEnv[[name]]$annotations[[myPacket]] <- 
		c(StateEnv[[name]]$annotations[[myPacket]], theCall)
	StateEnv[[name]]$win$present()
}

.plotAndPlay_edit_annotations_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	theAnnots <- StateEnv[[name]]$annotations$all
	callTxt <- paste(unlist(lapply(theAnnots, deparse, 
		control=c("showAttributes"), width=50)), collapse="\n")
	repeat {
		newTxt <- guiTextInput(callTxt, title="Edit annotations", 
			prompt="", accepts.tab=F)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			errorDialog(paste("Error:", conditionMessage(tmp)))
		} else {
			StateEnv[[name]]$annotations$all <- tmp
			plotAndPlayUpdate()
			break
		}
	}
	StateEnv[[name]]$win$present()
}

.plotAndPlay_edit_in_place_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	# the original call -- this should match the code in plotAndPlayUpdate!
	callTxt <- deparseOneLine(StateEnv[[name]]$call, control="showAttributes")
	newTxt <- widget$getText()
	if (identical(newTxt, callTxt)) return()
	if (identical(newTxt, "")) return()
	tmp <- tryCatch(parse(text=newTxt), error=function(e)e)
	# check whether there was a syntax error
	if (inherits(tmp, "error")) {
		errorDialog(paste("Error:", conditionMessage(tmp)))
	} else {
		# if more than one call, wrap them in braces
		StateEnv[[name]]$call <- if (length(tmp) > 1)
			as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
		plotAndPlayInit()
		plotAndPlayUpdate()
	}
	StateEnv[[name]]$win$present()
}

.plotAndPlay_edit_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	theCall <- StateEnv[[name]]$call
	callTxt <- paste(deparse(theCall, control=c("showAttributes"),
		width=50), collapse="\n")
	repeat {
		newTxt <- guiTextInput(callTxt, title="Edit plot call", 
			prompt="", accepts.tab=F)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			errorDialog(paste("Error:", conditionMessage(tmp)))
		} else {
			# if more than one call, wrap them in braces
			StateEnv[[name]]$call <- if (length(tmp) > 1)
				as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
			plotAndPlayInit()
			plotAndPlayUpdate()
			break
		}
	}
	StateEnv[[name]]$win$present()
}

.plotAndPlay_layers_event <- function(widget, user.data=NULL) {
	name <- StateEnv$.current
	itemIdx <- user.data$index
	itemID <- user.data$ID
	layerType <- user.data$layerType
	
	if (is.null(itemID)) {
		menuItems <- user.data$menu$getChildren()
		n <- length(StateEnv[[name]]$layers.names)
		items <- paste(1:n, StateEnv[[name]]$layers.names)
		states <- sapply(menuItems, function(x) x['active'])
		newItems <- select.list(items, preselect=items[states], 
			multiple=T, title="Select layers")
		StateEnv[[name]]$win$present()
		newStates <- (items %in% newItems)
		if (all(states == newStates)) return()
		if (all(newStates == FALSE)) return() # might be 'cancel'
		StateEnv[[name]]$skip.updates <- T
		for (i in seq_along(newStates)) {
			menuItems[[i]]['active'] <- newStates[i]
		}
		StateEnv[[name]]$skip.updates <- F
		plotAndPlayUpdate()
		return()
	}
	# a single menu item toggled
	isActive <- widget['active']
	if (layerType == "layers") {
		layerName <- names(StateEnv[[name]]$call$layers)[itemIdx]
		if (is.null(layerName)) layerName <- ""
		if (isActive) { 
			layerName <- sub("\\.off$", "", layerName)
			names(StateEnv[[name]]$call$layers)[itemIdx] <- layerName
		} else {
			layerName <- paste(layerName, ".off", sep="")
			names(StateEnv[[name]]$call$layers)[itemIdx] <- layerName
		}
	} else if (layerType == "sp.layout") {
		# TODO: store existing `which`
		if (isActive) {
			StateEnv[[name]]$call$sp.layout[[itemIdx]]$which <- NULL
		} else {
			StateEnv[[name]]$call$sp.layout[[itemIdx]]$which <- 0
		}
	} else if (layerType == "panel") {
		target <- paste('body(StateEnv[[name]]$call$panel)',toIndexStr(itemID),sep='')
		if (isActive) {
			cmd <- paste(target, " <- ", target, "[[2]]", sep='')
		} else {
			cmd <- paste(target, "<- call('quote',", target, ")")
		}
		eval(parse(text=cmd))
	}
	plotAndPlayUpdate()
}

.plotAndPlay_zoomin3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	zoom <- eval(StateEnv[[name]]$call$zoom, StateEnv[[name]]$env)
	StateEnv[[name]]$call$zoom <- signif(zoom * 1.5, 4)
	plotAndPlayUpdate()
}

.plotAndPlay_zoomout3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	zoom <- eval(StateEnv[[name]]$call$zoom, StateEnv[[name]]$env)
	StateEnv[[name]]$call$zoom <- signif(zoom / 1.5, 4)
	plotAndPlayUpdate()
}

.plotAndPlay_flyleft3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	screen <- eval(StateEnv[[name]]$call$screen, StateEnv[[name]]$env)
	if (names(screen)[1] == 'z') screen[[1]] <- screen[[1]] + 45
	else screen <- c(z = 45, screen)
	# convert list to call so that deparse is pretty
	StateEnv[[name]]$call$screen <- as.call(c(quote(list), screen))
	plotAndPlayUpdate()
}

.plotAndPlay_flyright3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
	screen <- eval(StateEnv[[name]]$call$screen, StateEnv[[name]]$env)
	if (names(screen)[1] == 'z') screen[[1]] <- screen[[1]] - 45
	else screen <- c(z = -45, screen)
	# convert list to call so that deparse is pretty
	StateEnv[[name]]$call$screen <- as.call(c(quote(list), screen))
	plotAndPlayUpdate()
}

.plotAndPlay_flyup3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
}

.plotAndPlay_flydown3d_event <- function(widget, user.data) {
	name <- StateEnv$.current
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

