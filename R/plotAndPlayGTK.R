## plotAndPlayGTK: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

MAJOR <- "0"
MINOR <- "8"
REVISION <- unlist(strsplit("$Revision: 78 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- "(c) 2007 Felix Andrews <felix@nfrac.org>"
WEBSITE <- "http://plotandplay-gtk.googlecode.com/"

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

#button to go into nav mode?

#gtkEntry with time; gtkCalendar in a gtkDialog
#gtkHScrollbar(adjustment)
#makeScrollbarTool <- function()
#gtkAdjustment(value = NULL, lower = NULL, upper = NULL, step.incr = NULL, page.incr = NULL, page.size = NULL) 
#setUpdatePolicy(GtkUpdateType["discontinuous"])

# hide 'fit data' button if [xy]lim is NULL?

# trellis.focus("panel", 1, 1, highlight=F)
# which(trellis.currentLayout() > 0)[1]
# okPanel <- which(trellis.currentLayout() > 0, arr.ind=T)[1,]
# trellis.focus("panel", okPanel['col'], okPanel['row'], highlight=F)

# look at rpanel and tkwidgets and GeoXP and iplots etc

# superpose button:
# off = ~ data | which
# on = ~ data,  groups=which
# hmm = data ~ which

# gtkColorButton(color)
# gtkFontButton

#gdkPixbufGetFromDrawable(dest = NULL, src, cmap = NULL, src.x, src.y, dest.x, dest.y, width, height)
# TODO: xlim / ylim list if scales$relation %in% c("free", "sliced")

latticeNames <- c("barchart", "bwplot", "cloud", "contourplot", "densityplot", 
	"dotplot", "histogram", "levelplot", "parallel", "qq", "qqmath", "rfs", 
	"splom", "stripplot", "tmd", "wireframe", "xyplot",
	# packages sp and latticeExtra
	"spplot", "bubble", "mapplot", "gplot", "ecdfplot", "rootogram")

playwith <- function(expr, name="plot", nav.scales=c("x","y"), trans.scales=c("y"), 
	buttons=list("annotate", "identify", "zoom", "zoomfit"), 
	extra.buttons=list("zero"), basic.buttons=plotAndPlayBasicButtons, 
	labels=NULL, label.args=list(cex=1), identify.call=NULL, plot.call, 
	is.lattice=NA, eval.args=NA, invert.match=F, envir=parent.frame(), 
	restore.on.close=NULL) {
	
	if (missing(plot.call) == missing(expr)) stop("give only one of 'expr' and 'plot.call'")
	if (missing(plot.call)) plot.call <- substitute(expr)
	if (is.expression(plot.call)) {
		plot.call <- if (length(plot.call) > 1)
			as.call(c(as.symbol("{"), plot.call)) else plot.call[[1]]
	}
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
	# work out evaluation rules
	env <- new.env()
	inherits <- !is.na(eval.args)
	if (is.na(eval.args)) eval.args <- (environmentName(envir) != "R_GlobalEnv")
	if (!identical(eval.args, FALSE)) {
		copyArgsIntoEnv(plot.call, envir=envir, newEnv=env, inherits=inherits, 
			pattern=eval.args, invert.match=invert.match)
	}
	# check whether the window already exists
	if (is.null(StateEnv[[name]]$win) 
	|| (inherits(StateEnv[[name]]$win, "<invalid>"))) {
		StateEnv[[name]] <- list()
		# create a new GTK window and set up cairoDevice for plotting
		myWin <- gtkWindow(show=FALSE)
		if (!inherits(myWin, "GtkWindow")) stop(paste(
			"Could not create the GTK window.",
			"Make sure you have recent versions of",
			"RGtk2 and the GTK+ libraries.",
			"See http://www.ggobi.org/rgtk2/"))
		myWin["default-width"] <- 640
		myWin["default-height"] <- 480
		myWin["title"] <- name
		gSignalConnect(myWin, "delete-event", 
			.plotAndPlay_close_event, data=list(name=name))
		myWin$show()
		myVBox <- gtkVBox()
		myWin$add(myVBox)
		myDA <- gtkDrawingArea()
		myVBox$packStart(myDA)
		asCairoDevice(myDA)
		trellis.device(new=F)
		# create the toolbar
		theToolbar <- gtkToolbar()
		theToolbar["toolbar-style"] <- GtkToolbarStyle['both']
		myVBox$packStart(theToolbar, expand=FALSE)
		# add the call bar
		callHBox <- gtkHBox()
		callEntry <- gtkEntry()
		callEntry['has-frame'] <- FALSE
		callEditButton <- gtkButton(label="Edit call...")
		gSignalConnect(callEntry, "editing-done", .plotAndPlay_edit_in_place_event)
		gSignalConnect(callEntry, "activate", .plotAndPlay_edit_in_place_event)
		gSignalConnect(callEditButton, "clicked", .plotAndPlay_edit_event)
		#tooltip="Edit the plot call as text"
		callHBox$packStart(callEntry)
		callHBox$packStart(callEditButton, expand=FALSE, pad=5)
		myVBox$packStart(callHBox, expand=FALSE)
		# set StateEnv$.current to 'name' whenever this window comes to front
		gSignalConnect(myWin, "focus-in-event", 
			.plotAndPlay_window_focus_in_event, data=list(name=name))
		gSignalConnect(myWin, "focus-out-event", 
			.plotAndPlay_window_focus_out_event, data=list(name=name))
	} else {
		# window exists
		myWin <- StateEnv[[name]]$win
		# switch to the device
		dev.set(StateEnv[[name]]$dev)
	}
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
		annotations=list(),
		is.lattice=is.lattice,
		focus=list(col=0, row=0),
		page=1,
		restore.on.close=restore.on.close,
		.args=list(
			buttons=buttons, 
			extra.buttons=extra.buttons,
			basic.buttons=basic.buttons,
			missing_buttons=missing(buttons),
			missing_extra.buttons=missing(extra.buttons),
			identify.call=identify.call,
			is.lattice=is.lattice,
			labels=labels
		)
	)
	StateEnv$.current <- name
	# set up toolbar buttons and work out label for identification
	plotAndPlayInit()
	# bring window to front
	myWin$present()
	# do the plot
	invisible(plotAndPlayUpdate())
}

plotAndPlayInit <- function() {
	name <- StateEnv$.current
	plot.call <- StateEnv[[name]]$call
	env <- StateEnv[[name]]$env
	# clear the current plot if any, to avoid redraws
	plot.new()
	# clear toolbar
	theToolbar <- plotAndPlayGetToolbar()
	for (x in rev(theToolbar$getChildren())) x$destroy()
	# get access to some of the original arguments
	argfoo <- StateEnv[[name]]$.args
	attach(argfoo)
	on.exit(detach(argfoo))
	labels <- argfoo$labels # otherwise can be masked by base package?
	buttons <- argfoo$buttons
	# put call into canonical form
	callFun <- eval(plot.call[[1]], env)
	callName <- paste(deparse(plot.call[[1]]), collapse="")
	if (is.na(is.lattice)) is.lattice <- (callName %in% latticeNames)
	# check whether the call accepts arguments
	noArgs <- F
	if ((typeof(callFun) == "closure") && !is.null(formals(callFun))) {
		plot.call <- match.call(callFun, plot.call)
		if (is.call(plot.call[[2]]) && plot.call[[2]][[1]] == as.symbol("~")) {
			names(plot.call)[2] <- ""
		}
		if (is.lattice) names(plot.call)[2] <- ""
	} else {
		noArgs <- T
	}
	if (is.lattice &&
		!(callName %in% c("splom", "cloud", "levelplot",
			"contourplot", "wireframe", "parallel")) ) {
		# need this for correctly identifying points
		plot.call$subscripts <- quote(T)
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
				tmp.data <- NULL
				if ('data' %in% names(plot.call)) {
					tmp.data <- eval(plot.call$data, env)
					labels <- row.names(tmp.data)
				}
				if (is.null(labels)) {
					# try to get labels from formula
					tmp.x <- eval(plot.call[[2]], env)
					if (inherits(tmp.x, "formula")) {
						xObj <- if (length(tmp.x) == 2)
							tmp.x[[2]] else tmp.x[[3]]
						while (is.call(xObj) && as.character(xObj[[1]]) %in% 
							c("|", "*", "+"))
							xObj <- xObj[[2]]
						xObj <- eval(xObj, tmp.data, environment(tmp.x))
						labels <- makeLabels(xObj)
						
					} else {
						labels <- makeLabels(tmp.x)
					}
				}
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
					if (inherits(tmp.x, "formula")) {
						xObj <- if (length(tmp.x) == 2)
							tmp.x[[2]] else tmp.x[[3]]
						xObj <- eval(xObj, environment(tmp.x), env)
						labels <- makeLabels(xObj)
						
					} else {
						labels <- makeLabels(tmp.x)
					}
				}
			}
			if (!is.null(labels)) identify.call$labels <- labels
		}
	}
	# put identify call into canonical form
	identify.call <- match.call(eval(identify.call[[1]], env), identify.call)
	# set which buttons are visible
	if (noArgs) {
		if (missing_buttons) buttons <- list("annotate")
		if (missing_extra.buttons) extra.buttons <- NULL
	}
	if (is.lattice &&
		(callName %in% c("cloud", "wireframe")) ) {
		if (is.null(plot.call$zoom)) plot.call$zoom <- 1
		if (is.null(plot.call$screen)) plot.call$screen <- quote(list(z=40, x=-60))
		if (missing_buttons) buttons <- list("annotate",
			"zoomin.3d", "zoomout.3d", "fly.left.3d", "fly.right.3d")
		if (missing_extra.buttons) extra.buttons <- list()
	}
	if (is.lattice &&
		(callName %in% c("splom")) ) {
		if (missing_buttons) buttons <- list("annotate",
			"brush", "brush.region", "brush.drag", "clear")
		if (missing_extra.buttons) extra.buttons <- list()
	}
	if (is.lattice &&
		any(c("layers", "sp.layout", "panel") %in% names(plot.call))) {
		if (missing_buttons) buttons <- c(buttons, list("layers"))
	}
	buttons <- c(buttons, extra.buttons)
	if (is.lattice) {
		# these are always added to the toolbar as non-visible
		# they will be show()n only when relevant (see plotAndPlayUpdate)
		widget_expand <- eval(plotAndPlayButtons[["expand"]])
		widget_pages <- eval(plotAndPlayButtons[["pages"]])
		widget_expand$hide()
		widget_pages$hide()
		StateEnv[[name]]$widget_expand <- widget_expand
		StateEnv[[name]]$widget_pages <- widget_pages
		buttons <- c(buttons, widget_expand, widget_pages)
	}
	if (!is.lattice) basic.buttons$greyscale <- NULL
	if (length(buttons) > 0) buttons <- c(buttons, gtkSeparatorToolItem())
	buttons <- c(buttons, basic.buttons)
	# update state (need to do this before eval buttons - they can alter call)
	StateEnv[[name]]$call <- plot.call
	StateEnv[[name]]$id.call <- identify.call
	StateEnv[[name]]$is.lattice <- is.lattice
	# add buttons
	for (i in seq_along(buttons)) {
		tryResult <- try({
			newButton <- eval(buttons[[i]])
			if (is.character(newButton)) {
				newButton <- eval(plotAndPlayButtons[[newButton]])
			}
		})
		if (inherits(tryResult, "try-error")) next
		if (identical(newButton, NA)) next
		if (is.null(newButton)) {
			stop("Unrecognised (NULL) button at position ", i)
		}
		theToolbar$insert(newButton, -1)
	}
}

.plotAndPlay_window_focus_in_event <- function(widget, event, user.data) {
	name <- user.data$name
	plotAndPlaySetCurrID(name)
	return(FALSE)
}

.plotAndPlay_window_focus_out_event <- function(widget, event, user.data) {
	name <- user.data$name
	# revert to previous device
	#dev.set(StateEnv[[name]]$old.dev)
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
	if (length(others <- ls(StateEnv))) StateEnv$.current <- others[1]
}

plotAndPlayGetState <- function(item=NULL, name=plotAndPlayGetCurrID()) {
	stopifnot(name %in% ls(StateEnv))
	if (!is.null(item)) return(StateEnv[[name]][[item]])
	# otherwise return the whole list
	StateEnv[[name]]
}

plotAndPlaySetState <- function(..., name=plotAndPlayGetCurrID()) {
	stopifnot(name %in% ls(StateEnv))
	dots <- list(...)
	if (length(dots) != 1) stop("give one object.")
	state <- dots[[1]]
	item <- names(dots)
	if (is.null(item)) {
		stopifnot(is.list(state))
		stopifnot(c("win","dev","call","env") %in% names(state))
		StateEnv[[name]] <- state
	} else {
		StateEnv[[name]][[item]] <- state
	}
	invisible()
}

plotAndPlayGetCurrState <- plotAndPlayGetState
plotAndPlaySetCurrState <- plotAndPlaySetState

plotAndPlayGetCurrID <- function() {
	name <- StateEnv$.current
	if (!(name %in% ls(StateEnv))) stop("There is no active plot.")
	StateEnv$.current
}

plotAndPlaySetCurrID <- function(name) {
	if (!(name %in% ls(StateEnv)))
		stop(dQuote(name), " is not the one of the current plots: ",
			paste(dQuote(ls(StateEnv)), sep=", "))
	StateEnv$.current <- name
	# switch to this device
	StateEnv[[name]]$old.dev <- dev.cur() # unused
	dev.set(StateEnv[[name]]$dev)
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
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	StateEnv[[name]]$win$getWindow()$setCursor(gdkCursorNew("watch"))
	on.exit(StateEnv[[name]]$win$getWindow()$setCursor(NULL), add=T)
	# add current call to text box
	callTxt <- ""
	if (object.size(StateEnv[[name]]$call) < 50000) {
		callTxt <- deparseOneLine(StateEnv[[name]]$call, 
			control="showAttributes")
		if (name == "plot") StateEnv[[name]]$win["title"] <- 
			toString(callTxt, width=34)
	}
	plotAndPlayGetCallEntry()$setText(callTxt)
	if (isTRUE(StateEnv[[name]]$skip.updates)) return()
	# do the plot
	result <- eval(StateEnv[[name]]$call, StateEnv[[name]]$env)
	if (inherits(result, "trellis")) {
		# work out panels and pages
		nPanels <- prod(dim(result))
		panelsPerPage <- nPanels
		nPages <- 1
		if (!is.null(myLayout <- result$layout)) {
			panelsPerPage <- myLayout[1] * myLayout[2]
			if (myLayout[1] == 0) panelsPerPage <- myLayout[2]
			nPages <- ceiling(nPanels / panelsPerPage)
			result$layout[3] <- 1
		}
		if (nPages > 1) 
			StateEnv[[name]]$widget_pages$show() else 
			StateEnv[[name]]$widget_pages$hide()
		if (panelsPerPage > 1 || StateEnv[[name]]$widget_expand['active'])
			StateEnv[[name]]$widget_expand$show() else
			StateEnv[[name]]$widget_expand$hide()
		spinner <- StateEnv[[name]]$widget_pages$getChildren()[[1]]$getChildren()[[2]]
		if (StateEnv[[name]]$page > nPages) StateEnv[[name]]$page <- 1
		spinner['adjustment']['upper'] <- nPages
		gSignalHandlerBlock(spinner, StateEnv[[name]]$widget_pages_sig_id)
		spinner['value'] <- StateEnv[[name]]$page
		gSignalHandlerUnblock(spinner, StateEnv[[name]]$widget_pages_sig_id)
		# plot trellis object
		curPage <- StateEnv[[name]]$page
		plot(result, packet.panel=packet.panel.page(curPage))
		packets <- trellis.currentLayout(which="packet")
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
		# draw persistent labels
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
				ids <- which(subscripts %in% ids)
				# this would be faster, but can't assume sorted!
				#ids <- findInterval(ids, subscripts)
			}
			if (length(labels) > 0) {
				label.args <- StateEnv[[name]]$label.args
				do.call(panel.text, c(list(xy$x[ids], xy$y[ids], 
					labels=labels[ids], pos=1), label.args))
			}
			trellis.unfocus()
		}
		# draw annotations
		for (myPacket in names(StateEnv[[name]]$annotations)) {
			if (myPacket == "all") {
				trellis.focus("toplevel", highlight=F)
			} else {
				whichOne <- which(packets == as.numeric(myPacket))
				if (length(whichOne) == 0) next
				myCol <- col(packets)[whichOne]
				myRow <- row(packets)[whichOne]
				trellis.focus("panel", myCol, myRow, highlight=F)
			}
			for (expr in StateEnv[[name]]$annotations[[myPacket]]) {
				eval(expr, StateEnv[[name]]$env)
			}
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
		# draw annotations
		for (expr in StateEnv[[name]]$annotations$all) {
			eval(expr, StateEnv[[name]]$env)
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

plotAndPlayGetCallEntry <- function() {
	name <- StateEnv$.current
	StateEnv[[name]]$win$getChildren()[[1]]$getChildren()[[3]]$getChildren()[[1]]
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

plotAndPlaySetRawXLim <- function(x) {
	plotAndPlaySetRawLim(x, "x")
}

plotAndPlaySetRawYLim <- function(x) {
	plotAndPlaySetRawLim(x, "y")
}

plotAndPlaySetRawLim <- function(x, x.or.y=c("x", "y")) {
	name <- StateEnv$.current
	x.or.y <- match.arg(x.or.y)
	# convert back from log scale if required
	x <- unlogXY(x, StateEnv[[name]]$call, StateEnv[[name]]$is.lattice, x.or.y=x.or.y)
	if (StateEnv[[name]]$is.lattice) {
		x.panel <- trellis.panelArgs()[[x.or.y]]
		# convert new scale to appropriate date time class if required
		if (inherits(x.panel, "Date") || inherits(x.panel, "POSIXt")) {
			mostattributes(x) <- attributes(x.panel)
		}
		# TODO: class "dates", "times"? do they come through to panelArgs?
		
		# convert new scale to factor levels if required
		if (is.factor(x.panel)) {
			#offset <- 0
			#limTerm <- the.call[[paste(x.or.y, "lim", sep="")]]
			#if (is.character(sublevels <- try(eval(limTerm)))) {
			#	offset <- 1 - match(sublevels[1], levels(x.panel))
			#}
			#newlevels.i <- pmax(0, -offset + seq(ceiling(min(x)), max(x)))
			#x <- levels(x.panel)[newlevels.i]
			if (is.null(StateEnv[[name]]$call$scales[[x.or.y]]$labels)) {
				StateEnv[[name]]$call$scales[[x.or.y]]$labels <- levels(x.panel)
				StateEnv[[name]]$call$scales[[x.or.y]]$at <- 1:nlevels(x.panel)
			}
		}
	}
	if ("numeric" %in% class(x)) x <- signif(x, 4)
	if (x.or.y == "x") StateEnv[[name]]$call$xlim <- x
	if (x.or.y == "y") StateEnv[[name]]$call$ylim <- x
}

untransformXlim <- function(x, the.call, is.lattice=T) {
	untransformXYlim(x, the.call, is.lattice, x.or.y="x")
}

untransformYlim <- function(x, the.call, is.lattice=T) {
	untransformXYlim(x, the.call, is.lattice, x.or.y="y")
}

unlogXY <- function(x, the.call, is.lattice=T, x.or.y=c("x", "y")) {
	x.or.y <- match.arg(x.or.y)
	scalesArg <- the.call$scales
	if (is.lattice) {
		if (!is.null(scalesArg[[x.or.y]]$log)) {
			logBase <- latticeLogBase(scalesArg[[x.or.y]]$log)
			if (!is.null(logBase)) x <- logBase ^ x
		} else {
			logBase <- latticeLogBase(scalesArg$log)
			if (!is.null(logBase)) x <- logBase ^ x
		}
	} else {
		# traditional graphics plot
		if (par(paste(x.or.y, "log", sep=""))) x <- 10 ^ x
	}
	x
}

unlogX <- function(x, the.call, is.lattice=T) {
	unlogXY(x, the.call, is.lattice, x.or.y="x")
}

unlogY <- function(x, the.call, is.lattice=T) {
	unlogXY(x, the.call, is.lattice, x.or.y="y")
}

latticeLogBase <- function(x) {
	x <- eval(x)
	if (is.null(x) || identical(x, FALSE)) return(NULL)
	if (isTRUE(x)) return(10)
	if (identical(x, "e")) return(exp(1))
	x
}

placeLabelDialog <- function(text="", title="New label", prompt="", width.chars=-1) {
	editBox <- gtkDialog(title=title, NULL, NULL,
		"OK", GtkResponseType["ok"], "Cancel", GtkResponseType["cancel"],
		show = F)
	editBox$setDefaultResponse(GtkResponseType["ok"])
	if (nchar(prompt) > 0) {
		editBox[["vbox"]]$packStart(gtkLabel(prompt), expand=F, pad=2)
	}
	editEntry <- gtkEntry()
	editEntry['activates-default'] <- T
	editEntry['text'] <- text
	editEntry['width-chars'] <- width.chars
	editBox[["vbox"]]$packStart(editEntry, pad=10)
	alignHBox <- gtkHBox()
	alignHBox$packStart(gtkLabel("Position relative to point: "))
	alignTable <- gtkTable(rows=3, columns=3)
	alignRadios <- list(list(),list(),list())
	myGroup <- NULL
	for (col in 1:3) for (row in 1:3) {
		thisRadio <- gtkRadioButtonNewFromWidget(group=myGroup)
		if (is.null(myGroup)) myGroup <- thisRadio
		alignRadios[[col]][[row]] <- thisRadio
		alignTable$attachDefaults(thisRadio,
			left=col-1, right=col, top=row-1, bot=row) # xpadding ypadding 
	}
	alignRadios[[2]][[2]]['active'] <- T
	alignHBox$packStart(alignTable)
	editBox[["vbox"]]$packStart(alignHBox)
	editBox$showAll()
	result <- editBox$run() # make it modal
	newTxt <- editEntry['text']
	newAlign <- c(0,0)
	for (col in 1:3) for (row in 1:3) {
		if (alignRadios[[col]][[row]]['active'])
			newAlign <- c( (3-col)/2, (row-1)/2 )
	}
	editBox$destroy()
	if (result != GtkResponseType["ok"]) return(invisible(NULL))
	list(text=newTxt, align=newAlign)
}

makeLabels <- function(x) {
	labels <- row.names(x)
	if (inherits(x, "POSIXt"))
		labels <- format(x)
	if (inherits(x, "Date"))
		labels <- format(x)
	if (inherits(x, "ts") || inherits(x, "zoo"))
		labels <- rep(format(stats::time(x)), NCOL(x))
	labels
}

xy.coords.call <- function(the.call, envir=parent.frame(), log=NULL, recycle=TRUE) {
	stopifnot(is.call(the.call))
	# put call into canonical form
	the.call <- match.call(eval(the.call[[1]], envir=envir), the.call)
	tmp.x <- eval(the.call$x, envir)
	tmp.y <- if ('y' %in% names(the.call)) eval(the.call$y, envir)
	if (inherits(tmp.x, "zoo") && is.null(tmp.y)) 
		return(xy.coords(stats::time(tmp.x), as.vector(tmp.x), log=log, recycle=recycle))
	xy.coords(tmp.x, tmp.y, log=log, recycle=recycle)
}

substitute.call <- function(the.call, ...) 
	do.call(substitute, list(the.call, ...))

copyArgsIntoEnv <- function(the.call, envir=parent.frame(), newEnv, inherits=F, pattern=T, invert.match=F) {
	stopifnot(is.call(the.call) || is.list(the.call) || is.expression(the.call))
	isMatch <- !invert.match
	for (i in seq_along(the.call)) {
		if (is.call(the.call) && (i == 1)) next
		this.arg <- the.call[[i]]
		# skip literal symbol in "$" extractor
		if (is.call(this.arg) && this.arg[[1]] == as.symbol("$"))
			this.arg <- this.arg[[2]]
		
		if (mode(this.arg) %in% c("call", "(", "list", "expression")) {
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
		# leave constants alone
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

recursive.as.list.call <- function(x) {
	stopifnot(is.call(x))
	x <- as.list(x)
	lapply(x, function(z) if (is.call(z))
		recursive.as.list.call(z) else z)
}

toIndexStr <- function(x) paste('[[', x ,']]', sep='', collapse='')

deparseOneLine <- function(expr, width.cutoff=500, ...) {
	tmp <- deparse(expr, width.cutoff=width.cutoff, ...)
	indents <- attr(regexpr("^ *", tmp), "match.length")
	breaks <- c(diff(indents) <= 0, FALSE)
	tmp <- gsub("^ +", "", tmp)
	tmp <- gsub(" +$", "", tmp)
	breaks[c(tmp[-1]=="{", FALSE)] <- F
	tmp <- paste(tmp, ifelse(breaks, ";", ""), sep="", collapse=" ")
	tmp <- gsub("\\{;", "\\{", tmp)
	tmp <- gsub(";\\}", " \\}", tmp)
	tmp <- gsub(";\\{", " \\{", tmp)
	tmp
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

playwith.trellis <- 
   function(x, position = NULL, split = NULL, more = FALSE, newpage = TRUE,
            packet.panel = packet.panel.default, draw.in = NULL, ...)
{
   dev.interactive2 <- function(orNone)
   {
       dev.interactive(orNone) ||
       (interactive() && .Device == "null device" &&
        getOption("device") == "Cairo")
   }
   #dots <- list(...)
   #new <- (any(dots$new) && is.null(dots$draw) &&
   new <- (newpage && is.null(draw.in) &&
           !lattice:::lattice.getStatus("print.more"))
   if (dev.interactive2(TRUE) && new) {
       ## starting a new plot on an interactive device
       eval.parent(call("playwith", x$call), n=2)
       return(invisible())
   }
   ## call `plot.trellis` from lattice package, as usual
   ocall <- sys.call()
   ocall[[1]] <- quote(plot)
   eval.parent(ocall)
}

setAutoPlaywith <- function(on=TRUE)
{
   library("lattice") # requires lattice >= 0.17-1
   lattice.options(print.function = if (on) playwith.trellis else NULL)
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


