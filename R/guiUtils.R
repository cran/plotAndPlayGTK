## plotAndPlayGTK: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org> and others
## GPL version 2 or newer
##
## Some of these functions are based on ones in Rattle v2.1
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2
## Graham.Williams@togaware.com

# generally useful RGtk2 GUI things

guiDo <- function(expr, call, string, doLog=T, doFailureLog=doLog, logFunction=addToLog, doFailureDialog=T, doStop=T, envir=if (doLog) .GlobalEnv else parent.frame()) {
	if (missing(expr) + missing(call) + missing(string) != 2) {
		stop("Give one of 'expr', 'call' or 'string'")
	}
	if (missing(string) && missing(call)) {
		call <- substitute(expr)
	}
	isString <- !missing(string)
	# set default log function in case 'addToLog' is not defined
	if (doLog || doFailureLog) {
		if (inherits(try(eval(logFunction), silent=T), "try-error")
		|| !is.function(eval(logFunction))) {
			logFunction <- print
		}
	}
	# log it
	if (doLog) {
		theCall <- if (isString) {
			try(parse(text=string)[[1]], silent=T)
		} else {
			call
		}
		if (isString && inherits(theCall, "try-error")) {
			# syntax error
			logFunction(string)
		} else {
			callPretty <- paste(capture.output(
				# if the code is in a simple block, omit braces
				if (identical(theCall[[1]], as.symbol("{"))) {
					for (i in 2:length(theCall)) {
						print(theCall[[i]])
					}
				} else {
					print(theCall)
				}
			), collapse="\n")
			logFunction(callPretty)
		}
	}
	# set up error handler
	handleIt <- function(e) {
		# show error dialog
		if (doFailureDialog) {
			commandText <- if (isString) { string } else { deparse(call) }
			msgText <- conditionMessage(e)
			callText <- deparse(conditionCall(e), width.cutoff=500)[1]
			if (length(msgText)==0) { msgText <- "" }
			if (length(callText)==0) { callText <- "" }
			errorDialog(paste(sep='',
				'A command has failed. The error was:',
				'\n\n<span foreground="#aa0000">', 
					pangoEscape(msgText),
				'</span>\n\n',
				'The error occurred in: \n\n<tt>',
					pangoEscape(callText), 
				'</tt>\n\n', 
				'The original command was: \n\n<tt>',
					pangoEscape(commandText), 
				'</tt>\n\n',
				'If this is not your fault, you might want to select ',
				'this text and copy it into a bug report. Please also ',
				'include the output from <tt>sessionInfo()</tt>'),
				isMarkup=T)
		}
		if (doFailureLog) {
			logFunction("# FAILED")
		}
		# propagate the error
		if (doStop) {
			stop(e)
		}
		return(e)
	}
	# evaluate it
	if (isString) {
		result <- tryCatch(eval(parse(text=string), envir=envir), 
			error=handleIt)
	} else {
		result <- tryCatch(eval(call, envir=envir), 
			error=handleIt)
	}
	return(result)
}

errorDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="error", ..., isMarkup=isMarkup)
}

infoDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="info", ..., isMarkup=isMarkup)
}

questionDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="question", ..., isMarkup=isMarkup)
}

guiMessageDialog <- function(type="info", ..., isMarkup=F) {
	myString <- paste(sep='', ...)
	myButtons <- switch(type,
		error="close",
		info="ok",
		question="yes-no"
	)
	dialog <- gtkMessageDialogNew(NULL, NULL, type, myButtons, myString)
	if (isMarkup) {
		dialog$setMarkup(myString)
	}
	result <- dialog$run() # make it modal
	dialog$destroy()
	if (result == GtkResponseType["yes"]) {
		return("yes")
	} else {
		return(invisible(NULL))
	}
}

pangoEscape <- function(x) {
	x <- gsub('%', '%%', x)
	x <- gsub('&', '&amp;', x)
	x <- gsub('<', '&lt;', x)
	x <- gsub('>', '&gt;', x)
	#x <- gsub('&&', '&amp;&amp;', x)
	#x <- gsub('& ', '&amp; ', x)
	#x <- gsub('<<', '&lt;&lt;', x)
	#x <- gsub('<-', '&lt;-', x)
	#x <- gsub('< ', '&lt; ', x)
	x
}

guiTextInput <- function(text="", title="Text Input", prompt="") {
	# construct dialog
	editBox <- gtkDialog(title=title, NULL, NULL,
		"OK", GtkResponseType["ok"], "Cancel", GtkResponseType["cancel"],
		show = F)
	editBox$setDefaultResponse(GtkResponseType["ok"])
	editBox$setDefaultSize(400,300)
	if (nchar(prompt) > 0) {
		editBox[["vbox"]]$packStart(gtkLabel(prompt), expand=F, pad=2)
	}
	editTV <- gtkTextView()
	setTextviewMonospace(editTV)
	setTextview(editTV, text)
	editBox[["vbox"]]$add(editTV)
	result <- editBox$run() # make it modal
	newTxt <- getTextviewText(editTV)
	editBox$destroy()
	if (result != GtkResponseType["ok"]) { return(invisible(NULL)) }
	newTxt
}

## EDIT DATA FRAMES AS TEXT

editAsText <- function(x, title=NULL, edit.row.names=any(row.names(x) != 1:nrow(x))) {
	if (!is.data.frame(x)) { stop("'x' must be a data frame") }
	if (is.null(title)) {
		title <- paste("Editing", deparse(substitute(x)))
	}
	# make table text block from data frame 'x'
	foo <- capture.output(
		write.table(x, sep="\t", quote=F, row.names=edit.row.names,
		col.names=if (edit.row.names) {NA} else {T})
	)
	tableTxt <- paste(paste(foo, collapse="\n"), "\n", sep='')
	if (edit.row.names) { tableTxt <- paste("row.names", tableTxt, sep='') }
	# show text box and repeat if there was an error
	readOK <- F
	while (!readOK) {
		newTableTxt <- guiTextInput(text=tableTxt, title=title, 
			prompt=paste("Copy and paste to/from a spreadsheet,",
				"or edit the text here (in tab-separated format).\n",
				"Do not move the columns around,",
				"they must stay in this order."))
		if (is.null(newTableTxt)) { return(x) }
		# convert table text block back to data frame
		zz <- textConnection(newTableTxt)
		newData <- tryCatch(
			read.delim(file=zz, header=T, colClasses=sapply(x, class),
			row.names=if (edit.row.names) {1} else {NULL}),
			error=function(e)e)
		close(zz)
		# check whether there was an error in reading the table
		if (inherits(newData, "error")) {
			errorDialog("Error reading table: ", conditionMessage(newData))
			tableTxt <- newTableTxt
		} else {
			readOK <- T
		}
	}
	# warn if the number of rows has changed
	if (nrow(newData) != nrow(x)) {
		warning("Number of rows changed from ",
			nrow(x), " to ", nrow(newData))
	} else if (!edit.row.names) {
		# keep original row names 
		row.names(newData) <- attr(x, "row.names")
	}
	# ensure factor levels are the same
	for (i in which(sapply(x, class) == "factor")) {
		newData[[i]] <- factor(newData[[i]], levels=levels(x[[i]]))
	}
	newData
}


## Textview widget support

setTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  tv$getBuffer()$setText(msg)
}

addTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  tv.buf <- tv$getBuffer()
  loc <- tv.buf$getEndIter()$iter
  tv.buf$insert(loc, msg)
}

getTextviewText <- function(tv)
{
  ## Extract text content of specified textview
  log.buf <- tv$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  return(log.buf$getText(start, end))
}

setTextviewMonospace <- function(tv)
{
  tv$modifyFont(pangoFontDescriptionFromString("monospace 10"))
}

Filters <- structure(c("R or S files (*.R,*.q,*.ssc,*.S)", "Postscript files (*.ps)", "PDF files (*.pdf)", "Png files (*.png)",  "Jpeg files (*.jpeg,*.jpg)",  "Text files (*.txt)", "R images (*.RData,*.rda)", "Zip files (*.zip)",  "All files (*.*)", "*.R;*.q;*.ssc;*.S", "*.ps", "*.pdf",  "*.png", "*.jpeg;*.jpg", "*.txt", "*.RData;*.rda", "*.zip",  "*.*"), .Dim = c(9L, 2L), .Dimnames = list(c("R", "ps",  "pdf", "png", "jpeg", "txt", "RData", "zip", "All"), NULL))

# returns character string, or NA if cancelled
choose.file.save <- function(default="", caption="Save File", filters=Filters[c("All"),], index=0) {
	dialog <- gtkFileChooserDialog(caption, NULL, "save",
		"gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
	dialog$setCurrentName(default)
	
	if (length(filters)==2) {
		filters <- matrix(filters, nrow=1, ncol=2)
	}
	
	for (i in seq(1, nrow(filters))) {
		ff <- gtkFileFilterNew()
		ff$setName(filters[i,1])
		for (x in strsplit(filters[i,2], ';')[[1]]) {
			ff$addPattern(x)
		}
		dialog$addFilter(ff)
		if (i == index) { dialog$setFilter(ff) }
	}
	
	#dialog$setDoOverwriteConfirmation(T) crap, appears behind filechooser
	if (dialog$run() == GtkResponseType["accept"]) {
		filename <- dialog$getFilename()
		if (file.exists(filename)) {
			if (is.null(questionDialog("Replace existing file?"))) {
				filename <- NA
			}
		}
		dialog$destroy()
		return(filename)
	} else {
		dialog$destroy()
		return(NA)
	}
}

get.extension <- function(path)
{
  ## Extract and return the extension part of a filename
  
  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""
  last
}

