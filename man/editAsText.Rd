\name{editAsText}
\alias{editAsText}
\alias{guiTextInput}
\title{Edit a data frame in spreadsheet-compatible format}
\description{
  This is analogous to \code{\link{edit.data.frame}}, but rather than a spreadsheet-like
  table, it writes the data in flat text format (tab-delimited) into a text box,
  and reads it back after editing.
  It is more difficult to read than a table, but it has the advantage of being able to 
  copy and paste to/from an external spreadsheet program.
}
\usage{
editAsText(x, title = NULL, edit.row.names = any(row.names(x) != 1:nrow(x)))

guiTextInput(text = "", title = "Text Input", prompt = "", accepts.tab = T, 
             wrap.mode = c("none", "char", "word", "word_char"))
}
\arguments{
  \item{x}{ a \code{\link{data.frame}}. }
  \item{title}{ the window title. For \code{editAsText} the default is based on \code{x}. }
  \item{edit.row.names}{ whether to include row names as an editable column. }
  \item{text}{ initial text to put in the \code{gtkTextView}. }
  \item{prompt}{ a prompt to display (as a \code{gtkLabel} above the text box. }
  \item{accepts.tab}{ whether a tab can be typed in the text box; otherwise it changes the focus. }
  \item{wrap.mode}{ type of text wrapping, from \code{GtkWrapMode}. }
}
\details{
  If the number of rows changes a warning is issued.
  
  New factor levels can not be added. Factor levels not in the original set will be NA.
}
\value{
	\code{editAsText} returns a data frame, which should have the same column types as \code{x}.
	
	\code{guiTextInput} returns the text from the text box.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{edit.data.frame}}, \code{\link{edit}}, \code{\link{read.delim}} }
\examples{
\dontrun{

str(iris)
str(editAsText(iris))

guiTextInput()

}
}

\keyword{ utilities }
\keyword{ manip }
