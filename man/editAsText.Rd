\name{editAsText}
\alias{editAsText}
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
}
\arguments{
  \item{x}{ a \code{\link{data.frame}}. }
  \item{title}{ the window title. For \code{editAsText} the default is based on \code{x}. }
  \item{edit.row.names}{ whether to include row names as an editable column. }
}
\details{
  If the number of rows changes a warning is issued.
  
  New factor levels can not be added. Factor levels not in the original set will be NA.
}
\value{
	\code{editAsText} returns a data frame, which should have the same column types as \code{x}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{edit.data.frame}}, \code{\link{edit}}, \code{\link{read.delim}} }
\examples{
\dontrun{

str(iris)
str(editAsText(iris))

}
}

\keyword{ utilities }
\keyword{ manip }
