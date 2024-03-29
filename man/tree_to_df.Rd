% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataFrame_Functions.R
\name{tree_to_df}
\alias{tree_to_df}
\title{Tree to Data.Frame}
\usage{
tree_to_df(node, ..., attrs = NULL, attributes_only = TRUE)
}
\arguments{
\item{node}{a data.tree Node object}

\item{attrs}{character, optional. Attributes to be included in the output.
If not provided, will default to include all attributes present in the data.tree.}

\item{attributes_only}{logical, optional. If \code{TRUE}, the default, only
attribute columns and pathString are returned in the data.frame. If
\code{FALSE} columns added by the function \code{ToDataFrameTree} are also
returned. These include \code{name}, \code{levelName}, and \code{level},
which can be useful to have if you intend to rank nodes by level based on
some attribute (or function of attributes).}
}
\value{
data.frame
}
\description{
Function to convert data.tree object back to a data.frame, with all attrs.
By using ToDataFrameTable, you would get only the leaf-level information.
ToDataFrameTree gives aggregate values calculated at the different levels of
the hierarchy as well, which is needed to compute values for metrics at each
of the levels.
}
