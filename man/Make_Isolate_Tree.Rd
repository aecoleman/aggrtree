% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Isolate_Aggr.R
\name{Make_Isolate_Tree}
\alias{Make_Isolate_Tree}
\title{Make Isolate Tree}
\usage{
Make_Isolate_Tree(df, isolate_path, isolate, over, ...)
}
\arguments{
\item{df}{data.frame}

\item{isolate_path}{character, value of isolate that will be isolated}

\item{isolate}{symbol, a pathString (or other) field which will be held
constant}

\item{over}{symbol, a pathString field which will be aggregated according to
hierarchy}

\item{...}{symbols, columns to be retained as attributes in the resulting
data.tree. If missing, all other columns will be retained as attributes.}
}
\value{
data.tree
}
\description{
Make Isolate Tree
}
