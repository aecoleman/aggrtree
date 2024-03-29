% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Isolate_Aggr.R
\name{Make_Aggd_Df}
\alias{Make_Aggd_Df}
\title{Make Aggregated Dataframe}
\usage{
Make_Aggd_Df(aggd_tree, isolate_path, over, isolate)
}
\arguments{
\item{aggd_tree}{data.tree, most often the result returned by \code{Aggr_Attrs()}.}

\item{isolate_path}{character, value of isolate that will be isolated}

\item{over}{symbol, a pathString field which will be aggregated according to
hierarchy}

\item{isolate}{symbol, a pathString (or other) field which will be held
constant}
}
\value{
data.frame
}
\description{
Make Aggregated Dataframe
}
