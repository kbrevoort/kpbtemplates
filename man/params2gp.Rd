% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assemble-graphs.R
\name{params2gp}
\alias{params2gp}
\title{Params to Graphical Parameters}
\usage{
params2gp(p, bs)
}
\arguments{
\item{p}{Parameter vector from a ggplot object, as specified in the plot's
theme.}

\item{bs}{The base size of the fonts in the ggplot plot. This is necessary
for cases where the font size in the parameter vector is expressed as a
relative size.}
}
\description{
Converts the elements of a elemnt_text specification from ggplot2 into the
graphical parameter vector that is needed for grid graphics.
}
