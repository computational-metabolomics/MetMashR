% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mwb_refmet_database_class.R
\name{mwb_refmet_database}
\alias{mwb_refmet_database}
\title{mwb_refmet_database}
\usage{
mwb_refmet_database(bfc = NULL, ...)
}
\arguments{
\item{bfc}{(character) \code{BiocFileCache} is used to cache database
locally and prevent unnecessary downloads. If a path is provided then
\code{BiocFileCache} will use this location. If NULL it will use the
default location (see \link[BiocFileCache:BiocFileCache-class]{BiocFileCache::BiocFileCache} for details).
The default is \code{NULL}.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A \code{
mwb_refmet_database
} object. This object has no \code{output} slots.
}
\description{
Imports the Metabolomics Workbench refmet database.
}
\details{
This object makes use of functionality from the following
packages:\itemize{ \item{\code{BiocFileCache}} \item{\code{httr}}
\item{\code{plyr}}}
}
\section{Inheritance}{

A \code{mwb_refmet_database} object inherits the following \code{struct}
classes: \cr\cr
\verb{[mwb_refmet_database]} -> \verb{[annotation_database]} ->
\verb{[annotation_source]} -> \verb{[struct_class]}
}

\examples{
M = mwb_refmet_database( bfc = character(0), tag = character(0), data
= data.frame(), source = "ANY")

}
\references{
Shepherd L, Morgan M (2024). \emph{BiocFileCache: Manage Files
Across Sessions}. R package version 2.10.2.

Wickham H (2023). \emph{httr: Tools for Working with URLs and
HTTP}. R package version 1.4.7,
\url{https://CRAN.R-project.org/package=httr}.

Wickham H (2011). "The Split-Apply-Combine Strategy for
Data Analysis." \emph{Journal of Statistical Software}, \emph{40}(1), 1-29.
\url{https://www.jstatsoft.org/v40/i01/}.
}
