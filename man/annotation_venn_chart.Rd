% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotation_venn_chart.R
\name{annotation_venn_chart}
\alias{annotation_venn_chart}
\title{Annotation venn chart}
\usage{
annotation_venn_chart(
  factor_name,
  group_column = NULL,
  fill_colour = "white",
  line_colour = "black",
  labels = TRUE,
  legend = FALSE,
  ...
)
}
\arguments{
\item{factor_name}{(character) The name of the column(s) in the
\code{annotation_source} to generate a chart from. Up to seven columns can
be compared for a single \code{annotation_source}.}

\item{group_column}{(character, NULL) The name of the column in the
\code{annotation_source} to create groups from in the Venn diagram. This
parameter is ignored if there are multiple input tables, as each
table is considered to be a group. This parameter is also ignored if
more than one \code{factor_name} is provided, as each column is considered
a group. The default is \code{NULL}.}

\item{fill_colour}{(character) The line colour of the groups in a
format compatible with ggplot e.g. "black" or "#000000". Special case
".group" sets the colour based on the group label and "none" will not
fill the groups. The default is \code{"white"}.}

\item{line_colour}{(character) The line colour of the groups in a
format compatible with ggplot e.g. "black" or "#000000". Special case
".group" sets the colour based on the group label, and ".none" will
not display lines. The default is \code{"black"}.}

\item{labels}{(logical) Group labels. Allowed values are limited to
the following: \itemize{ \item{\code{"TRUE"}: Include group labels on
the plot.}\item{\code{"FALSE"}: Do not inlude group labels on the
plot.}} The default is \code{TRUE}.\cr}

\item{legend}{(logical) Legend. Allowed values are limited to the
following: \itemize{ \item{\code{"TRUE"}: Include a legend in the
plot.}\item{\code{"FALSE"}: Do not inlude a legend in the plot.}} The
default is \code{FALSE}.\cr}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A \code{
annotation_venn_chart
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to
plot this chart object.
}
\description{
Display a venn diagram of labels present in two
annotation_sources.
}
\details{
This object makes use of functionality from the following
packages:\itemize{ \item{\code{RVenn}} \item{\code{ggVennDiagram}}}
}
\section{Inheritance}{

A \code{annotation_venn_chart} object inherits the following \code{struct}
classes: \cr\cr
\verb{[annotation_venn_chart]} -> \verb{[chart]} -> \verb{[struct_class]}
}

\examples{
M = annotation_venn_chart( factor_name = "V1", line_colour =
".group", fill_colour = ".group", labels = FALSE, legend = FALSE,
group_column = NULL)

}
\references{
Akyol T (2019). \emph{RVenn: Set Operations for Many Sets}. R
package version 1.1.0, \url{https://CRAN.R-project.org/package=RVenn}.

Gao C, Dusa A (2024). \emph{ggVennDiagram: A 'ggplot2'
Implement of Venn Diagram}. R package version 1.5.2,
\url{https://CRAN.R-project.org/package=ggVennDiagram}.
}
