% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mwb_compound_lookup_class.R
\name{mwb_compound_lookup}
\alias{mwb_compound_lookup}
\title{Convert to/from kegg identifiers}
\usage{
mwb_compound_lookup(
  input_item = "inchi_key",
  query_column,
  output_item = "pubchem_id",
  suffix = "_mwb",
  ...
)
}
\arguments{
\item{input_item}{(character) A valid input item for the compound
context (see
https://www.metabolomicsworkbench.org/tools/mw_rest.php). The values
in the query_column should be of this type. The default is
\code{"inchi_key"}.}

\item{query_column}{(character) The name of a column in the
annotation table containing values to search in the api call.}

\item{output_item}{(character) A comma separated list of Valid output
items for the compound context (see
https://www.metabolomicsworkbench.org/tools/mw_rest.php). The default
is \code{"pubchem_id"}.}

\item{suffix}{(character) A suffix appended to all column names in
the returned result. The default is \code{"_mwb"}.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A \code{mwb_compound_lookup} object with the following
\code{output} slots:
\tabular{ll}{
\code{updated} \tab (annotation_source) The annotation_source after
adding data returned by the API. \cr
}
}
\description{
Searches MetabolomicsWorkbench for compound identifiers.
}
\details{
This object makes use of functionality from the following
packages:\itemize{ \item{\code{metabolomicsWorkbenchR}}
\item{\code{dplyr}}}
}
\section{Inheritance}{

A \code{mwb_compound_lookup} object inherits the following \code{struct}
classes: \cr\cr
\verb{[mwb_compound_lookup]} -> \verb{[rest_api]} -> \verb{[model]} ->
\verb{[struct_class]}
}

\examples{
M = mwb_compound_lookup( input_item = "inchi_key", output_item =
"inchi_key", base_url = "https://www.metabolomicsworkbench.org/rest",
url_template =
"<base_url>/compound/<input_item>/<query_column>/<output_item>",
query_column = character(0), cache = NULL, status_codes = list(),
delay = 0.5, suffix = "_rest_api")

}
\references{
Lloyd GR, Weber RJM (????). \emph{metabolomicsWorkbenchR:
Metabolomics Workbench in R}. R package version 1.14.1.

Wickham H, François R, Henry L, Müller K, Vaughan D
(2023). \emph{dplyr: A Grammar of Data Manipulation}. R package version
1.1.4, \url{https://CRAN.R-project.org/package=dplyr}.
}
\seealso{
Other {REST API's}: 
\code{\link{classyfire_lookup}},
\code{\link{kegg_lookup}},
\code{\link{lipidmaps_lookup}},
\code{\link{rest_api}}
}
\concept{{REST API's}}
