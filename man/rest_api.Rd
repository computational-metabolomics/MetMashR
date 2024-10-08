% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rest_api_class.R
\name{rest_api}
\alias{rest_api}
\title{rest_api}
\usage{
rest_api(
  base_url,
  url_template,
  suffix,
  status_codes,
  delay,
  cache = NULL,
  query_column,
  ...
)
}
\arguments{
\item{base_url}{(character) The base URL of the API.}

\item{url_template}{(character) A template describing how the URL
should be constructed from the base URL and input parameters. e.g.
<base_url>/\if{html}{\out{<context>}}/<input_item>/<search_term>/json.The url will be
constructed by replacing the values enclosed in <> with the value
from corresponding input parameter of the rest_api object.}

\item{suffix}{(character) A suffix appended to all column names in
the returned result.}

\item{status_codes}{(list) Named list of status codes and function
indicating how to respond. Should minimally contain a function to
parse a successful response for status code 200. Any codes not
provided will be passed to httr::stop_for_status().}

\item{delay}{(numeric, integer) Delay in seconds between API calls.}

\item{cache}{(annotation_database, NULL) A struct cache object that
contains parsed responses to previous api queries. If not using a
cache then set to NULL. The default is \code{NULL}.}

\item{query_column}{(character) The name of a column in the
annotation table containing values to search in the api call.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A \code{rest_api} object with the following \code{output}
slots:
\tabular{ll}{
\code{updated} \tab (annotation_source) The annotation_source after
adding data returned by the API. \cr
}
}
\description{
A base class providing common methods for making REST
API calls.
}
\section{Inheritance}{

A \code{rest_api} object inherits the following \code{struct} classes: \cr\cr
\verb{[rest_api]} -> \verb{[model]} -> \verb{[struct_class]}
}

\examples{
M = rest_api( base_url = "V1", url_template = character(0),
query_column = character(0), cache = NULL, status_codes = list(),
delay = 0.5, suffix = "_rest_api")

}
\seealso{
Other {REST API's}: 
\code{\link{classyfire_lookup}},
\code{\link{kegg_lookup}},
\code{\link{lipidmaps_lookup}},
\code{\link{mwb_compound_lookup}}
}
\concept{{REST API's}}
