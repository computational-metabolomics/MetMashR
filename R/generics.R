##### generics

#' Import annotation source
#'
#' Import an data from e.g. a raw file and parse it into an
#' [`annotation_source()`] object.
#'
#' @param obj an [`annotation_source()`] object
#' @param ... not currently used
#' @return an [`annotation_table()`], [`annotation_library()`] or
#' [`annotation_database()`] object
#' @examples
#' # prepare source
#' CD <- cd_source(
#'     source = system.file(
#'         paste0("extdata/MTox/CD/HILIC_POS.xlsx"),
#'         package = "MetMashR"
#'     )
#' )
#'
setGeneric(
    "read_source",
    function(obj, ...) standardGeneric("read_source")
)

#' Check for columns in an `annotation_source`
#'
#' This method checks for the presence of columns by name in an
#' [`annotation_source()`]. It returns TRUE if all are present, or a vector
#' of messages indicating which columns are missing from the data.frame. It is
#' used by MetMashR to ensure validity of certain objects.
#'
#' @param obj an [`annotation_source()`] object
#' @param msg TRUE/FALSE indicates whether to return a message if some columns
#' are missing. If `msg = FALSE` then the function returns FALSE if all columns
#' are not present.
#' @param ... the column names to check for
#' @return logical if all columns are present, or a vector of messages if
#' requested.
#' @examples
#' # test if column present
#' AT <- annotation_source(data.frame(id = character(0)))
#' check_for_columns(AT, "id") # TRUE
#' check_for_columns(AT, "cake") # FALSE
#'
#' # return a message if missing
#' check_for_columns(AT, "cake", msg = TRUE)
#'
setGeneric(
    "check_for_columns",
    function(obj, ..., msg = FALSE) standardGeneric("check_for_columns")
)

#' Read a database
#'
#' Reads an annotation_database and returns the data.frame.
#' @param obj An `annotation_database` object
#' @return A data.frame
#' @examples
#' M <- rds_database(tempfile())
#' df <- read_database(M)
setGeneric(
    "read_database",
    function(obj, ...) standardGeneric("read_database")
)

#' Write to a database
#'
#' Writes a data.frame to a `annotation_database`.
#' @param obj A `annotation_database` object
#' @return Silently returns TRUE if successful, FALSE otherwise
#' @examples
#' M <- rds_database(tempfile())
#' write_database(M, data.frame())
setGeneric(
    "write_database",
    function(obj, ...) standardGeneric("write_database")
)

#' Is database writable
#'
#' A function that returns TRUE if the database has been designed for use
#' in read and write mode.
#'
#' @param obj A `annotation_database` object
#' @return TRUE if the database is writable; FALSE otherwise. This method
#' does not check file properties, only the intended usage of the object.
#'
#' @examples
#'
#' M <- annotation_database()
#' is.writable(M)
#'
setGeneric(
    "is_writable",
    function(obj, ...) standardGeneric("is_writable")
)

#' Join sources vertically
#'
#' A function to join sources vertically. A vertical join involves matching
#' common columns across source data.frames and padding missing columns to
#' create a single new data.frame with data and records from multiple sources.
#'
#' @param x an `annotation_source` object
#' @param y an second `annotation_source` object to join with the first
#' @return an `annotation_source` object
#'
#' @examples
#'
#' M <- annotation_source()
#' N <- annotation_source()
#' O <- vertical_join(M, N)
#'
setGeneric(
    "vertical_join",
    function(x, y, ...) standardGeneric("vertical_join")
)

#######
#' Required columns in an annotation source
#'
#' Some `annotation_sources`, such as LCMS tables (`lcms_table`), require that
#' certain columns are present in the data.frame. These are defined by slots in
#' the source definition. The name of slots containing the required column names
#' for a source can be retrieved using the `required_cols` function, which will
#' collect and return the names of slots containing required column names for
#' the object and all of its parent objects.
#'
#' @param x an `annotation_source` object
#' @return a character vector of slot names
#'
#' @examples
#' # prepare object
#' M <- lcms_table(id_column = "id", mz_column = "mz", rt_column = "rt")
#'
#' #' # get values for required slots
#' r <- required(M)
#'
#' # get slot names for required columns
#' names(r)
setGeneric(
    "required_cols",
    function(obj, ...) standardGeneric("required_cols")
)

#####
