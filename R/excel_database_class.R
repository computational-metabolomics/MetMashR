#' @eval get_description('excel_database')
#' @export
#' @include annotation_database_class.R
#' @family annotation databases
excel_database <- function(
        source = character(0),
        sheet = 1,
        rowNames = FALSE,
        colNames = TRUE,
        startRow = 1,
        ...) {
    # new object
    out <- struct::new_struct(
        "excel_database",
        source = source,
        sheet = sheet,
        rowNames = rowNames,
        colNames = colNames,
        startRow = startRow,
        ...
    )
    return(out)
}

.excel_database <- setClass(
    "excel_database",
    contains = "annotation_database",
    slots = c(
        sheet = "entity",
        rowNames = "entity",
        colNames = "entity",
        startRow = "entity"
    ),
    prototype = list(
        name = "Excel database",
        description = "A data.frame imported from the sheet of an excel file",
        type = "excel_database",
        .writable = FALSE,
        .params = c("sheet", "rowNames", "colNames", "startRow"),
        libraries = "openxlsx",
        sheet = entity(
            name = "Sheet name",
            description = "The name of the sheet to import.",
            type = "character",
            max_length = 1
        ),
        rowNames = entity(
            name = "Row names",
            description = paste0(
                "If TRUE, first column of data will be used as row names."),
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        colNames = entity(
            name = "Col names",
            description = paste0(
                "If TRUE, first row of data will be used as column names."),
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        startRow = entity(
            name = "Start row",
            description = paste0(
                'First row to begin looking for data. Empty rows at the top ',
                'of a file are always skipped, regardless of the value of ',
                'startRow.'),
            type = c("numeric", "integer"),
            value = 1,
            max_length = 1
        )
    )
)

#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("excel_database"), definition = function(obj) {
        # read the file
        IN <- openxlsx::read.xlsx(
            xlsxFile = obj$source,
            sheet = obj$sheet,
            rowNames = obj$rowNames,
            colNames = obj$colNames,
            startRow = obj$startRow
        )
        colnames(IN) <- make.unique(colnames(IN))
        IN$.row_id <- seq_len(nrow(IN))

        # return
        return(IN)
    }
)
