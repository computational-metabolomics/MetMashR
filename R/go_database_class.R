#' @eval get_description('GO_database')
#' @export
#' @include AnnotationDb_database.R
#' @family annotation databases
#' @seealso [GO.db::GO()]
GO_database <- function(
        source = "GO.db",
        table = "GOBPOFFSPRING",
        ...) {
    # new object
    out <- struct::new_struct(
        "GO_database",
        source = source,
        table = table,
        ...
    )
    return(out)
}

.GO_database <- setClass(
    "GO_database",
    contains = "AnnotationDb_database",
    prototype = list(
        name = "GO.db",
        description = paste0(
            "Retrieve a table from the Gene Ontology using the `GO.db` ",
            "package."
        ),
        type = "GO_database",
        .writable = FALSE,
        libraries = "GO.db",
        table = enum(
            name = "GO table",
            description = paste0(
                "The name of a table to import from the GO.db package. ",
                "Allowed tables include: GOBPANCESTOR,GOBPPARENTS,",
                "GOBPCHILDREN,GOBPOFFSPRING (and their CC or MF equivalents), ",
                "GOTERM, GOSYNONYM, GOOBSOLETE"
            ),
            type = "character",
            value = "GOBPCHILDREN",
            max_length = 1,
            allowed = c(
                "GOBPANCESTOR", "GOBPPARENTS",
                "GOBPCHILDREN", "GOBPOFFSPRING",
                "GOCCANCESTOR", "GOCCPARENTS",
                "GOCCCHILDREN", "GOCCOFFSPRING",
                "GOMFANCESTOR", "GOMFPARENTS",
                "GOMFCHILDREN", "GOMFOFFSPRING",
                "GOTERM", "GOSYNONYM", "GOOBSOLETE"
            )
        )
    )
)
