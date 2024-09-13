#' @eval get_description('PathBank_metabolite_database')
#' @export
#' @include annotation_database_class.R BiocFileCache_database_class.R
PathBank_metabolite_database <- function(
        version = "primary",
        bfc_path = NULL,
        resource_name = "MetMashR_PathBank",
        ...) {
    # new object
    out <- struct::new_struct(
        "PathBank_metabolite_database",
        version = version,
        bfc_path = bfc_path,
        resource_name = resource_name,
        ...
    )
    return(out)
}

.PathBank_metabolite_database <- setClass(
    "PathBank_metabolite_database",
    contains = "BiocFileCache_database",
    slots = c(
        version = "entity"
    ),
    prototype = list(
        name = "PathBank_metabolite_database",
        description = paste0(
            "Imports the PathBank database (https://pathbank.org/) of ",
            "metabolites linked to pathways."
        ),
        type = "database",
        .params = c("version"),
        libraries = c("BiocFileCache", "httr"),
        citations = list(
            bibentry(
                bibtype = "Article",
                author = as.person(paste0(
                    "Wishart, David S and Li, Carin and Marcu, Ana and Badran,",
                    " Hasan and Pon, Allison and Budinski, Zachary and ",
                    "Patron, ",
                    "Jonas and Lipton, Debra and Cao, Xuan and Oler, Eponine ",
                    "and Li, Krissa and Paccoud, Maïlys and Hong, Chelsea and ",
                    "Guo, An C and Chan, Christopher and Wei, William and ",
                    "Ramirez-Gaona, Miguel"
                )),
                doi = "10.1093/nar/gkz861",
                journal = "Nucleic Acids Research",
                month = "10",
                pages = "D470-D478",
                title = paste0(
                    "PathBank: a comprehensive pathway database for model ",
                    "organisms"
                ),
                volume = "48",
                year = "2019"
            )
        ),
        version = enum(
            name = "PathBank version",
            description = c(
                paste0(
                    "The version of the PatchBank database to import. ",
                    "To prevent unecessary downloads `BiocFileCache` is used ",
                    "to ",
                    "store a local copy."
                ),
                complete = "The complete PathBank metabolite database.",
                primary = paste0(
                    "The PathBank metabolite database for primary pathways ",
                    "only."
                )
            ),
            type = "character",
            value = "primary",
            allowed = c("primary", "complete")
        ),
        import_fun = .set_entity_value(
            "BiocFileCache_database",
            "import_fun",
            value = read.csv
        ),
        bfc_fun = .set_entity_value(
            "BiocFileCache_database",
            "bfc_fun",
            value = .unzip
        )
    )
)



#' @export
setMethod(
    f = "read_database",
    signature = c("PathBank_metabolite_database"), definition = function(obj) {
        # update url for version
        if (obj$version == "primary") {
            db_url <- paste0(
                "https://pathbank.org/downloads/",
                "pathbank_primary_metabolites.csv.zip"
            )
        } else {
            db_url <- paste0(
                "https://pathbank.org/downloads/",
                "pathbank_all_metabolites.csv.zip"
            )
        }
        obj$source <- db_url
        
        # append version to rname
        obj$resource_name <- paste0(obj$resource_name, "_", obj$version)
        
        # reuse BiocFileCache_database
        df <- callNextMethod(obj)
        
        # return
        return(df)
    }
)
