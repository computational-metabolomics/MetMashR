#' @eval get_description('MTox700plus_database')
#' @export
#' @include annotation_database_class.R BiocFileCache_database_class.R zzz.R
MTox700plus_database <- function(version = "latest",
                                 bfc_path = NULL,
                                 resource_name = "MetMashR_MTox700plus",
                                 ...) {
    # new object
    out <- struct::new_struct(
        "MTox700plus_database",
        version = version,
        bfc_path = bfc_path,
        resource_name = resource_name,
        ...
    )
    return(out)
}

.MTox700plus_database <- setClass(
    "MTox700plus_database",
    contains = "BiocFileCache_database",
    slots = c(version = "entity"),
    prototype = list(
        name = "MTox700plus_database",
        description = paste0(
            "Imports the MTox700+ database, which is made available under the ",
            "ODC Attribution License. MTox700+ is a list of toxicologically ",
            "relevant metabolites derived from publications, public databases ",
            "and relevant toxicological assays."
        ),
        type = "mtox_source",
        .params = c("version"),
        libraries = c("BiocFileCache", "httr"),
        citations = list(
            bibentry(
                bibtype = "Article",
                author = as.person(paste0(
                    "Elena Sostare and Thomas N Lawson and Lucy R Saunders ",
                    "and John K Colbourne and Ralf J M Weber and Tomasz ",
                    "Sobanski and Mark R Viant"
                )),
                doi = "10.1093/toxsci/kfac007",
                issue = "2",
                journal = "Toxicological Sciences",
                month = "3",
                pages = "208-220",
                title = paste0(
                    "Knowledge-Driven Approaches to Create the ",
                    "MTox700+ Metabolite Panel for Predicting Toxicity"
                ),
                volume = "186",
                year = "2022"
            )
        ),
        version = entity(
            name = "MTox700+ version number",
            description = paste0(
                "The version number of the MTox700+ database to import. ",
                "Available versions are listed here: ",
                "[https://github.com/michabohealthscience/MTox700plus/releases].",
                ' `version` should match the tag of the release e.g. `"v1.0"`.',
                ' For convenience `version = "latest"` will always retrieve ',
                "the most recent release. To prevent unecessary downloads ",
                "`BiocFileCache` is used to store a local copy."
            ),
            type = "character",
            value = "v1.0"
        ),
        import_fun = .set_entity_value(
            obj = "BiocFileCache_database",
            param_id = "import_fun",
            value = .MTox700plus_import_fun
        ),
        bfc_fun = .set_entity_value(
            obj = "BiocFileCache_database",
            param_id = "bfc_fun",
            value = .cache_as_is
        )
    )
)



#' @export
setMethod(
    f = "read_database",
    signature = c("MTox700plus_database"), definition = function(obj) {
        # use github api to get url
        if (obj$version == "latest") {
            response <- httr::GET(paste0(
                "https://api.github.com/repos/michabohealthscience/",
                "MTox700plus/releases/latest"
            ))
        } else { # assume its a tag
            response <- httr::GET(paste0(
                "https://api.github.com/repos/michabohealthscience/",
                "MTox700plus/releases/tags/", obj$version
            ))
        }
        # stop if issue
        httr::stop_for_status(response)
        # otherwise parse content
        J <- httr::content(response, as = "parsed")

        # Use BiocFileCache database
        obj$source <- J$zipball_url
        obj$resource_name <- paste0(obj$resource_name, "_", J$tag_name)
        df <- callNextMethod(obj)

        # return
        return(df)
    }
)
