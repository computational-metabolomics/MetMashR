#' @eval get_description('github_file')
#' @export
#' @include annotation_database_class.R BiocFileCache_database_class.R zzz.R
github_file <- function(username,
                        repository_name,
                        file_path,
                        bfc_path = NULL,
                        resource_name = paste(username, repository_name,
                            file_path,
                            sep = "_"
                        ),
                        ...) {
    # new object
    out <- struct::new_struct(
        "github_file",
        username = username,
        repository_name = repository_name,
        file_path = file_path,
        bfc_path = bfc_path,
        resource_name = resource_name,
        ...
    )
    return(out)
}

.github_file <- setClass(
    "github_file",
    contains = "BiocFileCache_database",
    slots = c(
        username = "entity",
        repository_name = "entity",
        file_path = "entity"
    ),
    prototype = list(
        name = "GitHub file",
        description = paste0(
            "Uses the GitHub REST API to retrieve a file from a specified",
            "GitHub repository."
        ),
        type = "github_source",
        .params = c("username", "repository_name", "file_path"),
        libraries = c("BiocFileCache", "httr"),
        username = entity(
            name = "GitHub username",
            description = paste0(
                "The GitHub username to retireve the file from."
            ),
            type = "character",
            max_length = 1
        ),
        repository_name = entity(
            name = "GitHub repository name",
            description = paste0(
                "The name of a repository for the specified GitHub username",
                "that contains the file to download."
            ),
            type = "character",
            max_length = 1
        ),
        file_path = entity(
            name = "GitHub file path",
            description = paste0(
                "The path to the file to download within the specified ",
                "GitHub repository."
            ),
            type = "character",
            max_length = 1
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
    signature = c("github_file"), definition = function(obj) {
        # use github api to get url
        response <- httr::GET(paste0(
            "https://api.github.com/repos/", obj$username, "/",
            obj$repository_name, "/contents/", obj$file_path
        ))

        # stop if issue
        httr::stop_for_status(response)
        # otherwise parse content
        J <- httr::content(response, as = "parsed")

        # Use BiocFileCache database witht he download url
        obj$source <- J$download_url
        obj$resource_name <- paste0(obj$resource_name, "_", J$tag_name)
        df <- callNextMethod(obj)

        # return
        return(df)
    }
)
