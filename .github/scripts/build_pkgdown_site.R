## Builds the pkgdown site for GitHub pages (run by check-bioc.yml).
##
## Adds a "Versions" menu to the navbar linking to the Bioconductor landing
## page of every release that included MetMashR. The list of releases is read
## from the Bioconductor config at build time, so a new release is added
## automatically the next time the site is built. _pkgdown.yml is not changed.

## first Bioconductor release that included MetMashR
first_release <- package_version("3.20")

bioc_url <- "https://bioconductor.org/"

## MetMashR version in a given Bioconductor release, or "" if not found
release_pkg_version <- function(release) {
    tryCatch({
        views <- read.dcf(
            url(paste0(bioc_url, "packages/", release, "/bioc/VIEWS")),
            fields = c("Package", "Version")
        )
        v <- views[views[, "Package"] %in% "MetMashR", "Version"]
        if (length(v) == 1) v else ""
    }, error = function(e) "")
}

versions_menu <- function() {
    config <- yaml::read_yaml(paste0(bioc_url, "config.yaml"))
    current <- package_version(config$release_version)
    releases <- package_version(names(config$release_dates))
    releases <- releases[releases >= first_release & releases <= current]
    releases <- as.character(sort(releases, decreasing = TRUE))

    items <- lapply(releases, function(release) {
        label <- paste0("Bioconductor ", release)
        v <- release_pkg_version(release)
        if (nzchar(v)) {
            label <- paste0(label, " (v", v, ")")
        }
        list(
            text = label,
            href = paste0(
                bioc_url, "packages/", release, "/bioc/html/MetMashR.html"
            )
        )
    })

    list(
        text = "Versions",
        menu = c(list(list(text = "devel (this site)", href = "index.html")), items)
    )
}

pkg <- pkgdown::as_pkgdown(".")
menu <- tryCatch(versions_menu(), error = function(e) {
    message("Skipping Versions menu: ", conditionMessage(e))
    NULL
})
## added to the loaded config directly; pkgdown's `override` can't append to
## an unnamed list such as navbar$left
if (!is.null(menu)) {
    pkg$meta$navbar$left <- c(pkg$meta$navbar$left, list(menu))
}

pkgdown::build_site_github_pages(pkg, new_process = FALSE, install = FALSE)
