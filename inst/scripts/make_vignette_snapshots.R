# Stored copies of the online resources used by the vignettes.
#
# The vignettes must build without network access, because many web services
# now block automated queries (e.g. from Bioconductor build machines). This
# script downloads each resource once, keeps only the rows and columns the
# vignettes use, and saves it to inst/extdata/vignette_snapshots/. It also
# builds the LIPID MAPS response cache used by the "Getting started" vignette
# (inst/cached/lipidmaps_inchikey_cache.rds), and records where and when each
# resource was retrieved in snapshot_info.rds.
#
# Resources imported with BiocFileCache are taken from the local cache if
# they are already there, so their "retrieved" date is the date the cached
# copy was downloaded, not the date this script was run.
#
# Run from the MetMashR source directory (needs network access):
#   Rscript inst/scripts/make_vignette_snapshots.R
pkgload::load_all(".", quiet = TRUE)

out_dir <- file.path("inst", "extdata", "vignette_snapshots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
today <- as.character(Sys.Date())
info <- list()

save_snapshot <- function(x, file, resource, version, url,
    retrieved = today) {
    saveRDS(x, file.path(out_dir, file), compress = "xz")
    info[[file]] <<- data.frame(
        file = file, resource = resource, version = version,
        retrieved = retrieved, url = url
    )
}

# date the BiocFileCache copy of a resource was downloaded
bfc_date <- function(pattern) {
    bfc <- BiocFileCache::BiocFileCache(ask = FALSE)
    x <- as.data.frame(BiocFileCache::bfcquery(bfc, pattern, field = "rname"))
    as.character(as.Date(max(x$create_time)))
}

## MTox700+ (latest release on GitHub)
release <- .github_api_get(paste0(
    "https://api.github.com/repos/michabohealthscience/",
    "MTox700plus/releases/latest"
))
release <- httr::content(release, as = "parsed")$tag_name
mtox <- read_database(MTox700plus_database(version = release))
save_snapshot(
    mtox, "MTox700plus.rds", "MTox700+", release,
    "https://github.com/michabohealthscience/MTox700plus",
    retrieved = bfc_date(paste0("MTox700plus_", release))
)

## PathBank primary-pathway metabolites (columns used by the vignettes)
pathbank <- read_database(PathBank_metabolite_database(version = "primary"))
pathbank <- pathbank[, c(
    "InChI.Key", "PathBank.ID", "Pathway.Name", "Pathway.Subject", "Species",
    "HMDB.ID", "KEGG.ID", "ChEBI.ID", "DrugBank.ID", "SMILES"
)]
save_snapshot(
    pathbank, "PathBank_primary_metabolites.rds", "PathBank", "primary",
    "https://pathbank.org/downloads",
    retrieved = bfc_date("PathBank_primary")
)

## RefMet records for the names in the Extending vignette example
AN <- readRDS(file.path("inst", "extdata", "AN000465_subset.rds"))
refmet <- read_database(mwb_refmet_database())
refmet <- refmet[refmet$name %in% AN$refmet_name, ]
save_snapshot(
    refmet, "RefMet_AN000465_subset.rds", "RefMet",
    "subset for AN000465_subset.rds",
    "https://www.metabolomicsworkbench.org/databases/refmet",
    retrieved = bfc_date("refmet/all")
)

## Metabolomics Workbench studies used in the MMS case study
for (id in c("ST001039", "ST001048", "ST001317")) {
    df <- read_source(mwb_study_source(source = id, tag = id))$data
    save_snapshot(
        df, paste0("MWB_", id, ".rds"), paste("Metabolomics Workbench", id),
        "metabolites table",
        "https://www.metabolomicsworkbench.org/rest/study/study_id/"
    )
}

## Supplementary Table S3 of the MMS publication (Zenodo record 8226097)
gold <- read_database(zenodo_file(
    record_id = 8226097,
    file_name = "MDM_Suppl_vSubmitted.xlsx",
    import_fun = function(path) {
        openxlsx::read.xlsx(path, sheet = "Table S3", colNames = TRUE)
    }
))
save_snapshot(
    gold, "MMS_zenodo_8226097_TableS3.rds", "Zenodo 8226097",
    "MDM_Suppl_vSubmitted.xlsx, Table S3",
    "https://zenodo.org/records/8226097",
    retrieved = bfc_date("8226097")
)

## REST API response caches (inst/cached/) used by the vignettes
# Each vignette is run with its lookups switched from cache_mode = "offline"
# to "update", so that any query missing from a cache is sent to the service
# and added to the cache. pkgload::load_all() makes system.file() point at
# the source inst/ folder, so the caches in the source tree are updated. The
# MMS case study is not refreshed here; its caches are rebuilt separately.
refresh_caches <- function(vignette) {
    code <- knitr::purl(
        file.path("vignettes", vignette),
        output = tempfile(fileext = ".R"), quiet = TRUE
    )
    lines <- readLines(code)
    lines <- gsub(
        'cache_mode = "offline"', 'cache_mode = "update"', lines,
        fixed = TRUE
    )
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    eval(parse(text = lines), envir = new.env())
}
lipidmaps_cache <- file.path("inst", "cached", "lipidmaps_inchikey_cache.rds")
if (!file.exists(lipidmaps_cache)) {
    saveRDS(data.frame(.search = NA), lipidmaps_cache)
}
refresh_caches("using_MetMashR.Rmd")
refresh_caches("Extending_MetMashR.Rmd")
refresh_caches("annotate_mixtures.Rmd")
info[["lipidmaps"]] <- data.frame(
    file = "../../cached/lipidmaps_inchikey_cache.rds",
    resource = "LIPID MAPS REST API responses", version = "",
    retrieved = today, url = "https://www.lipidmaps.org/resources/rest"
)

saveRDS(do.call(rbind, info), file.path(out_dir, "snapshot_info.rds"))
