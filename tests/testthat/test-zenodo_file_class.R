test_that("zenodo_file downloads and caches a file with its extension", {
    skip_if_offline("zenodo.org")
    skip_if_not_installed("BiocFileCache")

    bfc <- file.path(tempdir(), "zenodo_bfc")
    on.exit(unlink(bfc, recursive = TRUE))

    D <- zenodo_file(
        record_id = 8226097,
        file_name = "MDM_Suppl_vSubmitted.xlsx",
        bfc_path = bfc,
        import_fun = function(path) {
            data.frame(path = path, size = file.size(path))
        }
    )
    df <- read_database(D)

    expect_true(endsWith(df$path, "MDM_Suppl_vSubmitted.xlsx"))
    expect_gt(df$size, 0)

    # second read comes from the cache without downloading again
    D$offline <- TRUE
    expect_equal(read_database(D)$path, df$path)
})
