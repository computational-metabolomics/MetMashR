db <- data.frame(
    dbid = c("A", "B", "C", "D", "E"),
    rt = c(10, 100, 200, 1, 1),
    mz = c(499.99, 500, 500.01, 1, 1),
    search = c(
        "WQZGKKKJIJFFOK-GASJEMHNSA-N", # glucose
        "YCGXMNQNHBKSKZ-YNWZBRBLSA-N", # Valclavam
        "IPCSVZSSVZVIGE-UHFFFAOYSA-N", # palmitic acid
        "GABNFKJEHDJCHD-DHFSJAKDHC-J", # spoof, no hits
        "XLYOFNOQVPJJNP-UHFFFAOYSA-N" # water
    )
)


test_that("rds cache reads/writes", {
    # prep cache object
    C <- rds_database(source = tempfile(fileext = "rds"))
    C <- read_source(C)

    # write to cache
    write_database(C, db)

    # read cache
    check <- read_database(C)

    # compare
    expect_true(all(check == db))
})

test_that("sqlite cache reads/writes", {
    # prep cache object
    C <- sqlite_database(source = tempfile(fileext = "db"), table = "test")

    # write to cache
    write_database(C, db)

    # read from cache
    check <- read_database(C)

    # compare
    expect_true(all(check == db))
})

test_that("rest_api cache_mode = 'offline' does not rewrite the cache", {
    tf <- tempfile(fileext = ".rds")
    cache <- rds_cache(source = tf)
    write_database(cache, data.frame(.search = "2244", Title = "Aspirin"))
    old_time <- as.POSIXct("2020-01-01", tz = "UTC")
    Sys.setFileTime(tf, old_time)

    D <- annotation_table(
        data = data.frame(id = c(1, 2), cid = c("2244", "702")),
        id_column = "id"
    )
    M <- pubchem_property_lookup(
        query_column = "cid",
        search_by = "cid",
        property = "Title",
        cache = cache,
        cache_mode = "offline"
    )
    expect_warning(M <- model_apply(M, D), "cache_mode")

    out <- predicted(M)$data
    expect_equal(out$Title_pubchem[1], "Aspirin")
    expect_true(is.na(out$Title_pubchem[2]))
    expect_equal(as.numeric(file.mtime(tf)), as.numeric(old_time))
})
