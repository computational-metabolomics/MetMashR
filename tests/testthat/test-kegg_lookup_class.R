test_that("kegg_lookup queries ok", {
    db <- data.frame(
        "id" = c(1, 2, 3),
        "pubchem_sid" = c(3937, 3938, 1)
    )

    D <- annotation_table(data = db, id_column = "id")


    M <- kegg_lookup(
        get = "compound",
        from = "pubchem_sid",
        query_column = "pubchem_sid",
        suffix = ""
    )

    with_mock_dir("kg0", {
        M <- model_apply(M, D)
    })

    out <- predicted(M)$data

    expect_equal(colnames(out)[3], "compound")
    expect_equal(out$compound[1], "C00668")
    expect_true(is.na(out$compound[3]))
})

test_that("kegg_lookup errors", {
    expect_error({
        M <- kegg_lookup(
            get = "compound",
            from = "drug",
            query_column = "pubchem_sid",
            suffix = ""
        )
    })

    expect_error({
        M <- kegg_lookup(
            get = "pubchem_sid",
            from = "chebi",
            query_column = "pubchem_sid",
            suffix = ""
        )
    })
})

test_that("kegg_lookup works correctly when there are no hits", {
    db <- data.frame(
        "id" = c(1, 2, 3),
        "pubchem_sid" = c(1, 1, 1)
    )

    D <- annotation_table(data = db, id_column = "id")

    M <- kegg_lookup(
        get = "compound",
        from = "pubchem_sid",
        query_column = "pubchem_sid",
        suffix = ""
    )

    with_mock_dir("kg2", {
        M <- model_apply(M, D)
    })


    out <- predicted(M)$data

    expect_true(all(is.na(out$compound)))
})

test_that("kegg_lookup works correctly when there are no data", {
    db <- data.frame(
        "id" = character(0),
        "pubchem_sid" = character(0)
    )

    D <- annotation_table(data = db, id_column = "id")

    M <- kegg_lookup(
        get = "compound",
        from = "pubchem_sid",
        query_column = "pubchem_sid",
        suffix = ""
    )

    with_mock_dir("kg3", {
        M <- model_apply(M, D)
    })


    out <- predicted(M)$data

    expect_equal(nrow(out), 0)
    expect_equal(ncol(out), 3)
    expect_equal(colnames(out)[3], "compound")
})

test_that("kegg_lookup cache_mode = 'offline' avoids live queries and leaves uncached values NA", {
    tf <- tempfile(fileext = ".rds")
    cache <- rds_cache(
        source = tf,
        data = data.frame(.search = "3937", compound = "C00668")
    )
    write_database(cache, data.frame(.search = "3937", compound = "C00668"))

    db <- data.frame(
        "id" = c(1, 2),
        "pubchem_sid" = c(3937, 3938)
    )
    D <- annotation_table(data = db, id_column = "id")

    M <- kegg_lookup(
        get = "compound",
        from = "pubchem_sid",
        query_column = "pubchem_sid",
        suffix = "",
        cache = cache,
        cache_mode = "offline"
    )

    expect_warning(
        M <- model_apply(M, D),
        "cache_mode"
    )

    out <- predicted(M)$data

    expect_equal(out$compound[1], "C00668") # from cache
    expect_true(is.na(out$compound[2])) # not in cache, left NA rather than queried
})

test_that("kegg_lookup cache_mode = 'offline' with an empty cache warns clearly", {
    tf <- tempfile(fileext = ".rds")
    cache <- rds_cache(source = tf)

    db <- data.frame(
        "id" = 1,
        "pubchem_sid" = 3937
    )
    D <- annotation_table(data = db, id_column = "id")

    M <- kegg_lookup(
        get = "compound",
        from = "pubchem_sid",
        query_column = "pubchem_sid",
        suffix = "",
        cache = cache,
        cache_mode = "offline"
    )

    expect_warning(
        M <- model_apply(M, D),
        "empty or not configured"
    )

    out <- predicted(M)$data
    expect_true(is.na(out$compound[1]))
})

test_that("kegg_lookup cache_mode = 'rebuild' re-queries and overwrites a stale cache entry", {
    # matches the "kg0" mock fixture, which was recorded for exactly this
    # 3-value request (pubchem_sid = c(3937, 3938, 1))
    tf <- tempfile(fileext = ".rds")
    cache <- rds_cache(
        source = tf,
        data = data.frame(.search = "3937", compound = "STALE_VALUE")
    )
    write_database(cache, data.frame(.search = "3937", compound = "STALE_VALUE"))

    db <- data.frame(
        "id" = c(1, 2, 3),
        "pubchem_sid" = c(3937, 3938, 1)
    )
    D <- annotation_table(data = db, id_column = "id")

    M <- kegg_lookup(
        get = "compound",
        from = "pubchem_sid",
        query_column = "pubchem_sid",
        suffix = "",
        cache = cache,
        cache_mode = "rebuild"
    )

    with_mock_dir("kg0", {
        M <- model_apply(M, D)
    })

    out <- predicted(M)$data
    expect_equal(out$compound[1], "C00668") # freshly queried, not the stale cached value

    # cache on disk should now hold the fresh value, not the stale one
    refreshed <- read_source(cache)$data
    expect_equal(refreshed$compound[refreshed$.search == "3937"], "C00668")
})
