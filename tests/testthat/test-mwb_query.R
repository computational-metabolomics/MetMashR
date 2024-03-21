test_that("mwb_lookup queries ok", {
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

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- mwb_compound_lookup(
        query_column = "search",
        input_item = "inchi_key",
        output_item = "all",
        cache = NULL,
        suffix = ""
    )

    with_mock_dir("mwb1", {
        M <- model_apply(M, AN)
    })

    out <- predicted(M)$data

    expect_equal(colnames(out)[5], "regno")
    expect_equal(out$search[1], "WQZGKKKJIJFFOK-GASJEMHNSA-N")
    expect_equal(out$formula[1], "C6H12O6")
    expect_true(is.na(out$exactmass[4]))
    expect_equal(out$name[3], "Palmitic acid")
})
