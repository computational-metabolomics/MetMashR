test_that("mz_match works", {
    obs <- data.frame(
        id = letters[seq_len(10)],
        rt = 100 + c(-10, -10, -10, -3, -3, -3, 3, 3, 3, 5),
        ppm = c(2, 2, 2, 5, 5, 5, 8, 8, 8, 3)
    )
    obs$mz <- 500 - (obs$ppm * 1e-6 * 500) # true mz is 500

    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- mz_match(
        variable_meta = obs,
        mz_column = "mz",
        ppm_window = 2.5,
        id_column = "id"
    )
    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_true(all(abs(out$ppm_match_diff_an) <= 5, na.rm = TRUE))
    expect_setequal(out$dbid, c("A", rep("B", 8), "C"))
    expect_setequal(out$mz_match_id, c(NA, c("a", "b", "c", "j"), NA))
})

test_that("mz_match errors", {
    obs <- data.frame(
        id = seq_len(10),
        rt = 100 + c(-10, -10, -10, -3, -3, -3, 3, 3, 3, 5),
        ppm = c(2, 2, 2, 5, 5, 5, 8, 8, 8, 3)
    )
    obs$mz <- 500 - (5 * 1e-6 * 500) # true mz is 500


    expect_error({
        M <- mz_match(
            variable_meta = obs,
            mz_column = "rt",
            ppm_window = c(2, 10),
            id_column = "id"
        )
    })
})


test_that("mz_match rownames", {
    obs <- data.frame(
        rt = 100 + c(-10, -10, -10, -3, -3, -3, 3, 3, 3, 5),
        ppm = c(2, 2, 2, 5, 5, 5, 8, 8, 8, 3)
    )
    obs$mz <- 500 - (obs$ppm * 1e-6 * 500) # true mz is 500
    rownames(obs) <- letters[seq_len(10)]

    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01)
    )


    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- mz_match(
        variable_meta = obs,
        mz_column = "mz",
        ppm_window = 2.5,
        id_column = "rownames"
    )
    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_true(all(abs(out$ppm_match_diff_an) <= 5, na.rm = TRUE))
    expect_setequal(out$dbid, c("A", rep("B", 8), "C"))
    expect_setequal(out$mz_match_id, c(NA, c("a", "b", "c", "j"), NA))
})
