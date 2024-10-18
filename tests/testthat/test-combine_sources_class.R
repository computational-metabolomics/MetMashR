test_that("combine sources works", {
    df1 <- data.frame(
        id = seq_len(10),
        source = "A",
        aa = seq_len(10),
        b = seq_len(10),
        c = seq_len(10)
    )

    AN1 <- annotation_table(df1, tag = "A", id_column = "id")


    df2 <- data.frame(
        id = seq(from = 11, to = 20),
        source = "B",
        a = seq(from = 11, to = 20),
        b = seq(from = 11, to = 20),
        d = seq(from = 11, to = 20)
    )

    AN2 <- annotation_table(df2, tag = "B", id_column = "id")

    M <- combine_sources(
        source_list = AN2,
        matching_columns = c(a = "a", a = "aa"),
        keep_cols = c("c", "d", "source"),
        source_col = "annotation_table"
    )

    M <- model_apply(M, AN1)

    out <- predicted(M)$data

    # check source column present
    expect_true("annotation_table" %in% colnames(out))
    # check source column has expected values
    expect_setequal(out$annotation_table, out$source)
    # "aa" column should not be present
    expect_false("aa" %in% colnames(out))
    # "aa" column merged into "a" column
    expect_setequal(out$a, c(df2$a, df1$aa))
    # "c" column is present
    expect_true("c" %in% colnames(out))
    # "c" has NA where not in source
    expect_setequal(out$c, c(rep(NA, 10), df1$c))
    # "c" is NA for source B
    expect_true(all(is.na(out$c[out$annotation_table == "B"])))
    # "d" column is present
    expect_true("d" %in% colnames(out))
    # "d" has NA where not in source
    expect_setequal(out$d, c(df2$d, rep(NA, 10)))
    # "d" is NA for source B
    expect_true(all(is.na(out$d[out$annotation_table == "A"])))
})


test_that("combine sources works for list input", {
    df1 <- data.frame(
        id = seq_len(10),
        source = "A",
        aa = seq_len(10),
        b = seq_len(10),
        c = seq_len(10)
    )

    AN1 <- annotation_table(df1, tag = "A", id_column = "id")


    df2 <- data.frame(
        id = seq(from = 11, to = 20),
        source = "B",
        a = seq(from = 11, to = 20),
        b = seq(from = 11, to = 20),
        d = seq(from = 11, to = 20)
    )

    AN2 <- annotation_table(df2, tag = "B", id_column = "id")

    M <- combine_sources(
        source_list = list(),
        matching_columns = c("a" = "a", "a" = "aa"),
        keep_cols = c("c", "d", "source"),
        source_col = "annotation_table"
    )

    M <- model_apply(M, list(AN1, AN2))

    out <- predicted(M)$data

    # check source column present
    expect_true("annotation_table" %in% colnames(out))
    # check source column has expected values
    expect_setequal(out$annotation_table, out$source)
    # "aa" column should not be present
    expect_false("aa" %in% colnames(out))
    # "aa" column merged into "a" column
    expect_setequal(out$a, c(df2$a, df1$aa))
    # "c" column is present
    expect_true("c" %in% colnames(out))
    # "c" has NA where not in source
    expect_setequal(out$c, c(rep(NA, 10), df1$c))
    # "c" is NA for source B
    expect_true(all(is.na(out$c[out$annotation_table == "B"])))
    # "d" column is present
    expect_true("d" %in% colnames(out))
    # "d" has NA where not in source
    expect_setequal(out$d, c(df2$d, rep(NA, 10)))
    # "d" is NA for source B
    expect_true(all(is.na(out$d[out$annotation_table == "A"])))
})


test_that("combine sources errors", {
    expect_error(
        {
            M <- combine_sources(
                source_list = list(1, "a"),
                matching_columns = NULL,
                keep_cols = NULL,
                tag_ids = FALSE,
                source_col = "annotation_table"
            )
        },
        regexp = "all source_list items must be annotation_source objects."
    )
})



test_that("combine sources warnings", {
    df1 <- data.frame(
        id = seq_len(10),
        source = "A",
        aa = seq_len(10),
        b = seq_len(10),
        c = seq_len(10),
        a = seq(from = 21, to = 30)
    )

    AN1 <- annotation_table(df1, tag = "A", id_column = "id")


    df2 <- data.frame(
        id = seq(from = 11, to = 20),
        source = "B",
        a = seq(from = 11, to = 20),
        b = seq(from = 11, to = 20),
        d = seq(from = 11, to = 20)
    )

    AN2 <- annotation_table(df2, tag = "B", id_column = "id")

    M <- combine_sources(
        source_list = AN2,
        matching_columns = c("a" = "a", "a" = "aa"),
        keep_cols = c("c", "d", "source"),
        source_col = "annotation_table"
    )

    expect_error(
        {
            M <- model_apply(M, AN1)
        },
        regexp = "Names must be unique"
    )
})

test_that("combine sources works for lcms_tables", {
    df1 <- data.frame(
        id = seq_len(10),
        source = "A",
        aa = seq_len(10),
        b = seq_len(10),
        c = seq_len(10),
        rt = seq_len(10),
        mz = seq_len(10)
    )

    AN1 <- lcms_table(df1,
        tag = "A", id_column = "id", rt_column = "rt",
        mz_column = "mz"
    )


    df2 <- data.frame(
        id = seq(from = 11, to = 20),
        source = "B",
        a = seq(from = 11, to = 20),
        b = seq(from = 11, to = 20),
        d = seq(from = 11, to = 20),
        rt = seq_len(10),
        mz = seq_len(10)
    )

    AN2 <- lcms_table(df2,
        tag = "B", id_column = "id", rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_sources(
        source_list = AN2,
        matching_columns = c("a" = "a", "a" = "aa"),
        keep_cols = c("c", "d", "source"),
        source_col = "annotation_table",
        as = lcms_table(id = "id", mz = "mz", rt = "rt")
    )

    M <- model_apply(M, AN1)

    out <- predicted(M)$data

    # check source column present
    expect_true("annotation_table" %in% colnames(out))
    # check source column has expected values
    expect_setequal(out$annotation_table, out$source)
    # "aa" column should not be present
    expect_false("aa" %in% colnames(out))
    # "aa" column merged into "a" column
    expect_setequal(out$a, c(df2$a, df1$aa))
    # "c" column is present
    expect_true("c" %in% colnames(out))
    # "c" has NA where not in source
    expect_setequal(out$c, c(rep(NA, 10), df1$c))
    # "c" is NA for source B
    expect_true(all(is.na(out$c[out$annotation_table == "B"])))
    # "d" column is present
    expect_true("d" %in% colnames(out))
    # "d" has NA where not in source
    expect_setequal(out$d, c(df2$d, rep(NA, 10)))
    # "d" is NA for source B
    expect_true(all(is.na(out$d[out$annotation_table == "A"])))
    # mz and rt columns should be present for lcms
    expect_true(all(c("mz", "rt") %in% colnames(out)))
    # expect lcms_table for merged output
    expect_true(is(predicted(M), "lcms_table"))
})

test_that("combine sources works with .all and exclude_cols", {
    df1 <- data.frame(
        id = seq_len(10),
        source = "A",
        aa = seq_len(10),
        b = seq_len(10),
        c = seq_len(10),
        rt = seq_len(10),
        mz = seq_len(10)
    )

    AN1 <- lcms_table(df1,
        tag = "A", id_column = "id", rt_column = "rt",
        mz_column = "mz"
    )


    df2 <- data.frame(
        id = seq(from = 11, to = 20),
        source = "B",
        a = seq(from = 11, to = 20),
        b = seq(from = 11, to = 20),
        d = seq(from = 11, to = 20),
        rt = seq_len(10),
        mz = seq_len(10)
    )

    AN2 <- lcms_table(df2,
        tag = "B", id_column = "id", rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_sources(
        source_list = AN2,
        matching_columns = c("a" = "a", "a" = "aa"),
        keep_cols = c(".all"),
        source_col = "annotation_table",
        exclude_cols = c("c"),
        as = lcms_table(id = "id", mz = "mz", rt = "rt")
    )

    M <- model_apply(M, AN1)

    out <- predicted(M)$data

    # check source column present
    expect_true("annotation_table" %in% colnames(out))
    # check source column has expected values
    expect_setequal(out$annotation_table, out$source)
    # "aa" column should not be present
    expect_false("aa" %in% colnames(out))
    # "aa" column merged into "a" column
    expect_setequal(out$a, c(df2$a, df1$aa))
    # "c" column is NOT present
    expect_false("c" %in% colnames(out))
    # "c" is NA for source B
    expect_true(all(is.na(out$c[out$annotation_table == "B"])))
    # "d" column is present
    expect_true("d" %in% colnames(out))
    # "d" has NA where not in source
    expect_setequal(out$d, c(df2$d, rep(NA, 10)))
    # "d" is NA for source B
    expect_true(all(is.na(out$d[out$annotation_table == "A"])))
    # mz and rt columns should be present for lcms
    expect_true(all(c("mz", "rt") %in% colnames(out)))
    # expect lcms_table for merged output
    expect_true(is(predicted(M), "lcms_table"))
})
