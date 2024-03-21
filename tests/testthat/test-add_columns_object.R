test_that("add-columnns object can be created", {
    df <- data.frame(a = seq_len(10), b = seq_len(10))

    M <- add_columns(
        new_columns = df,
        by = "a"
    )

    expect_true(is(M, "add_columns"))
})

test_that("add-columnns object throws error if by not a column name", {
    df <- data.frame(a = seq_len(10), b = seq_len(10))

    expect_error({
        M <- add_columns(
            new_columns = df,
            by = "column 1"
        )
    })
})

test_that("add-columnns object includes the requested columns", {
    # some data.frames
    df <- data.frame(a = seq(from = 11, to = 20), b = seq_len(10))

    # data only match the by column in some places
    an <- data.frame(a = seq(from = 11, to = 20, by = 2), id = seq_len(5))

    # annotation table
    AN <- annotation_table(
        data = an,
        id_column = "id"
    )

    # model
    M <- add_columns(new_columns = df, by = "a")

    # apply
    M <- model_apply(M, AN)

    # tests
    out <- predicted(M)$data

    expect_true("b" %in% colnames(out))
    expect_true(all(out$b == c(1, 3, 5, 7, 9)))
})
