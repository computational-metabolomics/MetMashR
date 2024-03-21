test_that("annotation_table can be created", {
    df <- data.frame(a = seq_len(10), b = seq_len(10), id = seq_len(10))
    AN <- annotation_table(data = df, tag = "test", id_column = "id")

    expect_true(is(AN, "annotation_table"))
    expect_output(show(AN), regexp = 'A "annotation_table" object')
})

test_that("lcms_table can be created", {
    df <- data.frame(a = seq_len(10), b = seq_len(10), id = seq_len(10), rt = seq_len(10), mz = seq_len(10))
    AN <- lcms_table(
        data = df, tag = "test",
        id_column = "id",
        mz_column = "mz",
        rt_column = "rt"
    )

    expect_true(is(AN, "annotation_table"))
    expect_true(is(AN, "lcms_table"))
    expect_output(
        {
            show(AN)
        },
        regexp = 'A "lcms_table" object'
    )
})

test_that("annotation_table throws error if id column is missing", {
    df <- data.frame(a = seq_len(10), b = seq_len(10), id = seq_len(10))

    expect_error({
        AN <- annotation_table(data = df, tag = "test", id_column = "cake")
    })
})

test_that("lcms_table throws error if named columns are missing", {
    df <- data.frame(a = seq_len(10), b = seq_len(10), id = seq_len(10), mz = seq_len(10), rt = seq_len(10))

    expect_error({
        AN <- lcms_table(
            data = df, tag = "test",
            id_column = "cake",
            mz_column = "mz",
            rt_column = "rt"
        )
    })

    expect_error({
        AN <- lcms_table(
            data = df, tag = "test",
            id_column = "id",
            mz_column = "cake",
            rt_column = "rt"
        )
    })

    expect_error({
        AN <- lcms_table(
            data = df, tag = "test",
            id_column = "id",
            mz_column = "mz",
            rt_column = "cake"
        )
    })
})
