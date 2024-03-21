test_that("prioritise_columns works", {
    df <- data.frame(
        a = c(1, 2, NA, 5, 6, 6, NA, 8, 9, 10),
        b = seq_len(10),
        id = seq_len(10)
    )

    AN <- annotation_table(data = df, id_column = "id")

    # priority is column  a
    M <- prioritise_columns(
        column_names = c("a", "b"),
        output_name = "combined",
        source_name = "df",
        source_tags = c("a", "b")
    )
    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_true("combined" %in% colnames(out))
    expect_true("df" %in% colnames(out))
    expect_setequal(out$combined, c(1, 2, 3, 5, 6, 6, 7, 8, 9, 10))
    expect_setequal(out$df, c("a", "a", "b", "a", "a", "a", "b", "a", "a", "a"))

    # priority is column b
    M <- prioritise_columns(
        column_names = c("b", "a"),
        output_name = "combined",
        source_name = "df",
        source_tags = c("b", "a")
    )
    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_true("combined" %in% colnames(out))
    expect_true("df" %in% colnames(out))
    expect_setequal(out$combined, seq_len(10))
    expect_true(all(out$df == "b"))
})
