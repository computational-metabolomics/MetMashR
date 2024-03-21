test_that("combine records collapses ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, 1)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .collapse(separator = " || ", na_string = NA),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A || B || C")
})

test_that("combine records does nothings ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, 1)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .nothing(),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 3) # all combined into 1 row
    expect_equal(out$combine_by, db$combine_by) # all combined into 1 row
})

test_that("combine unique ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, 1),
        levels = c(1, 1, 2)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    ##########
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .unique(separator = " || "),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A || B || C")
    expect_equal(out$levels, "1 || 2")

    ##########
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .unique(separator = " || ", digits = 1),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A || B || C")
    expect_equal(out$levels, "1 || 2")
    expect_equal(out$mz, "500")
})

test_that("combine select_exact ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, 1),
        levels = c(1, 1, 2)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_exact(
            match_col = "levels",
            match = 1,
            separator = " || "
        ),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A || B") # only A and B have leve = 1
    expect_equal(out$levels, "1")

    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_exact(
            match_col = "levels",
            match = 1,
            separator = NULL
        ),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 2) # all A and B not collapsed because sep=NULL
    expect_equal(out$dbid[1], "A") # only A
    expect_equal(out$dbid[2], "B") # only B
    expect_setequal(out$levels, c("1", "1"))
})

test_that("combine select_match ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c("1", "1", "1"),
        levels = c("1", "1", "2"),
        levels2 = c("1", "1", "1")
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    ##############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_match(match_col = "levels", search_col = "levels2", separator = " || "),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A || B") # only A and B have level and level2 equal 1
    expect_equal(out$levels, "1")

    ###############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_match(
            match_col = "levels", search_col = "levels2",
            separator = NULL
        ),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 2) # 2 rows, not combined
    expect_equal(out$dbid[1], "A") # A has level and level2 equal 1
    expect_equal(out$dbid[2], "B") # A has level and level2 equal 1
    expect_setequal(out$levels, c("1", "1"))
    expect_setequal(out$levels2, c("1", "1"))
})


test_that("combine select_min ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c("1", "1", "1"),
        levels = c("1", "1", "2"),
        levels2 = c("2", "1", "1"),
        rt2 = c(NA, NA, NA),
        rt3 = c(-10, -100, -200),
        rt4 = c(NA, 1, NA)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    #################
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_min(min_col = "rt"),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A") # A has min rt

    ################
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_min(min_col = "rt3", use_abs = TRUE),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "A") # A has min rt

    ###############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_min(min_col = "rt2", keep_NA = TRUE),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 3) # all NA so original table returned
    expect_equal(out$dbid, db$dbid) # all NA so original table returned

    ###############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_min(min_col = "rt4", keep_NA = FALSE),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # only one value is NA
    expect_equal(out$dbid[1], "B") # B has min rt
})

test_that("combine select_max ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c("1", "1", "1"),
        levels = c("1", "1", "2"),
        levels2 = c("2", "1", "1"),
        rt2 = c(NA, NA, NA),
        rt3 = c(-10, -100, -200),
        rt4 = c(NA, 1, NA)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    #################
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_max(max_col = "rt"),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "C") # C has max rt

    #################
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_max(max_col = "rt3", use_abs = TRUE),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # all combined into 1 row
    expect_equal(out$dbid[1], "C") # C has max rt

    ################
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_max(max_col = "rt2"),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 3) # all NA so original table returned
    expect_equal(out$dbid, db$dbid) # all NA so original table returned

    ###############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .select_min(min_col = "rt4", keep_NA = FALSE),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 1) # only one value is NA
    expect_equal(out$dbid[1], "B") # B has min rt
})

test_that("combine mean median mode ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c("1", "1", "1"),
        levels = c(1, 1, 2),
        levels2 = c(NA, "1", "1")
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .collapse(separator = " || "),
        fcns = list(
            "rt" = .median(),
            "mz" = .mean(),
            "levels" = .mode(),
            "levels2" = .mode()
        )
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(out$dbid[1], "A || B || C")
    expect_equal(out$rt, median(out$rt))
    expect_equal(out$mz, mean(out$mz))
    expect_equal(out$levels, 1)
})

test_that("combine records prioritise ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c("1", "1", "2"),
        levels = c("1", "2", "3"),
        levels2 = c("2", "1", "3")
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    #############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .prioritise(
            separator = NULL,
            match_col = "levels",
            priority = c("2", "1"),
            no_match = NA,
            na_string = NA
        ),
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    # level 2 prioritised over 1 for combine_by == 1
    expect_equal(out$dbid[1], "B")
    expect_equal(out$levels[1], "2")
    # NA if no matches to priority list
    expect_setequal(unlist(out[2, ]), c("2", rep(NA, 5)))

    ##############
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .prioritise(
            separator = NULL,
            match_col = "levels",
            priority = c("2", "1"),
            no_match = NULL,
            na_string = "NA"
        ),
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    # level 2 prioritised over 1 for combine_by == 1
    expect_equal(out$dbid[1], "B")
    expect_equal(out$levels[1], "2")
    # NA if no matches to priority list
    expect_setequal(unlist(out[2, ]), c(2, "C", 200, 500.01, 3, 3)) # when NULL full
    # record is returned
})

test_that("combine prioritise collapse ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c("1", "1", "1"),
        levels = c(1, 1, 2),
        levels2 = c("2", NA, "1")
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    ##################
    M <- combine_records(
        group_by = "combine_by",
        default_fcn = .prioritise(
            separator = " || ",
            match_col = "levels",
            priority = c("1", "2"),
            no_match = NA,
            na_string = "NA"
        ),
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(out$dbid[1], "A || B") # check collapsed expected rows
    expect_equal(out$levels, "1") # check priority 1
    expect_equal(out$levels2, c("2 || NA")) # check NA string replacement
})

test_that("combine records errors ok", {
    expect_error(
        {
            M <- combine_records(
                group_by = "combine_by",
                fcns = list(.collapse(separator = " || ", na_string = NA), "cake")
            )
        },
        "all fcns list items must be functions or calls."
    )
})


test_that("combine records with two groups ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, 1),
        combine_by2 = c(1, 1, 2)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = c("combine_by", "combine_by2"),
        default_fcn = .collapse(separator = " || ", na_string = NA),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 2) # all combined into 2 rows
    expect_equal(out$dbid[1], "A || B")
    expect_equal(out$combine_by[1], "1 || 1")
    expect_equal(out$combine_by2[2], "2")
})


test_that("combine records creates artificial groups for NA ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, NA)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = c("combine_by"),
        default_fcn = .collapse(separator = " || ", na_string = NA),
        fcns = list()
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 2) # all combined into 2 rows
    expect_equal(out$dbid[1], "C")
    expect_true(is.na(out$combine_by[1]))
    expect_equal(out$dbid[2], "A || B")
})


test_that("combine records works for count ok", {
    db <- data.frame(
        dbid = c("A", "B", "C"),
        rt = c(10, 100, 200),
        mz = c(499.99, 500, 500.01),
        combine_by = c(1, 1, NA)
    )

    AN <- lcms_table(
        data = db,
        id_column = "dbid",
        rt_column = "rt",
        mz_column = "mz"
    )

    M <- combine_records(
        group_by = c("combine_by"),
        default_fcn = .collapse(separator = " || ", na_string = NA),
        fcns = list(count = .count())
    )

    M <- model_apply(M, AN)

    out <- predicted(M)$data

    expect_equal(nrow(out), 2) # all combined into 2 rows
    expect_equal(out$dbid[1], "C")
    expect_true(is.na(out$combine_by[1]))
    expect_equal(out$dbid[2], "A || B")
    expect_setequal(out$count, c(1, 2))
})
