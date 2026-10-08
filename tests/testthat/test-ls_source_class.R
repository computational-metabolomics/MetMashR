# write a minimal LipidSearch export with two samples
write_ls_export <- function(df) {
    tf <- tempfile(fileext = ".txt")
    header <- c(
        "Rej.", "LipidIon", "LipidGroup", "Class", "FattyAcid",
        "CalcMz", "IonFormula", "ObsMz[c-1]", "ObsMz[c-2]",
        "Rt[c-1]", "Rt[c-2]", "Grade[c-1]", "Grade[c-2]"
    )
    body <- with(df, paste(
        0, LipidIon, LipidIon, Class, FattyAcid, CalcMz, "C1",
        CalcMz, CalcMz, 5, 5, Grade, "",
        sep = "\t"
    ))
    writeLines(
        c("#[c-1]:sample_1.raw", "#[c-2]:sample_2.raw",
          paste(header, collapse = "\t"), body),
        tf
    )
    tf
}

test_that("ls_source object can be created", {
    M <- ls_source(source = "x", tag = "test")
    expect_true(is(M, "ls_source"))
})

test_that("ls_source LipidName drops only the adduct", {
    # LipidIon values taken from real LipidSearch exports, except the "/"
    # names, which are sn-position names LipidSearch can also report
    df <- data.frame(
        LipidIon = c(
            "Hex1Cer(d18:1_22:0+O)+H", "Cer(t34:1+O)+H-H2O",
            "Cer(d42:1+O)+CH3COO", "GM3(d40:1+O)+H", "Cer(t18:0_26:0+O)-H",
            "PC(32:0)+H", "CL(68:2)-2H", "AEA(15:1)+NH4",
            "Cer(d18:1/16:0)+H", "GM3(d18:1/14:0)-H"
        ),
        Class = c(
            "Hex1Cer", "Cer", "Cer", "GM3", "Cer",
            "PC", "CL", "AEA", "Cer", "GM3"
        ),
        FattyAcid = c(
            "(d18:1_22:0+O)", "(t18:1_16:0+O)", "(d18:1_24:0+O)",
            "(d18:1_22:0+O)", "(t18:0_26:0+O)", "(16:0_16:0)",
            "(18:1_16:0_16:0_18:1)", "(15:1)", "(d18:1/16:0)", "(d18:1/14:0)"
        ),
        CalcMz = c(
            800.66, 554.51, 728.64, 1251.81, 680.63,
            734.57, 1373.94, 301.28, 538.52, 1123.70
        ),
        Grade = c("A", "C", "B", "B", "A", "C", "C", "D", "A", "A")
    )

    M <- read_source(ls_source(source = write_ls_export(df), tag = "test"))

    expect_equal(M$data$LipidIon, df$LipidIon)
    expect_equal(
        M$data$LipidName,
        c(
            "Hex1Cer(d18:1_22:0+O)", "Cer(t34:1+O)", "Cer(d42:1+O)",
            "GM3(d40:1+O)", "Cer(t18:0_26:0+O)",
            # sum composition kept, not the chains in FattyAcid
            "PC(32:0)", "CL(68:2)", "AEA(15:1)",
            "Cer(d18:1/16:0)", "GM3(d18:1/14:0)"
        )
    )
})
