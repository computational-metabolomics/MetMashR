#' @eval get_description('cd_source')
#' @include annotation_source_class.R lcms_table_class.R
#' @family {annotation sources}
#' @family {annotation tables}
#' @rawNamespace import(dplyr, except = as_data_frame)
#' @export
cd_source <- function(
        source,
        sheets = c(1, 1),
        tag = "CD",
        mz_column = "mz",
        rt_column = "rt",
        id_column = "id",
        data = NULL,
        ...) {
    
    if (is.null(data)) {
        data = data.frame()
    }
    
    if (nrow(data)==0 & ncol(data)==0) {
        data <- data.frame(
            id = character(0),
            mz = numeric(0),
            rt = numeric(0)
        )
        colnames(data) <- c(
            id_column,
            mz_column,
            rt_column
        )
    }
    
    # new object
    out <- new_struct(
        "cd_source",
        source = source,
        tag = tag,
        mz_column = mz_column,
        rt_column = rt_column,
        id_column = id_column,
        .required = c(mz_column, id_column, rt_column),
        data = data,
        ...
    )
    return(out)
}

.cd_source <- setClass(
    "cd_source",
    contains = c("lcms_table"),
    slots = c(
        sheets = "entity"
    ),
    prototype = list(
        source = entity(
            name = "CD source file(s)",
            description = paste0(
                "The path to the Compound Discoverer Excel files to import. ",
                "Both the compounds and isomers file should be included, in ",
                "that order."
            ),
            type = "character",
            max_length = 2,
        ),
        sheets = entity(
            name = "Sheet names",
            description = paste0(
                "The name or index of the sheets to read from the source ",
                "file(s). A sheet should be provided for each input file."
            ),
            value = c(2, 2),
            type = c("character", "numeric", "integer"),
            max_length = 2
        ),
        data = .set_entity_value(
            obj = "lcms_table",
            param_id = "data",
            value = data.frame(
                id = character(0),
                mz = character(0),
                rt = character(0)
            )
        ),
        id_column = .set_entity_value(
            obj = "lcms_table",
            param_id = "id_column",
            value = "id"
        ),
        mz_column = .set_entity_value(
            obj = "lcms_table",
            param_id = "mz_column",
            value = "mz"
        ),
        rt_column = .set_entity_value(
            obj = "lcms_table",
            param_id = "rt_column",
            value = "rt"
        ),
        .params = c("sheets")
    )
)



#' @export
#' @rdname read_source
setMethod(
    f = "read_source",
    signature = c("cd_source"),
    definition = function(obj) {
        M <- obj
        # read files
        TB1 <- .read_cd_compounds_file(M, 1)
        TB2 <- .read_cd_isomers_file(M, 2)
        
        # join blue and orange
        L <- list()
        L[[1]] <- left_join(TB1$blue, TB1$orange,
                            by = join_by(blue_id),
                            suffix = c(".blue", ".orange")
        )
        L[[2]] <- left_join(TB2$blue, TB2$orange,
                            by = join_by(blue_id),
                            suffix = c(".blue", ".orange")
        )
        
        # join grey
        L[[1]] <- left_join(L[[1]], TB1$grey, by = join_by(
            blue_id == blue_id,
            orange_id == orange_id
        ), suffix = c(".blueorange", ".grey"))
        
        # summarise
        L[[1]] <- L[[1]] %>%
            group_by(blue_id, Ion) %>%
            summarise(
                Compound = unique(Name),
                Formula = unique(Formula),
                RT = mean(as.numeric(.data[["RT [min].grey"]]) * 60,
                            na.rm = TRUE
                ),
                mzcloud_score = mean(as.numeric(mzCloud.Best.Match),
                                        na.rm = TRUE
                ),
                Charge = unique(Charge),
                mz = mean(as.numeric(.data[["m/z.grey"]]), na.rm = TRUE),
                area = max(Area),
                file_count = n()
            )
        
        # join compounds and isomers
        OUT <- left_join(L[[1]], L[[2]],
                        by = join_by(blue_id),
                        relationship = "many-to-many", suffix = c(".cpd", ".iso")
        )
        
        
        OUT <- OUT %>%
            # select relevant columns
            select(
                all_of(c(
                    compound = "Name.orange",
                    ion = "Ion",
                    formula = "Formula.orange",
                    mz = "mz",
                    rt = "RT",
                    file_count = "file_count",
                    area = "area",
                    kegg_id = "KEGG ID",
                    mzcloud_score = "Match",
                    mzcloud_confidence = "Confidence",
                    mzcloud_id = "mzCloud ID",
                    compound_match = "Compound Match",
                    library_ppm_diff = "DeltaMass [ppm]",
                    theoretical_mass = "Molecular Weight",
                    cd_id = "blue_id",
                    class = "Compound Class",
                    reference_ion = "Reference.Ion"
                ))
            ) %>%
            # convert some to numeric
            mutate(
                across(c(
                    "mz",
                    "rt",
                    "file_count",
                    "area",
                    "mzcloud_score",
                    "mzcloud_confidence",
                    "library_ppm_diff",
                    "theoretical_mass"
                ), as.numeric)
            )
        
        # calc theoretical mz
        OUT$theoretical_mz <- OUT$mz * 1e6 /
            (OUT$library_ppm_diff + 1e6)
        
        # id row id
        OUT$id <- as.character(seq_len(nrow(OUT)))
        
        # update object
        M$data <- as.data.frame(OUT)
        
        # return
        return(M)
    }
)


.read_cd_isomers_file <- function(M, idx) {
    # COMP FILE
    input_file <- M$source[idx]
    sheet <- M$sheets[idx]
    
    cd <- openxlsx::read.xlsx(
        input_file,
        sheet
    )
    
    # add some ids for joining later
    cd$blue_id <- NA
    cd$orange_id <- NA
    
    for (k in seq_len(nrow(cd))) {
        n <- sum(cd$Checked[seq_len(k)] %in% c("TRUE", "FALSE"), na.rm = TRUE)
        cd$blue_id[k] <- n
        n <- sum(cd$Name[seq_len(k)] %in% c("TRUE", "FALSE"), na.rm = TRUE)
        cd$orange_id[k] <- n
    }
    
    ## blue rows
    blue <- cd %>% filter(Checked == "FALSE")
    
    ## orange rows
    # find colnames
    w <- which(cd$Checked == "Tags")
    x <- which(cd[w[1], ] != "")
    x <- unlist(cd[w[1], x])
    x[names(x) == "blue_id"] <- "blue_id"
    x[names(x) == "orange_id"] <- "orange_id"
    x <- setNames(names(x), x)
    # filter
    orange <- cd %>%
        select(all_of(x)) %>%
        filter(Checked %in% c("TRUE", "FALSE"))
    
    return(
        list(
            blue = blue,
            orange = orange
        )
    )
}


.read_cd_compounds_file <- function(M, idx) {
    cd <- openxlsx::read.xlsx(
        M$source[idx],
        M$sheets[idx]
    )
    
    # add some ids for joining later
    cd$blue_id <- NA
    cd$orange_id <- NA
    
    for (k in seq_len(nrow(cd))) {
        n <- sum(cd$Checked[seq_len(k)] %in% c("TRUE", "FALSE"), na.rm = TRUE)
        cd$blue_id[k] <- n
        n <- sum(cd$Name[seq_len(k)] %in% c("TRUE", "FALSE"), na.rm = TRUE)
        cd$orange_id[k] <- n
    }
    
    ## blue rows
    blue <- cd %>%
        filter(Checked == "FALSE") %>%
        select(-orange_id)
    
    ## orange rows
    # find colnames
    w <- which(cd$Checked == "Tags")
    x <- which(cd[w[1], ] != "")
    x <- unlist(cd[w[1], x])
    x[names(x) == "blue_id"] <- "blue_id"
    x[names(x) == "orange_id"] <- "orange_id"
    x <- setNames(names(x), x)
    # filter
    orange <- cd %>%
        select(all_of(x)) %>%
        filter(Checked %in% c("TRUE", "FALSE"))
    
    ## grey rows
    # find colnames
    w <- which(cd$Name == "Tags")
    x <- which(cd[w[1], ] != "")
    x <- unlist(cd[w[1], x])
    x[names(x) == "blue_id"] <- "blue_id"
    x[names(x) == "orange_id"] <- "orange_id"
    x <- setNames(names(x), x)
    # filter
    grey <- cd %>%
        select(all_of(x)) %>%
        filter(Checked == "FALSE")
    
    return(
        list(
            blue = blue,
            orange = orange,
            grey = grey
        )
    )
}
