test_that("combine sources works", {
  
    df1 = data.frame(
            id=1:10,
            source='A',
            aa = 1:10,
            b=1:10,
            c = 1:10
    )
    
    AN1 = annotation_table(df1,tag='A',id_column='id')
    
    
    df2 = data.frame(
            id=11:20,
            source='B',
            a = 11:20,
            b=11:20,
            d=11:20
    )
    
    AN2 = annotation_table(df2,tag='B',id_column='id')
    
    M = combine_tables(
            source_list = AN2,
            matching_columns = list('a'=c('a','aa')),
            keep_cols = c('c','d'),
            tag_ids = FALSE,
            source_col = 'annotation_table'
        )
    
    M = model_apply(M,AN1)
    
    out=predicted(M)$annotations
    
    expect_true('annotation_table' %in% colnames(out)) # check source column present
    expect_setequal(out$annotation_table,out$source) # check source column has expected values
    expect_false('aa' %in% colnames(out)) # "aa" column should not be present
    expect_setequal(out$a,c(df2$a,df1$aa)) # "aa" column merged into "a" column
    expect_true('c' %in% colnames(out)) # "c" column is present
    expect_setequal(out$c,c(rep(NA,10),df1$c)) # "c" has NA where not in source
    expect_true(all(is.na(out$c[out$annotation_table=='B']))) # "c" is NA for source B
    expect_true('d' %in% colnames(out)) # "d" column is present
    expect_setequal(out$d,c(df2$d,rep(NA,10))) # "d" has NA where not in source
    expect_true(all(is.na(out$d[out$annotation_table=='A']))) # "d" is NA for source B
    
})


test_that("combine sources works for list input", {
    
    df1 = data.frame(
        id=1:10,
        source='A',
        aa = 1:10,
        b=1:10,
        c = 1:10
    )
    
    AN1 = annotation_table(df1,tag='A',id_column='id')
    
    
    df2 = data.frame(
        id=11:20,
        source='B',
        a = 11:20,
        b=11:20,
        d=11:20
    )
    
    AN2 = annotation_table(df2,tag='B',id_column='id')
    
    M = combine_tables(
        source_list = list(),
        matching_columns = list('a'=c('a','aa')),
        keep_cols = c('c','d'),
        tag_ids = FALSE,
        source_col = 'annotation_table'
    )
    
    M = model_apply(M,list(AN1,AN2))
    
    out=predicted(M)$annotations
    
    expect_true('annotation_table' %in% colnames(out)) # check source column present
    expect_setequal(out$annotation_table,out$source) # check source column has expected values
    expect_false('aa' %in% colnames(out)) # "aa" column should not be present
    expect_setequal(out$a,c(df2$a,df1$aa)) # "aa" column merged into "a" column
    expect_true('c' %in% colnames(out)) # "c" column is present
    expect_setequal(out$c,c(rep(NA,10),df1$c)) # "c" has NA where not in source
    expect_true(all(is.na(out$c[out$annotation_table=='B']))) # "c" is NA for source B
    expect_true('d' %in% colnames(out)) # "d" column is present
    expect_setequal(out$d,c(df2$d,rep(NA,10))) # "d" has NA where not in source
    expect_true(all(is.na(out$d[out$annotation_table=='A']))) # "d" is NA for source B
    
})


test_that("combine sources errors", {
    
    expect_error({
        M = combine_tables(
            source_list = list(1,'a'),
            matching_columns = NULL,
            keep_cols = NULL,
            tag_ids = FALSE,
            source_col = 'annotation_table'
        )
    },regexp = 'all source_list items must be annotation_table objects.'
    )
    
})


test_that("combine sources works with tags", {
    
    df1 = data.frame(
        id=1:10,
        source='A',
        aa = 1:10,
        b=1:10,
        c = 1:10
    )
    
    AN1 = annotation_table(df1,tag='A',id_column='id')
    
    
    df2 = data.frame(
        id=11:20,
        source='B',
        a = 11:20,
        b=11:20,
        d=11:20
    )
    
    AN2 = annotation_table(df2,tag='B',id_column='id')
    
    M = combine_tables(
        source_list = AN2,
        matching_columns = list('a'=c('a','aa')),
        keep_cols = c('c','d'),
        tag_ids = TRUE,
        source_col = 'annotation_table'
    )
    
    M = model_apply(M,AN1)
    
    out=predicted(M)$annotations
    
    expect_true('annotation_table' %in% colnames(out)) # check source column present
    expect_setequal(out$annotation_table,out$source) # check source column has expected values
    expect_false('aa' %in% colnames(out)) # "aa" column should not be present
    expect_setequal(out$a,c(df2$a,df1$aa)) # "aa" column merged into "a" column
    expect_true('c' %in% colnames(out)) # "c" column is present
    expect_setequal(out$c,c(rep(NA,10),df1$c)) # "c" has NA where not in source
    expect_true(all(is.na(out$c[out$annotation_table=='B']))) # "c" is NA for source B
    expect_true('d' %in% colnames(out)) # "d" column is present
    expect_setequal(out$d,c(df2$d,rep(NA,10))) # "d" has NA where not in source
    expect_true(all(is.na(out$d[out$annotation_table=='A']))) # "d" is NA for source B
    expect_true(all(c(paste0('A_',1:10),paste0('B_',11:20)) %in% out$id)) # all tagged ids are present
})

test_that("combine sources warnings", {
    
    df1 = data.frame(
        id=1:10,
        source='A',
        aa = 1:10,
        b=1:10,
        c = 1:10,
        a = 21:30
    )
    
    AN1 = annotation_table(df1,tag='A',id_column='id')
    
    
    df2 = data.frame(
        id=11:20,
        source='B',
        a = 11:20,
        b=11:20,
        d=11:20
    )
    
    AN2 = annotation_table(df2,tag='B',id_column='id')
    
    M = combine_tables(
        source_list = AN2,
        matching_columns = list('a'=c('a','aa')),
        keep_cols = c('c','d'),
        tag_ids = TRUE,
        source_col = 'annotation_table'
    )
    
    expect_warning({
        M = model_apply(M,AN1)
    },
        regexp = 'combine_annotations: more than one matching column for \"a\". Column \"aa\" will be used'
    )
    
    out=predicted(M)$annotations
    
    expect_true('annotation_table' %in% colnames(out)) # check source column present
    expect_setequal(out$annotation_table,out$source) # check source column has expected values
    expect_false('aa' %in% colnames(out)) # "aa" column should not be present
    expect_setequal(out$a,c(df2$a,df1$aa)) # "aa" column merged into "a" column
    expect_true('c' %in% colnames(out)) # "c" column is present
    expect_setequal(out$c,c(rep(NA,10),df1$c)) # "c" has NA where not in source
    expect_true(all(is.na(out$c[out$annotation_table=='B']))) # "c" is NA for source B
    expect_true('d' %in% colnames(out)) # "d" column is present
    expect_setequal(out$d,c(df2$d,rep(NA,10))) # "d" has NA where not in source
    expect_true(all(is.na(out$d[out$annotation_table=='A']))) # "d" is NA for source B
    expect_true(all(c(paste0('A_',1:10),paste0('B_',11:20)) %in% out$id)) # all tagged ids are present
})

test_that("combine sources works for lcms_tables", {
    
    df1 = data.frame(
        id=1:10,
        source='A',
        aa = 1:10,
        b=1:10,
        c = 1:10,
        rt=1:10,
        mz=1:10
    )
    
    AN1 = lcms_table(df1,tag='A',id_column='id',rt_column='rt',mz_column='mz')
    
    
    df2 = data.frame(
        id=11:20,
        source='B',
        a = 11:20,
        b=11:20,
        d=11:20,
        rt=1:10,
        mz=1:10
    )
    
    AN2 = lcms_table(df2,tag='B',id_column='id',rt_column='rt',mz_column='mz')
    
    M = combine_tables(
        source_list = AN2,
        matching_columns = list('a'=c('a','aa')),
        keep_cols = c('c','d'),
        tag_ids = FALSE,
        source_col = 'annotation_table'
    )
    
    M = model_apply(M,AN1)
    
    out=predicted(M)$annotations
    
    expect_true('annotation_table' %in% colnames(out)) # check source column present
    expect_setequal(out$annotation_table,out$source) # check source column has expected values
    expect_false('aa' %in% colnames(out)) # "aa" column should not be present
    expect_setequal(out$a,c(df2$a,df1$aa)) # "aa" column merged into "a" column
    expect_true('c' %in% colnames(out)) # "c" column is present
    expect_setequal(out$c,c(rep(NA,10),df1$c)) # "c" has NA where not in source
    expect_true(all(is.na(out$c[out$annotation_table=='B']))) # "c" is NA for source B
    expect_true('d' %in% colnames(out)) # "d" column is present
    expect_setequal(out$d,c(df2$d,rep(NA,10))) # "d" has NA where not in source
    expect_true(all(is.na(out$d[out$annotation_table=='A']))) # "d" is NA for source B
    expect_true(all(c('mz','rt') %in% colnames(out))) # mz and rt columns should be present for lcms
    expect_true(is(predicted(M),'lcms_table')) # expect lcms_table for merged output
    
})

test_that("combine sources works with .all and exclude_cols", {
    
    df1 = data.frame(
        id=1:10,
        source='A',
        aa = 1:10,
        b=1:10,
        c = 1:10,
        rt=1:10,
        mz=1:10
    )
    
    AN1 = lcms_table(df1,tag='A',id_column='id',rt_column='rt',mz_column='mz')
    
    
    df2 = data.frame(
        id=11:20,
        source='B',
        a = 11:20,
        b=11:20,
        d=11:20,
        rt=1:10,
        mz=1:10
    )
    
    AN2 = lcms_table(df2,tag='B',id_column='id',rt_column='rt',mz_column='mz')
    
    M = combine_tables(
        source_list = AN2,
        matching_columns = list('a'=c('a','aa')),
        keep_cols = c('.all'),
        tag_ids = FALSE,
        source_col = 'annotation_table',
        exclude_cols=c('c')
    )
    
    M = model_apply(M,AN1)
    
    out=predicted(M)$annotations
    
    expect_true('annotation_table' %in% colnames(out)) # check source column present
    expect_setequal(out$annotation_table,out$source) # check source column has expected values
    expect_false('aa' %in% colnames(out)) # "aa" column should not be present
    expect_setequal(out$a,c(df2$a,df1$aa)) # "aa" column merged into "a" column
    expect_false('c' %in% colnames(out)) # "c" column is NOT present
    expect_true(all(is.na(out$c[out$annotation_table=='B']))) # "c" is NA for source B
    expect_true('d' %in% colnames(out)) # "d" column is present
    expect_setequal(out$d,c(df2$d,rep(NA,10))) # "d" has NA where not in source
    expect_true(all(is.na(out$d[out$annotation_table=='A']))) # "d" is NA for source B
    expect_true(all(c('mz','rt') %in% colnames(out))) # mz and rt columns should be present for lcms
    expect_true(is(predicted(M),'lcms_table')) # expect lcms_table for merged output
    
})

