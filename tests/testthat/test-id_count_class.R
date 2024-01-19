test_that('id count works', {
    df = data.frame(a=1:10,b=1:10,id=c(1,1,1,1,1,2,2,2,3,3))
    
    
    AN = annotation_table(
        annotations = df,
        tag='test',
        id_column = 'id')
    
    M = id_counts(id_column='id',count_column='id_counts')
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    # check new column
    expect_true("id_counts" %in% colnames(out))
    # check counts for each id
    expect_true(all(out$id_counts[out$id==1]==5))
    expect_true(all(out$id_counts[out$id==2]==3))
    expect_true(all(out$id_counts[out$id==3]==2))
})

test_that('id count works with empty table', {
    df = data.frame(a=numeric(0),b=numeric(0),id=numeric(0))
    
    
    AN = annotation_table(
        annotations = df,
        tag='test',
        id_column = 'id')
    
    M = id_counts(id_column='id',count_column='id_counts')
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    # check new column
    expect_true("id_counts" %in% colnames(out))
    # check still no rows
    expect_true(nrow(out)==0)
})

test_that('id count works with NA', {
    df = data.frame(a=1:10,b=1:10,id=c(NA,1,1,1,1,2,2,2,3,3))
    
    
    AN = annotation_table(
        annotations = df,
        tag='test',
        id_column = 'id')
    
    # count_na = TRUE
    M = id_counts(id_column='id',count_column='id_counts',count_na=TRUE)
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    # check new column
    expect_true("id_counts" %in% colnames(out))
    # check counts for each id
    expect_true(all(out$id_counts[out$id==1]==4,na.rm = TRUE))
    expect_true(all(out$id_counts[out$id==2]==3,na.rm = TRUE))
    expect_true(all(out$id_counts[out$id==3]==2,na.rm = TRUE))
    expect_true(all(out$id_counts[is.na(out$id)]==1,na.rm = TRUE))
    
    # count_na = FALSE
    M = id_counts(id_column='id',count_column='id_counts',count_na=FALSE)
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    # check new column
    expect_true("id_counts" %in% colnames(out))
    # check counts for each id
    expect_true(all(out$id_counts[out$id==1]==4,na.rm = TRUE))
    expect_true(all(out$id_counts[out$id==2]==3,na.rm = TRUE))
    expect_true(all(out$id_counts[out$id==3]==2,na.rm = TRUE))
    expect_true(all(is.na(out$id_counts[is.na(out$id)]),na.rm = TRUE))
    
})


