test_that("filter-na object can be created", {
    
    M = filter_na('a')
    expect_true(is(M,'filter_na'))
})

test_that("filter_na removes na rows", {
    
    df = data.frame(a=1:10,b=1:10,id=1:10)
    df[3,'a']=NA
    df[4,'b']=NA
    
    AN = annotation_table(
            annotations = df,
            tag='test',
            id_column = 'id')
    
    M = filter_na(column_name = 'a')
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    # check a row was removed
    expect_true(nrow(out)==9)
    # check the third row was removed
    expect_true(all(out$a != 3))
    # check none of column a are NA
    expect_false(all(is.na(out$a)))
    # check the NA in b remains
    expect_true(any(is.na(out$b)))
    
})

test_that("filter_na works when nothing present in table", {
    
    df = data.frame(a=numeric(0),b=numeric(0),id=character(0))
    
    
    AN = annotation_table(
        annotations = df,
        tag='test',
        id_column = 'id')
    
    M = filter_na(column_name = 'a')
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    # check nothing happened
    expect_true(nrow(out)==0)
    expect_equal(df,out)
    
})
