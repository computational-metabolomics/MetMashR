test_that("id_lookup works", {
  
    df=data.frame(
            a=1:10,
            b=1:10,
            id=1:10
    )
    
    AN = annotation_table(annotations=df,id_column='id')
    
    db = data.frame(
            id=c(1,3,5),
            c = rep('cake',3),
            in_db = c(1,1,1)
    )
    
    M = database_lookup(
        query_column = 'id',
        database_column = 'id',
        database = db,
        include = NULL, # include all db columns
        suffix = '',     
        not_found = NA  # out found gets NA
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_true(all(c('c','in_db') %in% colnames(out)))
    expect_true(out[1,'c']=='cake')
    expect_true(out[3,'c']=='cake')
    expect_true(out[5,'c']=='cake')
    expect_true(out[1,'in_db']==1)
    expect_true(out[3,'in_db']==1)
    expect_true(out[5,'in_db']==1)
    expect_true(is.na(out[2,'in_db']))
    expect_true(is.na(out[4,'in_db']))
    expect_true(is.na(out[6,'in_db']))
    
    # check include columns
    M = database_lookup(
        query_column = 'id',
        database_column = 'id',
        database = db,
        include = 'c', # include c column only
        suffix = '',    
        not_found = NA  # out found gets NA
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    expect_true('c' %in% colnames(out))
    expect_false('in_db' %in% colnames(out))
    
    # check tag
    M = database_lookup(
        query_column = 'id',
        database_column = 'id',
        database = db,
        include = 'c', # include c column only
        suffix = '_tag', # tag db colnames
        not_found = NA  # out found gets NA
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_false('c' %in% colnames(out))
    expect_false('in_db' %in% colnames(out))
    expect_true('c_tag' %in% colnames(out))
    expect_false('in_db_tag' %in% colnames(out))
    
    # check not_found
    M = database_lookup(
        query_column = 'id',
        database_column = 'id',
        database = db,
        include = 'in_db', # include in_db column only
        suffix = NULL,     # dont tag db colnames
        not_found = 0  # out found gets 0
    )
    
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations

    
})
