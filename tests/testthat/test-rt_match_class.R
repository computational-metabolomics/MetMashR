test_that("rt_match works", {
  
    
    obs=data.frame(
        id = letters[1:10],
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (5*1e-6*500) # true mz is 500
    
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(300,500,700)
    )
    
    AN = lcms_table(
            annotations = db,
            id_column='dbid',
            rt_column='rt',
            mz_column='mz'
        )
    
    M=rt_match(
        variable_meta = obs,
        rt_column = 'rt',
        rt_window = 2.5,
        id_column = 'id'
      )
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_true(all(abs(out$rt_match_diff)<=5,na.rm=TRUE))
    expect_setequal(out$dbid,c('A',rep('B',8),'C'))
    expect_setequal(out$rt_match_id,c(NA,letters[4:10],NA))
    
    
})

test_that("rt_match errors", {
    
    
    obs=data.frame(
        id = 1:10,
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (5*1e-6*500) # true mz is 500
    

    expect_error({M=rt_match(
        variable_meta = obs,
        rt_column = 'rt',
        rt_window = c(2,10),
        id_column = 'id'
    )})
    
    
})

test_that("rt_match rownames", {
    
    
    obs=data.frame(
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (5*1e-6*500) # true mz is 500
    rownames(obs)=letters[1:10]
    
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(300,500,700)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M=rt_match(
        variable_meta = obs,
        rt_column = 'rt',
        rt_window = 2.5,
        id_column = 'rownames'
    )
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_true(all(abs(out$rt_match_diff)<=5,na.rm=TRUE))
    expect_setequal(out$dbid,c('A',rep('B',8),'C'))
    expect_setequal(out$rt_match_id,c(NA,letters[4:10],NA))
})


test_that("rt_match overlap vmeta", {
    
    
    obs=data.frame(
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (5*1e-6*500) # true mz is 500
    rownames(obs)=letters[1:10]
    
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(300,500,700)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M=rt_match(
        variable_meta = obs,
        rt_column = 'rt',
        rt_window = c('variable_meta'=5,'annotations'=0),
        id_column = 'rownames'
    )
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_true(all(abs(out$rt_match_diff)<=5,na.rm=TRUE))
    expect_setequal(out$dbid,c('A',rep('B',8),'C'))
    expect_setequal(out$rt_match_id,c(NA,letters[4:10],NA))
})

test_that("rt_match overlap annotations", {
    
    
    obs=data.frame(
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (5*1e-6*500) # true mz is 500
    rownames(obs)=letters[1:10]
    
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(300,500,700)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M=rt_match(
        variable_meta = obs,
        rt_column = 'rt',
        rt_window = c('variable_meta'=0,'annotations'=5),
        id_column = 'rownames'
    )
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_true(all(abs(out$rt_match_diff)<=5,na.rm=TRUE))
    expect_setequal(out$dbid,c('A',rep('B',8),'C'))
    expect_setequal(out$rt_match_id,c(NA,letters[4:10],NA))
})