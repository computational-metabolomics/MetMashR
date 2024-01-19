test_that("mzrt_match works", {
  
    obs=data.frame(
        id = letters[1:10],
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (obs$ppm*1e-6*500) # true mz is 500
    
    db =data.frame(
        dbid = c('A','B','C'),
        rt = c(10,100,200),
        mz = c(499.99,500,500.01)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M=mzrt_match(
        variable_meta = obs,
        mz_column = 'mz',
        ppm_window = 2.5,
        id_column = 'id',
        rt_column = 'rt',
        rt_window = 2.5
    )
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),1) # expect one result on both RT and MZ
    expect_equal(out$dbid[1],'B') # should be annotation B
    expect_equal(out$mzrt_match_id,'j') # should match peak j
    
    
})

test_that("mzrt_match errors", {
    
    
    obs=data.frame(
        id = 1:10,
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (5*1e-6*500) # true mz is 500
    
    
    expect_error({
        M=mzrt_match(
            variable_meta = obs,
            mz_column = 'mz',
            ppm_window = c(2.5,2.5),
            id_column = 'id',
            rt_column = 'rt',
            rt_window = 2.5
        )
    },regexp = 'If providing two ppm windows then the vector must be named')
    
    expect_error({
        M=mzrt_match(
            variable_meta = obs,
            mz_column = 'mz',
            ppm_window = 2.5,
            id_column = 'id',
            rt_column = 'rt',
            rt_window = c(2.5,2.5)
        )
    },regexp = 'If providing two retention time windows then the vector must be named')
})

test_that("mzrt_match works with no rows", {
    
    obs=data.frame(
        id = letters[1:10],
        rt = 100 + c(-10,-10,-10,-3,-3,-3,3,3,3,5),
        ppm= c(2,2,2,5,5,5,8,8,8,3)
    )
    obs$mz = 500 - (obs$ppm*1e-6*500) # true mz is 500
    
    db =data.frame(
        dbid = numeric(0),
        rt = numeric(0),
        mz = numeric(0)
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M=mzrt_match(
        variable_meta = obs,
        mz_column = 'mz',
        ppm_window = 2.5,
        id_column = 'id',
        rt_column = 'rt',
        rt_window = 2.5
    )
    M = model_apply(M,AN)
    
    out=predicted(M)$annotations
    
    expect_equal(nrow(out),0) # expect zero results on both RT and MZ
    expect_setequal(colnames(out),c("dbid","rt","mz","mz_match_diff","ppm_match_diff_an","ppm_match_diff_vm",
        "rt_match_diff","mzrt_match_id","mzrt_match_score"))
    
    
})