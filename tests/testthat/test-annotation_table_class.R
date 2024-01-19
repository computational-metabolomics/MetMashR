
test_that("annotation_table can be created", {
    df=data.frame(a=1:10,b=1:10,id=1:10)
    AN = annotation_table(annotations=df,tag='test',id_column='id')

    expect_true(is(AN,'annotation_table'))
    expect_output(show(AN),regexp = 'A "annotation_table" object')
})
    
test_that("lcms_table can be created", {
    df=data.frame(a=1:10,b=1:10,id=1:10,rt=1:10,mz=1:10)
    AN = lcms_table(annotations=df,tag='test',
        id_column='id',
        mz_column='mz',
        rt_column='rt')
    
    expect_true(is(AN,'annotation_table'))
    expect_true(is(AN,'lcms_table'))
    expect_output({show(AN)},regexp = 'A "lcms_table" object')
})

test_that("annotation_table throws error if id column is missing", {
    df=data.frame(a=1:10,b=1:10,id=1:10)
    
    expect_error({AN = annotation_table(annotations=df,tag='test',id_column='cake')})
    
})

test_that("lcms_table throws error if named columns are missing", {
    df=data.frame(a=1:10,b=1:10,id=1:10,mz=1:10,rt=1:10)
    
    expect_error({    
        AN = lcms_table(annotations=df,tag='test',
            id_column='cake',
            mz_column='mz',
            rt_column='rt')})
    
    expect_error({    
        AN = lcms_table(annotations=df,tag='test',
            id_column='id',
            mz_column='cake',
            rt_column='rt')})
    
    expect_error({    
        AN = lcms_table(annotations=df,tag='test',
            id_column='id',
            mz_column='mz',
            rt_column='cake')})
    
})