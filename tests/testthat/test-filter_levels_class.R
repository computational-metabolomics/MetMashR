test_that("filter_levels_works", {
  
    df = data.frame(
            a = c(rep('A',3),rep('B',3),rep('C',4)),
            b = 1:10,
            id = 1:10
        )
    
    AN = annotation_table(annotations = df, id_column='id')
    
    M = filter_levels(
        column_name = 'a',
        levels = 'B',
        mode = 'include'
    )
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_true(nrow(out)==3)
    expect_true(all(out$a=='B'))
    
    M = filter_levels(
        column_name = 'a',
        levels = 'B',
        mode = 'exclude'
    )
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_setequal(out$a,c(rep('A',3),rep('C',4)))
    
})




test_that("filter_levels_works with empty data.frame", {
    
    df = data.frame(
        a = numeric(0),
        b = numeric(0),
        id = numeric(0)
    )
    
    AN = annotation_table(annotations = df, id_column='id')
    
    M = filter_levels(
        column_name = 'a',
        levels = 'B',
        mode = 'include'
    )
    M = model_apply(M,AN)
    
    out = predicted(M)$annotations
    
    expect_true(nrow(out)==0)
   
})

