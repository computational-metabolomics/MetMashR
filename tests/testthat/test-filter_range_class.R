test_that("filter_range works with limits and equal to", {
  
    df=data.frame(a=1:10,b=11:20,id=1:10)
    
    AN = annotation_table(annotations=df,id_column='id')
    
    M = filter_range(
        column_name = 'a',
        upper_limit=5,
        lower_limit=2,
        equal_to=TRUE
    )
    M=model_apply(M,AN)
    out=predicted(M)$annotations
    
    expect_setequal(out$a,c(3,4))
    
})

test_that("filter_range works with limits and not equal to", {
    
    df=data.frame(a=1:10,b=11:20,id=1:10)
    
    AN = annotation_table(annotations=df,id_column='id')
    
    M = filter_range(
        column_name = 'a',
        upper_limit=5,
        lower_limit=2,
        equal_to=FALSE
    )
    M=model_apply(M,AN)
    out=predicted(M)$annotations
    
    expect_setequal(out$a,c(2,3,4,5))
    
})


test_that("filter_range works with Inf", {
    
    df=data.frame(a=1:10,b=11:20,id=1:10)
    
    AN = annotation_table(annotations=df,id_column='id')
    
    M = filter_range(
        column_name = 'a',
        upper_limit=Inf,
        lower_limit=2,
        equal_to=FALSE
    )
    M=model_apply(M,AN)
    out=predicted(M)$annotations
    
    expect_setequal(out$a,2:10)
    
    M = filter_range(
        column_name = 'a',
        upper_limit=5,
        lower_limit=-Inf,
        equal_to=FALSE
    )
    M=model_apply(M,AN)
    out=predicted(M)$annotations
    
    expect_setequal(out$a,1:5)
    
})

test_that("filter_range works returns error if limits incorrect", {
    
    df=data.frame(a=1:10,b=11:20,id=1:10)
    
    AN = annotation_table(annotations=df,id_column='id')

    expect_error({
        M = filter_range(
            column_name = 'a',
            upper_limit=-1,
            lower_limit=2,
            equal_to=FALSE
    )},'upper_limit must be greater than the lower_limit')
    
    expect_error({
        M = filter_range(
            column_name = 'a',
            upper_limit=-1,
            lower_limit=mean,
            equal_to=FALSE
        )
        model_apply(M,AN)
        },'upper_limit must be greater than the lower_limit')
    
    expect_error({
        M = filter_range(
            column_name = 'a',
            upper_limit=mean,
            lower_limit=Inf,
            equal_to=FALSE
        )
        model_apply(M,AN)
        },'upper_limit must be greater than the lower_limit')
})

test_that("filter_range works if zero rows", {
    
    df=data.frame(a=numeric(0),b=numeric(0),id=numeric(0))
    
    AN = annotation_table(annotations=df,id_column='id')
    
        M = filter_range(
            column_name = 'a',
            upper_limit=3,
            lower_limit=2,
            equal_to=FALSE
    )
    
    M=model_apply(M,AN)
    out = predicted(M)$annotations
    
    expect_equal(nrow(out),0)
    
})

test_that("filter_range works using functions as limits", {
    
    df=data.frame(a=1:10,b=1:10,id=1:10)
    
    AN = annotation_table(annotations=df,id_column='id')
    
    M = filter_range(
        column_name = 'a',
        upper_limit=mean,
        lower_limit=-Inf,
        equal_to=TRUE
    )
    
    M=model_apply(M,AN)
    out = predicted(M)$annotations
    
    expect_true(all(out$a<mean(df$a)))
    
    M = filter_range(
        column_name = 'a',
        upper_limit=Inf,
        lower_limit=median,
        equal_to=TRUE
    )
    
    M=model_apply(M,AN)
    out = predicted(M)$annotations
    
    expect_true(all(out$a>median(df$a)))

})




