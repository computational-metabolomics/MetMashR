test_that("kegg_lookup queries ok", {
    
    db=data.frame(
        'id'=c(1,2,3),
        'pubchem_sid' = c(3937,3938,1)
    )
    
    D = annotation_table(annotations = db,id_column='id')
    
    
    M = kegg_lookup(
        get='compound',
        from='pubchem',
        query_column = 'pubchem_sid',
        suffix = ''
    )
    
    with_mock_dir('kg0',{
        M = model_apply(M,D)
    })
    
    out = predicted(M)$annotations
    
    expect_equal(colnames(out)[3],'compound')
    expect_equal(out$compound[1],'C00668')
    expect_true(is.na(out$compound[3]))
})

test_that("kegg_lookup errors",{
    expect_error({
        M = kegg_lookup(
            get='compound',
            from='drug',
            query_column = 'pubchem_sid',
            suffix= ''
        )
    })
    
    expect_error({
        M = kegg_lookup(
            get='pubchem',
            from='chebi',
            query_column = 'pubchem_sid',
            suffix = ''
        )
    })
})

test_that("kegg_lookup works correctly when there are no hits",{
    db=data.frame(
        'id'=c(1,2,3),
        'pubchem_sid' = c(1,1,1)
    )
    
    D = annotation_table(annotations = db,id_column='id')
    
    M = kegg_lookup(
        get='compound',
        from='pubchem',
        query_column = 'pubchem_sid',
        suffix = ''
    )
    
    with_mock_dir('kg2',{
        M = model_apply(M,D)
    })
    
    
    out = predicted(M)$annotations
    
    expect_true(all(is.na(out$compound)))
})

test_that("kegg_lookup works correctly when there are no annotations",{
    db=data.frame(
        'id'=character(0),
        'pubchem_sid' = character(0)
    )
    
    D = annotation_table(annotations = db,id_column='id')
    
    M = kegg_lookup(
        get='compound',
        from='pubchem',
        query_column = 'pubchem_sid',
        suffix = ''
    )
    
    with_mock_dir('kg3',{
        M = model_apply(M,D)
    })
    
    
    out = predicted(M)$annotations
    
    expect_equal(nrow(out),0)
    expect_equal(ncol(out),3)
    expect_equal(colnames(out)[3],'compound')
})

