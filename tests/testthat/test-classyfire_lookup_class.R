test_that("classyfire_lookup queries ok", {
    
    db=data.frame(
        'id'=c(1,2,3,4,5),
        'inchikey' = c(
            'WQZGKKKJIJFFOK-GASJEMHNSA-N', # glucose
            'YCGXMNQNHBKSKZ-YNWZBRBLSA-N', # Valclavam
            'IPCSVZSSVZVIGE-UHFFFAOYSA-N', # palmitic acid
            'GABNFKJEHDJCHD-DHFSJAKDHC-J',  # spoof, no hits
            'XLYOFNOQVPJJNP-UHFFFAOYSA-N'  # water
        )
    )
    
    D = annotation_table(data = db,id_column='id')
    
    
    M = classyfire_lookup(
        query_column = 'inchikey',
        output_items = '.all',
        output_fields = 'name',
        suffix =''
    )
    
    with_mock_dir('cf1',{
        M = model_apply(M,D)
    })
    
    out = predicted(M)$data
    
    expect_equal(colnames(out)[3],'kingdom.name')
    expect_equal(out$inchikey[1],'WQZGKKKJIJFFOK-GASJEMHNSA-N')
    expect_equal(out$kingdom.name[1],'Organic compounds')
    expect_true(is.na(out$kingdom.name[4]))
    expect_equal(out$kingdom.name[5],'Inorganic compounds')
    
})


