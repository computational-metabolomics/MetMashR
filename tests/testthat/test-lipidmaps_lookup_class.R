test_that('lipidmaps_api search works', {
    db =data.frame(
        dbid = c('A','B','C','D'),
        rt = c(10,100,200,300),
        mz = c(499.99,500,500.01,600),
        lipid=c('nal1','TG(16:0_16:1_18:2)','nal2','PE(16:0_18:1)')
    )
    
    AN = lcms_table(
        annotations = db,
        id_column='dbid',
        rt_column='rt',
        mz_column='mz'
    )
    
    M = lipidmaps_lookup(
        query_column  = 'lipid',
        context = 'compound',
        context_item = 'abbrev_chains',
        output_item=c('inchi_key','hmdb_id'),
        suffix=''
    )

    with_mock_dir('lp',{
        M = model_apply(M,AN)
    })
    
    out=predicted(M)$annotations
    
    expect_true(out$inchi_key[2]=='HDFLQJUGWGNORO-BJPYDGQASA-N')
    expect_true(out$hmdb_id[2]=='HMDB05379')
    expect_true(is.na(out$inchi_key[1]))
    expect_true(is.na(out$inchi_key[3]))
    expect_true(is.na(out$hmdb_id[1]))
    expect_true(is.na(out$hmdb_id[3]))
    expect_true(all(out$dbid[4:7]=='D'))
    
    expect_error({
        M = lipidmaps_lookup(
            query_column  = 'lipid',
            context = 'not-a-context',
            context_item = 'abbrev_chains',
            output_item=c('inchi_key','hmdb_id'),
            suffix=''
        )
    })
    
    expect_error({
        M = lipidmaps_lookup(
            query_column  = 'lipid',
            context = 'compound',
            context_item = 'not-an-item',
            output_item=c('inchi_key','hmdb_id'),
            suffix=''
        )
    })
    
    expect_error({
        M = lipidmaps_lookup(
            query_column  = 'lipid',
            context = 'compound',
            context_item = 'abbrev_chains',
            output_item=c('inchi_key','not-an-output'),
            suffix=''
        )
    })

})


