with_mock_dir('pc1', {
    test_that("pubchem api works", {
        db = data.frame(
            dbid = c('A','B','C'),
            rt = c(10,100,200),
            mz = c(499.99,500,500.01),
            kegg=c('no_hit','C00267',NA)
        )
        
        AN = lcms_table(
            data = db,
            id_column='dbid',
            rt_column='rt',
            mz_column='mz'
        )
        
        M = pubchem_compound_lookup(
            query_column = 'kegg',
            search_by = 'name',
            suffix=''
        )
        
        M = model_apply(M,AN)
        
        out=predicted(M)$data
        expect_setequal(out$CID,c(NA,79025,NA)) 
    })
})

with_mock_dir('pc2', {
    test_that("pubchem api works", {
        db = data.frame(
            dbid = c('A','B','C'),
            rt = c(10,100,200),
            mz = c(499.99,500,500.01),
            kegg=c('no_hit','C00267',NA)
        )
        
        AN = lcms_table(
            data = db,
            id_column='dbid',
            rt_column='rt',
            mz_column='mz'
        )
        
        M = pubchem_property_lookup(
            query_column = 'kegg',
            search_by = 'name',
            suffix=''
        )
        
        M = model_apply(M,AN)
        
        out=predicted(M)$data
        expect_setequal(out$CID,c(NA,79025,NA)) 
        expect_setequal(out$InChIKey,c(NA,'WQZGKKKJIJFFOK-DVKNGEFBSA-N',NA))
    })
})


