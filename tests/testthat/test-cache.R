db =data.frame(
    dbid = c('A','B','C','D','E'),
    rt = c(10,100,200,1,1),
    mz = c(499.99,500,500.01,1,1),
    search=c(
        'WQZGKKKJIJFFOK-GASJEMHNSA-N', # glucose
        'YCGXMNQNHBKSKZ-YNWZBRBLSA-N', # Valclavam
        'IPCSVZSSVZVIGE-UHFFFAOYSA-N', # palmitic acid
        'GABNFKJEHDJCHD-DHFSJAKDHC-J',  # spoof, no hits
        'XLYOFNOQVPJJNP-UHFFFAOYSA-N'  # water
    )
)


test_that("rds cache reads/writes", {
    
    # prep cache object
    C = rds_database(path=tempfile(fileext = 'rds'))
    
    # write to cache
    write_database(C,db)
    
    # read cahce
    check=read_database(C)
    
    # compare
    expect_true(all(check==db))
})

test_that("sqlite cache reads/writes", {
    # prep cache object
    C = sqlite_database(path=tempfile(fileext = 'db'),table = 'test')
    
    # write to cache
    write_database(C,db)
    
    # read from cache
    check=read_database(C)
    
    # compare
    expect_true(all(check==db))
})





