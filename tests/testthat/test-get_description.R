test_that("get description works", {
  
    out = MetMasheR:::get_description('combine_records')
    expect_equal(out[1],"@title Combine annotation records (rows)")
    
    out = MetMasheR:::get_description('filter_levels')
    expect_equal(out[1],"@title Filter by factor levels")
})
