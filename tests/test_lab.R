library(testthat)
lab_file = file.path('..', 'lab.R')
source(lab_file, chdir = TRUE)



test_that("1. Dimensions of `ri_df`", {
    expect_identical(dim(ri_df), c(25L, 25L))
})

test_that("2.1. Contents of `dates`", {
    dates_ref = structure(c("43109", "08/15/2018", "42745", "41648", "41648", 
                            "03/15/2017", 
                            "42738", "42373", "37992", "38724", "42743", 
                            "40912", "40182", 
                            "39816", "02/15/2007", "03/15/2019", "03/15/2019",
                            "03/15/2017", 
                            "40918", "38237", "39091", "43103", "41283", 
                            "42011", "40183"))
    expect_identical(dates, !!dates_ref)
})

test_that("2.2. First pass through `as.Date()`", {
    pass1_ref = structure(c(17540, NA, 17176, 16079, 16079, NA, 17169, 16804, 
                            12423, 13155, 17174, 15343, 14613, 14247, NA, NA, 
                            NA, NA, 15349, 
                            12668, 13522, 17534, 15714, 16442, 14614), 
                          class = "Date")
    expect_identical(int_dates1, !!pass1_ref)
})


test_that("2.3. Second pass through `as.Date()`", {
    pass2_ref = structure(c(17775, NA, 17440, 16314, 16314, NA, 17226, 16892, 
                            12570, 13330, 17379, 15431, 14700, 14304, NA, NA, 
                            NA, NA, 15614, 
                            12608, 13757, 17591, 15949, 16617, 14730), 
                          class = "Date")
    expect_identical(int_dates2, !!pass2_ref)
})

test_that("3. Dates formatted like '08/15/2018'", {
    mdy_ref = structure(c(NA, 17758, NA, NA, NA, 17240, NA, NA, NA, NA, NA, 
                          NA, NA, NA, 13559, 17970, 17970, 17240, NA, NA, NA, 
                          NA, NA, NA, 
                          NA), class = "Date")
    expect_identical(mdy_dates, !!mdy_ref)
})

test_that("4.1. Combination of the two approaches in parsed_dates", {
    parsed_dates_ref = structure(c(17775, 17758, 17440, 16314, 16314, 17240,
                                   17226, 
                                   16892, 12570, 13330, 17379, 15431, 14700,
                                   14304, 13559, 17970, 
                                   17970, 17240, 15614, 12608, 13757, 17591,
                                   15949, 16617, 14730), 
                                 class = "Date")
    expect_identical(parsed_dates, !!parsed_dates_ref)
})

test_that("4.2. Tests on `parse_dates()`", {
    int_err_date = as.integer(as.Date('1900-01-02') - as.Date('1899-12-30'))
    expect_identical(parse_dates(!!int_err_date), as.Date('1900-02-01'))
    expect_identical(parse_dates('05/15/2019'), as.Date('2019-05-15'))
})

test_that("5.2. Length of `data_files`", {
    expect_identical(length(data_files), 3L)
})

test_that("5.3. Dimensions of `cleaned_df`", {
    expect_identical(dim(cleaned_df), c(3691L, 25L))
})