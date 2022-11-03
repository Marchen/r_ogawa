#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_missing_ids(): positive case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = c(1, 3:5),
            ind_id = c(1, 3:5)
        )
        r <- ogawa::find_missing_ids(test_data)
        testthat::expect_true(length(r$stem_id) == 1)
        testthat::expect_true(length(r$ind_id) == 1)
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_missing_ids(): negative case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = c(1:5),
            ind_id = c(1:5)
        )
        r <- ogawa::find_missing_ids(test_data)
        testthat::expect_true(length(r$stem_id) == 0)
        testthat::expect_true(length(r$ind_id) == 0)
    }
)
