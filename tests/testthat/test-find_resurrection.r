#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_resurrection(): positive case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 4),
            ld = c("L", "D", "L", "L"),
            deprecated = rep(NA, 4),
            year = 2020:2023
        )
        r <- ogawa::find_resurrection(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(length(r) > 0)
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 4),
            ld = c("L", "U", "L", "L"),
            deprecated = rep(NA, 4),
            year = 2020:2023
        )
        r <- ogawa::find_resurrection(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(length(r) > 0)
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 4),
            ld = c("L", "D", "U", "L"),
            deprecated = rep(NA, 4),
            year = 2020:2023
        )
        r <- ogawa::find_resurrection(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(length(r) > 0)
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_resurrection(): negative case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 4),
            ld = c("L", "L", "L", "L"),
            deprecated = rep(NA, 4),
            year = 2020:2023
        )
        r <- ogawa::find_resurrection(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(length(r) == 0)
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 4),
            ld = c("L", "L", "L", "D"),
            deprecated = rep(NA, 4),
            year = 2020:2023
        )
        r <- ogawa::find_resurrection(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(length(r) == 0)
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 4),
            ld = c("L", "L", "U", "D"),
            deprecated = rep(NA, 4),
            year = 2020:2023
        )
        r <- ogawa::find_resurrection(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(length(r)  == 0)
    }
)
