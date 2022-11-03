#==============================================================================
testthat::test_that(
    "Test find_unknown_with_measurement(): positive cases.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            year = 2002,
            stem_id = 1,
            ld = "U",
            gbh = 15.1,
            cls = "CAN",
            deprecated = NA
        )
        r <- ogawa::find_unknown_with_measurements(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(nrow(r) > 0, "Unknown in active row.")
        #----------------------------------------------------------------------
        test_data <- data.frame(
            year = rep(2022, 2),
            stem_id = rep(1, 2),
            ld = c("U", "L"),
            gbh = rep(15.1, 2),
            cls = rep(NA, 2),
            deprecated = c(1, NA)
        )
        r <- ogawa::find_unknown_with_measurements(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(nrow(r) > 0, "Unknown in deprecated row.")
    }
)

#==============================================================================
testthat::test_that(
    "Test find_unknown_with_measurement(): negative cases.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            year = 2002,
            stem_id = 1,
            ld = "L",
            gbh = 15.1,
            cls = "CAN",
            deprecated = NA
        )
        r <- ogawa::find_unknown_with_measurements(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(is.null(r), "No unknown in active row.")
        #----------------------------------------------------------------------
        test_data <- data.frame(
            year = rep(2022, 2),
            stem_id = rep(1, 2),
            ld = c("U", "L"),
            gbh = rep(NA, 2),
            cls = rep(NA, 2),
            deprecated = c(1, NA)
        )
        r <- ogawa::find_unknown_with_measurements(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(
            is.null(r), "Unknown in deprecated row, but no measurements."
        )
    }
)
