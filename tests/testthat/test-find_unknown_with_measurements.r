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
        #----------------------------------------------------------------------
        #   Test for unknowns for distant rows.
        #   This should not be detected because duplicated measurements of a
        #   stem observed in different quadrats can be caused by measurements
        #   with unknown (usually caused by wrong quadrat code in data) and
        #   correct measurements in other quadrat in a year.
        #----------------------------------------------------------------------
        test_data <- data.frame(
            year = rep(2022, 3),
            stem_id = c(1, 2, 1),
            ld = c("L", "L", "U"),
            gbh = c(15, 15, NA),
            cls = c("CAN", "CAN", NA),
            deprecated = c(NA, NA, NA)
        )
        r <- ogawa::find_unknown_with_measurements(
            test_data, deprecated_column = "deprecated"
        )
        testthat::expect_true(
            is.null(r), "Unknown at not continuous rows."
        )
    }
)
