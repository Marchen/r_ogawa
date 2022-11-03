#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_multiple_measurements(): positive case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 2),
            year = rep(2022, 2),
            deprecated = rep(NA, 2)
        )
        r <- ogawa::find_multiple_measurements(
            test_data, deprecated_column = "deprecated", id_columns = "stem_id"
        )
        expect_s3_class(r$stem_id, "data.frame")
        expect_true(nrow(r$stem_id) > 1)
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_multiple_measurements(): negative case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 2),
            year = rep(2022, 2),
            deprecated = c(1, NA)
        )
        r <- ogawa::find_multiple_measurements(
            test_data, deprecated_column = "deprecated", id_columns = "stem_id"
        )
        expect_true(length(r) == 0, "deprecated")
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = rep(1, 2),
            year = 2021:2022,
            deprecated = rep(NA, 2)
        )
        r <- ogawa::find_multiple_measurements(
            test_data, deprecated_column = "deprecated", id_columns = "stem_id"
        )
        expect_true(length(r) == 0, "no multiple measurement")
    }
)
