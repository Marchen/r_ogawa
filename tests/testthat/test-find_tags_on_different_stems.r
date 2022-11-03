#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_tags_on_different_stems(): positive case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = c(1, 2),
            tag_no = rep("A1", 2),
            deprecated = rep(NA, 2),
            year = 2020:2021
        )
        r <- find_tags_on_different_stems(
            test_data, tag_names = "tag_no", deprecated_column = "deprecated"
        )
        expect_true(length(r$tag_no) > 0)
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test find_tags_on_different_stems(): negative case.", {
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = c(1, 2),
            tag_no = c("A1", "A2"),
            deprecated = c(NA, NA),
            year = 2020:2021
        )
        r <- find_tags_on_different_stems(
            test_data, tag_names = "tag_no", deprecated_column = "deprecated"
        )
        expect_true(length(r$tag_no) == 0)
        #----------------------------------------------------------------------
        test_data <- data.frame(
            stem_id = c(1, 2),
            tag_no = rep("A1", 2),
            deprecated = c(1, NA),
            year = 2020:2021
        )
        r <- find_tags_on_different_stems(
            test_data, tag_names = "tag_no", deprecated_column = "deprecated"
        )
        expect_true(length(r$tag_no) == 0)
    }
)
