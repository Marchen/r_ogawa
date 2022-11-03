#------------------------------------------------------------------------------
testthat::test_that(
    "Test all_touching(): two Qs", {
        # Adjacent Qs.
        testthat::expect_true(
            ogawa::all_touching("A1a1", "A1a2", corner = TRUE)
        )
        testthat::expect_true(
            ogawa::all_touching("A1a1", "A1a2", corner = FALSE)
        )
        # Adjacent at corner.
        testthat::expect_true(
            ogawa::all_touching("A1a1", "A1a4", corner = TRUE)
        )
        testthat::expect_false(
            ogawa::all_touching("A1a1", "A1a4", corner = FALSE)
        )
        # Distant Qs.
        testthat::expect_false(
            ogawa::all_touching("A1a1", "A2a4", corner = TRUE)
        )
        testthat::expect_false(
            ogawa::all_touching("A1a1", "A2a4", corner = FALSE)
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test all_touching(): three Qs", {
        # Adjacent Qs.
        testthat::expect_true(
            ogawa::all_touching("A1a1", "A1a2", "A1a3", corner = TRUE)
        )
        testthat::expect_false(
            ogawa::all_touching("A1a1", "A1a2", "A1a3", corner = FALSE)
        )
        # Distant Qs.
        testthat::expect_false(
            ogawa::all_touching("A1a1", "A1a2", "A2a4", corner = TRUE)
        )
        testthat::expect_false(
            ogawa::all_touching("A1a1", "A1a2", "A2a4", corner = FALSE)
        )
    }
)
