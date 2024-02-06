test_xy <- function(sq2, x, y, msg = "") {
    if (msg != "") {
        testthat::expect_true(
            ogawa::invalid_xy(sq2, x, y),
            sprintf("SQ2=%s, x=%s, y=%s, %s", sq2, x, y, msg)
        )
    } else {
        testthat::expect_false(
            ogawa::invalid_xy(sq2, x, y),
            sprintf("SQ2=%s, x=%s, y=%s", sq2, x, y)
        )
    }
}

#==============================================================================
testthat::test_that(
    "Test invalid_xy(): positive cases.", {
        #----------------------------------------------------------------------
        test_xy(1, 3, 3, "invalid y")
        test_xy(1, 7, 7, "invalid x")
        test_xy(1, 7, 3, "invalid xy")
        #----------------------------------------------------------------------
        test_xy(2, 7, 3, "invalid y")
        test_xy(2, 3, 7, "invalid x")
        test_xy(2, 3, 3, "invalid xy")
        #----------------------------------------------------------------------
        test_xy(3, 3, 7, "invalid y")
        test_xy(3, 7, 3, "invalid x")
        test_xy(3, 7, 7, "invalid xy")
        #----------------------------------------------------------------------
        test_xy(4, 7, 7, "invalid y")
        test_xy(4, 3, 3, "invalid x")
        test_xy(4, 3, 7, "invalid xy")
        #----------------------------------------------------------------------
        test_xy(1, 5, 3, "invalid y")
        test_xy(1, 7, 5, "invalid x")
        #----------------------------------------------------------------------
        test_xy(2, 5, 3, "invalid y")
        test_xy(2, 3, 5, "invalid x")
        #----------------------------------------------------------------------
        test_xy(3, 5, 7, "invalid y")
        test_xy(3, 7, 5, "invalid x")
        #----------------------------------------------------------------------
        test_xy(4, 5, 7, "invalid y")
        test_xy(4, 3, 5, "invalid x")
    }
)

#==============================================================================
testthat::test_that(
    "Test invalid_xy(): negative cases.", {
        #----------------------------------------------------------------------
        test_xy(1, 3, 7)
        test_xy(2, 7, 7)
        test_xy(3, 3, 3)
        test_xy(4, 7, 3)
        #----------------------------------------------------------------------
        test_xy(1, 5, 5)
        test_xy(1, 3, 5)
        test_xy(1, 5, 7)
        #----------------------------------------------------------------------
        test_xy(2, 5, 5)
        test_xy(2, 7, 5)
        test_xy(2, 5, 7)
        #----------------------------------------------------------------------
        test_xy(3, 5, 5)
        test_xy(3, 3, 5)
        test_xy(3, 5, 3)
        #----------------------------------------------------------------------
        test_xy(4, 5, 5)
        test_xy(4, 7, 5)
        test_xy(4, 5, 3)
    }
)