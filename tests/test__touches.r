#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): adjacent Qs", {
        testthat::expect_true(ogawa::touches("A1a1", "A1a2", corner = TRUE))
        testthat::expect_true(ogawa::touches("A1a1", "A1a2", corner = FALSE))
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): Qs touching at their corner", {
        testthat::expect_true(ogawa::touches("A1a1", "A1a4", corner = TRUE))
        testthat::expect_false(ogawa::touches("A1a1", "A1a4", corner = FALSE))
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): distant Qs", {
        testthat::expect_false(ogawa::touches("A1a1", "A2a1", corner = TRUE))
        testthat::expect_false(ogawa::touches("A1a1", "A2a1", corner = FALSE))
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): same Qs", {
        testthat::expect_true(ogawa::touches("A1a1", "A1a1", corner = TRUE))
        testthat::expect_true(ogawa::touches("A1a1", "A1a1", corner = FALSE))
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with same length (TRUE cases)", {
        q1 <- rep("A1a1", 10)
        q2 <- rep("A1a2", 10)
        testthat::expect_true(
            length(ogawa::touches(q1, q2, corner = TRUE)) == 10,
            info = "Result should have same length as source"
        )
        testthat::expect_true(
            length(ogawa::touches(q1, q2, corner = FALSE)) == 10,
            info = "Result should have same length as source"
        )
        testthat::expect_true(
            all(ogawa::touches(q1, q2, corner = TRUE)),
            info = "All result should be TRUE"
        )
        testthat::expect_true(
            all(ogawa::touches(q1, q2, corner = FALSE)),
            info = "All result should be TRUE"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with same length (TRUE cases, corner)", {
        q1 <- rep("A1a1", 10)
        q2 <- rep("A1a4", 10)
        testthat::expect_true(
            length(ogawa::touches(q1, q2, corner = TRUE)) == 10,
            info = "Result should have same length as source"
        )
        testthat::expect_true(
            length(ogawa::touches(q1, q2, corner = FALSE)) == 10,
            info = "Result should have same length as source"
        )
        testthat::expect_true(
            all(ogawa::touches(q1, q2, corner = TRUE)),
            info = "All result should be TRUE"
        )
        testthat::expect_false(
            any(ogawa::touches(q1, q2, corner = FALSE)),
            info = "All result should be TRUE"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with same length (FALSE cases)", {
        q1 <- rep("A1a1", 10)
        q2 <- rep("A2a1", 10)
        testthat::expect_true(
            length(ogawa::touches(q1, q2, corner = TRUE)) == 10,
            info = "Result should have same length as source"
        )
        testthat::expect_true(
            length(ogawa::touches(q1, q2, corner = FALSE)) == 10,
            info = "Result should have same length as source"
        )
        testthat::expect_false(
            all(ogawa::touches(q1, q2, corner = TRUE)),
            info = "All result should be TRUE"
        )
        testthat::expect_false(
            any(ogawa::touches(q1, q2, corner = FALSE)),
            info = "All result should be TRUE"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): check result length for q1 or q2 having length of 1", {
        q1 <- "A1a1"
        q2 <- "A1a2"
        n <- 10
        testthat::expect_true(
            length(ogawa::touches(q1, rep(q2, n), corner = TRUE)) == n,
            info = "q1 having length of 1 should be broadcasted"
        )
        testthat::expect_true(
            length(ogawa::touches(q1, rep(q2, n), corner = FALSE)) == n,
            info = "q1 having length of 1 should be broadcasted"
        )
        testthat::expect_true(
            length(ogawa::touches(rep(q1, n), q2, corner = TRUE)) == n,
            info = "q2 having length of 1 should be broadcasted"
        )
        testthat::expect_true(
            length(ogawa::touches(rep(q1, n), q2, corner = FALSE)) == n,
            info = "q2 having length of 1 should be broadcasted"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with broadcasting (TRUE cases)", {
        q1 <- "A1a1"
        q2 <- "A1a2"
        n <- 10
        testthat::expect_true(
            all(ogawa::touches(q1, rep(q2, n), corner = TRUE)),
            info = "Adjacent Qs with broadcasting"
        )
        testthat::expect_true(
            all(ogawa::touches(rep(q1, n), q2, corner = FALSE)),
            info = "Adjacent Qs with broadcasting"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with broadcasting (corner cases)", {
        q1 <- "A1a1"
        q2 <- "A1a4"
        n <- 10
        testthat::expect_true(
            all(ogawa::touches(q1, rep(q2, n), corner = TRUE)),
            info = "Adjacent Qs with broadcasting"
        )
        testthat::expect_false(
            all(ogawa::touches(rep(q1, n), q2, corner = FALSE)),
            info = "Adjacent Qs with broadcasting"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with broadcasting (corner cases)", {
        q1 <- "A1a1"
        q2 <- "A1a4"
        n <- 10
        testthat::expect_true(
            all(ogawa::touches(q1, rep(q2, n), corner = TRUE)),
            info = "Adjacent Qs with broadcasting"
        )
        testthat::expect_false(
            any(ogawa::touches(rep(q1, n), q2, corner = FALSE)),
            info = "Adjacent Qs with broadcasting"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): multiple Qs with broadcasting (corner cases)", {
        q1 <- "A1a1"
        q2 <- "A2a1"
        n <- 10
        testthat::expect_false(
            any(ogawa::touches(q1, rep(q2, n), corner = TRUE)),
            info = "Adjacent Qs with broadcasting"
        )
        testthat::expect_false(
            any(ogawa::touches(rep(q1, n), q2, corner = FALSE)),
            info = "Adjacent Qs with broadcasting"
        )
    }
)

#------------------------------------------------------------------------------
testthat::test_that(
    "Test touches(): error cases", {
        q1 <- rep("A1a1", 10)
        q2 <- rep("A1a2", 9)
        testthat::expect_error(
            ogawa::touches(q1, q2), ".*length\\(q1\\) == length\\(q2\\).*",
            info = "q1 and q2 having different lengths shoud be error"
        )
    }
)
