library(testthat)

test_that("seq_smooth generates linear sequences correctly", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "linear")
  expect_equal(seq, c(0, 2.5, 5, 7.5, 10))

  seq <- seq_smooth(from = -5, to = 5, n = 3, type = "linear")
  expect_equal(seq, c(-5, 0, 5))
})

test_that("seq_smooth handles 'step' sequences correctly", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "step", step_count = 4)
  expect_equal(seq, c(0, 2.5, 5, 7.5, 10))

  seq <- seq_smooth(from = -5, to = 5, n = 6, type = "step", step_count = 3)
  expect_equal(seq, c(-5, -1.6666667, -1.6666667, 1.6666667, 1.6666667, 5), tolerance = 1e-6)
})

test_that("seq_smooth warns when ease is provided for linear sequences", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "linear", ease = "in")
  expect_equal(seq,c(0,2.5,5,7.5,10),tolerance = 1e-6)
})

test_that("seq_smooth warns when step_count is NULL for step sequences", {
  expect_warning(seq_smooth(from = 0, to = 10, n = 5, type = "step", step_count = NULL),
                 "Step count is 'NULL'. Using default 'step_count' = 4.")
})

test_that("seq_smooth warns when ease is used with 'step' sequences", {
  expect_warning(seq_smooth(from = 0, to = 10, n = 5, type = "step", step_count = 3,ease = "in"),
                 "'ease' has no effect on step functions. Step function is not continuous.")
})

test_that("seq_smooth generates quadratic easing correctly", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "quad", ease = "in")
  expect_equal(seq, c(0, 0.625, 2.5, 5.625, 10), tolerance = 1e-6)

  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "quad", ease = "out")
  expect_equal(seq, c(0, 4.375, 7.5, 9.375, 10), tolerance = 1e-6)
})

test_that("seq_smooth generates cubic easing correctly", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "cubic", ease = "in")
  expect_equal(seq, c(0, 0.15625, 1.25, 4.21875, 10), tolerance = 1e-6)

  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "cubic", ease = "out")
  expect_equal(seq, c(0, 5.78125, 8.75, 9.84375, 10), tolerance = 1e-6)
})

test_that("seq_smooth generates sequences with exponential easing correctly", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "exp", ease = "in")
  expect_equal(seq, c(0.0097656,0.0552427,0.3125,1.767767,10),tolerance = 1e-6)
})

test_that("seq_smooth respects default arguments", {
  seq <- seq_smooth()
  expect_equal(seq, rep(1, 100)) # Default is linear sequence from 1 to 1
})

test_that("seq_smooth produces correct easing for sine easing types", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "sine", ease = "in")
  expect_equal(seq, c(0, 0.7612047, 2.9289322, 6.1731657 , 10), tolerance = 1e-6)

  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "sine", ease = "out")
  expect_equal(seq, c(0, 3.826834, 7.071068, 9.238795, 10), tolerance = 1e-6)
})

test_that("seq_smooth handles bounce easing types correctly", {
  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "bounce", ease = "in")
  expect_equal(seq, c(0,0.2734375,2.3437500,5.2734375,  10), tolerance = 1e-6)

  seq <- seq_smooth(from = 0, to = 10, n = 5, type = "bounce", ease = "out")
  expect_equal(seq, c(0,4.726562, 7.656250,9.726562, 10),tolerance = 1e-6)
})
