
test_that("verbose returns an empty list if no verbose settings set", {
  clear_verbose()
  res <- verbose()
  expect_equal(res, list())
})


test_that("set_verbose() works for a single key = value pair", {
  clear_verbose()
  verbose(cevomod = 1)
  res <- verbose()
  expected <- list(cevomod = 1)
  expect_equal(res, expected)
})


test_that("set_verbose() works for many key = value pairs", {
  clear_verbose()
  verbose(cevomod = 1, readthis = 0)
  res <- verbose()
  expected <- list(cevomod = 1, readthis = 0)
  expect_equal(res, expected)
})


test_that("verbose() returns all verbosity levels set", {
  clear_verbose()
  verbose(cevomod = 1)
  verbose(readthis = 0)
  res <- verbose()
  expected <- list(cevomod = 1, readthis = 0)
  expect_equal(res, expected)
})


test_that("Getting a specific verbosity level works", {
  clear_verbose()
  verbose(cevomod = 1)
  verbose(readthis = 0)
  res <- verbose("cevomod")
  expect_equal(res, 1)
})


test_that("clear_verbose() works", {
  verbose(cevomod = 1)
  verbose(readthis = 1)
  clear_verbose()
  res <- verbose()
  expect_equal(res, list())
})
