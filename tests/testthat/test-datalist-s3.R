test_that("as_metpipe_datalist preserves existing fields", {
  x <- as_metpipe_datalist(list(pos = list(raw = 1), neg = NULL, extra = 5), stage = "imported")
  expect_true(is_metpipe_datalist(x))
  expect_identical(x$extra, 5)
})

test_that("validate_metpipe_datalist enforces merged stage fields", {
  x <- new_metpipe_datalist(pos = list(raw = 1), neg = NULL)
  expect_error(validate_metpipe_datalist(x, stage = "merged"), "missing merged outputs")
})

test_that("print.metpipe_datalist is callable", {
  x <- new_metpipe_datalist(pos = list(raw = 1), neg = NULL)
  expect_invisible(print(x))
})
