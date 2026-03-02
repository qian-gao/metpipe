test_that("impute_missing validates method and imputes LoD safely", {
  x <- data.frame(a = c(NA, NA, NA), b = c(1, NA, 3))

  expect_error(impute_missing(x, method = "unknown"), "Unsupported method")

  out <- impute_missing(x, method = "LoD")
  expect_true(is.list(out))
  expect_true(all(c("x", "method") %in% names(out)))
  expect_false(any(is.na(out$x)))
})

test_that("impute_missing knn validates group.info length", {
  x <- data.frame(a = c(1, 2, NA), b = c(2, 3, 4))
  expect_error(
    impute_missing(x, method = "knn", group.info = c("A", "B")),
    "group.info must have the same length"
  )
})

test_that("import_peaktable validates file path", {
  expect_error(
    import_peaktable(
      peaktable = "no_such_file.xlsx",
      rt_col_nr = 1,
      mz_col_nr = 2,
      identity_col_nr = 3,
      sample_col_nr = 4
    ),
    "peaktable must be an existing file path"
  )
})

test_that("normalize_with_best_internal_standard validates dimensions", {
  x <- data.frame(f1 = c(1, 2), f2 = c(3, 4))
  istds <- data.frame(is1 = c(1, 2, 3))

  expect_error(
    normalize_with_best_internal_standard(x = x, istds = istds, batch = c("B1", "B1")),
    "same number of rows"
  )

  istds2 <- data.frame(is1 = c(1, 2))
  expect_error(
    normalize_with_best_internal_standard(x = x, istds = istds2, batch = "B1"),
    "batch must have the same length"
  )
})

test_that("filter_by_missing validates inputs", {
  x <- matrix(c(1, NA, 3, 4), nrow = 2)

  expect_error(filter_by_missing(x = 1:3, method = "feature", threshold = 20), "data.frame or matrix")
  expect_error(filter_by_missing(x = x, method = "invalid", threshold = 20), "must be either 'sample' or 'feature'")
  expect_error(filter_by_missing(x = x, method = "feature", threshold = 120), "between 0 and 100")
})

test_that("mapping helpers validate required inputs", {
  expect_error(map_filename_to_meta(raw_files = character(0)), "non-empty character vector")
  expect_error(map_standard_name_to_meta(raw_files = character(0)), "non-empty character vector")
  expect_error(map_metabolite_info(input_file = data.frame(a = 1), db_file = "no_db.csv"), "db_file must be provided and exist")
})
