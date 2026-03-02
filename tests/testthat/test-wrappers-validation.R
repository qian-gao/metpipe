test_that("run_import_peaktable validates missing files", {
  expect_error(
    run_import_peaktable(
      peaktable_pos = "not_exists_pos.xlsx",
      peaktable_neg = "not_exists_neg.xlsx",
      rt_col_nr = 1,
      mz_col_nr = 2,
      identity_col_nr = 3,
      sample_col_nr = 4,
      standardized.name = FALSE
    ),
    "No valid peak table was found"
  )
})

test_that("run_clean_peaktable validates datalist structure", {
  expect_error(run_clean_peaktable(datalist = NULL), "datalist must be a list")
  expect_error(run_clean_peaktable(datalist = list(pos = NULL, neg = NULL)), "does not contain pos or neg")
})

test_that("as_metpipe_datalist supports backward-compatible list inputs", {
  out <- as_metpipe_datalist(list(pos = list(raw = 1), neg = NULL), stage = "imported")
  expect_true(is_metpipe_datalist(out))
  expect_true(is.list(out))
})

test_that("run_clean_peaktable applies mode-specific cleaning", {
  mock_env <- environment(run_clean_peaktable)
  local_mocked_bindings(
    clean_peaktable = function(datalist, ...) {
      datalist$cleaned <- TRUE
      datalist
    },
    .env = mock_env
  )

  input <- list(pos = list(raw = 1), neg = list(raw = 2))
  out <- run_clean_peaktable(datalist = input)

  expect_true(isTRUE(out$pos$cleaned))
  expect_true(isTRUE(out$neg$cleaned))
})

test_that("run_normalization validates datalist structure", {
  expect_error(run_normalization(datalist = NULL), "datalist must be a list")
  expect_error(run_normalization(datalist = list(pos = NULL, neg = NULL)), "does not contain pos or neg")
})

test_that("run_import_peaktable returns metpipe_datalist", {
  mock_env <- environment(run_import_peaktable)
  local_mocked_bindings(
    import_peaktable = function(...) list(raw = 1),
    file.exists = function(path) TRUE,
    .env = mock_env
  )

  out <- run_import_peaktable(
    peaktable_pos = "pos.xlsx",
    peaktable_neg = NULL,
    rt_col_nr = 1,
    mz_col_nr = 2,
    identity_col_nr = 3,
    sample_col_nr = 4
  )

  expect_true(is_metpipe_datalist(out))
  expect_false(is.null(out$pos))
})

test_that("run_normalization applies normalization to available modes", {
  mock_env <- environment(run_normalization)
  local_mocked_bindings(
    normalization = function(datalist, ...) {
      datalist$normalized <- TRUE
      datalist
    },
    .env = mock_env
  )

  input <- list(pos = list(raw = 1), neg = NULL)
  out <- run_normalization(datalist = input)

  expect_true(isTRUE(out$pos$normalized))
  expect_null(out$neg)
})

test_that("run_merge_amd_map validates required inputs", {
  expect_error(run_merge_amd_map(datalist = NULL), "datalist must be a list")
  expect_error(run_merge_amd_map(datalist = list(pos = NULL, neg = NULL)), "does not contain pos or neg")

  expect_error(
    run_merge_amd_map(datalist = list(pos = list(), neg = NULL), final.norm = NULL, eval.sample.to.use = "PO"),
    "final.norm must be provided"
  )

  expect_error(
    run_merge_amd_map(datalist = list(pos = list(), neg = NULL), final.norm = "raw", eval.sample.to.use = NULL),
    "eval.sample.to.use must be provided"
  )
})

test_that("run_merge_and_map is backward-compatible alias", {
  mock_env <- environment(run_merge_and_map)
  local_mocked_bindings(
    run_merge_amd_map = function(...) "ok",
    .env = mock_env
  )

  expect_identical(
    run_merge_and_map(
      datalist = list(pos = list()),
      final.norm = "raw",
      eval.sample.to.use = "PO"
    ),
    "ok"
  )
})

test_that("metpipe_datalist summary reports merged status", {
  x <- new_metpipe_datalist(pos = list(raw = 1), neg = NULL)
  s <- summary(x)

  expect_true(inherits(s, "summary.metpipe_datalist"))
  expect_true(isTRUE(s$has_pos))
  expect_false(isTRUE(s$has_merged))
})
