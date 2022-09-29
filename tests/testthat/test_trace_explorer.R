
#### eventlog ####

test_that("test trace_explorer on eventlog with default params", {

  load("./testdata/patients.rda")

  # No `coverage` or `n_traces` set.
  # ! Defaulting to `coverage` = 0.2 for `type` = "frequent" traces.
  expect_snapshot_warning(
    chart <- patients %>%
      trace_explorer()
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test trace_explorer on eventlog with param `coverage`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2),
    NA
  )
  expect_warning(
    patients %>%
      trace_explorer(coverage = 0.2),
    NA
  )

  expect_s3_class(chart, "ggplot")

  # `coverage` must be a <numeric> between 0 and 1.
  expect_snapshot_error(
    patients %>%
      trace_explorer(coverage = -1)
  )
  expect_snapshot_error(
    patients %>%
      trace_explorer(coverage = 2)
  )
  expect_snapshot_error(
    patients %>%
      trace_explorer(coverage = "0.2")
  )
})

test_that("test trace_explorer on eventlog with param `n_traces`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(n_traces = 1),
    NA
  )
  expect_warning(
    patients %>%
      trace_explorer(n_traces = 1),
    NA
  )

  expect_s3_class(chart, "ggplot")

  # `n_traces` must be an interger-like <numeric> larger than 0.
  expect_snapshot_error(
    patients %>%
      trace_explorer(n_traces = -1)
  )
  expect_snapshot_error(
    patients %>%
      trace_explorer(n_traces = 0)
  )
  expect_snapshot_error(
    patients %>%
      trace_explorer(n_traces = 1.5)
  )
  expect_snapshot_error(
    patients %>%
      trace_explorer(n_traces = "1")
  )
})

test_that("test trace_explorer on eventlog with param `type`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2, type = "frequent"),
    NA
  )

  expect_error(
    patients %>%
      trace_explorer(coverage = 0.05, type = "infrequent"),
    NA
  )

  # Warning: No `coverage` or `n_traces` set.
  # ! Defaulting to `coverage` = 0.2 for `type` = "frequent" traces.
  expect_snapshot_warning(
    patients %>%
      trace_explorer(type = "frequent")
  )

  # Warning: No `coverage` or `n_traces` set.
  # ! Defaulting to `coverage` = 0.05 for `type` = "infrequent" traces.
  expect_snapshot_warning(
    patients %>%
      trace_explorer(type = "infrequent")
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test trace_explorer on eventlog with param `coverage_labels`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2, coverage_labels = "relative"),
    NA
  )

  expect_error(
    patients %>%
      trace_explorer(coverage = 0.2, coverage_labels = "absolute"),
    NA
  )

  expect_error(
    patients %>%
      trace_explorer(coverage = 0.2, coverage_labels = "cumulative"),
    NA
  )

  expect_error(
    patients %>%
      trace_explorer(coverage = 0.2, coverage_labels = c("relative", "absolute")),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test trace_explorer on eventlog with param `abbreviate`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2, abbreviate = TRUE),
    NA
  )

  expect_error(
    patients %>%
      trace_explorer(coverage = 0.2, abbreviate = FALSE),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test trace_explorer on eventlog with param `show_labels`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2, show_labels = TRUE),
    NA
  )

  expect_error(
    patients %>%
      trace_explorer(coverage = 0.2, show_labels = FALSE),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test trace_explorer on eventlog with param `label_size`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2, label_size = 5),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test trace_explorer on eventlog with param `raw_data`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      trace_explorer(coverage = 0.2, raw_data = FALSE),
    NA
  )

  expect_s3_class(chart, "ggplot")

  expect_error(
    raw_data <- patients %>%
      trace_explorer(coverage = 0.2, raw_data = TRUE),
    NA
  )

  expect_s3_class(raw_data, "tbl_df")
})


#### activitylog ####

test_that("test trace_explorer on activitylog with default params", {

  load("./testdata/patients_act.rda")

  # No `coverage` or `n_traces` set.
  # ! Defaulting to `coverage` = 0.2 for `type` = "frequent" traces.
  expect_snapshot_warning(
    chart <- patients_act %>%
      trace_explorer()
  )

  expect_s3_class(chart, "ggplot")
})