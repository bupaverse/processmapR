
#### eventlog ####

test_that("test lined_chart on eventlog with default params", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      lined_chart(),
    NA
  )
  expect_warning(
    patients %>%
      lined_chart(),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test lined_chart on eventlog with param `x`", {

  load("./testdata/patients.rda")

  expect_error(
    patients %>%
      lined_chart(x = "absolute"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(x = "relative"),
    NA
  )
})

test_that("test lined_chart on eventlog with param `sort`", {

  load("./testdata/patients.rda")

  expect_error(
    patients %>%
      lined_chart(sort = "auto"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(sort = "start"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(sort = "end"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(sort = "duration"),
    NA
  )
})

test_that("test lined_chart on eventlog with param `color`", {

  load("./testdata/patients.rda")

  expect_error(
    patients %>%
      lined_chart(color = NULL),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(color = NA),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(color = resource_id(.)),
    NA
  )

  # Fails: employee is not present in log.
  expect_snapshot_error(
    patients %>%
      lined_chart(color = "employee")
  )
})

test_that("test lined_chart on eventlog with param `units`", {

  load("./testdata/patients.rda")

  # units param is only relavant when x = "relative".
  expect_error(
    patients %>%
      lined_chart(x = "relative", units = "auto"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(x = "relative", units = "secs"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(x = "relative", units = "mins"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(x = "relative", units = "hours"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(x = "relative", units = "days"),
    NA
  )

  expect_error(
    patients %>%
      lined_chart(x = "relative", units = "weeks"),
    NA
  )
})

test_that("test lined_chart on eventlog with param `line_width`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      lined_chart(line_width = 1),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test lined_chart on eventlog with param `plotly`", {

  load("./testdata/patients.rda")

  expect_error(
    chart <- patients %>%
      lined_chart(plotly = FALSE),
    NA
  )

  expect_s3_class(chart, "ggplot")

  expect_error(
    chart <- patients %>%
      lined_chart(plotly = TRUE),
    NA
  )

  expect_s3_class(chart, "plotly")
})

test_that("test lined_chart on grouped_eventlog with default params", {

  load("./testdata/patients_grouped_resource.rda")

  expect_error(
    chart <- patients_grouped_resource %>%
      lined_chart(),
    NA
  )
  expect_warning(
    patients_grouped_resource %>%
      lined_chart(),
    NA
  )

  expect_s3_class(chart, "ggplot")
})


#### activitylog ####

test_that("test lined_chart on activitylog with default params", {

  load("./testdata/patients_act.rda")

  expect_error(
    chart <- patients_act %>%
      lined_chart(),
    NA
  )
  expect_warning(
    patients_act %>%
      lined_chart(),
    NA
  )

  expect_s3_class(chart, "ggplot")
})

test_that("test lined_chart on grouped_activitylog with default params", {

  load("./testdata/patients_act_grouped_resource.rda")

  expect_error(
    chart <- patients_act_grouped_resource %>%
      lined_chart(),
    NA
  )
  expect_warning(
    patients_act_grouped_resource %>%
      lined_chart(),
    NA
  )

  expect_s3_class(chart, "ggplot")
})