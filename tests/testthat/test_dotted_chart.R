# Unit tests for dotted_chart.R


#### timeSinceStartOfDay ####

test_that("test timeSinceStartOfDay UTC", {
  time <- as.POSIXct("2022-01-18 18:36:52", format = "%F %T", tz = "UTC")

  expect_equal(as.integer(timeSinceStartOfDay(time)), 18 * 3600 + 36 * 60 + 52)
})

test_that("test timeSinceStartOfDay CET", {
  t1 <- as.POSIXct("2022-01-18 18:36:52 +0100", format = "%F %T %z")
  t2 <- as.POSIXct("2022-01-18 18:36:52", format = "%F %T", tz = "CET")

  expect_equal(as.integer(timeSinceStartOfDay(t1)), 18 * 3600 + 36 * 60 + 52)
  expect_equal(as.integer(timeSinceStartOfDay(t2)), 18 * 3600 + 36 * 60 + 52)
})

test_that("test timeSinceStartOfDay UTC summer/winter time", {
  summer_time <- as.POSIXct("2022-03-27 18:36:52", format = "%F %T", tz = "UTC")
  winter_time <- as.POSIXct("2022-10-30 18:36:52", format = "%F %T", tz = "UTC")

  # No clock changes in UTC
  expect_equal(as.integer(timeSinceStartOfDay(summer_time)), 18 * 3600 + 36 * 60 + 52)
  expect_equal(as.integer(timeSinceStartOfDay(winter_time)), 18 * 3600 + 36 * 60 + 52)
})

test_that("test timeSinceStartOfDay CET summer/winter time", {
  summer_time <- as.POSIXct("2022-03-27 18:36:52", format = "%F %T", tz = "CET")
  winter_time <- as.POSIXct("2022-10-30 18:36:52", format = "%F %T", tz = "CET")

  # Clock set 1h ahead
  expect_equal(as.integer(timeSinceStartOfDay(summer_time)), 17 * 3600 + 36 * 60 + 52)
  # Clock set 1h behind
  expect_equal(as.integer(timeSinceStartOfDay(winter_time)), 19 * 3600 + 36 * 60 + 52)
})


#### timeSinceStartOfWeek ####

# No need to test CET, see timeSinceStartOfDay

test_that("test timeSinceStartOfWeek UTC", {
  time <- as.POSIXct("2022-01-18 18:36:52", format = "%F %T", tz = "UTC")

  # Week starts at Monday 0 AM
  expect_equal(as.integer(timeSinceStartOfWeek(time)), 24 * 60 * 60 + 18 * 3600 + 36 * 60 + 52)
})

test_that("test timeSinceStartOfWeek UTC leap year", {
  time <- as.POSIXct("2020-03-01 18:36:52", format = "%F %T", tz = "UTC")

  # Week starts at Monday 0 AM, includes Sat 29th Februari
  expect_equal(as.integer(timeSinceStartOfWeek(time)), 6 * 24 * 60 * 60 + 18 * 3600 + 36 * 60 + 52)
})


#### timeFormat ####

test_that("test timeFormat UTC", {
  time <- as.POSIXct("2022-01-18 18:36:52", format = "%F %T", tz = "UTC")

  expect_equal(timeFormat(time), "18:36")
})

test_that("test timeFormat CET", {
  t1 <- as.POSIXct("2022-01-18 18:36:52 +0100", format = "%F %T %z")
  t2 <- as.POSIXct("2022-01-18 18:36:52", format = "%F %T", tz = "CET")

  expect_equal(timeFormat(t1), "18:36")
  expect_equal(timeFormat(t2), "18:36")
})


#### dotted_chart_data ####

test_that("test dotted_chart_data columns", {
  load("./testdata/patients.rda")

  chart_data <- dotted_chart_data(patients, NULL, "mins")

  expect_equal(dim(chart_data), c(5, 18))
  expect_equal(colnames(chart_data), c("patient", "activity", "activity_instance", "color", "start", "end", "start_week",
                                       "end_week", "start_day", "end_day", "start_case", "end_case", "dur", "start_case_week",
                                       "start_case_day", "start_relative", "end_relative", "start_case_rank"))
})

