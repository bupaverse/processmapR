
#### eventlog ####

test_that("test dotted_chart on eventlog with default params", {

	load("./testdata/patients.rda")

	expect_error(
		chart <- patients %>%
			dotted_chart(),
		NA
	)
	expect_warning(
		patients %>%
			dotted_chart(),
		NA
	)

	expect_s3_class(chart, "ggplot")
})

test_that("test dotted_chart on eventlog with param `x`", {

	load("./testdata/patients.rda")

	expect_error(
		patients %>%
			dotted_chart(x = "absolute"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(x = "relative"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(x = "relative_week"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(x = "relative_day"),
		NA
	)
})

test_that("test dotted_chart on eventlog with param `sort`", {

	load("./testdata/patients.rda")

	expect_error(
		patients %>%
			dotted_chart(sort = "auto"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(sort = "start"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(sort = "end"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(sort = "duration"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(sort = "start_week"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(sort = "start_day"),
		NA
	)
})

test_that("test dotted_chart on eventlog with param `color`", {

	load("./testdata/patients.rda")

	expect_error(
		patients %>%
			dotted_chart(color = NULL),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(color = NA),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(color = resource_id(.)),
		NA
	)

	# Fails: employee is not present in log.
	expect_snapshot_error(
		patients %>%
			dotted_chart(color = "employee")
	)
})

test_that("test dotted_chart on eventlog with param `units`", {

	load("./testdata/patients.rda")

	expect_error(
		patients %>%
			dotted_chart(units = "auto"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(units = "secs"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(units = "mins"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(units = "hours"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(units = "days"),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(units = "weeks"),
		NA
	)
})

test_that("test dotted_chart on eventlog with param `add_end_events`", {

	load("./testdata/patients.rda")

	expect_error(
		patients %>%
			dotted_chart(add_end_events = FALSE),
		NA
	)

	expect_error(
		patients %>%
			dotted_chart(add_end_events = TRUE),
		NA
	)
})

test_that("test dotted_chart on grouped_eventlog with default params", {

	load("./testdata/patients_grouped_resource.rda")

	expect_error(
		chart <- patients_grouped_resource %>%
			dotted_chart(),
		NA
	)
	expect_warning(
		patients_grouped_resource %>%
			dotted_chart(),
		NA
	)

	expect_s3_class(chart, "ggplot")
})


#### activitylog ####

test_that("test dotted_chart on activitylog with default params", {

	load("./testdata/patients_act.rda")

	expect_error(
		chart <- patients_act %>%
			dotted_chart(),
		NA
	)
	expect_warning(
		patients_act %>%
			dotted_chart(),
		NA
	)

	expect_s3_class(chart, "ggplot")
})

test_that("test dotted_chart on grouped_activitylog with default params", {

	load("./testdata/patients_act_grouped_resource.rda")

	expect_error(
		chart <- patients_act_grouped_resource %>%
			dotted_chart(),
		NA
	)
	expect_warning(
		patients_act_grouped_resource %>%
			dotted_chart(),
		NA
	)

	expect_s3_class(chart, "ggplot")
})