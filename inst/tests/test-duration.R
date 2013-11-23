library(testthat)
library(humanFormat)

test_that("individual duration formatting works", {
	expect_equal("0", formatDuration(0))
	expect_equal("1ns", formatDuration(1 * kNanosecond))
	expect_equal("1.1µs", formatDuration(1100 * kNanosecond))
	expect_equal("2.2ms", formatDuration(2200 * kMicrosecond))
	expect_equal("3.3s", formatDuration(3300 * kMillisecond))
	expect_equal("4m5s", formatDuration(4*kMinute + 5*kSecond))
	expect_equal("4m5.001s", formatDuration(4*kMinute + 5001*kMillisecond))
	expect_equal("5h6m7.001s", formatDuration(5*kHour + 6*kMinute + 7001*kMillisecond))
	expect_equal("8m0.000000001s", formatDuration(8*kMinute + 1*kNanosecond))
	expect_equal("768h4m0.013s",
		formatDuration(32 * 24 * kHour + 4 * kMinute + 13 * kMillisecond))
})

test_that("vector duration formatting works", {
	expect_equal(c("0", "1ns", "1.1µs", "2.2ms", "3.3s",
		"4m5.000000000s", "4m5.001000000s", 
		"5h6m7.001000000s", "8m0.000000001s"),
	formatDuration(c(0,
		1 * kNanosecond,
		1100 * kNanosecond,
		2200 * kMicrosecond,
		3300 * kMillisecond,
		4*kMinute + 5*kSecond,
		4*kMinute + 5001*kMillisecond,
		5*kHour + 6*kMinute + 7001*kMillisecond,
		8*kMinute + 1*kNanosecond)))
})