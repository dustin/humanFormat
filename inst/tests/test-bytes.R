library(testthat)
library(humanFormat)

test_that("A few small byte sizes render", {
	expect_equal("0", formatBytes(0))
	expect_equal("1", formatBytes(1))
	expect_equal("11", formatBytes(11))
})

test_that("SI sizes in a vector render", {
	expsSI <- c("2", "4", "8", "16", "32", "64", "128", "256", "512",
                "1.02 KB", "2.05 KB", "4.10 KB", "8.19 KB", "16.38 KB",
                "32.77 KB", "65.54 KB", "131.07 KB", "262.14 KB", "524.29 KB",
                "1.05 MB", "2.10 MB", "4.19 MB", "8.39 MB", "16.78 MB",
                "33.55 MB", "67.11 MB", "134.22 MB", "268.44 MB", "536.87 MB",
                "1.07 GB", "2.15 GB", "4.29 GB", "8.59 GB", "17.18 GB",
                "34.36 GB", "68.72 GB", "137.44 GB", "274.88 GB", "549.76 GB",
                "1.10 TB", "2.20 TB", "4.40 TB", "8.80 TB", "17.59 TB",
                "35.18 TB", "70.37 TB", "140.74 TB", "281.47 TB", "562.95 TB",
                "1.13 PB", "2.25 PB", "4.50 PB", "9.01 PB", "18.01 PB",
                "36.03 PB", "72.06 PB", "144.12 PB", "288.23 PB", "576.46 PB",
                "1.15 EB", "2.31 EB", "4.61 EB", "9.22 EB", "18.45 EB",
                "36.89 EB", "73.79 EB", "147.57 EB", "295.15 EB", "590.30 EB",
                "1.18 ZB", "2.36 ZB", "4.72 ZB", "9.44 ZB", "18.89 ZB",
                "37.78 ZB", "75.56 ZB", "151.12 ZB", "302.23 ZB", "604.46 ZB",
                "1.21 YB", "2.42 YB", "4.84 YB", "9.67 YB", "19.34 YB",
                "38.69 YB", "77.37 YB", "154.74 YB", "309.49 YB")

	expect_equal(formatBytes(2^(1:88)), expsSI)
})

test_that("IEC sizes in a vector render", {

	expsIEC <- c("2", "4", "8", "16", "32", "64", "128", "256", "512",
                 "1.00 KiB", "2.00 KiB", "4.00 KiB", "8.00 KiB", "16.00 KiB",
                 "32.00 KiB", "64.00 KiB", "128.00 KiB", "256.00 KiB",
                 "512.00 KiB", "1.00 MiB", "2.00 MiB", "4.00 MiB", "8.00 MiB",
                 "16.00 MiB", "32.00 MiB", "64.00 MiB", "128.00 MiB",
                 "256.00 MiB", "512.00 MiB", "1.00 GiB", "2.00 GiB",
                 "4.00 GiB", "8.00 GiB", "16.00 GiB", "32.00 GiB",
                 "64.00 GiB", "128.00 GiB", "256.00 GiB", "512.00 GiB",
                 "1.00 TiB", "2.00 TiB", "4.00 TiB", "8.00 TiB", "16.00 TiB",
                 "32.00 TiB", "64.00 TiB", "128.00 TiB", "256.00 TiB",
                 "512.00 TiB", "1.00 PiB", "2.00 PiB", "4.00 PiB", "8.00 PiB",
                 "16.00 PiB", "32.00 PiB", "64.00 PiB", "128.00 PiB",
                 "256.00 PiB", "512.00 PiB", "1.00 EiB", "2.00 EiB",
                 "4.00 EiB", "8.00 EiB", "16.00 EiB", "32.00 EiB",
                 "64.00 EiB", "128.00 EiB", "256.00 EiB", "512.00 EiB",
                 "1.00 ZiB", "2.00 ZiB", "4.00 ZiB", "8.00 ZiB", "16.00 ZiB",
                 "32.00 ZiB", "64.00 ZiB", "128.00 ZiB", "256.00 ZiB",
                 "512.00 ZiB", "1.00 YiB", "2.00 YiB", "4.00 YiB", "8.00 YiB",
                 "16.00 YiB", "32.00 YiB", "64.00 YiB", "128.00 YiB",
                 "256.00 YiB")

	expect_equal(formatIECBytes(2^(1:88)), expsIEC)
})
