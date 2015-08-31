library("testthat")
test_dir(".")

cat("\nRunning unit tests\n")

source("get_wifi.R")
source("indoor2dataframe.R")
source("select_wifi_mean.R")
source("select_wifi_coverage.R")
source("replace_NAs.R")

sensors <- c("00:19:e3:fa:36:57", "00:26:f2:fc:36:c4",
             "28:16:2e:9d:a8:89", "60:c3:97:10:d4:f1",
             "c0:83:0a:60:07:31", "c8:d7:19:35:fd:3d")

test_that("get_wifi", {
    cat("\nTesting get_wifi.R \n")
    a <- get_wifi("test.json")
    expect_that(length(a), equals(6))
})

test_that("indoor2dataframe", {
    cat("\nTesting indoor2dataframe.R \n")
    a <- indoor2dataframe("test.json")
    expect_that(length(a), equals(9) )
    expect_that(names(a)[c(1,2)], equals(c("Source","Room")))
    expect_that(nrow(a), equals(8))
})

test_that("select_wifi_mean", {
    cat("\nTesting select_wifi_mean.R \n")
    a <- select_wifi_mean("indoor-location-test.csv")
    expect_that(length(a), equals(6))
})

test_that("select_wifi_coverage", {
    cat("\nTesting select_wifi_coverage.R \n")
    a <- select_wifi_coverage("indoor-location-test.csv")              
    expect_that(length(a), equals(5))
})

test_that("replace_NAs.R", {
    cat("\nTesting replace_NAs.R \n")
    s <- c("b0.e7.54.64.62.f9","f8.d1.11.5a.a0.54")
    a <- replace_NAs("indoor-location-test.csv", s)
    expect_that(median(a[,s[1]]), equals(-95))
})
