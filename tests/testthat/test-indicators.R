library(testthat)
source(here::here("R", "functions.R"))

hh <- function(...) {
  base <- list(fcs_cereals = 0, fcs_pulses = 0, fcs_dairy = 0, fcs_meat = 0, fcs_vegetables = 0, fcs_fruit = 0,
               fcs_oil = 0, fcs_sugar = 0, rcsi_less_preferred = 0, rcsi_borrow_food = 0, rcsi_limit_portions = 0,
               rcsi_restrict_adults = 0, rcsi_reduce_meals = 0, hhs_no_food = "no", hhs_no_food_freq = NA,
               hhs_sleep_hungry = "no", hhs_sleep_hungry_freq = NA, hhs_whole_day = "no", hhs_whole_day_freq = NA,
               lcsi_spend_savings = "no_not_needed", lcsi_sell_assets = "no_not_needed",
               lcsi_reduce_health_education = "no_not_needed", lcsi_sell_productive_assets = "no_not_needed",
               lcsi_sell_house_land = "no_not_needed", lcsi_beg = "no_not_needed")
  as.data.frame(modifyList(base, list(...)), stringsAsFactors = FALSE)
}

test_that("FCS uses the WFP weights and thresholds", {
  every_day <- hh(fcs_cereals = 7, fcs_pulses = 7, fcs_dairy = 7, fcs_meat = 7, fcs_vegetables = 7, fcs_fruit = 7,
                  fcs_oil = 7, fcs_sugar = 7)
  expect_equal(fcs_score(every_day), 112)
  expect_equal(as.character(fcs_category(c(21, 21.5, 35, 35.5))), c("poor", "borderline", "borderline", "acceptable"))
})

test_that("rCSI weights each strategy and tops out at 56", {
  expect_equal(rcsi_score(hh(rcsi_borrow_food = 2, rcsi_restrict_adults = 1)), 2 * 2 + 3 * 1)
  all7 <- hh(rcsi_less_preferred = 7, rcsi_borrow_food = 7, rcsi_limit_portions = 7, rcsi_restrict_adults = 7,
             rcsi_reduce_meals = 7)
  expect_equal(rcsi_score(all7), 56)
})

test_that("HHS scores rarely or sometimes as 1 and often as 2", {
  d <- hh(hhs_no_food = "yes", hhs_no_food_freq = "often", hhs_sleep_hungry = "yes", hhs_sleep_hungry_freq = "rarely")
  expect_equal(hhs_score(d), 3)
  expect_equal(as.character(hhs_category(c(0, 1, 2, 3, 4, 6))),
               c("none_slight", "none_slight", "moderate", "moderate", "severe", "severe"))
})

test_that("LCSI takes the most severe strategy used, counting exhausted as used", {
  expect_equal(lcsi_category(hh()), "none")
  expect_equal(lcsi_category(hh(lcsi_spend_savings = "yes")), "stress")
  expect_equal(lcsi_category(hh(lcsi_spend_savings = "yes", lcsi_sell_productive_assets = "no_exhausted")), "crisis")
  expect_equal(lcsi_category(hh(lcsi_beg = "yes")), "emergency")
})

test_that("haversine distances are right", {
  expect_equal(haversine_m(8, 10, 8, 11), 111195, tolerance = 1e-3)
  expect_equal(haversine_m(8, 10, 8, 10), 0)
})
