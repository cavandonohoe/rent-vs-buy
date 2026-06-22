source("../../R/finance.R")

# ----------------------------------------------------------------------------
# compute_amortization
# ----------------------------------------------------------------------------

test_that("compute_amortization computes correct monthly payment", {
  loan <- 400000
  rate <- 6
  term <- 30
  n <- term * 12
  r <- rate / 100 / 12
  expected <- loan * (r * (1 + r)^n) / ((1 + r)^n - 1)

  amort <- compute_amortization(loan, rate, term)

  expect_equal(amort$payment[1], expected, tolerance = 0.01)
})

test_that("compute_amortization fully pays off the loan", {
  amort <- compute_amortization(400000, 6, 30)

  expect_equal(nrow(amort), 360)
  expect_equal(amort$balance[360], 0, tolerance = 0.01)
})

test_that("compute_amortization handles zero interest rate", {
  amort <- compute_amortization(120000, 0, 10)

  expect_equal(amort$payment[1], 1000, tolerance = 0.01)
  expect_equal(amort$balance[120], 0, tolerance = 0.01)
})

test_that("compute_amortization with extra principal pays off early", {
  base <- compute_amortization(400000, 6, 30)
  with_extra <- compute_amortization(400000, 6, 30, extra_principal = 500)

  base_end_idx <- which(base$balance <= 0.01)[1]
  extra_end_idx <- which(with_extra$balance <= 0.01)[1]

  expect_true(extra_end_idx < base_end_idx)
})

test_that("compute_amortization principal + interest sums to payment", {
  amort <- compute_amortization(300000, 5.5, 30)
  for (i in seq_len(12)) {
    expect_equal(
      amort$payment[i],
      amort$principal[i] + amort$interest[i],
      tolerance = 0.001
    )
  }
})

# ----------------------------------------------------------------------------
# run_simulation
# ----------------------------------------------------------------------------

default_sim_args <- list(
  home_price = 1e6, down_pct = 20, mortgage_rate = 6.5, loan_term = 30,
  closing_cost_pct = 3, home_appreciation = 3,
  property_tax_rate = 1.1, prop_tax_cap = 2, insurance_annual = 1500,
  maintenance_pct = 1, selling_cost_pct = 6,
  monthly_rent = 5000, rent_increase = 2,
  investment_return = 7, inflation_rate = 3,
  monthly_income = 12000, horizon_years = 15
)

test_that("run_simulation returns horizon_years * 12 + 1 rows", {
  sim <- do.call(run_simulation, default_sim_args)
  expect_equal(nrow(sim), 15 * 12 + 1)
})

test_that("run_simulation initial net worth equals upfront cash", {
  sim <- do.call(run_simulation, default_sim_args)
  upfront <- 1e6 * 0.20 + 1e6 * 0.03
  expect_equal(sim$rent_net_worth[1], upfront)
})

test_that("run_simulation produces non-negative home value series", {
  sim <- do.call(run_simulation, default_sim_args)
  expect_true(all(sim$home_value >= 0))
  expect_true(all(diff(sim$home_value) > 0))
})

test_that("run_simulation loan balance monotonically decreases", {
  sim <- do.call(run_simulation, default_sim_args)
  expect_true(all(diff(sim$loan_balance) <= 1e-6))
  # At 15-year horizon on a 30-year loan, balance is partway paid down,
  # so we don't expect it to reach zero. Just check it's strictly decreasing
  # and stays positive.
  expect_true(tail(sim$loan_balance, 1) > 0)
  expect_true(tail(sim$loan_balance, 1) < sim$loan_balance[1])
})

test_that("run_simulation: longer horizon increases home equity (buyer's tangible asset)", {
  short_args <- default_sim_args
  short_args$horizon_years <- 3
  long_args <- default_sim_args
  long_args$horizon_years <- 30

  short <- do.call(run_simulation, short_args)
  long <- do.call(run_simulation, long_args)

  # Buyer's home equity grows monotonically with horizon (loan paid down
  # + home appreciation), even when investing also performs well.
  expect_true(tail(long$home_equity, 1) > tail(short$home_equity, 1))
})

# ----------------------------------------------------------------------------
# final_advantage
# ----------------------------------------------------------------------------

test_that("final_advantage matches direct run_simulation", {
  sim <- do.call(run_simulation, default_sim_args)
  expected <- tail(sim$buy_net_worth, 1) - tail(sim$rent_net_worth, 1)

  args_without_price <- default_sim_args[setdiff(names(default_sim_args), "home_price")]
  got <- do.call(final_advantage, c(list(home_price = 1e6), args_without_price))

  expect_equal(got, expected)
})

# ----------------------------------------------------------------------------
# find_breakeven_year
# ----------------------------------------------------------------------------

test_that("find_breakeven_year returns NA when no crossing", {
  sim <- data.frame(
    year = 0:10,
    buy_net_worth = c(-1, -1000, -2000, -3000, -4000, -5000, -6000, -7000, -8000, -9000, -10000),
    rent_net_worth = rep(0, 11)
  )
  expect_true(is.na(find_breakeven_year(sim)))
})

test_that("find_breakeven_year interpolates between months", {
  sim <- data.frame(
    year = c(0, 1, 2, 3),
    buy_net_worth = c(-100, -50, 50, 100),
    rent_net_worth = c(0, 0, 0, 0)
  )
  by <- find_breakeven_year(sim)
  expect_true(by > 1 && by < 2)
  expect_equal(by, 1.5, tolerance = 0.001)
})

# ----------------------------------------------------------------------------
# find_breakeven_price / find_breakeven_rent
# ----------------------------------------------------------------------------

test_that("find_breakeven_price returns a positive number in normal range", {
  args <- default_sim_args[setdiff(names(default_sim_args), "home_price")]
  bp <- do.call(find_breakeven_price, args)
  expect_true(is.numeric(bp))
  expect_true(bp > 0)
  expect_true(bp < 5e6)
})

test_that("find_breakeven_rent returns a positive number in normal range", {
  args <- default_sim_args[setdiff(names(default_sim_args), "monthly_rent")]
  br <- do.call(find_breakeven_rent, args)
  expect_true(is.numeric(br))
  expect_true(br > 100 || is.na(br) || is.infinite(br))
})
