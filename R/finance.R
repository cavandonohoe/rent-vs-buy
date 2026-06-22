# R/finance.R
#
# Pure financial-math functions used by the Shiny app. These are kept separate
# from the UI/server code so they can be unit-tested in isolation
# (see tests/testthat/test-finance.R).
#
# Functions:
#   compute_amortization(loan_amount, annual_rate, term_years, extra_principal)
#     - Month-by-month amortization schedule with optional extra principal.
#   run_simulation(...)
#     - Full month-by-month buy-vs-rent net-worth simulation.
#   final_advantage(home_price, ...)
#     - Net-worth delta (buy - rent) at horizon. Wrapper around run_simulation.
#   find_breakeven_price(...)
#     - Binary-searches the max home price where buying still wins.
#   find_breakeven_rent(...)
#     - Binary-searches the min rent where buying beats renting.
#   find_breakeven_year(sim_data)
#     - First year buy_net_worth >= rent_net_worth (with linear interpolation).

compute_amortization <- function(loan_amount, annual_rate, term_years,
                                 extra_principal = 0) {
  n <- term_years * 12
  r <- annual_rate / 100 / 12
  if (r == 0) {
    base_payment <- loan_amount / n
  } else {
    base_payment <- loan_amount * (r * (1 + r)^n) / ((1 + r)^n - 1)
  }

  balance <- numeric(n)
  interest <- numeric(n)
  principal <- numeric(n)
  payment_v <- numeric(n)
  extra_v <- numeric(n)
  bal <- loan_amount

  for (i in seq_len(n)) {
    if (bal <= 0) {
      payment_v[i] <- 0
      interest[i] <- 0
      principal[i] <- 0
      extra_v[i] <- 0
      balance[i] <- 0
      next
    }
    int <- bal * r
    sched_prin <- base_payment - int
    extra <- min(extra_principal, max(bal - sched_prin, 0))
    prin <- sched_prin + extra
    if (prin > bal) {
      prin <- bal
      extra <- max(prin - sched_prin, 0)
    }
    bal <- bal - prin
    interest[i] <- int
    principal[i] <- prin
    extra_v[i] <- extra
    payment_v[i] <- int + prin
    balance[i] <- max(bal, 0)
  }

  data.frame(
    month = seq_len(n),
    payment = payment_v,
    interest = interest,
    principal = principal,
    extra_principal = extra_v,
    balance = balance
  )
}

run_simulation <- function(
    home_price, down_pct, mortgage_rate, loan_term,
    closing_cost_pct, home_appreciation,
    property_tax_rate, prop_tax_cap, insurance_annual, maintenance_pct,
    selling_cost_pct,
    monthly_rent, rent_increase,
    investment_return, inflation_rate,
    monthly_income, horizon_years,
    home_revenue = 0, revenue_growth = 0,
    extra_principal = 0
) {
  months <- horizon_years * 12

  down_payment <- home_price * down_pct / 100
  closing_costs <- home_price * closing_cost_pct / 100
  upfront_cash <- down_payment + closing_costs
  loan_amount <- home_price - down_payment

  amort <- compute_amortization(loan_amount, mortgage_rate, loan_term,
    extra_principal = extra_principal)

  monthly_inv_return <- (1 + investment_return / 100)^(1 / 12) - 1

  # Prop 13 style: assessed value grows at the capped rate, not market rate
  monthly_assess_growth <- (1 + prop_tax_cap / 100)^(1 / 12)

  # Pre-allocate
  buy_net_worth <- numeric(months + 1)
  rent_net_worth <- numeric(months + 1)
  buy_monthly_cost <- numeric(months)
  buy_monthly_revenue <- numeric(months)
  rent_monthly_cost <- numeric(months)
  home_values <- numeric(months + 1)
  assessed_values <- numeric(months + 1)
  home_equity <- numeric(months + 1)
  rent_portfolio <- numeric(months + 1)
  buy_portfolio <- numeric(months + 1)
  loan_balance <- numeric(months + 1)

  home_values[1] <- home_price
  assessed_values[1] <- home_price
  loan_balance[1] <- loan_amount
  home_equity[1] <- home_price * (1 - selling_cost_pct / 100) - loan_amount
  rent_portfolio[1] <- upfront_cash
  buy_portfolio[1] <- 0
  buy_net_worth[1] <- home_equity[1]
  rent_net_worth[1] <- upfront_cash

  monthly_appr <- (1 + home_appreciation / 100)^(1 / 12)

  for (t in seq_len(months)) {
    year <- ceiling(t / 12)
    hv <- home_values[t] * monthly_appr
    home_values[t + 1] <- hv

    av <- assessed_values[t] * monthly_assess_growth
    assessed_values[t + 1] <- av

    if (t <= nrow(amort)) {
      # Scheduled P&I (without extra) for cost-matching — keeps the renter's
      # side budget unaffected by the buyer's voluntary extra principal.
      scheduled_pmt <- amort$interest[t] + amort$principal[t] - amort$extra_principal[t]
      extra_pmt <- amort$extra_principal[t]
      bal <- amort$balance[t]
    } else {
      scheduled_pmt <- 0
      extra_pmt <- 0
      bal <- 0
    }
    loan_balance[t + 1] <- bal

    prop_tax_monthly <- av * property_tax_rate / 100 / 12
    ins_monthly <- insurance_annual / 12
    maint_monthly <- hv * maintenance_pct / 100 / 12

    buy_gross_cost <- scheduled_pmt + prop_tax_monthly + ins_monthly + maint_monthly

    # Rental revenue the buyer collects from the property (roommate, photoshoots, etc.).
    # Grows annually at revenue_growth and is netted against ownership costs.
    monthly_revenue <- home_revenue * (1 + revenue_growth / 100)^(year - 1)

    buy_cost <- buy_gross_cost - monthly_revenue
    buy_monthly_cost[t] <- buy_cost + extra_pmt
    buy_monthly_revenue[t] <- monthly_revenue

    current_rent <- monthly_rent * (1 + rent_increase / 100)^(year - 1)
    renter_insurance <- 20
    rent_cost <- current_rent + renter_insurance
    rent_monthly_cost[t] <- rent_cost

    # Whoever pays less invests the difference; both have same total outflow
    diff <- buy_cost - rent_cost

    # Renter's portfolio
    rp <- rent_portfolio[t] * (1 + monthly_inv_return)
    if (diff > 0) {
      rp <- rp + diff
    }
    rent_portfolio[t + 1] <- rp

    # Buyer's portfolio (when rent exceeds buy cost)
    bp <- buy_portfolio[t] * (1 + monthly_inv_return)
    if (diff < 0) {
      bp <- bp + abs(diff)
    }
    # Extra principal is cash the buyer diverts from savings into loan paydown.
    # It reduces their investible portfolio but builds equity via the lower balance.
    bp <- bp - extra_pmt
    buy_portfolio[t + 1] <- bp

    home_equity[t + 1] <- hv * (1 - selling_cost_pct / 100) - bal
    buy_net_worth[t + 1] <- home_equity[t + 1] + bp
    rent_net_worth[t + 1] <- rp
  }

  month_seq <- 0:months

  data.frame(
    month = month_seq,
    year = month_seq / 12,
    home_value = home_values,
    loan_balance = loan_balance,
    home_equity = home_equity,
    buy_portfolio = buy_portfolio,
    buy_net_worth = buy_net_worth,
    rent_portfolio = rent_portfolio,
    rent_net_worth = rent_net_worth,
    buy_monthly_cost = c(NA, buy_monthly_cost),
    buy_monthly_revenue = c(NA, buy_monthly_revenue),
    rent_monthly_cost = c(NA, rent_monthly_cost)
  )
}

final_advantage <- function(home_price, ...) {
  d <- run_simulation(home_price = home_price, ...)
  tail(d, 1)$buy_net_worth - tail(d, 1)$rent_net_worth
}

find_breakeven_price <- function(
    down_pct, mortgage_rate, loan_term,
    closing_cost_pct, home_appreciation,
    property_tax_rate, prop_tax_cap, insurance_annual, maintenance_pct,
    selling_cost_pct,
    monthly_rent, rent_increase,
    investment_return, inflation_rate,
    monthly_income, horizon_years,
    home_revenue = 0, revenue_growth = 0,
    extra_principal = 0,
    tol = 500
) {
  shared <- list(
    down_pct = down_pct, mortgage_rate = mortgage_rate,
    loan_term = loan_term, closing_cost_pct = closing_cost_pct,
    home_appreciation = home_appreciation,
    property_tax_rate = property_tax_rate,
    prop_tax_cap = prop_tax_cap,
    insurance_annual = insurance_annual, maintenance_pct = maintenance_pct,
    selling_cost_pct = selling_cost_pct, monthly_rent = monthly_rent,
    rent_increase = rent_increase, investment_return = investment_return,
    inflation_rate = inflation_rate, monthly_income = monthly_income,
    horizon_years = horizon_years,
    home_revenue = home_revenue, revenue_growth = revenue_growth,
    extra_principal = extra_principal
  )
  f <- function(p) do.call(final_advantage, c(list(home_price = p), shared))

  lo <- 10000
  hi <- 5000000
  f_lo <- f(lo)
  f_hi <- f(hi)

  if (f_lo < 0) return(NA_real_)
  if (f_hi > 0) return(Inf)

  while ((hi - lo) > tol) {
    mid <- (lo + hi) / 2
    if (f(mid) > 0) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}

find_breakeven_rent <- function(
    home_price, down_pct, mortgage_rate, loan_term,
    closing_cost_pct, home_appreciation,
    property_tax_rate, prop_tax_cap, insurance_annual, maintenance_pct,
    selling_cost_pct,
    rent_increase,
    investment_return, inflation_rate,
    monthly_income, horizon_years,
    home_revenue = 0, revenue_growth = 0,
    extra_principal = 0,
    tol = 10
) {
  shared <- list(
    home_price = home_price, down_pct = down_pct,
    mortgage_rate = mortgage_rate, loan_term = loan_term,
    closing_cost_pct = closing_cost_pct,
    home_appreciation = home_appreciation,
    property_tax_rate = property_tax_rate,
    prop_tax_cap = prop_tax_cap,
    insurance_annual = insurance_annual, maintenance_pct = maintenance_pct,
    selling_cost_pct = selling_cost_pct, rent_increase = rent_increase,
    investment_return = investment_return, inflation_rate = inflation_rate,
    monthly_income = monthly_income, horizon_years = horizon_years,
    home_revenue = home_revenue, revenue_growth = revenue_growth,
    extra_principal = extra_principal
  )
  f <- function(r) do.call(final_advantage, c(list(monthly_rent = r), shared))

  lo <- 100
  hi <- 20000
  f_lo <- f(lo)
  f_hi <- f(hi)

  if (f_hi > 0) return(Inf)
  if (f_lo < 0) return(NA_real_)

  while ((hi - lo) > tol) {
    mid <- (lo + hi) / 2
    if (f(mid) > 0) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}

find_breakeven_year <- function(sim_data) {
  d <- sim_data
  advantage <- d$buy_net_worth - d$rent_net_worth
  crossings <- which(diff(sign(advantage)) != 0)
  if (length(crossings) == 0) return(NA_real_)
  # Linear interpolation at first crossing
  i <- crossings[1]
  a1 <- advantage[i]
  a2 <- advantage[i + 1]
  frac <- a1 / (a1 - a2)
  (d$year[i] + frac * (d$year[i + 1] - d$year[i]))
}
