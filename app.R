library(shiny)
library(bslib)
library(ggplot2)
library(scales)

# -- Computation ---------------------------------------------------------------

compute_amortization <- function(loan_amount, annual_rate, term_years) {
  n <- term_years * 12
  r <- annual_rate / 100 / 12
  if (r == 0) {
    payment <- loan_amount / n
    schedule <- data.frame(
      month = seq_len(n),
      payment = payment,
      interest = 0,
      principal = payment,
      balance = loan_amount - cumsum(rep(payment, n))
    )
    return(schedule)
  }
  payment <- loan_amount * (r * (1 + r)^n) / ((1 + r)^n - 1)
  balance <- numeric(n)
  interest <- numeric(n)
  principal <- numeric(n)
  bal <- loan_amount
  for (i in seq_len(n)) {
    int <- bal * r
    prin <- payment - int
    bal <- bal - prin
    interest[i] <- int
    principal[i] <- prin
    balance[i] <- max(bal, 0)
  }
  data.frame(
    month = seq_len(n),
    payment = payment,
    interest = interest,
    principal = principal,
    balance = balance
  )
}

run_simulation <- function(
    home_price, down_pct, mortgage_rate, loan_term,
    closing_cost_pct, home_appreciation,
    property_tax_rate, insurance_annual, maintenance_pct,
    selling_cost_pct,
    monthly_rent, rent_increase,
    investment_return, inflation_rate,
    monthly_income, horizon_years
) {
  months <- horizon_years * 12

  down_payment <- home_price * down_pct / 100
  closing_costs <- home_price * closing_cost_pct / 100
  upfront_cash <- down_payment + closing_costs
  loan_amount <- home_price - down_payment

  amort <- compute_amortization(loan_amount, mortgage_rate, loan_term)

  monthly_inv_return <- (1 + investment_return / 100)^(1 / 12) - 1

  # Pre-allocate
  buy_net_worth <- numeric(months + 1)
  rent_net_worth <- numeric(months + 1)
  buy_monthly_cost <- numeric(months)
  rent_monthly_cost <- numeric(months)
  home_values <- numeric(months + 1)
  home_equity <- numeric(months + 1)
  rent_portfolio <- numeric(months + 1)
  buy_portfolio <- numeric(months + 1)
  loan_balance <- numeric(months + 1)

  home_values[1] <- home_price
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

    if (t <= nrow(amort)) {
      mortgage_pmt <- amort$payment[t]
      bal <- amort$balance[t]
    } else {
      mortgage_pmt <- 0
      bal <- 0
    }
    loan_balance[t + 1] <- bal

    prop_tax_monthly <- hv * property_tax_rate / 100 / 12
    ins_monthly <- insurance_annual / 12
    maint_monthly <- hv * maintenance_pct / 100 / 12

    buy_cost <- mortgage_pmt + prop_tax_monthly + ins_monthly + maint_monthly
    buy_monthly_cost[t] <- buy_cost

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
    property_tax_rate, insurance_annual, maintenance_pct,
    selling_cost_pct,
    monthly_rent, rent_increase,
    investment_return, inflation_rate,
    monthly_income, horizon_years,
    tol = 500
) {
  shared <- list(
    down_pct = down_pct, mortgage_rate = mortgage_rate,
    loan_term = loan_term, closing_cost_pct = closing_cost_pct,
    home_appreciation = home_appreciation,
    property_tax_rate = property_tax_rate,
    insurance_annual = insurance_annual, maintenance_pct = maintenance_pct,
    selling_cost_pct = selling_cost_pct, monthly_rent = monthly_rent,
    rent_increase = rent_increase, investment_return = investment_return,
    inflation_rate = inflation_rate, monthly_income = monthly_income,
    horizon_years = horizon_years
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
    property_tax_rate, insurance_annual, maintenance_pct,
    selling_cost_pct,
    rent_increase,
    investment_return, inflation_rate,
    monthly_income, horizon_years,
    tol = 10
) {
  shared <- list(
    home_price = home_price, down_pct = down_pct,
    mortgage_rate = mortgage_rate, loan_term = loan_term,
    closing_cost_pct = closing_cost_pct,
    home_appreciation = home_appreciation,
    property_tax_rate = property_tax_rate,
    insurance_annual = insurance_annual, maintenance_pct = maintenance_pct,
    selling_cost_pct = selling_cost_pct, rent_increase = rent_increase,
    investment_return = investment_return, inflation_rate = inflation_rate,
    monthly_income = monthly_income, horizon_years = horizon_years
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

# -- UI ------------------------------------------------------------------------

input_home <- accordion_panel(
  "Home Purchase",
  icon = icon("house"),
  numericInput("home_price", "Home Price ($)", 500000, min = 50000, step = 10000),
  sliderInput("down_pct", "Down Payment (%)", 5, 50, 20, step = 1),
  sliderInput("mortgage_rate", "Mortgage Rate (%)", 2, 10, 6.5, step = 0.125),
  selectInput("loan_term", "Loan Term", choices = c(15, 20, 30), selected = 30),
  sliderInput("closing_cost_pct", "Closing Costs (%)", 0, 6, 3, step = 0.5),
  sliderInput("home_appreciation", "Home Appreciation (%/yr)", -2, 10, 3, step = 0.5),
  sliderInput("selling_cost_pct", "Selling Costs (%)", 0, 8, 5, step = 0.5)
)

input_ownership <- accordion_panel(
  "Ownership Costs",
  icon = icon("wrench"),
  sliderInput("property_tax", "Property Tax (%/yr)", 0, 4, 1.2, step = 0.1),
  numericInput("insurance_annual", "Insurance ($/yr)", 1800, min = 0, step = 100),
  sliderInput("maintenance_pct", "Maintenance (% of home/yr)", 0, 3, 1, step = 0.25)
)

input_rental <- accordion_panel(
  "Rental",
  icon = icon("building"),
  numericInput("monthly_rent", "Monthly Rent ($)", 2500, min = 100, step = 100),
  sliderInput("rent_increase", "Annual Rent Increase (%)", 0, 10, 3, step = 0.5)
)

input_financial <- accordion_panel(
  "Financial Assumptions",
  icon = icon("chart-line"),
  sliderInput("investment_return", "Investment Return (%/yr)", 0, 15, 7, step = 0.5),
  sliderInput("inflation_rate", "Inflation (%/yr)", 0, 8, 2.5, step = 0.5)
)

input_personal <- accordion_panel(
  "Personal",
  icon = icon("user"),
  numericInput("monthly_income", "Gross Monthly Income ($)", 12000, min = 1000, step = 500),
  sliderInput("horizon", "Time Horizon (years)", 1, 40, 15, step = 1)
)

ui <- page_sidebar(
  title = "Rent vs. Buy",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    "navbar-bg" = "#2c3e50"
  ),
  sidebar = sidebar(
    width = 360,
    accordion(
      open = c("Home Purchase", "Rental"),
      input_home,
      input_ownership,
      input_rental,
      input_financial,
      input_personal
    )
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Verdict",
      value = textOutput("verdict_text"),
      showcase = icon("scale-balanced"),
      theme = "primary",
      full_screen = FALSE
    ),
    value_box(
      title = "Buy Net Worth",
      value = textOutput("buy_final_nw"),
      showcase = icon("house"),
      theme = "success"
    ),
    value_box(
      title = "Rent Net Worth",
      value = textOutput("rent_final_nw"),
      showcase = icon("building"),
      theme = "info"
    )
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Monthly Mortgage P&I",
      value = textOutput("mortgage_payment"),
      showcase = icon("money-bill"),
      theme = "light"
    ),
    value_box(
      title = "Total Buy Cost (Month 1)",
      value = textOutput("buy_cost_m1"),
      showcase = icon("receipt"),
      theme = "light"
    ),
    value_box(
      title = "Housing-to-Income Ratio",
      value = textOutput("affordability"),
      showcase = icon("gauge-high"),
      theme = "light"
    )
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Breakeven Home Price",
      value = textOutput("breakeven_price"),
      showcase = icon("house-circle-check"),
      theme = "warning",
      p("Max price where buying still wins")
    ),
    value_box(
      title = "Breakeven Rent",
      value = textOutput("breakeven_rent"),
      showcase = icon("arrow-up-right-dots"),
      theme = "warning",
      p("Min rent where buying wins")
    ),
    value_box(
      title = "Breakeven Year",
      value = textOutput("breakeven_year"),
      showcase = icon("calendar-check"),
      theme = "warning",
      p("When buying overtakes renting")
    )
  ),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Net Worth Over Time",
      plotOutput("net_worth_plot", height = "450px")
    ),
    nav_panel(
      "Monthly Costs",
      plotOutput("monthly_cost_plot", height = "450px")
    ),
    nav_panel(
      "Equity Breakdown",
      plotOutput("equity_plot", height = "450px")
    ),
    nav_panel(
      "Breakeven Analysis",
      plotOutput("breakeven_plot", height = "450px")
    ),
    nav_panel(
      "Summary Table",
      tableOutput("summary_table")
    )
  )
)

# -- Server --------------------------------------------------------------------

server <- function(input, output, session) {

  sim <- reactive({
    run_simulation(
      home_price = input$home_price,
      down_pct = input$down_pct,
      mortgage_rate = input$mortgage_rate,
      loan_term = as.integer(input$loan_term),
      closing_cost_pct = input$closing_cost_pct,
      home_appreciation = input$home_appreciation,
      property_tax_rate = input$property_tax,
      insurance_annual = input$insurance_annual,
      maintenance_pct = input$maintenance_pct,
      selling_cost_pct = input$selling_cost_pct,
      monthly_rent = input$monthly_rent,
      rent_increase = input$rent_increase,
      investment_return = input$investment_return,
      inflation_rate = input$inflation_rate,
      monthly_income = input$monthly_income,
      horizon_years = input$horizon
    )
  })

  final <- reactive({
    d <- sim()
    d[nrow(d), ]
  })

  mortgage_pmt <- reactive({
    dp <- input$home_price * input$down_pct / 100
    loan <- input$home_price - dp
    r <- input$mortgage_rate / 100 / 12
    n <- as.integer(input$loan_term) * 12
    if (r == 0) return(loan / n)
    loan * (r * (1 + r)^n) / ((1 + r)^n - 1)
  })

  # Value boxes
  output$verdict_text <- renderText({
    f <- final()
    diff <- f$buy_net_worth - f$rent_net_worth
    if (diff > 0) {
      paste0("Buy wins by ", dollar(diff, accuracy = 1))
    } else if (diff < 0) {
      paste0("Rent wins by ", dollar(abs(diff), accuracy = 1))
    } else {
      "Dead even"
    }
  })

  output$buy_final_nw <- renderText(dollar(final()$buy_net_worth, accuracy = 1))
  output$rent_final_nw <- renderText(dollar(final()$rent_net_worth, accuracy = 1))
  output$mortgage_payment <- renderText(dollar(mortgage_pmt(), accuracy = 1))

  output$buy_cost_m1 <- renderText({
    d <- sim()
    dollar(d$buy_monthly_cost[2], accuracy = 1)
  })

  output$affordability <- renderText({
    d <- sim()
    buy_cost <- d$buy_monthly_cost[2]
    ratio <- buy_cost / input$monthly_income * 100
    paste0(sprintf("%.0f%%", ratio), " of income")
  })

  # Breakeven computations
  shared_params <- reactive({
    list(
      down_pct = input$down_pct, mortgage_rate = input$mortgage_rate,
      loan_term = as.integer(input$loan_term),
      closing_cost_pct = input$closing_cost_pct,
      home_appreciation = input$home_appreciation,
      property_tax_rate = input$property_tax,
      insurance_annual = input$insurance_annual,
      maintenance_pct = input$maintenance_pct,
      selling_cost_pct = input$selling_cost_pct,
      rent_increase = input$rent_increase,
      investment_return = input$investment_return,
      inflation_rate = input$inflation_rate,
      monthly_income = input$monthly_income,
      horizon_years = input$horizon
    )
  })

  be_price <- reactive({
    p <- shared_params()
    p$monthly_rent <- input$monthly_rent
    do.call(find_breakeven_price, p)
  })

  be_rent <- reactive({
    p <- shared_params()
    p$home_price <- input$home_price
    do.call(find_breakeven_rent, p)
  })

  be_year <- reactive(find_breakeven_year(sim()))

  output$breakeven_price <- renderText({
    v <- be_price()
    if (is.na(v)) "Rent always wins"
    else if (is.infinite(v)) "Buy always wins"
    else dollar(v, accuracy = 1000)
  })

  output$breakeven_rent <- renderText({
    v <- be_rent()
    if (is.na(v)) "Buy always wins"
    else if (is.infinite(v)) "Rent always wins"
    else paste0(dollar(v, accuracy = 10), "/mo")
  })

  output$breakeven_year <- renderText({
    v <- be_year()
    if (is.na(v)) {
      f <- final()
      if (f$buy_net_worth >= f$rent_net_worth) "Buy leads from start"
      else "Never (in horizon)"
    } else {
      sprintf("Year %.1f", v)
    }
  })

  # Net worth plot
  output$net_worth_plot <- renderPlot({
    d <- sim()
    ggplot(d, aes(x = year)) +
      geom_line(aes(y = buy_net_worth, colour = "Buy"), linewidth = 1.2) +
      geom_line(aes(y = rent_net_worth, colour = "Rent + Invest"), linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
      scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      scale_colour_manual(values = c("Buy" = "#27ae60", "Rent + Invest" = "#2980b9")) +
      labs(
        x = "Years", y = "Net Worth from Housing Decision",
        colour = NULL,
        caption = "Buy net worth includes home equity (after selling costs) + invested savings.\nRent net worth = down payment + cost savings invested at market return."
      ) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        legend.position = "top",
        plot.caption = element_text(size = 10, colour = "grey50", hjust = 0)
      )
  })

  # Monthly cost plot
  output$monthly_cost_plot <- renderPlot({
    d <- sim()[-1, ]
    ggplot(d, aes(x = year)) +
      geom_line(aes(y = buy_monthly_cost, colour = "Buy"), linewidth = 1) +
      geom_line(aes(y = rent_monthly_cost, colour = "Rent"), linewidth = 1) +
      scale_y_continuous(labels = label_dollar()) +
      scale_colour_manual(values = c("Buy" = "#27ae60", "Rent" = "#2980b9")) +
      labs(x = "Years", y = "Monthly Housing Cost", colour = NULL) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(legend.position = "top")
  })

  # Equity breakdown
  output$equity_plot <- renderPlot({
    d <- sim()
    ggplot(d, aes(x = year)) +
      geom_area(aes(y = home_value), fill = "#27ae60", alpha = 0.2) +
      geom_line(aes(y = home_value, colour = "Home Value"), linewidth = 1) +
      geom_line(aes(y = loan_balance, colour = "Loan Balance"), linewidth = 1) +
      geom_line(aes(y = home_equity, colour = "Home Equity (net of selling costs)"), linewidth = 1) +
      scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      scale_colour_manual(values = c(
        "Home Value" = "#27ae60",
        "Loan Balance" = "#e74c3c",
        "Home Equity (net of selling costs)" = "#f39c12"
      )) +
      labs(x = "Years", y = NULL, colour = NULL) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(legend.position = "top")
  })

  # Breakeven sensitivity plot
  output$breakeven_plot <- renderPlot({
    p <- shared_params()
    p$monthly_rent <- input$monthly_rent
    base_price <- input$home_price
    prices <- seq(
      max(50000, base_price * 0.5),
      base_price * 2,
      length.out = 40
    )
    advantages <- vapply(prices, function(hp) {
      do.call(final_advantage, c(list(home_price = hp), p))
    }, numeric(1))

    plot_df <- data.frame(home_price = prices, advantage = advantages)

    ggplot(plot_df, aes(x = home_price, y = advantage)) +
      geom_line(colour = "#2c3e50", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#e74c3c", linewidth = 0.8) +
      geom_vline(
        xintercept = base_price, linetype = "dotted",
        colour = "#7f8c8d", linewidth = 0.8
      ) +
      annotate(
        "text", x = base_price, y = max(advantages) * 0.9,
        label = paste0("Your price: ", dollar(base_price)),
        hjust = -0.1, size = 4, colour = "#7f8c8d"
      ) +
      geom_area(
        data = plot_df[plot_df$advantage > 0, ],
        aes(y = advantage), fill = "#27ae60", alpha = 0.15
      ) +
      geom_area(
        data = plot_df[plot_df$advantage < 0, ],
        aes(y = advantage), fill = "#e74c3c", alpha = 0.15
      ) +
      scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      labs(
        x = "Home Price",
        y = paste0("Buy Advantage at Year ", input$horizon),
        caption = "Green = buying wins, Red = renting wins. Dotted line = your current home price."
      ) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(plot.caption = element_text(size = 10, colour = "grey50", hjust = 0))
  })

  # Summary table at key milestones
  output$summary_table <- renderTable({
    d <- sim()
    horizon <- input$horizon
    milestones <- sort(unique(c(1, 3, 5, 7, 10, 15, 20, 25, 30, horizon)))
    milestones <- milestones[milestones <= horizon]
    rows <- d[d$year %in% milestones, ]
    data.frame(
      Year = as.integer(rows$year),
      `Home Value` = dollar(rows$home_value, accuracy = 1),
      `Loan Balance` = dollar(rows$loan_balance, accuracy = 1),
      `Home Equity` = dollar(rows$home_equity, accuracy = 1),
      `Buy Net Worth` = dollar(rows$buy_net_worth, accuracy = 1),
      `Rent Portfolio` = dollar(rows$rent_portfolio, accuracy = 1),
      `Rent Net Worth` = dollar(rows$rent_net_worth, accuracy = 1),
      `Buy Advantage` = dollar(rows$buy_net_worth - rows$rent_net_worth, accuracy = 1),
      `Monthly Buy Cost` = dollar(rows$buy_monthly_cost, accuracy = 1),
      `Monthly Rent Cost` = dollar(rows$rent_monthly_cost, accuracy = 1),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrrrrrrrrr")
}

shinyApp(ui, server)
