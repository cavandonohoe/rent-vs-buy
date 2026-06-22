library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(scales)
library(jsonlite)

# -- Saved inputs --------------------------------------------------------------

# Inputs that should be persisted/restored. Keep this list in sync with the UI.
SAVED_INPUT_IDS <- c(
  "home_price", "down_pct", "mortgage_rate", "loan_term", "extra_principal",
  "closing_cost_pct", "home_appreciation", "selling_cost_pct",
  "property_tax", "prop_tax_cap", "insurance_annual", "maintenance_pct",
  "monthly_rent", "rent_increase",
  "home_revenue", "revenue_growth",
  "investment_return", "inflation_rate",
  "monthly_income", "horizon"
)

# Numeric (autonumericInput) inputs need updateAutonumericInput; everything else
# is a slider or selectInput we can update via the standard helpers.
AUTONUMERIC_INPUTS <- c(
  "home_price", "extra_principal", "insurance_annual",
  "monthly_rent", "home_revenue", "monthly_income"
)
SELECT_INPUTS <- c("loan_term")

apply_saved_inputs <- function(session, values) {
  if (!is.list(values)) return(invisible())
  for (id in intersect(names(values), SAVED_INPUT_IDS)) {
    val <- values[[id]]
    if (is.null(val) || length(val) == 0) next
    if (length(val) == 1 && is.na(val)) next
    if (id %in% AUTONUMERIC_INPUTS) {
      shinyWidgets::updateAutonumericInput(session, id, value = val)
    } else if (id %in% SELECT_INPUTS) {
      updateSelectInput(session, id, selected = as.character(val))
    } else {
      updateSliderInput(session, id, value = val)
    }
  }
  invisible()
}


# -- Computation ---------------------------------------------------------------
#
# Pure financial-math functions live in R/finance.R so they can be unit-tested
# in isolation (see tests/testthat/test-finance.R). The source() resolves both
# in local development and after rsconnect::deployApp() bundles the app dir.

source("R/finance.R")

# -- UI ------------------------------------------------------------------------

help_text <- function(...) tags$small(class = "text-muted", ...)

input_home <- accordion_panel(
  "Home Purchase",
  icon = icon("house"),
  autonumericInput("home_price", "Home Price ($)", 1000000,
    currencySymbol = "$", currencySymbolPlacement = "p",
    decimalPlaces = 0, minimumValue = 0, modifyValueOnWheel = FALSE,
    selectOnFocus = TRUE, emptyInputBehavior = "null",
    overrideMinMaxLimits = "ignore"),
  help_text("Total purchase price of the property."),
  sliderInput("down_pct", "Down Payment (%)", 5, 50, 20, step = 1),
  help_text(
    "Cash you pay upfront. 20% avoids PMI. This money could be invested instead,",
    "which is the core opportunity cost of buying."
  ),
  sliderInput("mortgage_rate", "Mortgage Rate (%)", 2, 10, 6.5, step = 0.125),
  help_text("Annual interest rate on your loan. Even small changes have a big impact over 30 years."),
  selectInput("loan_term", "Loan Term", choices = c(15, 20, 30), selected = 30),
  help_text("Shorter terms mean higher monthly payments but far less total interest paid."),
  autonumericInput("extra_principal", "Extra Principal Payment ($/mo)", 0,
    currencySymbol = "$", currencySymbolPlacement = "p",
    decimalPlaces = 0, minimumValue = 0, modifyValueOnWheel = FALSE,
    selectOnFocus = TRUE, emptyInputBehavior = "null",
    overrideMinMaxLimits = "ignore"),
  help_text(
    "Extra payment toward principal each month on top of the scheduled P&I.",
    "Pays the loan off faster, saves interest, and builds equity sooner.",
    "Modeled as cash diverted from your investment portfolio into the mortgage \u2014",
    "i.e., the opportunity cost of prepaying vs. investing at the market rate is captured."
  ),
  sliderInput("closing_cost_pct", "Closing Costs (%)", 0, 6, 3, step = 0.5),
  help_text(
    "One-time fees at purchase: appraisal, title insurance, origination fees, etc.",
    "Typically 2\u20135% of the home price. This is money spent that you never get back."
  ),
  sliderInput("home_appreciation", "Home Appreciation (%/yr)", -2, 10, 3, step = 0.5),
  help_text(
    "How fast the home gains value each year. The US long-run average is ~3\u20134%.",
    "This is the main driver of whether buying wins."
  ),
  sliderInput("selling_cost_pct", "Selling Costs (%)", 0, 8, 5, step = 0.5),
  help_text(
    "When you eventually sell: agent commissions (5\u20136%), transfer taxes, repairs.",
    "Deducted from your equity to show what you'd actually pocket."
  )
)

input_ownership <- accordion_panel(
  "Ownership Costs",
  icon = icon("wrench"),
  sliderInput("property_tax", "Property Tax (%/yr)", 0, 4, 1.1, step = 0.1),
  help_text(
    "Annual tax as a % of assessed value. California's base rate is 1% (Prop 13),",
    "plus local bonds and Mello-Roos districts typically add 0.1\u20130.4%.",
    "New buyers in the Bay Area often pay ~1.1\u20131.25% effective."
  ),
  sliderInput("prop_tax_cap", "Assessment Growth Cap (%/yr)", 0, 10, 2, step = 0.5),
  help_text(
    "Max annual increase in assessed value. California Prop 13 caps this at 2%/yr",
    "regardless of market appreciation. Set to match home appreciation to disable",
    "the cap (i.e., tax on full market value, as in most other states)."
  ),
  autonumericInput("insurance_annual", "Insurance ($/yr)", 1800,
    currencySymbol = "$", currencySymbolPlacement = "p",
    decimalPlaces = 0, minimumValue = 0, modifyValueOnWheel = FALSE,
    selectOnFocus = TRUE, emptyInputBehavior = "null",
    overrideMinMaxLimits = "ignore"),
  help_text("Homeowner's insurance. Required by your lender. Covers damage, liability, etc."),
  sliderInput("maintenance_pct", "Maintenance (% of home/yr)", 0, 3, 1, step = 0.25),
  help_text(
    "Ongoing upkeep: roof, HVAC, plumbing, appliances, landscaping.",
    "The 1% rule is a common estimate. Older homes trend higher."
  )
)

input_rental <- accordion_panel(
  "Rental",
  icon = icon("building"),
  autonumericInput("monthly_rent", "Monthly Rent ($)", 3000,
    currencySymbol = "$", currencySymbolPlacement = "p",
    decimalPlaces = 0, minimumValue = 0, modifyValueOnWheel = FALSE,
    selectOnFocus = TRUE, emptyInputBehavior = "null",
    overrideMinMaxLimits = "ignore"),
  help_text("Your current or expected monthly rent for a comparable place."),
  sliderInput("rent_increase", "Annual Rent Increase (%)", 0, 10, 2, step = 0.5),
  help_text(
    "How much rent goes up each year. Santa Monica rent control caps increases",
    "at 75% of CPI (max 3%/yr). Recent actuals: 2.3% (2025), 3.0% (2024),",
    "2.8% (2023). Historical average is ~2\u20132.5%. Set higher for non-rent-controlled units",
    "or if you expect to move to a new lease at market rate."
  )
)

input_home_revenue <- accordion_panel(
  "Home Revenue (Buyer)",
  icon = icon("sack-dollar"),
  help_text(
    "Monthly income the buyer collects from the property. This reduces the buyer's",
    "effective monthly housing cost \u2014 the renter cannot sublet a house they don't own,",
    "so this only applies to the Buy scenario. Modeled as pre-tax nominal revenue."
  ),
  autonumericInput("home_revenue", "Monthly Home Revenue ($/mo)", 0,
    currencySymbol = "$", currencySymbolPlacement = "p",
    decimalPlaces = 0, minimumValue = 0, modifyValueOnWheel = FALSE,
    selectOnFocus = TRUE, emptyInputBehavior = "null",
    overrideMinMaxLimits = "ignore"),
  help_text(
    "Combined monthly revenue from any rentals: roommate rent, photoshoot/film",
    "location fees, Airbnb, ADU, etc. Use a realistic average that already accounts",
    "for vacancy or unbooked months."
  ),
  sliderInput("revenue_growth", "Revenue Growth (%/yr)", 0, 10, 2, step = 0.5),
  help_text(
    "How fast rental/shoot revenue grows each year. Usually tracks local rent inflation."
  )
)

input_financial <- accordion_panel(
  "Financial Assumptions",
  icon = icon("chart-line"),
  sliderInput("investment_return", "Investment Return (%/yr)", 0, 15, 7, step = 0.5),
  help_text(
    "Expected annual return if you invest in the stock market instead.",
    "S&P 500 historical average is ~10% nominal, ~7% after inflation.",
    "This is what your down payment and monthly savings could earn."
  ),
  sliderInput("inflation_rate", "Inflation (%/yr)", 0, 8, 2.5, step = 0.5),
  help_text("General price inflation. Included for reference; the model uses nominal values.")
)

input_saved <- accordion_panel(
  "Saved Inputs",
  icon = icon("bookmark"),
  help_text(
    "Your inputs auto-save to this browser, so they're restored next time you ",
    "visit on the same device. To move inputs between devices, share with ",
    "someone, or keep multiple scenarios, use the buttons below."
  ),
  div(
    class = "d-grid gap-2 mt-2",
    actionButton(
      "copy_link", "Copy shareable link",
      icon = icon("link"), class = "btn-outline-primary btn-sm"
    ),
    downloadButton(
      "download_inputs", "Download inputs (.json)",
      icon = icon("download"), class = "btn-outline-secondary btn-sm"
    ),
    fileInput(
      "upload_inputs", NULL,
      buttonLabel = list(icon("upload"), " Load inputs (.json)"),
      placeholder = "No file selected",
      accept = c("application/json", ".json")
    ),
    actionButton(
      "reset_inputs", "Reset to defaults",
      icon = icon("rotate-left"), class = "btn-outline-danger btn-sm"
    )
  ),
  div(id = "saved_status", class = "text-muted small mt-2")
)

input_personal <- accordion_panel(
  "Personal",
  icon = icon("user"),
  autonumericInput("monthly_income", "Gross Monthly Income ($)", 12000,
    currencySymbol = "$", currencySymbolPlacement = "p",
    decimalPlaces = 0, minimumValue = 0, modifyValueOnWheel = FALSE,
    selectOnFocus = TRUE, emptyInputBehavior = "null",
    overrideMinMaxLimits = "ignore"),
  help_text(
    "Pre-tax monthly income. Used to calculate your housing-to-income ratio.",
    "Lenders typically want this under 28%. Above 36% is risky territory."
  ),
  sliderInput("horizon", "Time Horizon (years)", 1, 40, 15, step = 1),
  help_text(
    "How long you plan to stay. Buying almost never wins under 5 years",
    "because closing + selling costs eat your equity. The longer you stay,",
    "the more time appreciation and principal paydown have to compound."
  )
)

ui <- page_sidebar(
  title = "Rent vs. Buy",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    "navbar-bg" = "#2c3e50"
  ),
  tags$head(
    tags$style(HTML("
    .bslib-value-box .value-box-value {
      font-size: clamp(1rem, 2.5vw, 1.75rem) !important;
      white-space: nowrap !important;
      overflow: visible !important;
    }
    .bslib-value-box .value-box-title {
      white-space: nowrap !important;
      overflow: visible !important;
    }
    #saved_status.flash {
      color: #27ae60 !important;
      transition: color 0.3s ease;
    }
  ")),
    tags$script(HTML(sprintf("
    (function() {
      var STORAGE_KEY = 'rentVsBuy.inputs.v1';
      var SAVED_IDS = %s;
      var QUERY_PARAM = 'inputs';

      function flashStatus(msg) {
        var el = document.getElementById('saved_status');
        if (!el) return;
        el.textContent = msg;
        el.classList.add('flash');
        setTimeout(function() { el.classList.remove('flash'); }, 1500);
      }

      function readQueryInputs() {
        try {
          var qs = new URLSearchParams(window.location.search);
          var raw = qs.get(QUERY_PARAM);
          if (!raw) return null;
          var json = atob(raw.replace(/-/g, '+').replace(/_/g, '/'));
          return JSON.parse(decodeURIComponent(escape(json)));
        } catch (e) {
          console.warn('Failed to parse inputs from URL', e);
          return null;
        }
      }

      function readLocalInputs() {
        try {
          var raw = localStorage.getItem(STORAGE_KEY);
          if (!raw) return null;
          return JSON.parse(raw);
        } catch (e) {
          return null;
        }
      }

      function encodeForUrl(obj) {
        var json = JSON.stringify(obj);
        var b64 = btoa(unescape(encodeURIComponent(json)));
        return b64.replace(/\\+/g, '-').replace(/\\//g, '_').replace(/=+$/, '');
      }

      Shiny.addCustomMessageHandler('rvb_save_local', function(values) {
        try {
          localStorage.setItem(STORAGE_KEY, JSON.stringify(values));
        } catch (e) {}
      });

      Shiny.addCustomMessageHandler('rvb_clear_local', function(_) {
        try { localStorage.removeItem(STORAGE_KEY); } catch (e) {}
        flashStatus('Cleared saved inputs.');
      });

      Shiny.addCustomMessageHandler('rvb_copy_link', function(values) {
        var url = window.location.origin + window.location.pathname +
          '?' + QUERY_PARAM + '=' + encodeForUrl(values);
        var done = function() { flashStatus('Link copied to clipboard.'); };
        var fail = function() {
          window.prompt('Copy this link to share/save your inputs:', url);
        };
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(url).then(done, fail);
        } else {
          fail();
        }
      });

      Shiny.addCustomMessageHandler('rvb_flash_status', function(msg) {
        flashStatus(msg);
      });

      // On startup, send any existing URL/localStorage inputs to R.
      $(document).on('shiny:connected', function() {
        var fromUrl = readQueryInputs();
        var fromLocal = readLocalInputs();
        var payload = fromUrl || fromLocal || null;
        Shiny.setInputValue('rvb_initial_inputs', {
          values: payload,
          source: fromUrl ? 'url' : (fromLocal ? 'local' : 'none')
        }, { priority: 'event' });
      });
    })();
  ", jsonlite::toJSON(SAVED_INPUT_IDS))))
  ),
  sidebar = sidebar(
    width = 360,
    accordion(
      open = c("Home Purchase", "Rental"),
      input_home,
      input_ownership,
      input_rental,
      input_home_revenue,
      input_financial,
      input_personal,
      input_saved
    )
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = textOutput("verdict_title"),
      value = textOutput("verdict_text"),
      showcase = icon("scale-balanced"),
      theme = "primary",
      full_screen = FALSE,
      p(textOutput("verdict_detail"))
    ),
    value_box(
      title = textOutput("buy_nw_title"),
      value = textOutput("buy_final_nw"),
      showcase = icon("house"),
      theme = "success",
      p("Home equity (after selling costs) + invested savings")
    ),
    value_box(
      title = textOutput("rent_nw_title"),
      value = textOutput("rent_final_nw"),
      showcase = icon("building"),
      theme = "info",
      p("Down payment + monthly savings invested at market return")
    )
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Monthly Mortgage Payment",
      value = textOutput("mortgage_payment"),
      showcase = icon("money-bill"),
      theme = "light"
    ),
    value_box(
      title = "Net Buy Cost (Month 1)",
      value = textOutput("buy_cost_m1"),
      showcase = icon("receipt"),
      theme = "light",
      p(textOutput("buy_cost_m1_detail"))
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
      p(textOutput("breakeven_price_note"))
    ),
    value_box(
      title = "Breakeven Rent",
      value = textOutput("breakeven_rent"),
      showcase = icon("arrow-up-right-dots"),
      theme = "warning",
      p(textOutput("breakeven_rent_note"))
    ),
    value_box(
      title = "Breakeven Year",
      value = textOutput("breakeven_year"),
      showcase = icon("calendar-check"),
      theme = "warning",
      p(textOutput("breakeven_year_note"))
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
    ),
    nav_panel(
      "Amortization",
      tableOutput("amort_table")
    ),
    nav_panel(
      "How It Works",
      div(
        class = "p-3",
        style = "max-width: 800px;",
        tags$h4("The Core Question"),
        tags$p(
          "If you have enough cash for a down payment, should you use it to buy a home,",
          "or keep renting and invest that cash in the stock market? This tool runs both",
          "scenarios side-by-side and tells you which one leaves you wealthier."
        ),

        tags$h4("How the Model Works"),
        tags$p(tags$strong("Both people start with the same cash"), " \u2014 equal to the",
          "down payment + closing costs the buyer would spend. Both people also have the",
          "same monthly budget (whichever housing cost is higher that month)."),
        tags$ul(
          tags$li(
            tags$strong("The Buyer"), " spends the cash on the down payment and closing costs.",
            "Each month they pay mortgage principal & interest, property tax, insurance,",
            "and maintenance. They build equity as the loan balance drops and the home",
            "appreciates. If their monthly cost is", tags$em("less"), "than the renter's,",
            "they invest the difference."
          ),
          tags$li(
            tags$strong("The Renter"), " invests all of that upfront cash in the stock market",
            "on day one. Each month they pay rent (+ renter's insurance). If their monthly",
            "cost is", tags$em("less"), "than the buyer's, they invest the difference too."
          )
        ),
        tags$p(
          "At the end of your time horizon, we compare total net worth from the housing",
          "decision: the buyer's home equity (after selling costs) + any investments, vs.",
          "the renter's investment portfolio."
        ),

        tags$h4("Key Concepts"),
        tags$dl(
          tags$dt("Opportunity Cost"),
          tags$dd(
            "The biggest hidden cost of buying. Your down payment could be earning 7\u201310%",
            "per year in the stock market. If your home only appreciates at 3%, you're",
            "giving up potential returns. The model captures this by having the renter invest",
            "the full down payment on day one."
          ),
          tags$dt("Home Equity"),
          tags$dd(
            "What you'd actually pocket if you sold today: the home's market value,",
            "minus the remaining loan balance, minus selling costs (agent commissions,",
            "transfer taxes, etc.). Early on, most of your mortgage payment goes to",
            tags$em("interest"), " \u2014 you build equity slowly at first, then faster."
          ),
          tags$dt("Amortization"),
          tags$dd(
            "Your fixed monthly payment is split between interest and principal.",
            "In month 1 of a $400K loan at 6.5%, about $2,167 goes to interest and",
            "only $361 goes to principal. By year 20, that flips. A 15-year loan builds",
            "equity much faster but costs more per month."
          ),
          tags$dt("Extra Principal Payments"),
          tags$dd(
            "Paying extra each month above the scheduled P&I goes directly to principal,",
            "shrinking the balance faster and saving compound interest. The model treats",
            "this extra as cash diverted from your investment portfolio, so it captures",
            "the true trade-off: prepaying earns a guaranteed ",
            tags$em("mortgage-rate"), "return, while investing earns a (risky) market",
            "return. If your mortgage rate < expected investment return, prepaying is",
            "typically a worse financial move \u2014 but it provides psychological peace",
            "of mind and guaranteed savings that the market doesn't."
          ),
          tags$dt("Housing-to-Income Ratio"),
          tags$dd(
            "The percentage of your gross income that goes to housing. Lenders use 28%",
            "as a guideline for mortgage approval (the 'front-end ratio'). Above 36%",
            "is generally considered stretched. This tool shows yours so you can gut-check",
            "affordability \u2014 not just whether buying is optimal, but whether it's",
            tags$em("comfortable"), "."
          ),
          tags$dt("Breakeven Home Price"),
          tags$dd(
            "The maximum you could pay for a home and still beat renting over your time",
            "horizon, with all other assumptions held constant. If the actual price is above",
            "this, renting + investing wins."
          ),
          tags$dt("Breakeven Rent"),
          tags$dd(
            "The minimum rent that would make buying worthwhile. If your rent is below",
            "this threshold, renting is the better deal. Useful for comparing apartments",
            "at different price points."
          ),
          tags$dt("Breakeven Year"),
          tags$dd(
            "How many years until buying overtakes renting. If this is longer than you plan",
            "to stay, buying doesn't make financial sense regardless of the final numbers."
          )
        ),

        tags$h4("What This Model Does NOT Include"),
        tags$ul(
          tags$li("Tax benefits (mortgage interest deduction, property tax deduction, capital gains exclusion)"),
          tags$li("Taxes on rental / photoshoot revenue (model is pre-tax)"),
          tags$li("PMI (private mortgage insurance) if down payment < 20%"),
          tags$li("Emotional factors: stability, freedom to renovate, stress of maintenance, privacy cost of a roommate"),
          tags$li("Transaction costs of investing (negligible with index funds)"),
          tags$li("State/local income tax variations")
        ),
        tags$p(
          class = "text-muted mt-3",
          "Tax benefits tend to favor buying; PMI and emotional costs tend to favor renting.",
          "For most people these roughly wash out, but your situation may differ."
        ),

        tags$h4("Home Revenue (House-hacking)"),
        tags$p(
          "If you rent a room to a roommate, use the property for photoshoots, host on",
          "Airbnb, etc., that revenue reduces your effective monthly housing cost. This",
          "applies only to the", tags$em("Buy"), "scenario \u2014 a renter generally",
          "can't sublet or commercially lease a home they don't own."
        ),
        tags$p(
          "The model treats the Monthly Home Revenue input as pre-tax nominal income that",
          "grows each year at the Revenue Growth rate. Use a realistic average that already",
          "accounts for vacancy and unbooked months. The net buy cost",
          "(gross ownership costs \u2212 revenue) is what gets compared to rent in the",
          "cash-flow matching logic; if the net buy cost drops below rent, the buyer",
          "invests the surplus each month."
        ),
        tags$p(
          class = "text-muted",
          "Note: rental income is taxable, but the model is pre-tax throughout.",
          "Depreciation, Schedule E deductions, and the ~14-day rule for short-term",
          "rentals can materially change after-tax economics \u2014 talk to a CPA."
        ),

        tags$h4("California / Prop 13 Note"),
        tags$p(
          "This model supports Prop 13-style property tax assessment caps. In California,",
          "your property is taxed on its", tags$em("assessed"), "value (purchase price),",
          "not its current market value. The assessed value can increase at most 2% per year,",
          "even if the home appreciates at 5\u201310%. This means your property tax bill grows",
          "much slower than in states that reassess at market value annually."
        ),
        tags$p(
          "To model a non-Prop-13 state, set the Assessment Growth Cap equal to your Home",
          "Appreciation rate, so the assessed value tracks the market."
        ),

        tags$h4("Rules of Thumb"),
        tags$ul(
          tags$li(tags$strong("The 5-year rule:"),
            " Buying rarely makes sense if you'll move within 5 years.",
            " Closing + selling costs typically eat any equity you've built."),
          tags$li(tags$strong("Price-to-rent ratio:"),
            " Divide annual rent into home price. Under 15 = buying favors you.",
            " 15\u201320 = toss-up. Over 20 = renting likely wins.",
            " (e.g., $500K home / $30K annual rent = 16.7)"),
          tags$li(tags$strong("The 28/36 rule:"),
            " Housing costs should be under 28% of gross income.",
            " Total debt payments should be under 36%.")
        )
      )
    )
  )
)

# -- Server --------------------------------------------------------------------

server <- function(input, output, session) {

  safe_val <- function(x, default = 0) {
    if (is.null(x) || length(x) == 0 || is.na(x)) default else x
  }

  # -- Saved inputs: capture, restore, persist ---------------------------------

  current_input_values <- function() {
    vals <- list()
    for (id in SAVED_INPUT_IDS) {
      v <- isolate(input[[id]])
      if (!is.null(v) && length(v) > 0 && !all(is.na(v))) {
        vals[[id]] <- v
      }
    }
    vals
  }

  # Track the initial restore so we don't immediately overwrite the saved blob
  # with defaults during the brief moment before inputs are reapplied.
  has_restored <- reactiveVal(FALSE)

  observeEvent(input$rvb_initial_inputs, {
    payload <- input$rvb_initial_inputs
    if (!is.null(payload$values)) {
      apply_saved_inputs(session, payload$values)
      msg <- if (identical(payload$source, "url")) {
        "Loaded inputs from link."
      } else {
        "Restored your saved inputs."
      }
      session$sendCustomMessage("rvb_flash_status", msg)
    }
    has_restored(TRUE)
  }, ignoreNULL = FALSE, once = TRUE)

  # Debounce auto-save so we don't hammer localStorage on every slider tick.
  inputs_to_save <- reactive({
    lapply(SAVED_INPUT_IDS, function(id) input[[id]])
  }) |> debounce(500)

  observe({
    inputs_to_save()
    if (!isTRUE(has_restored())) return()
    session$sendCustomMessage("rvb_save_local", current_input_values())
  })

  observeEvent(input$copy_link, {
    session$sendCustomMessage("rvb_copy_link", current_input_values())
  })

  output$download_inputs <- downloadHandler(
    filename = function() {
      paste0("rent-vs-buy-inputs-", format(Sys.Date(), "%Y%m%d"), ".json")
    },
    content = function(file) {
      jsonlite::write_json(
        current_input_values(), file,
        auto_unbox = TRUE, pretty = TRUE
      )
    },
    contentType = "application/json"
  )

  observeEvent(input$upload_inputs, {
    f <- input$upload_inputs
    if (is.null(f)) return()
    parsed <- tryCatch(
      jsonlite::read_json(f$datapath, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed) || !is.list(parsed)) {
      session$sendCustomMessage("rvb_flash_status", "Could not read that file.")
      return()
    }
    apply_saved_inputs(session, parsed)
    session$sendCustomMessage("rvb_flash_status", "Loaded inputs from file.")
  })

  observeEvent(input$reset_inputs, {
    defaults <- list(
      home_price = 1000000, down_pct = 20, mortgage_rate = 6.5,
      loan_term = "30", extra_principal = 0, closing_cost_pct = 3,
      home_appreciation = 3, selling_cost_pct = 5,
      property_tax = 1.1, prop_tax_cap = 2, insurance_annual = 1800,
      maintenance_pct = 1,
      monthly_rent = 3000, rent_increase = 2,
      home_revenue = 0, revenue_growth = 2,
      investment_return = 7, inflation_rate = 2.5,
      monthly_income = 12000, horizon = 15
    )
    apply_saved_inputs(session, defaults)
    session$sendCustomMessage("rvb_clear_local", "")
  })

  sim <- reactive({
    run_simulation(
      home_price = safe_val(input$home_price, 1000000),
      down_pct = input$down_pct,
      mortgage_rate = input$mortgage_rate,
      loan_term = as.integer(input$loan_term),
      closing_cost_pct = input$closing_cost_pct,
      home_appreciation = input$home_appreciation,
      property_tax_rate = input$property_tax,
      prop_tax_cap = input$prop_tax_cap,
      insurance_annual = safe_val(input$insurance_annual, 1800),
      maintenance_pct = input$maintenance_pct,
      selling_cost_pct = input$selling_cost_pct,
      monthly_rent = safe_val(input$monthly_rent, 3000),
      rent_increase = input$rent_increase,
      investment_return = input$investment_return,
      inflation_rate = input$inflation_rate,
      monthly_income = safe_val(input$monthly_income, 12000),
      horizon_years = input$horizon,
      home_revenue = safe_val(input$home_revenue, 0),
      revenue_growth = safe_val(input$revenue_growth, 0),
      extra_principal = safe_val(input$extra_principal, 0)
    )
  })

  final <- reactive({
    d <- sim()
    d[nrow(d), ]
  })

  mortgage_pmt <- reactive({
    hp <- safe_val(input$home_price, 1000000)
    dp <- hp * input$down_pct / 100
    loan <- hp - dp
    r <- input$mortgage_rate / 100 / 12
    n <- as.integer(input$loan_term) * 12
    if (r == 0) return(loan / n)
    loan * (r * (1 + r)^n) / ((1 + r)^n - 1)
  })

  # Value boxes
  output$verdict_title <- renderText({
    paste0(input$horizon, "-Year Verdict")
  })

  output$verdict_text <- renderText({
    f <- final()
    diff <- f$buy_net_worth - f$rent_net_worth
    if (diff > 0) {
      paste0("Buy +", dollar(diff, accuracy = 1))
    } else if (diff < 0) {
      paste0("Rent +", dollar(abs(diff), accuracy = 1))
    } else {
      "Dead even"
    }
  })

  output$verdict_detail <- renderText({
    f <- final()
    diff <- f$buy_net_worth - f$rent_net_worth
    winner <- if (diff >= 0) "Buying" else "Renting"
    paste0(winner, " wins by ", dollar(abs(diff), accuracy = 1))
  })

  output$buy_nw_title <- renderText({
    paste0("Buy (Yr ", input$horizon, ")")
  })

  output$rent_nw_title <- renderText({
    paste0("Rent (Yr ", input$horizon, ")")
  })

  output$buy_final_nw <- renderText(dollar(final()$buy_net_worth, accuracy = 1))
  output$rent_final_nw <- renderText(dollar(final()$rent_net_worth, accuracy = 1))
  output$mortgage_payment <- renderText(dollar(mortgage_pmt(), accuracy = 1))

  output$buy_cost_m1 <- renderText({
    d <- sim()
    dollar(d$buy_monthly_cost[2], accuracy = 1)
  })

  output$buy_cost_m1_detail <- renderText({
    d <- sim()
    rev <- d$buy_monthly_revenue[2]
    if (is.na(rev) || rev <= 0) {
      "Mortgage + taxes + insurance + maintenance"
    } else {
      gross <- d$buy_monthly_cost[2] + rev
      paste0(
        "Gross ", dollar(gross, accuracy = 1),
        " \u2212 revenue ", dollar(rev, accuracy = 1)
      )
    }
  })

  output$affordability <- renderText({
    d <- sim()
    buy_cost <- d$buy_monthly_cost[2]
    ratio <- buy_cost / safe_val(input$monthly_income, 12000) * 100
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
      prop_tax_cap = input$prop_tax_cap,
      insurance_annual = safe_val(input$insurance_annual, 1800),
      maintenance_pct = input$maintenance_pct,
      selling_cost_pct = input$selling_cost_pct,
      rent_increase = input$rent_increase,
      investment_return = input$investment_return,
      inflation_rate = input$inflation_rate,
      monthly_income = safe_val(input$monthly_income, 12000),
      horizon_years = input$horizon,
      home_revenue = safe_val(input$home_revenue, 0),
      revenue_growth = safe_val(input$revenue_growth, 0),
      extra_principal = safe_val(input$extra_principal, 0)
    )
  })

  be_price <- reactive({
    p <- shared_params()
    p$monthly_rent <- safe_val(input$monthly_rent, 3000)
    do.call(find_breakeven_price, p)
  })

  be_rent <- reactive({
    p <- shared_params()
    p$home_price <- safe_val(input$home_price, 1000000)
    do.call(find_breakeven_rent, p)
  })

  be_year <- reactive(find_breakeven_year(sim()))

  output$breakeven_price <- renderText({
    v <- be_price()
    if (is.na(v)) "N/A"
    else if (is.infinite(v)) "Any"
    else dollar(v, accuracy = 1000)
  })

  output$breakeven_price_note <- renderText({
    v <- be_price()
    if (is.na(v)) "Renting wins at any price"
    else if (is.infinite(v)) "Buying wins at any price"
    else "Max price where buying still wins"
  })

  output$breakeven_rent <- renderText({
    v <- be_rent()
    if (is.na(v)) "N/A"
    else if (is.infinite(v)) "Any"
    else paste0(dollar(v, accuracy = 10), "/mo")
  })

  output$breakeven_rent_note <- renderText({
    v <- be_rent()
    if (is.na(v)) "Buying wins at any rent"
    else if (is.infinite(v)) "Renting wins at any rent"
    else "Min rent where buying wins"
  })

  output$breakeven_year <- renderText({
    v <- be_year()
    if (is.na(v)) {
      f <- final()
      if (f$buy_net_worth >= f$rent_net_worth) "Day 1"
      else "Never"
    } else {
      sprintf("Year %.1f", v)
    }
  })

  output$breakeven_year_note <- renderText({
    v <- be_year()
    if (is.na(v)) {
      f <- final()
      if (f$buy_net_worth >= f$rent_net_worth) "Buying leads from the start"
      else paste0("Buying never catches up within ", input$horizon, " years")
    } else {
      "When buying overtakes renting"
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
    has_revenue <- any(d$buy_monthly_revenue > 0, na.rm = TRUE)
    buy_gross <- d$buy_monthly_cost + d$buy_monthly_revenue

    p <- ggplot(d, aes(x = year)) +
      geom_line(aes(y = buy_monthly_cost, colour = "Buy (net)"), linewidth = 1) +
      geom_line(aes(y = rent_monthly_cost, colour = "Rent"), linewidth = 1)

    colours <- c("Buy (net)" = "#27ae60", "Rent" = "#2980b9")

    if (has_revenue) {
      p <- p +
        geom_line(
          data = data.frame(year = d$year, y = buy_gross),
          aes(y = y, colour = "Buy (gross, before revenue)"),
          linewidth = 0.8, linetype = "dashed"
        ) +
        geom_line(
          aes(y = buy_monthly_revenue, colour = "Home Revenue"),
          linewidth = 0.8, linetype = "dotted"
        )
      colours <- c(colours,
        "Buy (gross, before revenue)" = "#7f8c8d",
        "Home Revenue" = "#8e44ad"
      )
    }

    p +
      scale_y_continuous(labels = label_dollar()) +
      scale_colour_manual(values = colours) +
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
    p$monthly_rent <- safe_val(input$monthly_rent, 3000)
    base_price <- safe_val(input$home_price, 1000000)
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

  # Amortization table with rent vs buy comparison
  output$amort_table <- renderTable({
    hp <- safe_val(input$home_price, 1000000)
    dp <- hp * input$down_pct / 100
    loan <- hp - dp
    term <- as.integer(input$loan_term)
    extra <- safe_val(input$extra_principal, 0)
    a <- compute_amortization(loan, input$mortgage_rate, term, extra_principal = extra)
    a$cum_interest <- cumsum(a$interest)
    a$cum_principal <- cumsum(a$principal)
    a$cum_payment <- cumsum(a$payment)
    a$cum_extra <- cumsum(a$extra_principal)

    yearly <- a[a$month %% 12 == 0, ]
    yr_interest <- yearly$cum_interest - c(0, yearly$cum_interest[-nrow(yearly)])
    yr_principal <- yearly$cum_principal - c(0, yearly$cum_principal[-nrow(yearly)])
    yr_payment <- yearly$cum_payment - c(0, yearly$cum_payment[-nrow(yearly)])
    yr_extra <- yearly$cum_extra - c(0, yearly$cum_extra[-nrow(yearly)])

    d <- sim()
    horizon <- input$horizon
    years <- seq_len(min(term, horizon))
    sim_rows <- d[d$year %in% years, ]

    n <- min(nrow(yearly), length(years))
    yearly <- yearly[seq_len(n), ]
    sim_rows <- sim_rows[seq_len(n), ]
    yr_interest <- yr_interest[seq_len(n)]
    yr_principal <- yr_principal[seq_len(n)]
    yr_payment <- yr_payment[seq_len(n)]
    yr_extra <- yr_extra[seq_len(n)]

    out <- data.frame(
      Year = yearly$month / 12,
      `Mortgage Payment` = dollar(yr_payment, accuracy = 1),
      `To Interest` = dollar(yr_interest, accuracy = 1),
      `To Principal` = dollar(yr_principal, accuracy = 1),
      check.names = FALSE
    )
    if (extra > 0) {
      out$`Extra Principal` <- dollar(yr_extra, accuracy = 1)
    }
    out$`Cumul. Interest` <- dollar(yearly$cum_interest, accuracy = 1)
    out$`Loan Balance` <- dollar(yearly$balance, accuracy = 1)
    out$`Home Value` <- dollar(sim_rows$home_value, accuracy = 1)
    out$`Home Equity` <- dollar(sim_rows$home_equity, accuracy = 1)
    out$`Buy Net Worth` <- dollar(sim_rows$buy_net_worth, accuracy = 1)
    out$`Rent Net Worth` <- dollar(sim_rows$rent_net_worth, accuracy = 1)
    out$`Buy Advantage` <- dollar(
      sim_rows$buy_net_worth - sim_rows$rent_net_worth, accuracy = 1
    )
    out
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrrrrrrrrrrr")

  # Summary table at key milestones
  output$summary_table <- renderTable({
    d <- sim()
    horizon <- input$horizon
    milestones <- sort(unique(c(1, 3, 5, 7, 10, 15, 20, 25, 30, horizon)))
    milestones <- milestones[milestones <= horizon]
    rows <- d[d$year %in% milestones, ]
    has_revenue <- any(d$buy_monthly_revenue > 0, na.rm = TRUE)
    out <- data.frame(
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
    if (has_revenue) {
      out$`Monthly Home Revenue` <- dollar(rows$buy_monthly_revenue, accuracy = 1)
    }
    out
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrrrrrrrrr")
}

shinyApp(ui, server)
