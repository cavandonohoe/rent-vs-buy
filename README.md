# Rent vs. Buy Calculator

**Live app:** [cavandonohoe.shinyapps.io/rent-vs-buy](https://cavandonohoe.shinyapps.io/rent-vs-buy/)

An interactive Shiny app that compares the financial outcome of buying a home versus renting and investing the difference. Adjust any assumption and see the impact in real time.

## What it does

Both scenarios start with the same amount of cash (equal to the buyer's down payment + closing costs). The buyer spends it on the house; the renter invests it. Each month, whoever pays less for housing invests the difference. At the end of your time horizon, the app compares total net worth.

### Inputs

| Category | Parameters |
|---|---|
| **Home Purchase** | Price, down payment %, mortgage rate, loan term (15/20/30), closing costs, appreciation rate, selling costs |
| **Ownership Costs** | Property tax rate, Prop 13 assessment cap, insurance, maintenance |
| **Rental** | Monthly rent, annual rent increase |
| **Financial** | Investment return rate, inflation rate |
| **Personal** | Gross monthly income, time horizon |

### Outputs

- **Verdict** — which option wins and by how much
- **Net worth over time** — two-line chart (buy vs. rent + invest)
- **Monthly cost comparison** — how housing costs diverge over time
- **Equity breakdown** — home value, loan balance, and net equity
- **Breakeven analysis** — sensitivity chart sweeping home prices to find the inflection point
- **Amortization table** — year-by-year mortgage breakdown alongside rent vs. buy net worth
- **Summary table** — key metrics at milestone years
- **How It Works** — plain-English explainer of every concept and assumption

### Breakeven metrics

- **Breakeven Home Price** — max price where buying still wins
- **Breakeven Rent** — min rent where buying beats renting
- **Breakeven Year** — when the buy line crosses above rent

## California / Prop 13

The model supports assessment caps. In California, property tax is based on purchase price (not market value) and the assessed value can only increase 2%/year regardless of appreciation. This is the default. To model other states, set the assessment growth cap equal to your home appreciation rate.

Rent increase defaults to 2%/year based on Santa Monica rent control (75% of CPI, capped at 3%).

## Run locally

```bash
git clone https://github.com/cavandonohoe/rent-vs-buy.git
cd rent-vs-buy
Rscript -e 'shiny::runApp("app.R")'
```

**Requirements:** R with `shiny`, `bslib`, `shinyWidgets`, `ggplot2`, `scales`.

```r
install.packages(c("shiny", "bslib", "shinyWidgets", "ggplot2", "scales"))
```

## Deploy to shinyapps.io

```r
rsconnect::deployApp(".", appName = "rent-vs-buy", account = "cavandonohoe", server = "shinyapps.io")
```

## What the model does NOT include

- Tax benefits (mortgage interest deduction, capital gains exclusion)
- PMI (if down payment < 20%)
- Rental income from house-hacking
- Emotional factors (stability, freedom to renovate, maintenance stress)

Tax benefits tend to favor buying; PMI and emotional costs tend to favor renting. For most people these roughly offset.
