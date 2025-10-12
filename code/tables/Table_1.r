# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(knitr)
library(kableExtra)

# CONSTRUCT TABLE 1. VARIABLE DESCRIPTIONS ===========================
var_table <- data.frame(
  Label = c("Free Fair", "Equal Law", "Equal Access",
               "FDI Inflows","FDI Outflows","Trade","Tariff Rate",
               "GDP per capita","GDP Growth","Inflation"),

  Description = c(
    "The extent to which elections are free and fair, which connotes an
    absence of registration fraud, systematic irregularities,
    government intimidation of the opposition, vote buying, and election
    violence.",

    "The extent to which laws are transparent and rigorously enforced and
    public administration impartial, and to which citizens enjoy access to
    justice, secure property rights, freedom from forced labor,
    freedom of movement, physical integrity rights, and freedom of religion.",

    "The extent to which all groups should enjoy equal \\textit{de facto}
    capabilities to participate, to serve in positions of political power, to
    put issues on the agenda, and to influence policymaking",

    "The net inflows of investment to acquire a lasting management interest (10
    percent or more of voting stock) in an enterprise operating in an economy
    other than that of the investor, calculated as the sum of equity capital,
    reinvestment of earnings, other long-term capital, and short-term capital
    as shown in the balance of payments, expressed as percentage of GDP",

    "The net outflows of investment to acquire a lasting management interest (10
    percent or more of voting stock) in an enterprise operating in an economy
    other than that of the investor, calculated as the sum of equity capital,
    reinvestment of earnings, other long-term capital, and short-term capital
    as shown in the balance of payments, expressed as percentage of GDP",

    "The sum of exports and imports of goods and services, expressed as a
    percentage of GDP.",

    "The average of effectively applied rates weighted by the product import
    shares corresponding to each partner country.",

    "The total income earned through production in an economy during an
    accounting period, expressed in constant 2021 international dollars,
    converted using purchasing power parities and divided by the
    general population.",

    "The annual growth rate of gross domestic product per capita, measured using
    constant 2015 U.S. dollars and expressed as the percentage change from the
    previous year.",

    "The annual growth rate of the GDP implicit deflator (the ratio of GDP in
    current local currency to GDP in constant local currency."
  )
)

table_1 <- kable(var_table,
                 format = "latex",
                 caption = "\\centering Variable Descriptions",
                 booktabs = TRUE,
                 col.names = c("Variable", "\\multicolumn{1}{c}{Description}"),
                 escape = FALSE,
                 linesep = "\\addlinespace[1.2ex]") %>%
                 kable_styling(latex_options = c("hold_position"),
                 font_size = 10,
                 position = "center") %>%
                 column_spec(1, width = "3cm") %>%
                 column_spec(2, width = "12cm")

cat(table_1, file = "tables/table_1.tex")
