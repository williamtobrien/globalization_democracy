# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(stargazer)

# IMPORT DATA ===========================
full_panel <- read.csv("data/full_panel.csv")

# TABLE 2. DESCRIPTIVE STATISTICS ==========================
temp <- full_panel %>%
  filter(year != 1959) %>%
  select(freefair, equalaccess, equallaw,
         fdi_inflows, fdi_outflows, trade,
         tariff_rate, gdppc, gdp_growth,
         inflation) %>%
  mutate(across(everything(), ~(as.numeric(.)))) %>%
  as.data.frame()

table_2 <- stargazer(temp,
          type = "latex",
          title = "Descriptive Statistics",
          style = "aer",
          summary = TRUE,
          summary.stat = c("n", "mean", "sd", "min", "max"),
          column.labels = c("Observations", "Mean", "Standard Deviation", "Minimum", "Maximum"),
          covariate.labels = c("Free Fair", "Equal Law", "Equal Access",
                               "FDI Inflows", "FDI Outflows", "Trade",
                               "Tariff Rate", "GDP per capita", "GDP Growth",
                               "Inflation"),
          notes = "\\hspace*{-\\tabcolsep}\\parbox{\\dimexpr\\textwidth+2\\tabcolsep\\relax}{\\textit{Notes:}
                   This table presents the descriptive statistics of my
                   variables over the period 1960 to 2024. Statistics are
                   computed over all available observations for each variable.
                   Democracy data are from the Varieties of Democracy (V-Dem)
                   Institute's Dataset v15. Data for the globalization and
                   control variables come from the World Bank's World
                   Development Indicators.}",
          notes.align = "l",
          notes.label = "",
          digits = 2
          )

cat(table_2, file = "table_2.tex")
