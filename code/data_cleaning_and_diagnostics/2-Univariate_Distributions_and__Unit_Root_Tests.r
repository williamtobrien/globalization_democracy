# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(ggplot2)
library(plm)

# IMPORT DATA ===========================
full_panel <- read.csv("data/full_panel.csv")

# UNIVARIATE DISTRIBUTIONS ===========================
ggplot(full_panel, aes(x = freefair)) +
  geom_histogram(bins = 100)

ggplot(full_panel, aes(x = equallaw)) +
  geom_histogram(bins = 100)

ggplot(full_panel, aes(x = equalaccess)) +
  geom_histogram(bins = 100)

ggplot(full_panel, aes(x = fdi_inflows)) +
  geom_histogram(bins = 100)

ggplot(full_panel, aes(x = fdi_outflows)) +
  geom_histogram(bins = 100)

# Log, helps maintain linearity assumption
ggplot(full_panel, aes(x = trade)) +
  geom_histogram(bins = 100)

ggplot(full_panel, aes(x = log(tariff_rate))) +
  geom_histogram(bins = 100)

# Should log, very right-skewed and is common practice
ggplot(full_panel, aes(x = gdppc)) +
  geom_histogram(bins = 100)

ggplot(full_panel, aes(x = gdp_growth)) +
  geom_histogram(bins = 100)

# Often logged, but takes on negative values here so will leave
ggplot(full_panel, aes(x = inflation)) +
  geom_histogram(bins = 100)

# DIAGNOSTICS

test_panel <- full_panel %>%
  filter(!is.na(fdi_inflows),
         !is.na(fdi_outflows),
         !is.na(trade),
         !is.na(tariff_rate))

  ## Test for Unit Root Non-Stationarity ----
  # I drop panels of fewer than 10 years (the minimum for which the test runs)
  test_panel <- test_panel %>%
    group_by(country) %>%
    filter(n() >= 10) %>%
    ungroup()

  length(test_panel$country)

  unit_root_test <- function(dv) {
    tests <- c("ips", "madwu", "Pm")

    test_names <- character(length(tests))
    statistics <- numeric(length(tests))
    p_values <- numeric(length(tests))

    for (i in seq_along(tests)) {
      output <- purtest(as.formula(paste(dv, "~ trend")),
                        data = test_panel,
                        index = c("country", "year"),
                        test = tests[i],
                        exo = "trend",
                        lags = 1,
                        na.rm = TRUE)
      test_names[i] <- tests[i]
      p_values[i] <- output$statistic$p.value
    }

    results <- data.frame(
      Test = test_names,
      P_Value = p_values
    )

    return(results)
  }

  # all 3 tests strongly reject the null of a unit root for all independent and dependent variables
  unit_root_test("freefair")
  unit_root_test("equallaw")
  unit_root_test("equalaccess")
  unit_root_test("fdi_inflows")
  unit_root_test("fdi_outflows")
  unit_root_test("trade")
  unit_root_test("tariff_rate")

  rm(test_panel)
