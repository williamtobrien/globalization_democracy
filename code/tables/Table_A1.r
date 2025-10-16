# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(lmtest)
library(plm)
library(stargazer)

# IMPORT DATA ===========================
full_panel <- read.csv("data/full_panel.csv")

# CLEAN SAMPLE ===========================
temp <- full_panel %>%
  filter(!is.na(lag_fdi_inflows),
         !is.na(lag_fdi_outflows),
         !is.na(lag_trade),
         !is.na(lag_tariff_rate),
         !is.na(lag_gdppc),
         !is.na(lag_gdp_growth),
         !is.na(lag_inflation),
         !is.na(lag_pop_size),
         !is.na(lag_pop_growth))

temp <- temp %>%
  group_by(country) %>%
  filter(n() > 4) %>%
  ungroup()

# Have to do this so the table displays the "Log Population Size" variable
temp <- temp %>%
  mutate(lag_pop_size_log = log(lag_pop_size))

# FREEFAIR MODELS ===========================

## Dynamic TWFE Free Fair Models ----

# model 34, dynamic TWFE with controls and 1 DV lag
m34 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_pop_size_log +
            lag_pop_growth + lag_freefair,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse34 <- coeftest(m34, vcovHC(m34, type = "HC1", cluster = "group"))
crse34
nobs(m34)
wooldridge_m34 <- pwartest(m34)


# model 35, dynamic TWFE with controls and 2 DV lags
m35 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_pop_size_log +
            lag_pop_growth + lag_freefair + lag2_freefair,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse35 <- coeftest(m35, vcovHC(m35, type = "HC1", cluster = "group"))
crse35
nobs(m35)
wooldridge_m35 <- pwartest(m35)

## Arellano-Bond GMM Free Fair Models ----

# model 36, Arellano-Bond GMM with controls and 1 DV lag
m36 <- pgmm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_pop_size_log +
             lag_pop_growth + lag_freefair
           | lag(freefair, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse36 <- coeftest(m36, vcovHC(m36, type = "HC1", cluster = "group"))
crse36
nobs(m36)
ar2_m36 <- mtest(m36, order = 2)

# model 37, Arellano-Bond GMM with controls and 2 DV lags
m37 <- pgmm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_pop_size_log +
             lag_pop_growth + lag_freefair + lag2_freefair
           | lag(freefair, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse37 <- coeftest(m37, vcovHC(m37, type = "HC1", cluster = "group"))
crse37
nobs(m37)
ar2_m37 <- mtest(m37, order = 2)

# EQUALLAW MODELS ===========================

## Dynamic TWFE Equal Law Models ----

# model 38, dynamic TWFE with controls and 1 DV lag
m38 <- plm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_pop_size_log +
            lag_pop_growth + lag_equallaw,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse38 <- coeftest(m38, vcovHC(m38, type = "HC1", cluster = "group"))
crse38
nobs(m38)
wooldridge_m38 <- pwartest(m38)

# model 6, dynamic TWFE with controls and 2 DV lags
m39 <- plm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_pop_size_log +
            lag_pop_growth + lag_equallaw + lag2_equallaw,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse39 <- coeftest(m39, vcovHC(m39, type = "HC1", cluster = "group"))
crse39
nobs(m39)
wooldridge_m39 <- pwartest(m39)

## Arellano-Bond GMM Equal Law Models ----

# model 40, Arellano-Bond GMM with controls and 1 DV lag
m40 <- pgmm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_pop_size_log +
             lag_pop_growth + lag_equallaw
           | lag(equallaw, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse40 <- coeftest(m40, vcovHC(m40, type = "HC1", cluster = "group"))
crse40
nobs(m40)
ar2_m40 <- mtest(m40, order = 2)

# model 41, Arellano-Bond GMM with controls and 2 DV lags
m41 <- pgmm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_pop_size_log +
             lag_pop_growth + lag_equallaw + lag2_equallaw
           | lag(equallaw, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse41 <- coeftest(m41, vcovHC(m41, type = "HC1", cluster = "group"))
crse41
nobs(m41)
ar2_m41 <- mtest(m41, order = 2)

# EQUALACCESS MODELS ===========================

## Dynamic TWFE Equal Access Models ----

# model 42, dynamic TWFE with controls and 1 DV lag
m42 <- plm(equalaccess~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_pop_size_log +
            lag_pop_growth + lag_equalaccess,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse42 <- coeftest(m42, vcovHC(m42, type = "HC1", cluster = "group"))
crse42
nobs(m42)
wooldridge_m42 <- pwartest(m42)

# model 43, dynamic TWFE with controls and 2 DV lags
m43 <- plm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_pop_size_log +
             lag_pop_growth + lag_equalaccess + lag2_equalaccess,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse43 <- coeftest(m43, vcovHC(m43, type = "HC1", cluster = "group"))
crse43
nobs(m43)
wooldridge_m43 <- pwartest(m43)

## Arellano-Bond GMM Equal Access Models ---

# model 44, Arellano-Bond GMM with controls and 1 DV lag
m44 <- pgmm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + log(lag_gdppc) +
              lag_gdp_growth + lag_inflation + lag_pop_size_log +
              lag_pop_growth + lag_equalaccess
            | lag(equalaccess, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse44 <- coeftest(m44, vcovHC(m35, type = "HC1", cluster = "group"))
crse44
nobs(m44)
ar2_m44 <- mtest(m44, order = 2)

# model 45, Arellano-Bond GMM with controls and 2 DV lags
m45 <- pgmm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + log(lag_gdppc) +
              lag_gdp_growth + lag_inflation + lag_pop_size_log +
              lag_pop_growth + lag_equalaccess + lag2_equalaccess
            | lag(equalaccess, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse45 <- coeftest(m45, vcovHC(m45, type = "HC1", cluster = "group"))
crse45
nobs(m45)
ar2_m45 <- mtest(m45, order = 2)

# Because there are three DVs, I need to extract their coefficients, standard errors, and significance stars separately

extract_stars <- function(coef, p) {
  stars <- ""
  if (p < 0.01) {
    stars <- "$^{***}$" }
  else if (p < 0.05) {
    stars <- "$^{**}$" }
  else if (p < 0.10) {
    stars <- "$^{*}$" }
  return(paste0(round(coef, 2), stars))
}

extract_ses <- function(se) {
  paste0("(", round(se, 2), ")")
}

lag1_coef <- c(
  extract_stars(coef(m34)["lag_freefair"], crse34["lag_freefair", 4]),
  extract_stars(coef(m35)["lag_freefair"], crse35["lag_freefair", 4]),
  extract_stars(coef(m36)["lag_freefair"], crse36["lag_freefair", 4]),
  extract_stars(coef(m37)["lag_freefair"], crse37["lag_freefair", 4]),
  extract_stars(coef(m38)["lag_equallaw"], crse38["lag_equallaw", 4]),
  extract_stars(coef(m39)["lag_equallaw"], crse39["lag_equallaw", 4]),
  extract_stars(coef(m40)["lag_equallaw"], crse40["lag_equallaw", 4]),
  extract_stars(coef(m41)["lag_equallaw"], crse41["lag_equallaw", 4]),
  extract_stars(coef(m42)["lag_equalaccess"], crse42["lag_equalaccess", 4]),
  extract_stars(coef(m43)["lag_equalaccess"], crse43["lag_equalaccess", 4]),
  extract_stars(coef(m44)["lag_equalaccess"], crse44["lag_equalaccess", 4]),
  extract_stars(coef(m45)["lag_equalaccess"], crse45["lag_equalaccess", 4])
)


lag1_se <- c(
  extract_ses(crse34["lag_freefair", 2]),
  extract_ses(crse35["lag_freefair", 2]),
  extract_ses(crse36["lag_freefair", 2]),
  extract_ses(crse37["lag_freefair", 2]),
  extract_ses(crse38["lag_equallaw", 2]),
  extract_ses(crse39["lag_equallaw", 2]),
  extract_ses(crse40["lag_equallaw", 2]),
  extract_ses(crse41["lag_equallaw", 2]),
  extract_ses(crse42["lag_equalaccess", 2]),
  extract_ses(crse43["lag_equalaccess", 2]),
  extract_ses(crse44["lag_equalaccess", 2]),
  extract_ses(crse45["lag_equalaccess", 2])
)

lag2_coef <- c(
  "",
  extract_stars(coef(m35)["lag2_freefair"], crse35["lag2_freefair", 4]),
  "",
  extract_stars(coef(m37)["lag2_freefair"], crse37["lag2_freefair", 4]),
  "",
  extract_stars(coef(m39)["lag2_equallaw"], crse39["lag2_equallaw", 4]),
  "",
  extract_stars(coef(m41)["lag2_equallaw"], crse41["lag2_equallaw", 4]),
  "",
  extract_stars(coef(m43)["lag2_equalaccess"], crse43["lag2_equalaccess", 4]),
  "",
  extract_stars(coef(m45)["lag2_equalaccess"], crse45["lag2_equalaccess", 4]),
  ""
)


lag2_se <- c(
  "",
  extract_ses(crse35["lag2_freefair", 2]),
  "",
  extract_ses(crse37["lag2_freefair", 2]),
  "",
  extract_ses(crse39["lag2_equallaw", 2]),
  "",
  extract_ses(crse41["lag2_equallaw", 2]),
  "",
  extract_ses(crse43["lag2_equalaccess", 2]),
  "",
  extract_ses(crse45["lag2_equalaccess", 2])
)


table_A1 <- stargazer(m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, m45,
                     type = "latex",
                     title = "Pooled Analysis of Globalization's Effect on Democracy, Demographic Controls",
                     float = TRUE,
                     float.env = "sidewaystable",

                     se = list(crse34[,2], crse35[,2], crse36[,2], crse37[,2],
                               crse38[,2], crse39[,2], crse40[,2], crse41[,2],
                               crse42[,2], crse43[,2], crse44[,2], crse45[,2]),

                     apply.coef = function(x) x * 100,
                     apply.se = function(x) x * 100,

                     column.labels = c("TWFE", "Arellano-Bond",
                                       "TWFE", "Arellano-Bond",
                                       "TWFE", "Arellano-Bond"),
                     column.separate = c(2, 2, 2, 2, 2, 2),
                     keep = c("lag_fdi_inflows", "lag_fdi_outflows", "lag_trade", "lag_tariff_rate",
                              "lag_pop_size", "lag_pop_growth"),
                     covariate.labels = c("FDI Inflows", "FDI Outflows", "Trade", "Tariff Rate",
                                          "Population Size", "Population Growth"),

                     dep.var.caption = "Dependent Variable",
                     dep.var.labels = c("Free Fair", "Equal Law", "Equal Access"),
                     model.names = FALSE,

                     add.lines = list(
                       c("Lag 1", lag1_coef),
                       c("", lag1_se),
                       c("Lag 2", lag2_coef),
                       c("", lag2_se),
                       c("Wooldridge/AR2 test p-value", round(wooldridge_m34$p.value, 3),
                         round(wooldridge_m35$p.value, 3), round(ar2_m36$p.value, 3),
                         round(ar2_m37$p.value, 3),round(wooldridge_m38$p.value, 3), round(wooldridge_m39$p.value, 3),
                         round(ar2_m40$p.value, 3), round(ar2_m41$p.value, 3), round(wooldridge_m42$p.value, 3),
                         round(wooldridge_m43$p.value, 3), round(ar2_m44$p.value, 3),
                         round(ar2_m45$p.value, 3)),
                       c("Observations", nobs(m34), nobs(m35), nobs(m36), nobs(m37),
                         nobs(m38), nobs(m39), nobs(m40), nobs(m41),
                         nobs(m42), nobs(m43), nobs(m44), nobs(m45))
                     ),

                     column.sep.width = "2pt",
                     digits = 3,
                     font.size = "small",
                     omit.stat = c("f","ser","aic","ll", "rsq", "adj.rsq", "n"),
                     star.cutoffs = c(0.10, 0.05, 0.01),
                     star.char = c("*","**","***"),
                     notes = "\\parbox{1.2\\linewidth}{\\textit{Notes:} $^{*}p<0.1$; $^{**}
            p<0.05$; $^{***} p<0.01$. Robust standard errors clustered at the
            country level are reported in parentheses below their corresponding
            point estimates. The globalization variable coefficients and their
            standard errors are multiplied by 100 for ease of comparison and
            viewing The lag coefficients are left at the standard scale for
            display. Columns 1-2, 5-6, and 9-10 report results from the standard
            two-way fixed effects estimation on \\textit{Free Fair},
            \\textit{Equal Law} and \\textit{Equal Access} respectively. Columns
            3-4, 7-8, and 11-12 report results from the Arellano-Bond
            generalized methods of moments estimator for each dependent
            variable. All models include economic and demographic controls, country fixed
            effects, and year fixed effects. The Wooldridge/AR2 row reports
            p-values for Wooldridge's test for serial correlation for the
            standard TWFE models and p-values for the AR2 test for second-order
            serial correlation for the GMM models.}",
                     notes.align = "l",
                     notes.label = "",
                     notes.append = FALSE
)

cat(table_A1, file = "table_A1.tex")
