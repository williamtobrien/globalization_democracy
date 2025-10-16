# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(multiwayvcov)
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
         !is.na(lag_inflation))

# Filter out countries with fewer than 5 observations so TWFE and Arellano-Bond run without errors, also helps balance
temp <- temp %>%
  group_by(country) %>%
  filter(n() > 4) %>%
  ungroup()

# FREEFAIR MODELS ===========================

## Dynamic TWFE Free Fair Models ----

# model 1, dynamic TWFE with controls and 1 DV lag
m1 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_freefair,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse1 <- coeftest(m1, vcovHC(m1, type = "HC1", cluster = "group"))
crse1
nobs(m1)
wooldridge_m1 <- pwartest(m1)


# model 2, dynamic TWFE with controls and 2 DV lags
m2 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_freefair + lag2_freefair,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse2 <- coeftest(m2, vcovHC(m2, type = "HC1", cluster = "group"))
crse2
nobs(m2)
wooldridge_m2 <- pwartest(m2)

## Arellano-Bond GMM Free Fair Models ----

# model 3, Arellano-Bond GMM with controls and 1 DV lag
m3 <- pgmm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_freefair
           | lag(freefair, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse3 <- coeftest(m3, vcovHC(m3, type = "HC1", cluster = "group"))
crse3
nobs(m3)
ar2_m3 <- mtest(m3, order = 2)

# model 4, Arellano-Bond GMM with controls and 2 DV lags
m4 <- pgmm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_freefair + lag2_freefair
           | lag(freefair, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse4 <- coeftest(m4, vcovHC(m4, type = "HC1", cluster = "group"))
crse4
nobs(m4)
ar2_m4 <- mtest(m4, order = 2)

# EQUALLAW MODELS ===========================

## Dynamic TWFE Equal Law Models ----

# model 5, dynamic TWFE with controls and 1 DV lag
m5 <- plm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_equallaw,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse5 <- coeftest(m5, vcovHC(m5, type = "HC1", cluster = "group"))
crse5
nobs(m5)
wooldridge_m5 <- pwartest(m5)

# model 6, dynamic TWFE with controls and 2 DV lags
m6 <- plm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_equallaw + lag2_equallaw,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse6 <- coeftest(m6, vcovHC(m6, type = "HC1", cluster = "group"))
crse6
nobs(m6)
wooldridge_m6 <- pwartest(m6)

## Arellano-Bond GMM Equal Law Models ----

# model 7, Arellano-Bond GMM with controls and 1 DV lag
m7 <- pgmm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + log(lag_gdppc) +
             lag_gdp_growth + lag_inflation + lag_equallaw
           | lag(equallaw, 2:99),
           data = temp,
           index = c("country", "year"),
           effect = "twoways",
           model = "onestep",
           transformation = "d",
           collapse = TRUE)

crse7 <- coeftest(m7, vcovHC(m7, type = "HC1", cluster = "group"))
crse7
nobs(m7)
ar2_m7 <- mtest(m7, order = 2)

# model 8, Arellano-Bond GMM with controls and 2 DV lags
m8 <- pgmm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + log(lag_gdppc) +
              lag_gdp_growth + lag_inflation + lag_equallaw + lag2_equallaw
            | lag(equallaw, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse8 <- coeftest(m8, vcovHC(m8, type = "HC1", cluster = "group"))
crse8
nobs(m8)
ar2_m8 <- mtest(m8, order = 2)

# EQUALACCESS MODELS ===========================

## Dynamic TWFE Equal Access Models ----

# model 9, dynamic TWFE with controls and 1 DV lag
m9 <- plm(equalaccess~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_equalaccess,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse9 <- coeftest(m9, vcovHC(m9, type = "HC1", cluster = "group"))
crse9
nobs(m9)
wooldridge_m9 <- pwartest(m9)

# model 10, dynamic TWFE with controls and 2 DV lags
m10 <- plm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_equalaccess + lag2_equalaccess,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse10 <- coeftest(m10, vcovHC(m10, type = "HC1", cluster = "group"))
crse10
nobs(m10)
wooldridge_m10 <- pwartest(m10)

## Arellano-Bond GMM Equal Access Models ---

# model 11, Arellano-Bond GMM with controls and 1 DV lag
m11 <- pgmm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + log(lag_gdppc) +
              lag_gdp_growth + lag_inflation + lag_equalaccess
            | lag(equalaccess, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse11 <- coeftest(m11, vcovHC(m11, type = "HC1", cluster = "group"))
crse11
nobs(m11)
ar2_m11 <- mtest(m11, order = 2)

# model 12, Arellano-Bond GMM with controls and 2 DV lags
m12 <- pgmm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + log(lag_gdppc) +
              lag_gdp_growth + lag_inflation + lag_equalaccess + lag2_equalaccess
            | lag(equalaccess, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse12 <- coeftest(m12, vcovHC(m12, type = "HC1", cluster = "group"))
crse12
nobs(m12)
ar2_m12 <- mtest(m12, order = 2)

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
  extract_stars(coef(m1)["lag_freefair"], crse1["lag_freefair", 4]),
  extract_stars(coef(m2)["lag_freefair"], crse2["lag_freefair", 4]),
  extract_stars(coef(m3)["lag_freefair"], crse3["lag_freefair", 4]),
  extract_stars(coef(m4)["lag_freefair"], crse4["lag_freefair", 4]),
  extract_stars(coef(m5)["lag_equallaw"], crse5["lag_equallaw", 4]),
  extract_stars(coef(m6)["lag_equallaw"], crse6["lag_equallaw", 4]),
  extract_stars(coef(m7)["lag_equallaw"], crse7["lag_equallaw", 4]),
  extract_stars(coef(m8)["lag_equallaw"], crse8["lag_equallaw", 4]),
  extract_stars(coef(m9)["lag_equalaccess"], crse9["lag_equalaccess", 4]),
  extract_stars(coef(m10)["lag_equalaccess"], crse10["lag_equalaccess", 4]),
  extract_stars(coef(m11)["lag_equalaccess"], crse11["lag_equalaccess", 4]),
  extract_stars(coef(m12)["lag_equalaccess"], crse12["lag_equalaccess", 4])
)


lag1_se <- c(
  extract_ses(crse1["lag_freefair", 2]),
  extract_ses(crse2["lag_freefair", 2]),
  extract_ses(crse3["lag_freefair", 2]),
  extract_ses(crse4["lag_freefair", 2]),
  extract_ses(crse5["lag_equallaw", 2]),
  extract_ses(crse6["lag_equallaw", 2]),
  extract_ses(crse7["lag_equallaw", 2]),
  extract_ses(crse8["lag_equallaw", 2]),
  extract_ses(crse9["lag_equalaccess", 2]),
  extract_ses(crse10["lag_equalaccess", 2]),
  extract_ses(crse11["lag_equalaccess", 2]),
  extract_ses(crse12["lag_equalaccess", 2])
)

lag2_coef <- c(
  "",
  extract_stars(coef(m2)["lag2_freefair"], crse2["lag2_freefair", 4]),
  "",
  extract_stars(coef(m4)["lag2_freefair"], crse4["lag2_freefair", 4]),
  "",
  extract_stars(coef(m6)["lag2_equallaw"], crse6["lag2_equallaw", 4]),
  "",
  extract_stars(coef(m8)["lag2_equallaw"], crse8["lag2_equallaw", 4]),
  "",
  extract_stars(coef(m10)["lag2_equalaccess"], crse10["lag2_equalaccess", 4]),
  "",
  extract_stars(coef(m12)["lag2_equalaccess"], crse12["lag2_equalaccess", 4])
)


lag2_se <- c(
  "",
  extract_ses(crse2["lag2_freefair", 2]),
  "",
  extract_ses(crse4["lag2_freefair", 2]),
  "",
  extract_ses(crse6["lag2_equallaw", 2]),
  "",
  extract_ses(crse8["lag2_equallaw", 2]),
  "",
  extract_ses(crse10["lag2_equalaccess", 2]),
  "",
  extract_ses(crse12["lag2_equalaccess", 2])
)


table_3 <- stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12,
            type = "latex",
            title = "Pooled Analysis of Globalization's Effect on Democracy",
            float = TRUE,
            float.env = "sidewaystable",

            se = list(crse1[,2], crse2[,2], crse3[,2], crse4[,2],
                    crse5[,2], crse6[,2], crse7[,2], crse8[,2],
                    crse9[,2], crse10[,2], crse11[,2], crse12[,2]),

            apply.coef = function(x) x * 100,
            apply.se = function(x) x * 100,

            column.labels = c("TWFE", "Arellano-Bond",
                            "TWFE", "Arellano-Bond",
                            "TWFE", "Arellano-Bond"),
            column.separate = c(2, 2, 2, 2, 2, 2),
            keep = c("lag_fdi_inflows", "lag_fdi_outflows", "lag_trade", "lag_tariff_rate"),
            covariate.labels = c("FDI Inflows", "FDI Outflows", "Trade", "Tariff Rate"),

            dep.var.caption = "Dependent Variable",
            dep.var.labels = c("Free Fair", "Equal Law", "Equal Access"),
            model.names = FALSE,

            add.lines = list(
              c("Lag 1", lag1_coef),
              c("", lag1_se),
              c("Lag 2", lag2_coef),
              c("", lag2_se),
              c("Wooldridge/AR2 test p-value", round(wooldridge_m1$p.value, 3),
              round(wooldridge_m2$p.value, 3), round(ar2_m3$p.value, 3),
              round(ar2_m4$p.value, 3),round(wooldridge_m5$p.value, 3), round(wooldridge_m6$p.value, 3),
              round(ar2_m7$p.value, 3), round(ar2_m8$p.value, 3), round(wooldridge_m9$p.value, 3),
              round(wooldridge_m10$p.value, 3), round(ar2_m11$p.value, 3),
              round(ar2_m12$p.value, 3)),
              c("Observations", nobs(m1), nobs(m2), nobs(m3), nobs(m4),
              nobs(m5), nobs(m6), nobs(m7), nobs(m8),
              nobs(m9), nobs(m10), nobs(m11), nobs(m12))
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
            variable. All models include economic controls, country fixed
            effects, and year fixed effects. The Wooldridge/AR2 row reports
            p-values for Wooldridge's test for serial correlation for the
            standard TWFE models and p-values for the AR2 test for second-order
            serial correlation for the GMM models.}",
            notes.align = "l",
            notes.label = "",
            notes.append = FALSE
            )

cat(table_3, file = "table_3.tex")
