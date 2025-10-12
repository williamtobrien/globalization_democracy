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
         !is.na(lag_tariff_rate))

# Filter out countries with fewer than 5 observations so TWFE and Arellano-Bond run without errors, also helps balance
temp <- temp %>%
  group_by(country) %>%
  filter(n() > 4) %>%
  ungroup()

# FREEFAIR MODELS ===========================

## Dynamic TWFE Free Fair Models ----

# model 13, dynamic TWFE with controls and 1 DV lag
m13 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + lag_freefair,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse13 <- coeftest(m13, vcovHC(m13, type = "HC1", cluster = "group"))
crse13
nobs(m13)
wooldridge_m13 <- pwartest(m13)


# model 14, dynamic TWFE with controls and 2 DV lags
m14 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + lag_freefair + lag2_freefair,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse14 <- coeftest(m14, vcovHC(m14, type = "HC1", cluster = "group"))
crse14
nobs(m14)
wooldridge_m14 <-pwartest(m14)

## Arellano-Bond GMM Free Fair Models ----

# model 15, Arellano-Bond GMM with controls and 1 DV lag
m15 <- pgmm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + lag_freefair
            | lag(freefair, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse15 <- coeftest(m15, vcovHC(m15, type = "HC1", cluster = "group"))
crse15
nobs(m15)
ar2_m15 <- mtest(m15, order = 2)

# model 16, Arellano-Bond GMM with controls and 2 DV lags
m16 <- pgmm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + lag_freefair + lag2_freefair
            | lag(freefair, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse16 <- coeftest(m16, vcovHC(m16, type = "HC1", cluster = "group"))
crse16
nobs(m16)
ar2_m16 <- mtest(m16, order = 2)

# EQUALLAW MODELS ===========================

## Dynamic TWFE Equal Law Models ----

# model 17, dynamic TWFE with controls and 1 DV lag
m17 <- plm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + lag_equallaw,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse17 <- coeftest(m17, vcovHC(m17, type = "HC1", cluster = "group"))
crse17
nobs(m17)
wooldridge_m17 <- pwartest(m17)

# model 18, dynamic TWFE with controls and 2 DV lags
m18 <- plm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + lag_equallaw + lag2_equallaw,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse18 <- coeftest(m18, vcovHC(m18, type = "HC1", cluster = "group"))
crse18
nobs(m18)
wooldridge_m18 <- pwartest(m18)

## Arellano-Bond GMM Equal Law Models ----

# model 19, Arellano-Bond GMM with controls and 1 DV lag
m19 <- pgmm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + lag_equallaw
            | lag(equallaw, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse19 <- coeftest(m19, vcovHC(m19, type = "HC1", cluster = "group"))
crse19
nobs(m19)
ar2_m19 <- mtest(m19, order = 2)

# model 20, Arellano-Bond GMM with controls and 2 DV lags
m20 <- pgmm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + lag_equallaw + lag2_equallaw
            | lag(equallaw, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse20 <- coeftest(m20, vcovHC(m20, type = "HC1", cluster = "group"))
crse20
nobs(m20)
ar2_m20 <- mtest(m20, order = 2)

# EQUALACCESS MODELS ===========================

## Dynamic TWFE Equal Access Models ----

# model 21, dynamic TWFE with controls and 1 DV lag
m21 <- plm(equalaccess~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + lag_equalaccess,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse21 <- coeftest(m21, vcovHC(m21, type = "HC1", cluster = "group"))
crse21
nobs(m21)
wooldridge_m21 <- pwartest(m21)

# model 22, dynamic TWFE with controls and 2 DV lags
m22 <- plm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
             lag_trade + lag_tariff_rate + lag_equalaccess + lag2_equalaccess,
           data = temp,
           index = c("country", "year"),
           model = "within",
           effect = "twoways")

crse22 <- coeftest(m22, vcovHC(m22, type = "HC1", cluster = "group"))
crse22
nobs(m22)
wooldridge_m22 <- pwartest(m22)

## Arellano-Bond GMM Equal Access Models ---

# model 23, Arellano-Bond GMM with controls and 1 DV lag
m23 <- pgmm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + lag_equalaccess
            | lag(equalaccess, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse23 <- coeftest(m23, vcovHC(m23, type = "HC1", cluster = "group"))
crse23
nobs(m23)
ar2_m23 <- mtest(m23, order = 2)

# model 24, Arellano-Bond GMM with controls and 2 DV lags
m24 <- pgmm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
              lag_trade + lag_tariff_rate + lag_equalaccess + lag2_equalaccess
            | lag(equalaccess, 2:99),
            data = temp,
            index = c("country", "year"),
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE)

crse24 <- coeftest(m24, vcovHC(m24, type = "HC1", cluster = "group"))
crse24
nobs(m24)
ar2_m24 <- mtest(m24, order = 2)


# Because there are three DVs, I need to extract their coefficients, standard errors, and significance stars separately
extract_stars <- function(coef, p) {
  stars <- ""
  if (p < 0.01) {
    stars <- "$^{***}$" }
  else if (p < 0.017) {
    stars <- "$^{**}$" }
  else if (p < 0.22) {
    stars <- "$^{*}$" }
  return(paste0(round(coef, 2), stars))
}

extract_ses <- function(se) {
  paste0("(", round(se, 2), ")")
}

lag1_coef <- c(
  extract_stars(coef(m13)["lag_freefair"], crse13["lag_freefair", 4]),
  extract_stars(coef(m14)["lag_freefair"], crse14["lag_freefair", 4]),
  extract_stars(coef(m15)["lag_freefair"], crse15["lag_freefair", 4]),
  extract_stars(coef(m16)["lag_freefair"], crse16["lag_freefair", 4]),
  extract_stars(coef(m17)["lag_equallaw"], crse17["lag_equallaw", 4]),
  extract_stars(coef(m18)["lag_equallaw"], crse18["lag_equallaw", 4]),
  extract_stars(coef(m19)["lag_equallaw"], crse19["lag_equallaw", 4]),
  extract_stars(coef(m20)["lag_equallaw"], crse20["lag_equallaw", 4]),
  extract_stars(coef(m21)["lag_equalaccess"], crse21["lag_equalaccess", 4]),
  extract_stars(coef(m22)["lag_equalaccess"], crse22["lag_equalaccess", 4]),
  extract_stars(coef(m23)["lag_equalaccess"], crse23["lag_equalaccess", 4]),
  extract_stars(coef(m24)["lag_equalaccess"], crse24["lag_equalaccess", 4])
)


lag1_se <- c(
  extract_ses(crse13["lag_freefair", 2]),
  extract_ses(crse14["lag_freefair", 2]),
  extract_ses(crse15["lag_freefair", 2]),
  extract_ses(crse16["lag_freefair", 2]),
  extract_ses(crse17["lag_equallaw", 2]),
  extract_ses(crse18["lag_equallaw", 2]),
  extract_ses(crse19["lag_equallaw", 2]),
  extract_ses(crse20["lag_equallaw", 2]),
  extract_ses(crse21["lag_equalaccess", 2]),
  extract_ses(crse22["lag_equalaccess", 2]),
  extract_ses(crse23["lag_equalaccess", 2]),
  extract_ses(crse24["lag_equalaccess", 2])
)

lag2_coef <- c(
  "",
  extract_stars(coef(m14)["lag2_freefair"], crse14["lag2_freefair", 4]),
  "",
  extract_stars(coef(m16)["lag2_freefair"], crse16["lag2_freefair", 4]),
  "",
  extract_stars(coef(m18)["lag2_equallaw"], crse18["lag2_equallaw", 4]),
  "",
  extract_stars(coef(m20)["lag2_equallaw"], crse20["lag2_equallaw", 4]),
  "",
  extract_stars(coef(m22)["lag2_equalaccess"], crse22["lag2_equalaccess", 4]),
  "",
  extract_stars(coef(m24)["lag2_equalaccess"], crse24["lag2_equalaccess", 4])
)


lag2_se <- c(
  "",
  extract_ses(crse14["lag2_freefair", 2]),
  "",
  extract_ses(crse16["lag2_freefair", 2]),
  "",
  extract_ses(crse18["lag2_equallaw", 2]),
  "",
  extract_ses(crse20["lag2_equallaw", 2]),
  "",
  extract_ses(crse22["lag2_equalaccess", 2]),
  "",
  extract_ses(crse24["lag2_equalaccess", 2])
)


table_4 <- stargazer(m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, m23, m24,
                     type = "latex",
                     title = "Pooled Analysis of Globalization's Effect on Democracy, No Controls",
                     float = TRUE,
                     float.env = "sidewaystable",

                     se = list(crse13[,2], crse14[,2], crse15[,2], crse16[,2],
                               crse17[,2], crse18[,2], crse19[,2], crse20[,2],
                               crse21[,2], crse22[,2], crse23[,2], crse24[,2]),

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
                       c("Wooldridge/AR2 test p-value", round(wooldridge_m13$p.value, 3),
                         round(wooldridge_m14$p.value, 3), round(ar2_m15$p.value, 3),
                         round(ar2_m16$p.value, 3),round(wooldridge_m17$p.value, 3), round(wooldridge_m18$p.value, 3),
                         round(ar2_m19$p.value, 3), round(ar2_m20$p.value, 3), round(wooldridge_m21$p.value, 3),
                         round(wooldridge_m22$p.value, 3), round(ar2_m23$p.value, 3),
                         round(ar2_m24$p.value, 3)),
                       c("Observations", nobs(m13), nobs(m14), nobs(m15), nobs(m16),
                         nobs(m17), nobs(m18), nobs(m19), nobs(m20),
                         nobs(m21), nobs(m22), nobs(m23), nobs(m24))
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
            variable. All models include country and year fixed effects. The
            Wooldridge/AR2 row reports p-values for Wooldridge's test for
            serial correlation for the standard TWFE models and p-values for
            the AR2 test for second-order serial correlation for the GMM
            models.}",
                     notes.align = "l",
                     notes.label = "",
                     notes.append = FALSE
)

cat(table_4, file = "tables/table_4.tex")
