# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(multiwayvcov)
library(lmtest)

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

temp <- temp %>%
  group_by(country) %>%
  filter(n() > 4) %>%
  ungroup()


# FREE FAIR MODELS ===========================

# establish baseline model, same as m2 from Table 3, but explicitly modeling time/year dummies in equation
mod_m2 <- lm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_freefair + lag2_freefair +
              as.factor(country) + as.factor(year),
            data = temp)

# compute standardized residuals and cook's distance, then modify original sample
std_resid <- rstandard(mod_m2)
cooks_d <- cooks.distance(mod_m2)

resid_df <- temp[!is.na(std_resid)&
                   abs(std_resid) < 1.96, ]

cooks_df <- temp[!is.na(cooks_d) &
                   cooks_d < 4/nobs(mod_m2), ]


# Residual Free Fair
m46 <- plm(
  freefair ~ lag_fdi_inflows + lag_fdi_outflows + lag_trade + lag_tariff_rate +
    log(lag_gdppc) + lag_gdp_growth + lag_inflation +
    lag_freefair + lag2_freefair,
  data = resid_df,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

crse46 <- coeftest(m46, vcovHC(m46, type = "HC1", cluster = "group"))
crse46
nobs(m46)
wooldridge_m46 <- pwartest(m46)

# Cook's Free Fair
m47 <- plm(
  freefair ~ lag_fdi_inflows + lag_fdi_outflows + lag_trade + lag_tariff_rate +
    log(lag_gdppc) + lag_gdp_growth + lag_inflation +
    lag_freefair + lag2_freefair,
  data = cooks_df,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

crse47 <- coeftest(m47, vcovHC(m47, type = "HC1", cluster = "group"))
crse47
nobs(m47)
wooldridge_m47 <- pwartest(m47)

# EQUAL LAW MODELS ===========================

# same as m6 from table 3
mod_m6 <- lm(equallaw ~ lag_fdi_inflows + lag_fdi_outflows +
               lag_trade + lag_tariff_rate + log(lag_gdppc) +
               lag_gdp_growth + lag_inflation + lag_equallaw + lag2_equallaw +
               as.factor(country) + as.factor(year),
             data = temp)

# compute standardized residuals and cook's distance, then modify original sample
std_resid <- rstandard(mod_m6)
cooks_d <- cooks.distance(mod_m6)

resid_df <- temp[!is.na(std_resid)&
                   abs(std_resid) < 1.96, ]

cooks_df <- temp[!is.na(cooks_d) &
                   cooks_d < 4/nobs(mod_m2), ]


# Residual Equal Law
m48 <- plm(
  equallaw ~ lag_fdi_inflows + lag_fdi_outflows + lag_trade + lag_tariff_rate +
    log(lag_gdppc) + lag_gdp_growth + lag_inflation +
    lag_equallaw + lag2_equallaw,
  data = resid_df,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

crse48 <- coeftest(m48, vcovHC(m48, type = "HC1", cluster = "group"))
crse48
nobs(m48)
wooldridge_m48 <- pwartest(m48)

# Cook's Equal Law

m49 <- plm(
  equallaw ~ lag_fdi_inflows + lag_fdi_outflows + lag_trade + lag_tariff_rate +
    log(lag_gdppc) + lag_gdp_growth + lag_inflation +
    lag_equallaw + lag2_equallaw,
  data = cooks_df,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

crse49 <- coeftest(m49, vcovHC(m49, type = "HC1", cluster = "group"))
crse49
nobs(m49)
wooldridge_m49 <- pwartest(m49)

# EQUAL ACCESS MODELS ===========================

# same as m10 from Table 3
mod_m10 <- lm(equalaccess ~ lag_fdi_inflows + lag_fdi_outflows +
                lag_trade + lag_tariff_rate + log(lag_gdppc) +
                lag_gdp_growth + lag_inflation + lag_equalaccess + lag2_equalaccess +
                as.factor(country) + as.factor(year),
              data = temp)

# compute standardized residuals and cook's distance, then modify original sample
std_resid <- rstandard(mod_m10)
cooks_d <- cooks.distance(mod_m10)

resid_df <- temp[!is.na(std_resid)&
                   abs(std_resid) < 1.96, ]

cooks_df <- temp[!is.na(cooks_d) &
                   cooks_d < 4/nobs(mod_m10), ]


# Residual Equal Access
m50 <- plm(
  equalaccess ~ lag_fdi_inflows + lag_fdi_outflows + lag_trade + lag_tariff_rate +
    log(lag_gdppc) + lag_gdp_growth + lag_inflation +
    lag_equalaccess + lag2_equalaccess,
  data = resid_df,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

crse50 <- coeftest(m50, vcovHC(m50, type = "HC1", cluster = "group"))
crse50
nobs(m50)
wooldridge_m50 <- pwartest(m50)

# Cook's Equal Access

m51 <- plm(
  equalaccess ~ lag_fdi_inflows + lag_fdi_outflows + lag_trade + lag_tariff_rate +
    log(lag_gdppc) + lag_gdp_growth + lag_inflation +
    lag_equalaccess + lag2_equalaccess,
  data = cooks_df,
  index = c("country", "year"),
  model = "within",
  effect = "twoways")

crse51 <- coeftest(m51, vcovHC(m51, type = "HC1", cluster = "group"))
crse51
nobs(m51)
wooldridge_m51 <- pwartest(m51)

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
  extract_stars(coef(m46)["lag_freefair"], crse46["lag_freefair", 4]),
  extract_stars(coef(m47)["lag_freefair"], crse47["lag_freefair", 4]),
  extract_stars(coef(m48)["lag_equallaw"], crse48["lag_equallaw", 4]),
  extract_stars(coef(m49)["lag_equallaw"], crse49["lag_equallaw", 4]),
  extract_stars(coef(m50)["lag_equalaccess"], crse50["lag_equalaccess", 4]),
  extract_stars(coef(m51)["lag_equalaccess"], crse51["lag_equalaccess", 4])
)


lag1_se <- c(
  extract_ses(crse46["lag_freefair", 2]),
  extract_ses(crse47["lag_freefair", 2]),
  extract_ses(crse48["lag_equallaw", 2]),
  extract_ses(crse49["lag_equallaw", 2]),
  extract_ses(crse50["lag_equalaccess", 2]),
  extract_ses(crse51["lag_equalaccess", 2])
)

lag2_coef <- c(
  extract_stars(coef(m46)["lag2_freefair"], m46["lag2_freefair", 4]),
  extract_stars(coef(m47)["lag2_freefair"], crse47["lag2_freefair", 4]),
  extract_stars(coef(m48)["lag2_equallaw"], crse48["lag2_equallaw", 4]),
  extract_stars(coef(m49)["lag2_equallaw"], crse49["lag2_equallaw", 4]),
  extract_stars(coef(m50)["lag2_equalaccess"], crse50["lag2_equalaccess", 4]),
  extract_stars(coef(m51)["lag2_equalaccess"], crse51["lag2_equalaccess", 4])
)


lag2_se <- c(
  extract_ses(crse_resid_m2["lag2_freefair", 2]),
  extract_ses(crse_m47["lag2_freefair", 2]),
  extract_ses(crse_m48["lag2_equallaw", 2]),
  extract_ses(crse_m49["lag2_equallaw", 2]),
  extract_ses(crse_m50["lag2_equalaccess", 2]),
  extract_ses(crse_m51["lag2_equalaccess", 2])
)


table_A2 <- stargazer(m46, m47, m48, m49, m50, m51,
                      type = "latex",
                      title = "Pooled Analysis of Globalization's Effect on Democracy, No Outliers",
                      float = TRUE,

                      se = list(crse46[,2], crse47[,2], crse48[,2], crse49[,2],
                                crse50[,2], crse51[,2]),

                      apply.coef = function(x) x * 100,
                      apply.se = function(x) x * 100,

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
                        c("Wooldridge/AR2 test p-value", round(wooldridge_m46$p.value, 3),
                          round(wooldridge_m47$p.value, 3), round(wooldridge_m48$p.value, 3),
                          round(wooldridge_m49$p.value, 3),round(wooldridge_m50$p.value, 3),
                          round(wooldridge_m51$p.value, 3)),
                        c("Observations", nobs(m46), nobs(m47), nobs(m48), nobs(m49),
                          nobs(m50), nobs(m51))
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
            display. All models report results from the standard
            two-way fixed effects estimation without outliers, have two lags, and include economic controls, country fixed
            effects, and year fixed effects. Columns 1, 3, and 5 report results from TWFE models excluding observations with a standardized residual above an absolute value of 1.96. Columns 2, 5, and 6 exclude observations with a Cook's distance above 4/n. The Wooldridge row reports
            p-values for Wooldridge's test for serial correlation for the
            standard TWFE models.}",
                      notes.align = "l",
                      notes.label = "",
                      notes.append = FALSE
)

cat(table_A2, file = "tables/table_A2.tex")


