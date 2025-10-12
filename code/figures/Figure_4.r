# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(multiwayvcov)
library(lmtest)
library(plm)
library(ggplot2)
library(MASS)

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


m2 <- plm(freefair ~ lag_fdi_inflows + lag_fdi_outflows +
            lag_trade + lag_tariff_rate + log(lag_gdppc) +
            lag_gdp_growth + lag_inflation + lag_freefair + lag2_freefair,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse2 <- coeftest(m2, vcovHC(m2, type = "HC1", cluster = "group"))
crse2

# LONG-RUN EFFECT OF TRADE ON FREE FAIR ===========================
# Within-country annual standard deviation of trade over time
trade_shock <- temp %>%
  group_by(country) %>%
  summarise(sd_trade = sd(lag_trade)) %>%
  summarise(mean_sd = mean(sd_trade)) %>%
  pull(mean_sd)

trade_shock

# Show effect over 30 years
time_horizon <- 30

coefs <- coef(m2)[c("lag_trade", "lag_freefair","lag2_freefair")]
vcm <- vcovHC(m2, type = "HC1", cluster = "group")[c("lag_trade", "lag_freefair","lag2_freefair"),
                                                 c("lag_trade", "lag_freefair","lag2_freefair")]

# Define function to recursively generate the over-time effects of the shock
lre <- function(coef) {
  trade <- coef["lag_trade"]
  lag1 <- coef["lag_freefair"]
  lag2 <- coef["lag2_freefair"]
  effect <- numeric(time_horizon)
  y1 <- 0
  y2 <- 0

  # Iterate through each time period to calculate the long-run effect
  for (t in seq_len(time_horizon)) {
    yt <- trade*trade_shock + lag1*y1 + lag2*y2
    effect[t] <- yt
    y2 <- y1
    y1 <- yt
  }

  # Multiply by 100 so the final result is in percentage points
  effect * 100
}

## Bootstrap confidence intervals ----
set.seed(891)
coef_draws <- mvrnorm(n = 1000, mu = coefs, Sigma = vcm)

# Need to redefine the column names so the above function can find the variables
colnames(coef_draws) <- names(coefs)

effect_draws <- apply(coef_draws, 1, lre)
mean_effect <- lre(coefs)
ci_low <- apply(effect_draws, 1, quantile, 0.025)
ci_high <- apply(effect_draws, 1, quantile, 0.975)

df <- data.frame(
  year = 0:time_horizon,
  mean_effect = c(0, mean_effect),
  ci_low = c(0, ci_low),
  ci_high = c(0, ci_high)
  )

rib_data <- data.frame(
  x    = df$year,
  ymin = df$ci_low,
  ymax = df$ci_high
)


## Plot ----
figure_4 <- ggplot(df, aes(x = year, y = mean_effect)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_line(data = rib_data, aes(x = x, y = ymax),
            color = "grey45", linetype = "longdash") +
  geom_line(data = rib_data, aes(x = x, y = ymin),
            color = "grey45", linetype = "longdash") +
  xlab("Years after annual 1-SD increase in trade openness") +
  ylab("Change in Free Fair percentage points") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),

    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(5, "pt"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )


ggsave("figures/figure_4.pdf", plot = figure_4, device = cairo_pdf)


