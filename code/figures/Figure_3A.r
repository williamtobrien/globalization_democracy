# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(lmtest)
library(plm)

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


## I credit Rau and Stokes (2025) for this figure idea. I use their code as a basis for mine.
# Standardize variables
temp <- temp %>%
  mutate(lag_fdi_inflows_stan = as.numeric(scale(lag_fdi_inflows)),
         lag_fdi_outflows_stan = as.numeric(scale(lag_fdi_outflows)),
         lag_trade_stan = as.numeric(scale(lag_trade)),
         lag_tariff_rate_stan = as.numeric(scale(lag_tariff_rate)),
         lag_gdppc_stan = as.numeric(scale(log(lag_gdppc))),
         lag_gdp_growth_stan = as.numeric(scale(lag_gdp_growth)),
         lag_inflation_stan = as.numeric(scale(lag_inflation)))

# Run m2 from Table 3 with standardized variables
m2 <- plm(freefair ~ lag_fdi_inflows_stan + lag_fdi_outflows_stan +
            lag_trade_stan + lag_tariff_rate_stan + lag_gdppc_stan +
            lag_gdp_growth_stan + lag_inflation_stan + lag_freefair + lag2_freefair,
          data = temp,
          index = c("country", "year"),
          model = "within",
          effect = "twoways")

crse2 <- coeftest(m2, vcovHC(m2, type = "HC1", cluster = "group"))
crse2

# Extract relevant data and construct table
var_names <- c("FDI Inflows", "FDI Outflows", "Trade", "Tariff Rate")

coefs <- crse2[c("lag_fdi_inflows_stan", "lag_fdi_outflows_stan",
                    "lag_trade_stan", "lag_tariff_rate_stan"), 1]

ses <- crse2[c("lag_fdi_inflows_stan", "lag_fdi_outflows_stan",
                 "lag_trade_stan", "lag_tariff_rate_stan"), 2]

sig <- as.numeric(crse2[c("lag_fdi_inflows_stan", "lag_fdi_outflows_stan",
                           "lag_trade_stan", "lag_tariff_rate_stan"), 4] < 0.05)

fig3A_df <- data.frame(var_names, coefs, ses, sig) %>%
  mutate(var_nums = c(1, 2, 3, 4)) %>%
  mutate(vf = fct_reorder(as.factor(var_names), -var_nums))

# Plot ----
figure_3A <- fig3A_df %>%
  ggplot(aes(x = coefs, y = vf, shape = as.factor(sig))) +
  geom_vline(xintercept = 0, color = "gray75") +
  geom_errorbarh(aes(xmin = coefs - 1.96*ses, xmax = coefs + 1.96*ses), height = 0) +
  geom_point(fill = "white", size = 3) +
  labs(
    x = "Standardized Coefficient", y = "") +
  scale_shape_manual(values = c(21, 19)) +
  theme_bw() +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    panel.border      = element_blank(),
    axis.title.x      = element_text(size = 19),
    axis.title.y      = element_text(size = 19),
    axis.text.x       = element_text(size = 17),
    axis.text.y       = element_text(size = 17),
    axis.line         = element_line(color = "black", linewidth = 0.6),
    axis.ticks        = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(5, "pt"),
    plot.margin       = margin(5.5, 15, 5.5, 5.5)
  )

print(figure_3A)
ggsave("figure_3A.pdf", plot = figure_3A, device = cairo_pdf)
