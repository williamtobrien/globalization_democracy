# SETWD ===========================
rm(list = ls())

# LIBRARIES ===========================
library(tidyverse)
library(multiwayvcov)
library(lmtest)
library(plm)

# IMPORT DATA ===========================
full_panel <- read.csv("data/full_panel.csv")


# CREATE FIGURE  ===========================

temp <- full_panel %>%
  filter(!is.na(polyarchy),
         !is.na(libdem),
         year > 2016)

avgs <- temp %>%
  group_by(year) %>%
  summarise(
    polyarchy = mean(polyarchy),
    libdem = mean(libdem),
    .groups = "drop"
  ) %>%
  pivot_longer(c(polyarchy, libdem)
  )


# Build the plot
figure_2 <- ggplot(avgs, aes(x = year, y = value, linetype = name)) +
  geom_line(color = "black", linewidth = 0.5) +
  xlab("Year") +
  ylab("Average democracy score") +
  scale_x_continuous(
    breaks = seq(min(avgs$year), max(avgs$year), by = 1)
  ) +
  scale_linetype_manual(
    name   = "",
    values = c(polyarchy = "solid", libdem = "longdash"),
    labels = c(polyarchy = "Electoral democracy",
               libdem    = "Liberal democracy"),
    breaks = c("polyarchy", "libdem")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),

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
    plot.margin = margin(5.5, 5.5, 5.5, 5.5),

    legend.margin = margin(t = -5)
  )


ggsave("figures/figure_2.pdf", plot = figure_2, device = cairo_pdf)
