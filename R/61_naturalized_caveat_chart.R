# ==============================================================================
# 61_naturalized_caveat_chart.R -> fig16_naturalized_arrival_epop.png
# H9 caveat: native-born citizens (both generations) fell, per fig15. But
# naturalized citizens rose across every arrival cohort, consistent with the
# H25 prior (see R/43_batch2_men_citizens.R): a shock to new-immigrant labor
# supply should benefit prior immigrants most, since they are closer
# substitutes for new arrivals than natives are. Caveat: the dose-response
# regression on occupational exposure is null (output/b2_h25_dose_response_
# citizenship.csv), so the gain is real but not concentrated where the
# substitution mechanism predicts it should be.
# ==============================================================================
suppressMessages({library(tidyverse); library(scales)})
dir.create("graphics", showWarnings = FALSE)
NAVY <- "#2c3254"; RED <- "#ff8361"; GREEN <- "#70ad8f"; GREY <- "#8d8b7f"; GOLD <- "#e0a23c"
my_style <- list(
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = .3),
        plot.title = element_text(face = "bold", color = NAVY, size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 9.5),
        plot.caption = element_text(color = "grey40", size = 8),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9)),
  coord_cartesian(clip = "off"))
W <- 7.2; H <- 4.4; DPI <- 200
YRX <- scale_x_continuous(breaks = seq(2015, 2026, 1))
CAP <- paste0("Source: IPUMS CPS microdata, author's calculations. No household survey in October 2025.\n",
             "2026 covers January-August. Mike Konczal.")

ar <- read_csv("output/b2_h25_naturalized_by_arrival.csv", show_col_types = FALSE) %>%
  mutate(arrgrp = factor(arrgrp, levels = c("Arrived before 2000", "Arrived 2000-2015",
                                            "Arrived 2016 or later")))
p <- ar %>% filter(yr >= 2019) %>%
  ggplot(aes(yr, epop, color = arrgrp)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_color_manual(values = c("Arrived before 2000" = NAVY, "Arrived 2000-2015" = GOLD,
                                "Arrived 2016 or later" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Naturalized Citizens' Employment Rate Rose, Unlike Native-Born Citizens'",
       subtitle = paste("Prime-age (25-54) employment-population ratio for naturalized citizens, by arrival cohort.",
                        "\nThe gain shows up in every cohort, consistent with prior immigrants benefiting most from a",
                        "\nshock to new-immigrant labor supply — but see caveats in the text."),
       x = NULL, y = "Prime-age employment rate", caption = CAP) + my_style
ggsave("graphics/fig16_naturalized_arrival_epop.png", p, width = W, height = H, dpi = DPI)
cat("DONE 61_naturalized_caveat_chart.R\n")
