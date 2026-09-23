# ==============================================================================
# 00_blog_style.R   One look for every graphic in blog_post.md
#
# Copied from the my_style block in 21_charts_micro.R (the job-finding chart,
# Hypothesis 7), so all twelve figures match it: white background, navy bold
# title, navy subtitle, grey caption, light major gridlines, 7.2 x 4.4 in at
# 200 dpi. Every title carries its "Hypothesis N:" prefix via hyp_title().
# source() this from any chart script; it only defines objects.
# ==============================================================================
BLOG_NAVY  <- "#2c3254"
BLOG_RED   <- "#ff8361"
BLOG_GREEN <- "#70ad8f"
BLOG_GREY  <- "#8d8b7f"
BLOG_GOLD  <- "#e0a23c"
BLOG_ORANGE <- "#e0762e"
BLOG_PURPLE <- "#7a6fb0"
# years compared in the month-by-month charts
BLOG_YEAR_COLORS <- c(`2024` = BLOG_GOLD, `2025` = "#b8b6ab", `2026` = BLOG_NAVY)

BLOG_W <- 7.2; BLOG_H <- 4.4; BLOG_DPI <- 200

blog_theme <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "grey88", linewidth = .3),
    plot.title = ggplot2::element_text(face = "bold", color = BLOG_NAVY, size = 13),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(color = BLOG_NAVY, size = 9.5),
    plot.caption = ggplot2::element_text(color = "grey40", size = 8),
    legend.position = "top", legend.title = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = 9),
    strip.text = ggplot2::element_text(face = "bold", color = BLOG_NAVY, size = 9.5),
    plot.background = ggplot2::element_rect(fill = "white", color = NA))

# "Hypothesis N: Title"; titles longer than one line (~72 characters at this
# size and width) are split into two lines of roughly equal length.
hyp_title <- function(n, title, max_chars = 72) {
  full <- paste0("Hypothesis ", n, ": ", title)
  if (nchar(full) <= max_chars) return(full)
  sp <- gregexpr(" ", full)[[1]]
  cut <- sp[which.min(pmax(sp, nchar(full) - sp))]
  paste0(substr(full, 1, cut - 1), "\n", substr(full, cut + 1, nchar(full)))
}

# exact percent axis labels: never collapses two gridlines onto one rounded label
blog_pct <- function(x) paste0(sub("\\.?0+$", "", sprintf("%.2f", 100*x)), "%")

blog_save <- function(file, plot, width = BLOG_W, height = BLOG_H) {
  ggplot2::ggsave(file, plot, width = width, height = height, dpi = BLOG_DPI, bg = "white")
}
