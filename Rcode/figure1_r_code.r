# =============================================================================
# Figure 1: CEGS and Comparison Publications Per Year (2001-2022)
# CEGS Program Evaluation — Nature Genetics Analysis
# =============================================================================
# NOTE on "both" category: 59 publications acknowledged both CEGS and
# comparison grants. Consistent with the report, these are counted toward
# both groups. If you prefer to exclude them from both, set
# COUNT_BOTH_IN_EACH = FALSE below.
# =============================================================================

library(readxl)
library(dplyr)
library(ggplot2)

COUNT_BOTH_IN_EACH <- TRUE  # set FALSE to exclude "both" from both groups

# -----------------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------------
df <- read_excel("CEGS Evaluation Publications.xlsx")

# -----------------------------------------------------------------------------
# 2. Filter to 2001-2022 and relevant categories
# -----------------------------------------------------------------------------
df_filtered <- df |>
  filter(pub_year >= 2001, pub_year <= 2022) |>
  filter(category_cegs %in% c("CEGS", "comparison", "both"))

# -----------------------------------------------------------------------------
# 3. Count publications per year for each group
#    "both" publications are added to each group's count
# -----------------------------------------------------------------------------
cegs_counts <- df_filtered |>
  filter(category_cegs %in% c("CEGS", if (COUNT_BOTH_IN_EACH) "both")) |>
  count(pub_year) |>
  rename(year = pub_year, n_pubs = n) |>
  mutate(group = "CEGS")

comp_counts <- df_filtered |>
  filter(category_cegs %in% c("comparison", if (COUNT_BOTH_IN_EACH) "both")) |>
  count(pub_year) |>
  rename(year = pub_year, n_pubs = n) |>
  mutate(group = "Comparison R01s")

plot_data <- bind_rows(cegs_counts, comp_counts) |>
  mutate(group = factor(group, levels = c("CEGS", "Comparison R01s")))

# -----------------------------------------------------------------------------
# 4. Verify annual totals match report values
#    Report states:
#      2001-2016: avg ~59 CEGS/year, ~206 comparison/year
#      2017-2022: avg ~144 CEGS/year, ~231 comparison/year
# -----------------------------------------------------------------------------
check <- plot_data |>
  mutate(period = if_else(year <= 2016, "2001-2016", "2017-2022")) |>
  group_by(group, period) |>
  summarise(avg_per_year = round(mean(n_pubs), 1), .groups = "drop")

message("--- Verification against reported averages ---")
print(check)

# -----------------------------------------------------------------------------
# 5. Plot — Nature Genetics style
#    - Clean minimal theme
#    - Two lines: solid for CEGS, dashed for Comparison
#    - Color-blind-friendly palette
# -----------------------------------------------------------------------------

cegs_color      <- "#0072B2"   # blue
comp_color      <- "#009E73"   # teal/green
annotation_size <- 3.2

fig1 <- ggplot(plot_data, aes(x = year, y = n_pubs,
                               color = group, linetype = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +

  # Dashed vertical line at 2016/2017 boundary
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             color = "grey50", linewidth = 0.5) +

  # Period average annotations — CEGS
  annotate("text", x = 2008.5, y = 75,
           label = "Avg. 59/year\n(2001–2016)",
           color = cegs_color, size = annotation_size,
           hjust = 0.5, fontface = "italic") +
  annotate("text", x = 2019.5, y = 175,
           label = "Avg. 144/year\n(2017–2022)",
           color = cegs_color, size = annotation_size,
           hjust = 0.5, fontface = "italic") +

  # Period average annotations — Comparison
  annotate("text", x = 2008.5, y = 235,
           label = "Avg. 206/year\n(2001–2016)",
           color = comp_color, size = annotation_size,
           hjust = 0.5, fontface = "italic") +
  annotate("text", x = 2019.5, y = 260,
           label = "Avg. 231/year\n(2017–2022)",
           color = comp_color, size = annotation_size,
           hjust = 0.5, fontface = "italic") +

  scale_color_manual(
    values = c("CEGS" = cegs_color, "Comparison R01s" = comp_color)
  ) +
  scale_linetype_manual(
    values = c("CEGS" = "solid", "Comparison R01s" = "dashed")
  ) +
  scale_x_continuous(
    breaks = seq(2001, 2022, by = 2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 400, by = 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x     = "Year of publication",
    y     = "Number of publications",
    color = "Grant group",
    linetype = "Grant group"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 10),
    legend.position  = c(0.15, 0.88),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width  = unit(1.5, "cm"),
    legend.text       = element_text(size = 9),
    legend.title      = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.margin       = margin(10, 15, 5, 5)
  )

# -----------------------------------------------------------------------------
# 6. Save
#    Nature Genetics requires figures at 300 dpi minimum.
#    Single-column width: ~85 mm; double-column width: ~170 mm.
#    A line graph like this works well at double-column width.
# -----------------------------------------------------------------------------
ggsave(
  filename = "Figure1_CEGS_publications_per_year.pdf",
  plot     = fig1,
  width    = 170,
  height   = 100,
  units    = "mm",
  dpi      = 300
)

ggsave(
  filename = "Figure1_CEGS_publications_per_year.tiff",
  plot     = fig1,
  width    = 170,
  height   = 100,
  units    = "mm",
  dpi      = 300,
  compression = "lzw"
)

message("Figure 1 saved as PDF and TIFF.")
