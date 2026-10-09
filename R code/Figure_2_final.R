# Figure 2: CEGS multipanel figure
# Final layout matching the approved version
# Panels: a) advances in genomics, b) biggest success themes,
#         c) education/outreach participation, d) career stage
#
# Uses only base R + grid for drawing.
# Exports PDF, SVG, and 600-dpi PNG.

library(grid)

# -----------------------------
# Data
# -----------------------------

adv <- data.frame(
  label = c(
    "New methods or technologies",
    "New understanding of biology",
    "New data-analysis methods",
    "New concepts in genomics"
  ),
  n   = c(61, 61, 59, 60),
  neg = c(10, 18, 15, 22),
  mod = c(8, 15, 19, 22),
  pos = c(82, 67, 66, 57)
)

theme_groups <- list(
  list(
    domain = "Technical outputs",
    color  = "#0072B2",
    items  = data.frame(
      label = c("New technology or method",
                "Link biology-technology",
                "New resources"),
      v   = c(20, 7, 4),
      pct = c(71, 25, 14)
    )
  ),
  list(
    domain = "Scientific field effects",
    color  = "#E69F00",
    items  = data.frame(
      label = c("Change/create new fields",
                "Biological questions",
                "Difficulty/challenge"),
      v   = c(14, 12, 5),
      pct = c(50, 43, 18)
    )
  ),
  list(
    domain = "Research environment",
    color  = "#009E73",
    items  = data.frame(
      label = c("Collaborations",
                "Freedom to take risks"),
      v   = c(12, 7),
      pct = c(43, 25)
    )
  ),
  list(
    domain = "Human/community",
    color  = "#CC79A7",
    items  = data.frame(
      label = c("People/training",
                "Education/outreach"),
      v   = c(7, 6),
      pct = c(25, 21)
    )
  )
)

outreach <- data.frame(
  label = c(
    "Summer programs",
    "Journal club/seminar",
    "Undergrad courses/mentoring",
    "IDP completion",
    "Sci comm (non-academic)",
    "Other"
  ),
  pct = c(71, 63, 54, 42, 38, 17)
)

career <- data.frame(
  label = c(
    "Postdoctoral fellow",
    "Senior faculty",
    "Junior faculty",
    "Graduate student",
    "Staff scientist",
    "Other"
  ),
  pct = c(39, 23, 21, 13, 13, 8)
)

# -----------------------------
# Colors
# -----------------------------

BLUE <- "#0072B2"
VERM <- "#D55E00"
GRAY <- "#BDBDBD"
DARK <- "#222222"
MID  <- "#555555"
LT   <- "#999999"

# -----------------------------
# Figure geometry
# -----------------------------

FIG_W <- 15.2
FIG_H <- 10.8

# Overall 2 x 2 arrangement.
# The larger row gap is intentional.
left_margin   <- 0.035
right_margin  <- 0.010
top_margin    <- 0.050
bottom_margin <- 0.055
col_gap       <- 0.075
row_gap       <- 0.135

panel_w <- (1 - left_margin - right_margin - col_gap) / 2
panel_h <- (1 - top_margin - bottom_margin - row_gap) / 2

x_left  <- left_margin
x_right <- left_margin + panel_w + col_gap
y_top   <- bottom_margin + panel_h + row_gap
y_bot   <- bottom_margin

panel_vp <- function(x, y) {
  viewport(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(panel_w, "npc"),
    height = unit(panel_h, "npc"),
    just = c("left", "bottom"),
    xscale = c(0, 1),
    yscale = c(0, 1)
  )
}

# -----------------------------
# Helpers
# -----------------------------

txt <- function(label, x, y, size = 12, col = DARK,
                fontface = "plain", just = c("left", "center")) {
  grid.text(
    label,
    x = unit(x, "native"),
    y = unit(y, "native"),
    just = just,
    gp = gpar(
      fontsize = size,
      col = col,
      fontfamily = "Arial",
      fontface = fontface
    )
  )
}

rect_native <- function(x, y, w, h, fill) {
  grid.rect(
    x = unit(x, "native"),
    y = unit(y, "native"),
    width = unit(w, "native"),
    height = unit(h, "native"),
    just = c("left", "bottom"),
    gp = gpar(fill = fill, col = NA)
  )
}

# -----------------------------
# Panel a
# -----------------------------

draw_panel_a <- function() {
  pushViewport(panel_vp(x_left, y_top))

  txt("a  Survey: advances in genomics (n=62)",
      0.00, 1.02, size = 17, fontface = "bold",
      just = c("left", "bottom"))

  y_positions <- c(0.82, 0.57, 0.32, 0.07)
  zero  <- 0.38
  scale <- 0.0061
  bar_h <- 0.095

  # Dotted zero line
  grid.lines(
    x = unit(c(zero, zero), "native"),
    y = unit(c(0.01, 0.99), "native"),
    gp = gpar(col = "#666666", lwd = 1, lty = "dashed")
  )

  for (i in seq_len(nrow(adv))) {
    y <- y_positions[i]

    # Black item label
    g1 <- textGrob(
      adv$label[i],
      x = unit(0.005, "native"),
      y = unit(y + 0.115, "native"),
      just = c("left", "bottom"),
      gp = gpar(
        fontsize = 13.5,
        col = DARK,
        fontfamily = "Arial"
      )
    )
    grid.draw(g1)

    # Put gray n immediately after the rendered black label
    label_width_npc <- convertWidth(grobWidth(g1), "npc", valueOnly = TRUE)
    txt(
      paste0("(n=", adv$n[i], ")"),
      0.005 + label_width_npc + 0.008,
      y + 0.115,
      size = 12.5,
      col = LT,
      just = c("left", "bottom")
    )

    neg_w <- adv$neg[i] * scale
    mod_w <- adv$mod[i] * scale
    pos_w <- adv$pos[i] * scale

    rect_native(zero - neg_w, y, neg_w, bar_h, VERM)
    rect_native(zero,         y, mod_w, bar_h, GRAY)
    rect_native(zero + mod_w, y, pos_w, bar_h, BLUE)

    txt(
      paste0(adv$neg[i], "%"),
      zero - neg_w - 0.01,
      y + bar_h / 2,
      size = 12,
      col = VERM,
      just = c("right", "center")
    )

    txt(
      paste0(adv$mod[i], "%"),
      zero + mod_w / 2,
      y + bar_h / 2,
      size = 11.5,
      col = MID,
      just = c("center", "center")
    )

    txt(
      paste0(adv$pos[i], "%"),
      zero + mod_w + pos_w + 0.012,
      y + bar_h / 2,
      size = 12,
      col = BLUE,
      fontface = "bold",
      just = c("left", "center")
    )
  }

  popViewport()
}

# -----------------------------
# Panel b
# -----------------------------

draw_panel_b <- function() {
  pushViewport(panel_vp(x_right, y_top))

  txt("b  Interviews: biggest success of CEGS grant (n=28)",
      0.00, 1.02, size = 15.8, fontface = "bold",
      just = c("left", "bottom"))

  # Lowered layout, while keeping Human/community fully visible
  y           <- 0.84
  header_step <- 0.046
  row_gap_b   <- 0.063
  group_gap   <- 0.031

  label_x   <- 0.45
  bar_x     <- 0.47
  bar_max_w <- 0.36
  val_x     <- 0.89
  bar_h_b   <- 0.048

  for (g in theme_groups) {
    txt(
      g$domain,
      0.00, y + 0.003,
      size = 14.2,
      col = g$color,
      fontface = "bold",
      just = c("left", "bottom")
    )

    y <- y - header_step

    for (i in seq_len(nrow(g$items))) {
      lab <- g$items$label[i]
      v   <- g$items$v[i]
      pct <- g$items$pct[i]

      txt(
        lab,
        label_x, y + bar_h_b / 2,
        size = 12.3,
        just = c("right", "center")
      )

      width <- (pct / 80) * bar_max_w

      rect_native(
        bar_x, y,
        width, bar_h_b,
        g$color
      )

      txt(
        paste0(v, " (", pct, "%)"),
        val_x, y + bar_h_b / 2,
        size = 11.8,
        col = "#444444",
        just = c("left", "center")
      )

      y <- y - row_gap_b
    }

    y <- y - group_gap
  }

  popViewport()
}

# -----------------------------
# Panel c
# -----------------------------

draw_panel_c <- function() {
  pushViewport(panel_vp(x_left, y_bot))

  txt("c  Survey: education & outreach participation (n=24)",
      0.00, 1.02, size = 17, fontface = "bold",
      just = c("left", "bottom"))

  label_x   <- 0.43
  bar_x     <- 0.455
  bar_max_w <- 0.54
  pct_x     <- 0.955
  y         <- 0.86
  bar_h     <- 0.058

  for (i in seq_len(nrow(outreach))) {
    txt(
      outreach$label[i],
      label_x, y + 0.027,
      size = 13,
      just = c("right", "center")
    )

    width <- (outreach$pct[i] / 80) * bar_max_w

    rect_native(
      bar_x, y,
      width, bar_h,
      BLUE
    )

    txt(
      paste0(outreach$pct[i], "%"),
      pct_x, y + 0.029,
      size = 12.5,
      col = "#444444",
      just = c("left", "center")
    )

    y <- y - 0.105
  }

  popViewport()
}

# -----------------------------
# Panel d
# -----------------------------

draw_panel_d <- function() {
  pushViewport(panel_vp(x_right, y_bot))

  txt("d  Survey: career stage during CEGS (n=62)",
      0.00, 1.02, size = 17, fontface = "bold",
      just = c("left", "bottom"))

  # Final approved version: contents shifted left
  label_x   <- 0.34
  bar_x     <- 0.36
  bar_max_w <- 0.54
  pct_x     <- 0.85
  y         <- 0.86
  bar_h     <- 0.058

  for (i in seq_len(nrow(career))) {
    txt(
      career$label[i],
      label_x, y + 0.027,
      size = 13,
      just = c("right", "center")
    )

    width <- (career$pct[i] / 45) * bar_max_w

    rect_native(
      bar_x, y,
      width, bar_h,
      BLUE
    )

    txt(
      paste0(career$pct[i], "%"),
      pct_x, y + 0.029,
      size = 12.5,
      col = "#444444",
      just = c("left", "center")
    )

    y <- y - 0.105
  }

  popViewport()
}

# -----------------------------
# Draw complete figure
# -----------------------------

draw_figure <- function() {
  grid.newpage()
  draw_panel_a()
  draw_panel_b()
  draw_panel_c()
  draw_panel_d()
}

# -----------------------------
# Export
# -----------------------------

# Vector PDF
cairo_pdf(
  "Figure_2_final.pdf",
  width = FIG_W,
  height = FIG_H
)
draw_figure()
dev.off()

# Vector SVG
svg(
  "Figure_2_final.svg",
  width = FIG_W,
  height = FIG_H,
  pointsize = 12
)
draw_figure()
dev.off()

# 600-dpi PNG
png(
  "Figure_2_final_600dpi.png",
  width = FIG_W,
  height = FIG_H,
  units = "in",
  res = 600,
  type = "cairo-png"
)
draw_figure()
dev.off()

# Optional: draw interactively in RStudio
# draw_figure()
