#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Figure 4: response times per step, 5 replicates, by method
#
# Summary: arithmetic mean +/- one standard deviation of the 5 replicates,
# matching the values reported in the Results and in Supplementary Table 3.
# A t-interval on the raw scale is NOT used: it goes negative for Step 8 and
# Step 15 SQL and cannot be drawn on a log axis. mean +/- SD stays positive for
# all 29 step x method series (smallest lower bound 0.0008 s, Step 8 SQL), so
# the error bars are drawable; the stopifnot() in section 4 enforces this.
#
# The x axis remains log10-scaled: that is a display choice for data spanning
# five orders of magnitude and is independent of the summary statistic. The
# individual replicates are plotted alongside the summary so that the skew
# within each series stays visible despite the symmetric error bar.
#
# Requires R >= 4.1 for the native |> pipe.
# ---------------------------------------------------------------------------

library(here,    warn.conflicts = FALSE)
library(readxl,  warn.conflicts = FALSE)
library(dplyr,   warn.conflicts = FALSE)
library(tidyr,   warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(scales,  warn.conflicts = FALSE)
library(grid,    warn.conflicts = FALSE)

out_dir <- here::here("writing", "floats")

infile  <- "Supplementary_Table_2_reactivity_times_replicates_20260727_V01D07.xlsx"
outstem <- "Figure_4_reactivity_times"

METHODS <- c("Web", "CLI", "GUI", "SQL")

# ---- 1. read + reshape to long ---------------------------------------------
raw <- readxl::read_excel(infile, sheet = 1)

long <- raw |>
  tidyr::pivot_longer(
    tidyr::starts_with("Replicate"),
    names_to  = "Replicate",
    values_to = "Time_s",
    values_transform = list(Time_s = as.numeric)
  ) |>
  dplyr::mutate(
    Replicate = as.integer(gsub("\\D", "", Replicate)),
    Step_num  = as.integer(gsub("\\D", "", Step)),
    Time_s    = as.numeric(Time_s)
  ) |>
  dplyr::filter(!is.na(Time_s))

# Guard: as.numeric() above turns anything unparseable into NA silently (e.g. a
# comma decimal separator), and the filter() would then drop replicates without
# a word, shrinking n and biasing the summary. Fail loudly instead.
n_expected <- nrow(raw) * 5
if (nrow(long) != n_expected) {
  stop(sprintf("Expected %d replicate values, kept %d - check for non-numeric cells in the workbook.",
               n_expected, nrow(long)))
}

# ---- 2. short strip labels ("Stichpunkte") ---------------------------------
# Step 1 vs 2 and Step 3 vs 4 share the same Action text (Upload / Load) and
# are distinguished only by file, so the file is added by hand for those four
# rows; every other label is a short form of its own Action text. Keeping this
# as an explicit, checkable table - rather than truncating Action
# programmatically - means each label can be read against the source data
# below and against the caption's full Action text, and the two are
# guaranteed not to silently drift apart.
short_label <- c(
  "1"  = "Upload GWAS Catalog",
  "2"  = "Upload GTEx",
  "3"  = "Load GWAS Catalog",
  "4"  = "Load GTEx",
  "5"  = "Log in",
  "6"  = "Locate analysis",
  "7"  = "Download report",
  "8"  = "Trait definition",
  "9"  = "Genotype freeze",
  "10" = "Top GWAS Catalog variant",
  "11" = "Open common results",
  "12" = "Filter common",
  "13" = "Sort common",
  "14" = "Open rare results",
  "15" = "Filter rare",
  "16" = "Sort rare",
  "17" = "Tissue expression"
)

# ---- 3. Panels with one method simply get one row instead of two
# facet_grid(space = "free_y") sizes each panel to however many methods
# it actually has, so nothing else needs to change.
dat <- long |>
  dplyr::mutate(
    # reversed levels => Web ends up on the TOP row inside each panel
    Method   = factor(Method, levels = rev(METHODS)),
    # "Step N: <short label>", ordered so Step 1 is the TOP panel
    Step_lab = sprintf("Step %d: %s", Step_num, short_label[as.character(Step_num)]),
    Step_lab = factor(Step_lab, levels = unique(Step_lab[order(Step_num)]))
  )

# ---- 4. arithmetic mean +/- standard deviation ------------------------------
summ <- dat |>
  dplyr::group_by(Step_num, Step_lab, Action, Method) |>
  dplyr::summarise(
    n       = dplyr::n(),
    mean_s  = mean(Time_s),
    sd_s    = sd(Time_s),
    lo      = mean(Time_s) - sd(Time_s),
    hi      = mean(Time_s) + sd(Time_s),
    .groups = "drop"
  )

# A non-positive lower bound cannot be drawn on a log axis. This holds for the
# current data; fail loudly rather than silently dropping an error bar if the
# measurements are ever extended or replaced.
stopifnot(all(summ$lo > 0))

print(as.data.frame(summ[, c("Step_num", "Method", "n", "mean_s", "sd_s", "lo", "hi")]),
      digits = 3)

# ---- 5. colours (Okabe-Ito; colour-blind safe + greyscale safe) -------------
# Wes Anderson alternative:
#   pal <- setNames(wesanderson::wes_palette("Zissou1", 4, "discrete"), METHODS)
pal <- c("Web" = "#0072B2", "CLI" = "#E69F00", "GUI" = "#009E73", "SQL" = "#CC79A7")

# ---- 6. caption: step number -> action name ---------------------------------
key <- dat |>
  dplyr::group_by(Step_num, Action) |>
  dplyr::summarise(
    methods = paste(intersect(METHODS, as.character(Method)), collapse = ", "),
    .groups = "drop"
  ) |>
  dplyr::arrange(Step_num) |>
  dplyr::mutate(txt = sprintf("Step %d, %s (%s)", Step_num, Action, methods))

caption_txt <- paste0(
  "Individual replicate times (n = 5 per step and method, jittered points) with the ",
  "arithmetic mean and error bars spanning one standard deviation. ",
  "One panel per step, covering all steps of the workflow; steps with a single access method show ",
  "one row per panel. The x axis is log10-scaled and shared across panels. Steps: ",
  paste(key$txt, collapse = "; "), "."
)
# NB: the caption is NOT drawn on the figure - it is printed to the console at
# the end of this script so it can be pasted into the manuscript (Word).
writeLines(caption_txt, "Figure_caption.txt")

# ---- 7. plot ----------------------------------------------------------------
# Replicates sit in their own lane ABOVE the summary, the mean +/- SD in a lane
# below it. Where an error bar is narrower than the plotting symbol (Step 1 Web,
# Step 12 SQL) the mean would otherwise sit on top of all five points.
# position_nudge() accepts a vector, so the per-point jitter is pre-computed
# here and added as a nudge - a plain position_jitter() cannot be combined with
# a nudge, and a discrete y scale will not accept numeric values.
PT_LANE <- 0.22; SUMM_LANE <- -0.22
set.seed(42)
dat$ynudge <- PT_LANE + runif(nrow(dat), -0.115, 0.115)

p <- ggplot2::ggplot(dat, ggplot2::aes(x = Time_s, y = Method, colour = Method)) +
  ggplot2::geom_point(
    position = ggplot2::position_nudge(y = dat$ynudge),
    size = 1.8, alpha = 0.5, shape = 16
  ) +
  ggplot2::geom_linerange(
    data = summ,
    ggplot2::aes(xmin = lo, xmax = hi, y = Method, colour = Method),
    inherit.aes = FALSE, linewidth = 1.3,
    position = ggplot2::position_nudge(y = SUMM_LANE)
  ) +
  ggplot2::geom_point(
    data = summ,
    ggplot2::aes(x = mean_s, y = Method, colour = Method),
    inherit.aes = FALSE, shape = 23, size = 2.7, stroke = 1.2,
    fill = scales::alpha("white", 0.35),   # hollow: never hides a replicate
    position = ggplot2::position_nudge(y = SUMM_LANE)
  ) +
  ggplot2::facet_grid(Step_lab ~ ., scales = "free_y", space = "free_y", switch = "y") +
  ggplot2::scale_x_log10(
    # extended to 10^-4: the Step 8 SQL lower bound (0.0008 s) falls below 10^-3
    breaks = 10^(-4:3),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    minor_breaks = NULL          # ticks and gridlines only at the powers of 10
  ) +
  ggplot2::scale_colour_manual(values = pal, breaks = METHODS, drop = FALSE) +
  ggplot2::labs(x = "Time (s, log scale)", y = NULL, colour = "Method") +
  ggplot2::theme_bw(base_size = 22) +
  ggplot2::theme(
    strip.placement    = "outside",
    strip.background.y = ggplot2::element_rect(fill = "grey92", colour = "grey70",
                                               linewidth = 0.3),
    # left-aligned, non-bold, sized to comfortably fit "Step 10: Top GWAS
    # Catalog variant" - the longest label - on one line without wrapping
    strip.text.y.left  = ggplot2::element_text(angle = 0, hjust = 0, size = 24,
                                               margin = ggplot2::margin(l = 9, r = 9)),
    panel.spacing.y    = grid::unit(3.5, "pt"),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.y        = ggplot2::element_text(size = 22, colour = "grey25"),
    axis.text.x        = ggplot2::element_text(size = 22),
    axis.title.x       = ggplot2::element_text(size = 24),
    axis.ticks.y       = ggplot2::element_blank(),
    legend.position    = "top",
    legend.text        = ggplot2::element_text(size = 22),
    legend.title        = ggplot2::element_text(size = 24)
  )
ggplot2::ggsave(paste0(outstem, ".png"), p, width = 22.5, height = 25.5, dpi = 300)
ggplot2::ggsave(paste0(outstem, ".pdf"), p, width = 22.5, height = 25.5)
# height unchanged in proportion (12-panel, all-double-row, plus 5 single-row
# panels for Steps 3, 4, 5, 11, 14).

cat("\nCaption (also written to Figure_caption.txt):\n", caption_txt, "\n", sep = "")
