#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Supplementary Figure 1: warm-up effect across replicates
#
# Each measurement is expressed relative to the steady-state time of its own
# step x method series, defined as the geometric mean of replicates 2-5. This
# removes the ~6 orders of magnitude of between-step variation that makes the
# effect invisible on a raw time axis, and states the question directly: how
# much slower is the first run than the settled performance of the same task?
#
# Panel A: all 145 measurements, series joined by faint grey lines, with the
#          pooled geometric mean and 95% t-interval (on log10 ratios) per
#          replicate.
# Panel B: replicate 1 only, one point per series, sorted, showing that the
#          effect is consistent across series rather than driven by a few.
#
# Requires patchwork for the two-panel layout (install.packages("patchwork")).
# To avoid that dependency, save pA and pB separately with ggplot2::ggsave().
#
# Paths are resolved from the project root by here::here(), so the script runs
# unchanged from any working directory and on any machine.
#
# Style: add-on package functions are called as package::function().
# Requires R >= 4.1 for the native |> pipe.
# ---------------------------------------------------------------------------

library(here,      warn.conflicts = FALSE)
library(readxl,    warn.conflicts = FALSE)
library(dplyr,     warn.conflicts = FALSE)
library(tidyr,     warn.conflicts = FALSE)
library(ggplot2,   warn.conflicts = FALSE)
library(scales,    warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)

# ---- 0. paths and constants ------------------------------------------------
data_dir <- here::here("data")
out_dir  <- here::here("writing", "floats")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

infile  <- file.path(
  data_dir,
  "Supplementary_Table_2_reactivity_times_replicates_20260727_V01D07.xlsx"
)
outstem <- file.path(out_dir, "Supplementary_Figure_1_warm_up_effect")

if (!file.exists(infile)) {
  stop("Input file not found: ", infile,
       "\nCheck data_dir, or that here::here() resolves to the project root.")
}

METHODS    <- c("Web", "CLI", "GUI", "SQL")
N_REPS     <- 5L                    # replicates per series
N_SERIES   <- 29L                   # step x method series reported in the paper
N_MEASURED <- 145L                  # total measurements reported in the paper

pal <- c("Web" = "#0072B2", "CLI" = "#E69F00",
         "GUI" = "#009E73", "SQL" = "#CC79A7")

# ---- 1. read, reshape, normalise -------------------------------------------
raw <- readxl::read_excel(infile, sheet = 1)

long_all <- raw |>
  tidyr::pivot_longer(
    tidyr::starts_with("Replicate"),
    names_to  = "Replicate",
    values_to = "Time_s",
    values_transform = list(Time_s = as.numeric)
  ) |>
  dplyr::mutate(
    Replicate = as.integer(gsub("\\D", "", Replicate)),
    Step_num  = as.integer(gsub("\\D", "", Step)),
    Method    = factor(Method, levels = METHODS),
    Series    = paste(Step_num, Method, sep = "_"),
    # Method shown in the label as well as in the point colour. To show it only
    # where a step contributes two series, swap in:
    #   Label = ifelse(duplicated(Step_num) | duplicated(Step_num, fromLast = TRUE),
    #                  paste0("Step ", Step_num, ", ", Method), paste0("Step ", Step_num))
    Label     = paste0("Step ", Step_num, ", ", Method)
  )

# Shape check on the reshaped table, BEFORE dropping N/A cells. N/A marks an
# action with no equivalent under a given access method, so those rows are
# expected and must not trip this check.
if (nrow(long_all) != nrow(raw) * N_REPS) {
  stop("Unexpected number of replicate values: got ", nrow(long_all),
       ", expected ", nrow(raw) * N_REPS, ".")
}

long <- dplyr::filter(long_all, !is.na(Time_s))

# Content checks against the numbers reported in the manuscript.
if (dplyr::n_distinct(long$Series) != N_SERIES) {
  warning("Expected ", N_SERIES, " step x method series, found ",
          dplyr::n_distinct(long$Series), ".")
}
if (nrow(long) != N_MEASURED) {
  warning("Expected ", N_MEASURED, " measurements, found ", nrow(long), ".")
}
if (anyNA(long$Method)) {
  warning("Some Method values are not in METHODS and became NA.")
}

cat(sprintf("Read %d rows -> %d measurements across %d series.\n",
            nrow(raw), nrow(long), dplyr::n_distinct(long$Series)))

# steady state = geometric mean of replicates 2-5 of the SAME series
long <- long |>
  dplyr::group_by(Series) |>
  dplyr::mutate(
    base_log = mean(log10(Time_s[Replicate >= 2])),
    rel_log  = log10(Time_s) - base_log,
    ratio    = 10^rel_log
  ) |>
  dplyr::ungroup()

# ---- 2. pooled summary per replicate ---------------------------------------
summ <- long |>
  dplyr::group_by(Replicate) |>
  dplyr::summarise(
    geo     = 10^mean(rel_log),
    lo      = 10^(mean(rel_log) - qt(0.975, dplyr::n() - 1) * sd(rel_log) / sqrt(dplyr::n())),
    hi      = 10^(mean(rel_log) + qt(0.975, dplyr::n() - 1) * sd(rel_log) / sqrt(dplyr::n())),
    .groups = "drop"
  )
cat("\nRatio to steady state, pooled over all series:\n")
print(as.data.frame(summ), digits = 3)

# ---- 3. formal test on replicate 1 -----------------------------------------
r1 <- dplyr::filter(long, Replicate == 1)
cat(sprintf("\nReplicate 1 slower than steady state in %d of %d series.\n",
            sum(r1$rel_log > 0), nrow(r1)))
print(t.test(r1$rel_log))                    # paired by construction (log ratios)
print(wilcox.test(r1$rel_log))               # distribution-free alternative

# ---- 4. panel A: all replicates --------------------------------------------
set.seed(42)
long$xjit <- long$Replicate + runif(nrow(long), -0.09, 0.09)

pA <- ggplot2::ggplot(long, ggplot2::aes(x = xjit, y = ratio)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  ggplot2::geom_line(ggplot2::aes(group = Series), colour = "grey60",
                     linewidth = 0.25, alpha = 0.4) +
  ggplot2::geom_point(ggplot2::aes(colour = Method), size = 1.4, alpha = 0.7, shape = 16) +
  ggplot2::geom_linerange(
    data = summ, ggplot2::aes(x = Replicate + 0.30, ymin = lo, ymax = hi),
    inherit.aes = FALSE, colour = "black", linewidth = 1.0
  ) +
  ggplot2::geom_point(
    data = summ, ggplot2::aes(x = Replicate + 0.30, y = geo),
    inherit.aes = FALSE, shape = 23, size = 2.5, stroke = 1.0,
    colour = "black", fill = "white"
  ) +
  ggplot2::scale_x_continuous(breaks = 1:5) +
  ggplot2::scale_y_log10(breaks = c(0.3, 0.5, 1, 2, 3, 5),
                         labels = function(x) paste0(x, "\u00d7"),
                         minor_breaks = NULL) +
  ggplot2::scale_colour_manual(values = pal, breaks = METHODS, drop = FALSE) +
  ggplot2::labs(x = "Replicate", y = "Time relative to steady state (replicates 2-5)",
                colour = "Method", title = "A   All replicates") +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(face = "bold", size = 11))

# ---- 5. panel B: replicate 1 by series -------------------------------------
# Order on Series (unique) rather than Label. With the method in the label these
# happen to coincide, but keeping Series as the key means the alternative
# step-only labelling above can be swapped in without silently collapsing rows.
r1 <- dplyr::mutate(r1, Series = factor(Series, levels = Series[order(ratio)]))
step_labels <- setNames(r1$Label, as.character(r1$Series))

pB <- ggplot2::ggplot(r1, ggplot2::aes(x = ratio, y = Series, colour = Method)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  ggplot2::geom_segment(ggplot2::aes(x = 1, xend = ratio, yend = Series),
                        colour = "grey70", linewidth = 0.3) +
  ggplot2::geom_point(size = 1.9) +
  ggplot2::scale_x_continuous(          # linear, equidistant breaks
    breaks = seq(1, 4.5, by = 0.5),
    labels = function(x) paste0(x, "\u00d7"),
    limits = c(0.8, 4.6)
  ) +
  ggplot2::scale_y_discrete(labels = step_labels) +
  ggplot2::scale_colour_manual(values = pal, breaks = METHODS, drop = FALSE) +
  ggplot2::labs(x = "Replicate 1 relative to steady state", y = NULL,
                colour = "Method", title = "B   Replicate 1") +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 7),
                 axis.ticks.y = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(face = "bold", size = 11))

p <- pA + pB +
  patchwork::plot_layout(widths = c(1, 1.45), guides = "collect") &
  ggplot2::theme(legend.position = "top")

# ---- 6. save ---------------------------------------------------------------
ggplot2::ggsave(paste0(outstem, ".png"), p, width = 13, height = 7, dpi = 300)
ggplot2::ggsave(paste0(outstem, ".pdf"), p, width = 13, height = 7)

cat("\nWritten:\n  ", paste0(outstem, ".png"),
    "\n  ", paste0(outstem, ".pdf"), "\n", sep = "")