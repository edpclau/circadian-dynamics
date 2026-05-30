# Visual verification ritual for refactor gates.
# Run at each gate (1A done, after 1B, after 1C) and SHOW the maintainer the
# period table + actograms, so refactors are validated against ground truth.
#
# GROUND-TRUTH-VALIDATED REFERENCE (data/trikinetics.rda, 2026-05-30, maintainer-blessed):
#   IND 1: arrhythmic (dead/empty channel; LSP p = 0.999)
#   IND 2: ACF 23.75 h ~ LSP 23.9 h   (strongly rhythmic; methods agree)
#   IND 3: LSP 27.0 h (weak, p = 0.037); ACF NA (below 0.2 peak threshold)
#   IND 4: ACF 28.0 h vs LSP 25.9 h   (a confirmed METHOD difference, not a bug)
# The automatic regression test for these lives in tests/testthat/test-baseline-trikinetics.R.
#
# Run with: pixi run Rscript data-raw/verify_baseline.R

suppressMessages(devtools::load_all("."))
suppressMessages({library(ggplot2); library(dplyr); library(lubridate)})
future::plan(future::sequential)

load("data/trikinetics.rda")            # -> `trikinetics`: list of 32 individuals
inds <- paste("IND", 1:4)
dir.create("baseline", showWarnings = FALSE)

downsample15 <- function(d) {           # 1-min is overkill for circadian LSP
  d %>%
    mutate(datetime = floor_date(datetime, "15 minutes")) %>%
    group_by(datetime) %>%
    summarise(ld = round(mean(ld, na.rm = TRUE)),
              value = sum(value, na.rm = TRUE), .groups = "drop")
}
sub <- lapply(trikinetics[inds], downsample15)

res <- process_timeseries.main(
  df = sub, make_windows = FALSE, sampling_rate = "15 minutes",
  from = 15, to = 33, detrend_data = FALSE, butterworth = TRUE,
  f_low = 1/(12*4), f_high = 1/(35*4), order = 2, ofac = 1, lomb_pvalue = 0.05
)
periods <- bind_rows(lapply(names(res), function(n) data.frame(
  individual          = n,
  acf_period_h        = round(res[[n]]$acf$results$period, 3),
  lsp_period_h        = round(res[[n]]$lomb$results$period, 3),
  acf_rhythm_strength = round(res[[n]]$acf$results$rhythm_strength, 2),
  lsp_p_value         = signif(res[[n]]$lomb$results$p_value, 3)
)))
cat("\n==== PERIODS (whole recording, 15-min) ====\n")
print(periods, row.names = FALSE)
write.csv(periods, "baseline/periods.csv", row.names = FALSE)

make_actogram <- function(df, title) {
  df <- df %>%
    mutate(bin = floor_date(datetime, "15 minutes")) %>%
    group_by(bin) %>%
    summarise(value = sum(value, na.rm = TRUE),
              ld = mean(ld, na.rm = TRUE), .groups = "drop") %>%
    mutate(day = as.integer(floor(as.numeric(difftime(bin, min(bin), units = "days")))),
           hod = hour(bin) + minute(bin)/60)
  maxday <- max(df$day)
  d2 <- bind_rows(df %>% mutate(row = day, x = hod),
                  df %>% mutate(row = day - 1, x = hod + 24)) %>%
    filter(row >= 0, row <= maxday)
  ggplot(d2, aes(x = x, y = value)) +
    geom_tile(aes(y = 0, height = Inf, fill = ld < 0.5), alpha = 0.22) +
    geom_col(width = 0.25) +
    scale_fill_manual(values = c(`TRUE` = "grey15", `FALSE` = "white"), guide = "none") +
    scale_x_continuous(breaks = seq(0, 48, 12), limits = c(0, 48), expand = c(0, 0)) +
    facet_grid(row ~ ., switch = "y") +
    labs(x = "time (double-plotted, h)", y = "day", title = title) +
    theme_minimal(base_size = 8) +
    theme(strip.text.y.left = element_text(angle = 0), panel.spacing = unit(0, "pt"),
          axis.text.y = element_blank(), panel.grid = element_blank())
}
for (n in inds) {
  ggsave(file.path("baseline", paste0(gsub(" ", "_", n), "_actogram.png")),
         make_actogram(trikinetics[[n]], paste0(n, " - double-plotted actogram")),
         width = 5, height = 7, dpi = 120)
}
cat("actograms written to baseline/*.png\n")
