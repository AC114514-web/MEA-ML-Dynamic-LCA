# R analysis for the customer's revised MEA/CO2 dataset.
# Outputs use only observed values: two blank rows are removed and duplicate rows retained.
# Model metrics are leave-one-out cross-validation (LOOCV) metrics.

required <- c("readxl", "ggplot2", "dplyr", "tidyr", "patchwork",
              "e1071", "kernlab", "randomForest", "xgboost", "ranger")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Please install missing packages: ", paste(missing, collapse = ", "))

suppressPackageStartupMessages({
  library(readxl); library(ggplot2); library(dplyr); library(tidyr); library(patchwork)
  library(e1071); library(kernlab); library(randomForest); library(xgboost); library(ranger)
})

set.seed(20260711)
input_file <- "C:/Users/liu19/Documents/WXWork/1688857759516802/Cache/File/2026-07/表格4.192(3).xlsx"
out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# CENTRAL TYPOGRAPHY AND EXPORT SETTINGS
# Change values in this block to update every figure consistently.
# All theme() text sizes are in points (pt). geom_text()/annotate() sizes are
# converted from pt to mm through pt_to_mm(), because ggplot uses mm there.
# =============================================================================
FONT_FAMILY <- "Arial"                 # Font family for every word, label and number.
FONT_BASE_PT <- 11                      # Default body text not overridden below.
FONT_FIGURE_TITLE_PT <- 16              # Overall title: Figures 1--4.
FONT_FIGURE_SUBTITLE_PT <- 12           # Overall subtitle: model/data notes.
FONT_PANEL_TITLE_PT <- 13               # Facet/individual-panel headers.
FONT_AXIS_TITLE_PT <- 13                # Axis title/label; customer-specified 13 pt.
FONT_AXIS_TICK_PT <- 11                 # Axis tick numbers and categorical tick labels.
FONT_ANNOTATION_PT <- 11                # n = ... sample-size annotations.
FONT_BAR_VALUE_PT <- 10                 # Numeric values printed above model bars.
FONT_LEGEND_TITLE_PT <- 12              # SHAP colour-bar title: Feature value.
FONT_LEGEND_LABEL_PT <- 11              # SHAP colour-bar labels: High / Low.
FONT_CAPTION_PT <- 9                    # Figure footnotes/captions.

TIFF_DPI <- 300                          # Required final TIFF resolution.
pt_to_mm <- function(pt) pt / ggplot2::.pt

# Exports each final figure as 300 dpi TIFF (LZW compressed) and vector PDF.
export_figure <- function(plot, stem, width_in, height_in) {
  ggsave(file.path(out_dir, paste0(stem, ".tiff")), plot,
         width = width_in, height = height_in, units = "in", dpi = TIFF_DPI,
         device = "tiff", compression = "lzw", bg = "white")
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot,
         width = width_in, height = height_in, units = "in",
         device = grDevices::cairo_pdf, family = FONT_FAMILY, bg = "white")
}

# -----------------------------------------------------------------------------
# 1. Read and create a clean analysis copy in memory only
# -----------------------------------------------------------------------------
raw <- read_excel(input_file, skip = 1, na = c("", " "))
if (ncol(raw) != 10) stop("Expected 10 fields after the two-row header.")
names(raw) <- c("solvent_type", "time_h", "temperature_c", "mea_conc_wt_pct",
                "co2_loading_mol_mol", "o2_vol_pct", "initial_ph",
                "mea_loss_rate_wt_pct_per_1000h", "regen_energy_gj_per_tco2",
                "co2_capture_efficiency_pct")
to_numeric <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
dat <- raw |> transmute(
  time_h = to_numeric(time_h),
  temperature_c = to_numeric(temperature_c),
  mea_conc_wt_pct = to_numeric(mea_conc_wt_pct),
  co2_loading_mol_mol = to_numeric(co2_loading_mol_mol),
  mea_loss_rate_wt_pct_per_1000h = to_numeric(mea_loss_rate_wt_pct_per_1000h),
  regen_energy_gj_per_tco2 = to_numeric(regen_energy_gj_per_tco2),
  co2_capture_efficiency_pct = to_numeric(co2_capture_efficiency_pct)
)
# Remove only rows with no values at all. Repeated measurements are retained.
dat <- dat |> filter(if_any(everything(), ~ !is.na(.x)))
write.csv(dat, file.path(out_dir, "customer_analysis_data_clean.csv"), row.names = FALSE)

input_vars <- c("time_h", "temperature_c", "mea_conc_wt_pct", "co2_loading_mol_mol")
input_labels <- c(
  time_h = "Time (hour)", temperature_c = "Temperature (°C)",
  mea_conc_wt_pct = "MEA conc (wt%)", co2_loading_mol_mol = "CO2 loading (mol/mol)"
)
target_vars <- c("co2_capture_efficiency_pct", "regen_energy_gj_per_tco2")
target_labels <- c(
  co2_capture_efficiency_pct = "CO2 capture efficiency (%)",
  regen_energy_gj_per_tco2 = "Regeneration energy (GJ/tCO2)"
)
output_vars <- c("mea_loss_rate_wt_pct_per_1000h", "regen_energy_gj_per_tco2",
                 "co2_capture_efficiency_pct")
output_labels <- c(
  mea_loss_rate_wt_pct_per_1000h = "MEA loss rate (wt%/1000 h)",
  regen_energy_gj_per_tco2 = "Regeneration energy (GJ/tCO2)",
  co2_capture_efficiency_pct = "CO2 capture efficiency (%)"
)

theme_customer <- theme_classic(base_size = FONT_BASE_PT, base_family = FONT_FAMILY) +
  theme(
    text = element_text(family = FONT_FAMILY),
    plot.title = element_text(hjust = .5, face = "bold", size = FONT_FIGURE_TITLE_PT),
    plot.subtitle = element_text(hjust = .5, colour = "#4B5563", size = FONT_FIGURE_SUBTITLE_PT),
    strip.background = element_rect(fill = "#EFF6F4", colour = NA),
    strip.text = element_text(face = "bold", size = FONT_PANEL_TITLE_PT),
    axis.title = element_text(size = FONT_AXIS_TITLE_PT),
    axis.text = element_text(size = FONT_AXIS_TICK_PT),
    legend.title = element_text(size = FONT_LEGEND_TITLE_PT),
    legend.text = element_text(size = FONT_LEGEND_LABEL_PT),
    plot.caption = element_text(size = FONT_CAPTION_PT),
    panel.grid.major.y = element_line(colour = "#E8ECEF", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    plot.margin = ggplot2::margin(10, 14, 10, 10)
  )

# -----------------------------------------------------------------------------
# Figure 1. Four input-variable multi-panel box plots
# -----------------------------------------------------------------------------
input_long <- dat |> select(all_of(input_vars)) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
  filter(!is.na(Value)) |>
  mutate(Variable = factor(Variable, levels = input_vars, labels = unname(input_labels)))
input_note <- input_long |> group_by(Variable) |>
  summarise(n = n(), y = max(Value) + .06 * diff(range(Value)), .groups = "drop")
p_input <- ggplot(input_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(width = .44, outlier.shape = 21, outlier.size = 1.55,
               outlier.fill = "white", outlier.colour = "#4A4A4A", colour = "#3A3A3A") +
  geom_point(stat = "summary", fun = mean, shape = 21, size = 1.8, fill = "#202124") +
  # Sample-size annotation; controlled by FONT_ANNOTATION_PT at the script head.
  geom_text(data = input_note, aes(x = 1, y = y, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = pt_to_mm(FONT_ANNOTATION_PT), vjust = 0) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("#89C7BA", "#FBF19B", "#B6B3D2", "#F5A676"), guide = "none") +
  labs(title = "Input Feature Distributions", x = NULL, y = NULL,
       caption = "Box: IQR; center line: median; black dot: mean; whiskers: 1.5×IQR") +
  theme_customer + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
export_figure(p_input, "01_input_feature_boxplots_customer", 10.5, 7.2)

# -----------------------------------------------------------------------------
# Figure 2. Three output-variable multi-panel box plots
# -----------------------------------------------------------------------------
output_long <- dat |> select(all_of(output_vars)) |>
  pivot_longer(everything(), names_to = "Output", values_to = "Value") |>
  filter(!is.na(Value)) |>
  mutate(Output = factor(Output, levels = output_vars, labels = unname(output_labels)))
make_output_boxplot <- function(output_key, y_upper = NULL) {
  output_label <- unname(output_labels[output_key])
  d <- output_long |> filter(as.character(Output) == output_label)
  y_note <- if (is.null(y_upper)) max(d$Value) + .06 * diff(range(d$Value)) else y_upper * .94
  p <- ggplot(d, aes(x = 1, y = Value)) +
    geom_boxplot(width = .42, fill = "#94D3B7", outlier.shape = 21, outlier.size = 1.45,
                 outlier.fill = "white", outlier.colour = "#4A4A4A", colour = "#3A3A3A") +
    geom_point(stat = "summary", fun = mean, shape = 21, size = 1.8, fill = "#202124") +
    # Sample-size annotation; controlled by FONT_ANNOTATION_PT at the script head.
    annotate("text", x = .76, y = y_note, label = paste0("n = ", nrow(d)), hjust = 0,
             size = pt_to_mm(FONT_ANNOTATION_PT)) +
    scale_x_continuous(limits = c(.5, 1.5), breaks = NULL) +
    labs(title = output_label, x = NULL, y = NULL) +
    # Per-output-variable panel title; controlled by FONT_PANEL_TITLE_PT.
    theme_customer + theme(plot.title = element_text(hjust = .5, face = "bold", size = FONT_PANEL_TITLE_PT),
                           plot.margin = ggplot2::margin(4, 8, 4, 8))
  if (!is.null(y_upper)) p <- p + coord_cartesian(ylim = c(0, y_upper))
  p
}
p_output <- make_output_boxplot("mea_loss_rate_wt_pct_per_1000h") +
  make_output_boxplot("regen_energy_gj_per_tco2", y_upper = 20) +
  make_output_boxplot("co2_capture_efficiency_pct") +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Output Feature Distributions",
    caption = "Box: IQR; center line: median; black dot: mean; whiskers: 1.5×IQR. Regeneration-energy display is truncated at 20 GJ/tCO2; all observations remain in the boxplot calculation.",
    theme = theme(plot.title = element_text(hjust = .5, face = "bold", size = FONT_FIGURE_TITLE_PT),
                  plot.caption = element_text(size = FONT_CAPTION_PT))
  )
export_figure(p_output, "02_output_feature_boxplots_customer", 13.5, 5.4)

# -----------------------------------------------------------------------------
# Model fitting and leave-one-out cross-validation
# -----------------------------------------------------------------------------
models_by_target <- list(
  co2_capture_efficiency_pct = c("XGBoost", "Random Forest", "Extra Trees", "Gaussian Process Regression"),
  regen_energy_gj_per_tco2 = c("Linear Regression", "Random Forest", "SVR", "Gaussian Process Regression")
)
plot_model_order <- c("XGBoost", "Linear Regression", "Random Forest", "Extra Trees", "SVR", "Gaussian Process Regression")

fit_model <- function(model_name, x, y, seed = 1) {
  x <- as.data.frame(x)
  if (model_name == "Linear Regression") {
    return(lm(y ~ ., data = cbind(y = y, x)))
  }
  if (model_name == "Random Forest") {
    set.seed(seed)
    return(randomForest(x = x, y = y, ntree = 1000, mtry = min(2, ncol(x)), nodesize = 2))
  }
  if (model_name == "XGBoost") {
    set.seed(seed)
    return(xgboost::xgb.train(
      data = xgboost::xgb.DMatrix(data = as.matrix(x), label = y),
      params = list(objective = "reg:squarederror", max_depth = 2, eta = .05,
                    subsample = .85, colsample_bytree = 1),
      nrounds = 80, verbose = 0
    ))
  }
  if (model_name == "Extra Trees") {
    return(ranger::ranger(
      x = x, y = y, num.trees = 1000, mtry = min(2, ncol(x)),
      splitrule = "extratrees", num.random.splits = 1, min.node.size = 2,
      seed = seed, num.threads = 1
    ))
  }
  if (model_name == "SVR") {
    return(e1071::svm(x = as.matrix(x), y = y, type = "eps-regression",
                      kernel = "radial", cost = 1, gamma = 1 / ncol(x),
                      epsilon = .1, scale = FALSE))
  }
  if (model_name == "Gaussian Process Regression") {
    return(kernlab::gausspr(x = as.matrix(x), y = y, type = "regression", scaled = FALSE,
                            kernel = "rbfdot", kpar = list(sigma = .15), var = .1))
  }
  stop("Unknown model: ", model_name)
}
predict_model <- function(fit, model_name, x) {
  if (model_name == "XGBoost") return(as.numeric(predict(fit, as.matrix(x))))
  if (model_name == "Extra Trees") return(as.numeric(predict(fit, data = as.data.frame(x))$predictions))
  if (model_name %in% c("SVR", "Gaussian Process Regression")) {
    return(as.numeric(predict(fit, as.matrix(x))))
  }
  as.numeric(predict(fit, as.data.frame(x)))
}
scale_train_test <- function(train_x, test_x) {
  # StandardScaler: fit mean and SD on the training fold only, then transform test.
  center <- sapply(train_x, mean)
  spread <- sapply(train_x, sd)
  spread[is.na(spread) | spread == 0] <- 1
  list(
    train = as.data.frame(sweep(sweep(train_x, 2, center, "-"), 2, spread, "/")),
    test = as.data.frame(sweep(sweep(test_x, 2, center, "-"), 2, spread, "/")),
    center = center, spread = spread
  )
}
loocv_model <- function(model_name, model_dat, target_name, feature_vars) {
  y <- model_dat[[target_name]]
  x <- model_dat[feature_vars]
  pred <- numeric(nrow(model_dat))
  for (i in seq_len(nrow(model_dat))) {
    scaled <- scale_train_test(x[-i, , drop = FALSE], x[i, , drop = FALSE])
    fit <- fit_model(model_name, scaled$train, y[-i], seed = 5000 + i)
    pred[i] <- predict_model(fit, model_name, scaled$test)
  }
  data.frame(observed = y, predicted = pred)
}
metric_calc <- function(cv, target_key, model_name) {
  y <- cv$observed; p <- cv$predicted
  data.frame(
    Target = unname(target_labels[target_key]),
    Model = model_name,
    n = length(y),
    R2 = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
    MAE = mean(abs(y - p)),
    RMSE = sqrt(mean((y - p)^2))
  )
}

model_sets <- lapply(target_vars, function(target) {
  dat |> select(all_of(c(input_vars, target))) |>
    filter(if_all(everything(), ~ !is.na(.x) & is.finite(.x)))
})
names(model_sets) <- target_vars
if (nrow(model_sets[[target_vars[1]]]) < 8 || nrow(model_sets[[target_vars[2]]]) < 8) {
  stop("Insufficient complete rows for LOOCV.")
}
active_features <- lapply(model_sets, function(x) {
  input_vars[vapply(x[input_vars], function(z) length(unique(z)) > 1, logical(1))]
})
if (any(vapply(active_features, length, integer(1)) < 1)) stop("No varying predictor is available for at least one target.")

metrics <- bind_rows(lapply(target_vars, function(target) {
  bind_rows(lapply(models_by_target[[target]], function(model_name) {
    cv <- loocv_model(model_name, model_sets[[target]], target, active_features[[target]])
    metric_calc(cv, target, model_name) |> mutate(Fitted_predictors = paste(active_features[[target]], collapse = "; "))
  }))
}))
write.csv(metrics, file.path(out_dir, "03_model_metrics_capture_energy_xgb_extratrees_LOOCV.csv"), row.names = FALSE)

metric_long <- metrics |> pivot_longer(c(R2, MAE, RMSE), names_to = "Metric", values_to = "Value") |>
  mutate(Target = factor(Target, levels = unname(target_labels)),
         Model = factor(Model, levels = plot_model_order),
         Metric = factor(Metric, levels = c("R2", "MAE", "RMSE")))
p_metrics <- ggplot(metric_long, aes(x = Model, y = Value, fill = Model)) +
  geom_col(width = .68, colour = "#313131", linewidth = .25) +
  # Values above/below each bar; controlled by FONT_BAR_VALUE_PT.
  geom_text(aes(label = sprintf("%.2f", Value)), vjust = ifelse(metric_long$Value >= 0, -.28, 1.18),
            size = pt_to_mm(FONT_BAR_VALUE_PT)) +
  facet_grid(Metric ~ Target, scales = "free",
             labeller = labeller(Metric = c(R2 = "R² (higher is better)",
                                             MAE = "MAE (lower is better)",
                                             RMSE = "RMSE (lower is better)"))) +
  scale_fill_manual(values = c("XGBoost" = "#8ED1C0", "Linear Regression" = "#8ED1C0",
                               "Random Forest" = "#6FA8DC", "Extra Trees" = "#9294E8",
                               "SVR" = "#9294E8", "Gaussian Process Regression" = "#3C57C8"),
                    guide = "none") +
  labs(title = "LOOCV Performance Comparison of Four Regression Models",
       subtitle = "CO2 capture efficiency: n = 25, four varying inputs fitted.\nRegeneration energy: n = 27, four varying inputs fitted.",
       x = NULL, y = NULL,
       caption = "Metrics are raw LOOCV values; panels use independent y-scales.") +
  # Model names on x-axis use FONT_AXIS_TICK_PT; metric/target strips use FONT_PANEL_TITLE_PT.
  theme_customer + theme(axis.text.x = element_text(angle = 15, hjust = 1, size = FONT_AXIS_TICK_PT),
                         strip.text.y = element_text(angle = 0, size = FONT_PANEL_TITLE_PT))
export_figure(p_metrics, "03_model_metric_comparison_capture_energy_xgb_extratrees_LOOCV", 13.5, 10)

# -----------------------------------------------------------------------------
# Figure 5. Model-agnostic permutation SHAP beeswarms for the best LOOCV model
# -----------------------------------------------------------------------------
fit_scaled_full <- function(model_name, model_dat, target, feature_vars) {
  x <- model_dat[feature_vars]
  center <- sapply(x, mean); spread <- sapply(x, sd)
  spread[is.na(spread) | spread == 0] <- 1
  scaled_x <- as.data.frame(sweep(sweep(x, 2, center, "-"), 2, spread, "/"))
  list(fit = fit_model(model_name, scaled_x, model_dat[[target]], seed = 9001),
       x = scaled_x, raw_x = x, raw_all = model_dat[input_vars])
}
shap_permutation <- function(fit, model_name, explain_x, background_x, features, m = 70) {
  result <- matrix(0, nrow(explain_x), length(features), dimnames = list(NULL, features))
  for (i in seq_len(nrow(explain_x))) {
    for (b in seq_len(m)) {
      current <- background_x[sample(seq_len(nrow(background_x)), 1), , drop = FALSE]
      order <- sample(features)
      before <- predict_model(fit, model_name, current)
      for (feature in order) {
        current[[feature]] <- explain_x[[feature]][i]
        after <- predict_model(fit, model_name, current)
        result[i, feature] <- result[i, feature] + (after - before) / m
        before <- after
      }
    }
  }
  as.data.frame(result)
}
best_models <- metrics |> group_by(Target) |> slice_max(R2, n = 1, with_ties = FALSE) |> ungroup()
shap_tables <- lapply(target_vars, function(target) {
  target_label <- unname(target_labels[target])
  model_name <- best_models |> filter(Target == target_label) |> pull(Model)
  current_features <- active_features[[target]]
  fixed_features <- setdiff(input_vars, current_features)
  full <- fit_scaled_full(model_name, model_sets[[target]], target, current_features)
  shap <- shap_permutation(full$fit, model_name, full$x, full$x, current_features, m = 70)
  shap_long <- shap |> mutate(Row = row_number()) |>
    pivot_longer(-Row, names_to = "Feature", values_to = "SHAP") |>
    left_join(full$raw_all |> mutate(Row = row_number()) |>
                pivot_longer(all_of(current_features), names_to = "Feature", values_to = "FeatureValue"),
              by = c("Row", "Feature")) |>
    bind_rows(if (length(fixed_features)) {
      full$raw_all |> mutate(Row = row_number()) |>
        pivot_longer(all_of(fixed_features), names_to = "Feature", values_to = "FeatureValue") |>
        mutate(SHAP = 0)
    } else tibble()) |>
    group_by(Feature) |> mutate(Importance = mean(abs(SHAP)),
                                FeatureScaled = ifelse(sd(FeatureValue, na.rm = TRUE) == 0, 0,
                                                       as.numeric(scale(FeatureValue)))) |> ungroup() |>
    mutate(Target = target_label, Model = model_name,
           FeatureLabel = unname(input_labels[Feature]))
  shap_long
})
shap_all <- bind_rows(shap_tables)
write.csv(shap_all, file.path(out_dir, "04_shap_values_capture_energy_xgb_extratrees_permutation.csv"), row.names = FALSE)

make_shap_plot <- function(target) {
  target_label <- unname(target_labels[target])
  d <- shap_all |> filter(Target == target_label)
  ordering <- d |> group_by(FeatureLabel) |> summarise(Importance = first(Importance), .groups = "drop") |>
    arrange(Importance) |> pull(FeatureLabel)
  n_obs <- nrow(model_sets[[target]])
  chosen_model <- unique(d$Model)
  fixed_message <- if (length(setdiff(input_vars, active_features[[target]]))) {
    " Time and Temperature are fixed conditions; their SHAP values are zero."
  } else ""
  ggplot(d, aes(x = SHAP, y = factor(FeatureLabel, levels = ordering), colour = FeatureScaled)) +
    geom_vline(xintercept = 0, colour = "#9AA1A8", linewidth = .5) +
    geom_point(position = position_jitter(height = .13, width = 0), alpha = .88, size = 2.1) +
    scale_colour_gradient2(low = "#1687F5", mid = "#A346D5", high = "#FA176C", midpoint = 0,
                           name = "Feature\nvalue", limits = c(-2, 2), oob = scales::squish,
                           breaks = c(-2, 2), labels = c("Low", "High"),
                           guide = guide_colorbar(barheight = grid::unit(36, "mm"),
                                                  barwidth = grid::unit(5, "mm"),
                                                  title.position = "top", label.position = "right",
                                                  ticks = FALSE)) +
    labs(title = paste0("SHAP Beeswarm: ", target_label),
         subtitle = paste0("Model: ", chosen_model, "; n = ", n_obs,
                           ifelse(n_obs == 10, " (exploratory only)", " (exploratory analysis)"), fixed_message),
         x = "SHAP value (impact on model output)", y = NULL,
         caption = "Model-agnostic permutation SHAP; point colour indicates standardized feature value.") +
    # SHAP panel title/subtitle, axis labels/ticks, and High/Low legend use the
    # FONT_PANEL_TITLE_PT, FONT_FIGURE_SUBTITLE_PT, FONT_AXIS_*, and FONT_LEGEND_* settings.
    theme_customer + theme(plot.title = element_text(size = FONT_PANEL_TITLE_PT, hjust = .5, face = "bold"),
                           plot.subtitle = element_text(size = FONT_FIGURE_SUBTITLE_PT, hjust = .5),
                           legend.position = "right",
                           panel.grid.major.y = element_line(colour = "#E8ECEF", linetype = "dotted"))
}
p_shap_loss <- make_shap_plot(target_vars[1])
p_shap_energy <- make_shap_plot(target_vars[2])
p_shap <- p_shap_loss / p_shap_energy +
  plot_annotation(title = "Feature Contributions to CO2 Capture Efficiency and Regeneration Energy",
                  subtitle = "Requested four-input structure; each target is fitted on its own complete-case dataset.",
                  theme = theme(plot.title = element_text(size = FONT_FIGURE_TITLE_PT, hjust = 0, face = "bold"),
                                plot.subtitle = element_text(size = FONT_FIGURE_SUBTITLE_PT, hjust = 0)))
export_figure(p_shap, "04_shap_beeswarm_capture_energy_xgb_extratrees", 12.5, 10.5)

# A compact, auditable summary of effective sample sizes.
sample_summary <- bind_rows(
  input_long |> count(Variable, name = "n") |> transmute(Type = "Input", Variable = as.character(Variable), n),
  output_long |> count(Output, name = "n") |> transmute(Type = "Output", Variable = as.character(Output), n),
  tibble(Type = "Model complete cases", Variable = unname(target_labels[target_vars]),
         n = vapply(model_sets, nrow, numeric(1)))
)
write.csv(sample_summary, file.path(out_dir, "data_availability_summary_capture_energy_xgb_extratrees.csv"), row.names = FALSE)

message("Completed customer figures using observed data only. Saved to: ", normalizePath(out_dir))
