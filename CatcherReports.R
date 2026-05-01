# --- Libraries ---
library(dplyr)
library(ggplot2)
library(patchwork)
library(gt)
library(xgboost)

# --- Load xgboost Model ---
base <- "/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www/final_model_xCalledStrike_model"
candidates <- c(paste0(base, ".ubj"), paste0(base, ".json"))

xstrike_model_path <- candidates[file.exists(candidates)][1]
if (is.na(xstrike_model_path)) {
  stop("Converted model (.ubj or .json) not found")
}

cat("✅ Loading xCalledStrike model from:", xstrike_model_path, "\n")
xstrike_model <- xgboost::xgb.load(xstrike_model_path)

# --- Prediction Function ---
predict_xstrike <- function(df) {
  feature_cols <- c("PlateLocSide", "PlateLocHeight")
  available_features <- feature_cols[feature_cols %in% names(df)]
  missing_features <- setdiff(feature_cols, available_features)
  
  if (length(missing_features) > 0) {
    warning("Missing features: ", paste(missing_features, collapse = ", "))
  }
  
  if (length(available_features) == 0) {
    stop("No matching features found in data. Available columns: ", 
         paste(names(df), collapse = ", "))
  }
  
  feature_data <- df[, available_features, drop = FALSE]
  feature_data <- as.data.frame(lapply(feature_data, function(x) {
    as.numeric(as.character(x))
  }))
  
  feature_matrix <- as.matrix(feature_data)
  
  for (col in 1:ncol(feature_matrix)) {
    col_mean <- mean(feature_matrix[, col], na.rm = TRUE)
    if (is.nan(col_mean) || is.na(col_mean)) col_mean <- 0
    feature_matrix[is.na(feature_matrix[, col]), col] <- col_mean
  }
  
  dmatrix <- xgb.DMatrix(data = feature_matrix)
  predictions <- predict(xstrike_model, dmatrix)
  
  return(predictions)
}

# --- Config ---
SZ_TOP <- 3.5; SZ_BOT <- 1.5; SZ_LEFT <- -0.83; SZ_RIGHT <- 0.83
SHADOW_BAND <- 0.1

# --- Utility Functions ---
pct_lab <- function(p) paste0(round(100 * p), "%")

base_zone_layers <- function() {
  list(
    geom_rect(aes(xmin = SZ_LEFT, xmax = SZ_RIGHT, ymin = SZ_BOT, ymax = SZ_TOP),
              color = "black", fill = NA, linewidth = 0.8),
    geom_segment(aes(x = SZ_LEFT + (SZ_RIGHT - SZ_LEFT)/3, 
                     xend = SZ_LEFT + (SZ_RIGHT - SZ_LEFT)/3,
                     y = SZ_BOT, yend = SZ_TOP), 
                 color = "black", linewidth = 0.5),
    geom_segment(aes(x = SZ_LEFT + 2*(SZ_RIGHT - SZ_LEFT)/3, 
                     xend = SZ_LEFT + 2*(SZ_RIGHT - SZ_LEFT)/3,
                     y = SZ_BOT, yend = SZ_TOP), 
                 color = "black", linewidth = 0.5),
    geom_segment(aes(x = SZ_LEFT, xend = SZ_RIGHT,
                     y = SZ_BOT + (SZ_TOP - SZ_BOT)/3, 
                     yend = SZ_BOT + (SZ_TOP - SZ_BOT)/3), 
                 color = "black", linewidth = 0.5),
    geom_segment(aes(x = SZ_LEFT, xend = SZ_RIGHT,
                     y = SZ_BOT + 2*(SZ_TOP - SZ_BOT)/3, 
                     yend = SZ_BOT + 2*(SZ_TOP - SZ_BOT)/3), 
                 color = "black", linewidth = 0.5),
    geom_rect(aes(xmin = SZ_LEFT - SHADOW_BAND, xmax = SZ_RIGHT + SHADOW_BAND,
                  ymin = SZ_BOT - SHADOW_BAND, ymax = SZ_TOP + SHADOW_BAND),
              color = "grey60", fill = NA, linetype = 2, linewidth = 0.6)
  )
}

in_shadow_zone <- function(x, z) {
  in_outer <- x >= (SZ_LEFT - SHADOW_BAND) & x <= (SZ_RIGHT + SHADOW_BAND) &
    z >= (SZ_BOT - SHADOW_BAND) & z <= (SZ_TOP + SHADOW_BAND)
  in_zone <- x >= SZ_LEFT & x <= SZ_RIGHT & z >= SZ_BOT & z <= SZ_TOP
  in_outer & !in_zone
}

plot_calls <- function(df, title_txt) {
  ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight)) +
    base_zone_layers() +
    geom_point(aes(color = PitchCall), size = 2.5, alpha = 0.85) +
    geom_label(aes(label = pct_lab(xStrike)), 
               vjust = -0.8, 
               size = 2.5,
               fontface = "bold",
               label.padding = unit(0.15, "lines"),
               label.size = 0.3,
               fill = "white") +
    scale_color_manual(
      values = c("Strike Called" = "#E74C3C", "Ball Called" = "#2ECC71"),
      name = "Pitch Call"
    ) +
    coord_equal(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    labs(title = title_txt, x = "Catcher's Perspective", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.background = element_rect(color = "black", fill = "white", linewidth = 1)
    )
}

# --- Catcher Report Function ---
make_catcher_report <- function(df, catcher_name, report_date) {
  original_pitches <- nrow(df)
  
  df <- df %>%
    filter(PitchCall %in% c("StrikeCalled", "Strike Called", "Called Strike", 
                            "BallCalled", "Ball Called", "BallinDirt", "Ball in Dirt"))
  
  called_pitches <- nrow(df)
  
  df <- df %>%
    mutate(called_strk = PitchCall %in% c("StrikeCalled", "Strike Called", "Called Strike"))
  
  num_called_strikes <- sum(df$called_strk)
  
  if (!("xStrike" %in% names(df))) {
    df$xStrike <- predict_xstrike(df)
  }
  
  df <- df %>%
    mutate(
      PitchCall = ifelse(called_strk, "Strike Called", "Ball Called"),
      in_shadow = in_shadow_zone(PlateLocSide, PlateLocHeight)
    )
  
  cat("\n=== DIAGNOSTIC INFO FOR", catcher_name, "===\n")
  cat("Total pitches in dataset:", original_pitches, "\n")
  cat("Called pitches (no swings):", called_pitches, "\n")
  cat("Called strikes:", num_called_strikes, "\n")
  cat("Called Strike%:", round(100 * num_called_strikes / called_pitches, 1), "%\n")
  cat("Mean xStrike:", round(100 * mean(df$xStrike, na.rm = TRUE), 1), "%\n")
  cat("\nPitch type breakdown:\n")
  print(table(df$PitchCall))
  cat("\n")
  
  p_low <- df %>%
    filter(PitchCall == "Strike Called") %>%
    arrange(xStrike) %>%
    slice_head(n = 5)
  
  p_shadow <- df %>% filter(in_shadow)
  
  p_high <- df %>%
    filter(PitchCall == "Ball Called") %>%
    arrange(desc(xStrike)) %>%
    slice_head(n = 5)
  
  p_all_stolen <- df %>%
    filter(PitchCall == "Strike Called" & xStrike < 0.50)
  
  p_all_lost <- df %>%
    filter(PitchCall == "Ball Called" & xStrike >= 0.50)
  
  g1 <- plot_calls(p_low, "Top 5 Stolen Strikes")
  g2 <- plot_calls(p_shadow, "Pitches in Shadow Zone")
  g3 <- plot_calls(p_high, "Top 5 Strikes Lost")
  g4 <- plot_calls(p_all_stolen, "All Stolen Strikes")
  g5 <- plot_calls(p_all_lost, "All Strikes Lost")
  
  called_strike_rate <- mean(df$PitchCall == "Strike Called", na.rm = TRUE)
  expected_strike_rate <- mean(df$xStrike, na.rm = TRUE)
  lift <- called_strike_rate - expected_strike_rate
  
  strikes_stolen <- sum(df$PitchCall == "Strike Called" & df$xStrike < 0.50, na.rm = TRUE)
  strikes_lost   <- sum(df$PitchCall == "Ball Called"   & df$xStrike >= 0.50, na.rm = TRUE)
  
  stats_plot <- ggplot() +
    annotate("text", x = 0.5, y = 0.98,
             label = "Catcher Stats",
             size = 6, fontface = "bold") +
    annotate("segment", x = 0.2, xend = 0.8, y = 0.90, yend = 0.90,
             linewidth = 0.8) +
    annotate("rect", xmin = 0.05, xmax = 0.95, ymin = 0.40, ymax = 0.82,
             fill = "white", color = "black", linewidth = 0.8) +
    annotate("segment", x = 0.243, xend = 0.243, y = 0.40, yend = 0.82,
             linewidth = 0.8, color = "black") +
    annotate("segment", x = 0.436, xend = 0.436, y = 0.40, yend = 0.82,
             linewidth = 0.8, color = "black") +
    annotate("segment", x = 0.629, xend = 0.629, y = 0.40, yend = 0.82,
             linewidth = 0.8, color = "black") +
    annotate("segment", x = 0.822, xend = 0.822, y = 0.40, yend = 0.82,
             linewidth = 0.8, color = "black") +
    annotate("segment", x = 0.05, xend = 0.95, y = 0.71, yend = 0.71,
             linewidth = 0.8, color = "black") +
    annotate("text", x = 0.1465, y = 0.765,
             label = "Called Strike%",
             size = 3.2, fontface = "bold") +
    annotate("text", x = 0.3395, y = 0.765,
             label = "xCalledStrike%",
             size = 3.2, fontface = "bold") +
    annotate("text", x = 0.5325, y = 0.765,
             label = "Cstr% Above Expected",
             size = 2.8, fontface = "bold") +
    annotate("text", x = 0.7255, y = 0.765,
             label = "Strikes Stolen",
             size = 2.8, fontface = "bold") +
    annotate("text", x = 0.89, y = 0.765,
             label = "Strikes Lost",
             size = 2.8, fontface = "bold") +
    annotate("text", x = 0.1465, y = 0.555,
             label = sprintf("%.1f%%", called_strike_rate * 100),
             size = 6, fontface = "bold") +
    annotate("text", x = 0.3395, y = 0.555,
             label = sprintf("%.1f%%", expected_strike_rate * 100),
             size = 6, fontface = "bold") +
    annotate("text", x = 0.5325, y = 0.555,
             label = sprintf("%.1f%%", lift * 100),
             size = 6, fontface = "bold",
             color = ifelse(lift > 0, "#27AE60", "#E74C3C")) +
    annotate("text", x = 0.7255, y = 0.555,
             label = as.character(strikes_stolen),
             size = 6, fontface = "bold", color = "#27AE60") +
    annotate("text", x = 0.89, y = 0.555,
             label = as.character(strikes_lost),
             size = 6, fontface = "bold", color = "#E74C3C") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
  
  common_theme <- theme(
    plot.margin = margin(10, 15, 10, 15),
    plot.background = element_rect(fill = "white", color = NA)
  )
  
  g1 <- g1 + common_theme
  g2 <- g2 + common_theme
  g3 <- g3 + common_theme
  g4 <- g4 + common_theme
  g5 <- g5 + common_theme
  stats_plot <- stats_plot + common_theme
  
  full_report <- (g4 + g5) / (g1 + g2 + g3) / stats_plot +
    plot_layout(heights = c(3.5, 3.5, 1)) +
    plot_annotation(
      title = paste0(catcher_name, " ", report_date, " Postgame Report"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, vjust = -12, size = 16, face = "bold"),
        plot.margin = margin(10, 15, 10, 15),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )
  
  full_report
}

# --- Generate Reports per Catcher ---
if (!("Catcher" %in% names(CSUF25))) {
  stop("CSUF25 must include a 'Catcher' column to group reports.")
}

if (!("Date" %in% names(CSUF25))) {
  stop("CSUF25 must include a 'Date' column.")
}

most_recent_game_date <- max(as.Date(CSUF25$Date), na.rm = TRUE)
report_date <- format(most_recent_game_date, "%B %d, %Y")

cat("\n📅 Generating reports for game date:", report_date, "\n")

unique_catchers <- unique(CSUF25$Catcher)

all_catchers_file <- paste0("All_Catcher_Reports_", format(most_recent_game_date, "%Y%m%d"), ".pdf")
pdf(all_catchers_file, width = 11, height = 14)

for (catcher in unique_catchers) {
  cat("\n🎯 Generating report for:", catcher, "...\n")
  
  catcher_data <- CSUF25 %>% filter(Catcher == catcher)
  
  if (nrow(catcher_data) == 0) {
    cat("⚠️ No data for catcher:", catcher, "\n")
    next
  }
  
  name_parts <- strsplit(catcher, ", ")[[1]]
  if (length(name_parts) == 2) {
    catcher_display_name <- paste(name_parts[2], name_parts[1])
  } else {
    catcher_display_name <- catcher
  }
  
  catcher_report <- make_catcher_report(
    catcher_data,
    catcher_name = catcher_display_name,
    report_date = report_date
  )
  
  print(catcher_report)
  
  file_name <- paste0("Catcher_Report_", gsub(" ", "_", catcher_display_name), "_", 
                      format(most_recent_game_date, "%Y%m%d"), ".pdf")
  ggsave(file_name, catcher_report, width = 11, height = 14, dpi = 300)
  
  cat("✅ Saved individual report:", file_name, "\n")
}

dev.off()
cat("\n✅ All reports combined in:", all_catchers_file, "\n")