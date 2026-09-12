# -----------------------------------------------------------------
# Survey ggplot2 Documentation | Individual Crosstabulation Plots
# Emmanuel Jr Gigantone, Ekonsepto 
# Developed with coding assistance from ChatGPT 
library(readxl)
library(ggplot2)
# Install once in the Console: install.packages("ggrepel")
library(ggrepel)
windowsFonts("Times New Roman" = windowsFont("TT Times New Roman"))
# Import data
dat <- read_excel(file.choose(), sheet = 1)
View(dat)
print(names(dat))

# Analysis settings
# grouped: selected DV within a grouping column.
# questions: understanding against exposure, with no demographic column.
analysis_mode <- "grouped"
iv_column <- "Age_generation"
dv_column <- "participant_type"
question_categories <- "raw"       # raw or reclassified (questions mode)
percentage_base <- "column"        # column or total (questions mode)
exposure_column <- "I4_S4G"
understanding_column <- "I5_S4G"
weight_column <- "Weight_Stratified"
stopifnot(analysis_mode %in% c("grouped", "questions"),
          question_categories %in% c("raw", "reclassified"),
          percentage_base %in% c("column", "total"))
use_participant_type <- analysis_mode == "grouped" && dv_column %in%
  c("participant_type", exposure_column, understanding_column)
reclassify <- use_participant_type ||
  (analysis_mode == "questions" && question_categories == "reclassified")
if (analysis_mode == "questions") {
  iv_column <- exposure_column
  dv_column <- understanding_column
}
required <- c(weight_column, if (reclassify) {
  c(iv_column, exposure_column, understanding_column)
} else c(iv_column, dv_column))
if (!all(required %in% names(dat))) stop("Check the configured column names against names(dat).")

# Standardize response text for matching
normalize_response <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[-\u2010-\u2015]", " ", x)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x <- trimws(gsub("[.!?]+$", "", x))
  x[x == ""] <- NA_character_
  x
}

# Classify responses; missing and unmatched values remain NA.
recode_response <- function(x, yes, no) {
  x <- normalize_response(x)
  result <- rep(NA, length(x))
  result[x %in% normalize_response(yes)] <- TRUE
  result[x %in% normalize_response(no)] <- FALSE
  unmatched <- unique(x[!is.na(x) & is.na(result)])
  if (length(unmatched)) warning("Unclassified responses: ", paste(unmatched, collapse = "; "))
  result
}
participant_levels <- c(
  "Consume and understand",
  "Consume but do not understand",
  "Do not consume but understand",
  "Do not consume and do not understand"
)
if (reclassify) {
  consumes <- recode_response(dat[[exposure_column]],
                              yes = c("isang beses sa isang linggo", "isang beses sa isang liggo",
                                      "dalawa hanggang tatlong beses sa isang linggo",
                                      "apat hanggang limang beses sa isang linggo",
                                      "araw-araw o halos araw-araw"),
                              no = "Hindi ako nagbabasa o nanonood ng mga balita tungkol sa ekonomiya"
  )
  understands <- recode_response(dat[[understanding_column]],
                                 yes = c("Sumasang-ayon", "Lubos na sumasang-ayon"),
                                 no = c("Neutral", "hindi sumasang-ayon", "Lubos na hindi sumasang-ayon")
  )
  # Map the two classifications to the ordered participant types.
  type_index <- 1L + 2L * as.integer(!consumes) + as.integer(!understands)
  dat$participant_type <- factor(participant_levels[type_index], levels = participant_levels)
  if (use_participant_type) dv_column <- "participant_type"
}

# Define category labels and order
# Replace iv_codes and iv_order with codebook values when labels are needed.
# Example: iv_codes <- c("1", "2", "3"); iv_order <- c("Class ABC", "Class D", "Class E")
iv_raw <- as.character(dat[[iv_column]])
dv_raw <- as.character(dat[[dv_column]])
iv_raw[!is.na(iv_raw) & trimws(iv_raw) == ""] <- NA_character_
dv_raw[!is.na(dv_raw) & trimws(dv_raw) == ""] <- NA_character_
iv_codes <- unique(iv_raw[!is.na(iv_raw)])
iv_order <- iv_codes
dv_codes <- if (use_participant_type) participant_levels else unique(dv_raw[!is.na(dv_raw)])
dv_order <- dv_codes
if (analysis_mode == "questions" && question_categories == "reclassified") {
  iv_raw <- ifelse(consumes, "Consume", "Do not consume")
  dv_raw <- ifelse(understands, "Understand", "Do not understand")
  iv_codes <- iv_order <- c("Consume", "Do not consume")
  dv_codes <- dv_order <- c("Understand", "Do not understand")
}
dat$iv <- factor(iv_raw, levels = iv_codes, labels = iv_order)
dat$dv <- factor(dv_raw, levels = dv_codes, labels = dv_order)

# Sum survey weights within each cross-tabulation cell
# Column percentages total 100 per column; total percentages total 100 across the table.
# Blank groups, unclassified responses, and missing weights are excluded.
# Apply the same exclusions and filters in the spreadsheet when comparing results.
dat$weight <- dat[[weight_column]]
if (!is.numeric(dat$weight) || any(!is.na(dat$weight) &
                                   (!is.finite(dat$weight) | dat$weight < 0))) {
  stop("Weights must be numeric, finite, and nonnegative, or missing.")
}
keep <- complete.cases(dat[c("iv", "dv", "weight")])
message("Excluded observations: ", sum(!keep))
analysis_data <- dat[keep, ]
if (!nrow(analysis_data)) stop("No complete, classifiable observations are available.")
weighted_cells <- xtabs(weight ~ iv + dv, data = analysis_data)
tab <- as.data.frame(weighted_cells)
names(tab)[3] <- "weighted_total"
# Unweighted counts describe sample size; percentages use weighted totals.
tab$n <- as.vector(table(analysis_data$iv, analysis_data$dv))
tab$group_n <- ave(tab$n, tab$iv, FUN = sum)
tab$group_weight <- ave(tab$weighted_total, tab$iv, FUN = sum)
tab <- tab[tab$group_weight > 0, ]
if (!nrow(tab)) stop("No group has a positive total weight.")
iv_order <- iv_order[iv_order %in% as.character(tab$iv)]
tab$iv <- factor(tab$iv, levels = iv_order)
denominator <- if (percentage_base == "column") tab$group_weight else sum(tab$weighted_total)
tab$pct <- 100 * tab$weighted_total / denominator
tab$label <- sprintf("%.1f", tab$pct)

# Display percentages with the DV in rows and IV in columns.
pivot_percent <- xtabs(pct ~ dv + iv, data = tab)
print(round(pivot_percent, 2))

# Figure labels
# Edit these labels or replace plot_title with your preferred wording.
group_label <- switch(iv_column,
                      Income_class = "income classes", Age_generation = "age generations",
                      Age_range = "age groups", Area = "areas", Voter_type = "voter types",
                      Sex = "sex categories", Sexual_orientation = "sexual orientation categories",
                      Rurban = "rural and urban residence categories", HH_OFW = "household OFW categories",
                      Partisanship = "partisanship categories", tolower(gsub("_", " ", iv_column))
)
outcome_label <- if (use_participant_type) "Participant types" else gsub("_", " ", dv_column)
plot_title <- paste(outcome_label, "across", group_label)
if (analysis_mode == "questions") {
  plot_title <- if (question_categories == "raw") {
    "Self-reported understanding across economic news exposure frequencies"
  } else "Self-reported understanding across economic news consumption categories"
}
percent_label <- if (percentage_base == "column") {
  "Weighted percent within column"
} else "Weighted percent of included sample"

# Figure formatting
wrap <- function(x) vapply(x, function(s) paste(strwrap(s, 24), collapse = "\n"), "")
fills <- setNames(gray.colors(length(dv_order), start = .18, end = .9), dv_order)
base_theme <- theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 11),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 6)),
    strip.background = element_rect(fill = "grey95")
  )
source_note <- paste(
  "Source: Derived calculations using survey dataset.", sep = "\n"
)

# Stacked bar chart
# Small percentages appear beside each bar with guide lines.
# Midpoints use the full stack so moving labels does not change their positions.
stack_labels <- tab[order(tab$iv, tab$dv), ]
stack_labels$x <- as.integer(stack_labels$iv)
stack_labels$mid <- ave(stack_labels$pct, stack_labels$iv, FUN = cumsum) - stack_labels$pct / 2
small_label_cutoff <- 5
p_stacked <- ggplot(stack_labels, aes(x, pct, fill = dv)) +
  geom_col(width = .55, colour = "black", linewidth = .25,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = subset(stack_labels, pct >= small_label_cutoff),
            aes(y = mid, label = label,
                colour = as.integer(dv) <= ceiling(length(dv_order) / 2)),
            size = 11 / .pt, family = "Times New Roman") +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  scale_fill_manual(values = fills, labels = wrap) +
  scale_x_continuous(breaks = seq_along(iv_order), labels = wrap(iv_order),
                     limits = c(.5, length(iv_order) + .8)) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = c(.02, .03))) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title = plot_title, x = NULL,
       y = percent_label, caption = source_note) + base_theme

# Separate label layers keep each group beside its own bar.
for (group in iv_order) {
  small_labels <- subset(stack_labels, iv == group & pct < small_label_cutoff)
  p_stacked <- p_stacked + geom_text_repel(
    data = small_labels, aes(x = x + .275, y = mid, label = label),
    inherit.aes = FALSE, nudge_x = .12, direction = "y", hjust = 0,
    ylim = c(0, 100), box.padding = .3, point.padding = 0,
    min.segment.length = 0, segment.color = "grey40", segment.size = .3,
    max.overlaps = Inf, seed = 42, size = 11 / .pt, family = "Times New Roman"
  )
}

# Grouped bar chart
dodge <- position_dodge(width = .8)
p_grouped <- ggplot(tab, aes(iv, pct, fill = dv)) +
  scale_x_discrete(labels = wrap) +
  geom_col(position = dodge, width = .7, colour = "black", linewidth = .25) +
  geom_text(aes(label = label), position = dodge, vjust = -.4,
            size = 11 / .pt, family = "Times New Roman") +
  scale_fill_manual(values = fills, labels = wrap) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0, .03))) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title = plot_title, x = NULL,
       y = percent_label, caption = source_note) + base_theme

# Faceted dot plot
p_dots <- ggplot(tab, aes(pct, iv)) +
  geom_point(size = 2.5) +
  geom_text(
    aes(label = label, hjust = ifelse(pct > 90, 1, 0)), 
    nudge_x = ifelse(tab$pct > 90, -3, 3), size = 11 / .pt,
    family = "Times New Roman") +
  facet_wrap(~dv, ncol = 2, labeller = labeller(dv = wrap)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     expand = expansion(mult = .05)) +
  scale_y_discrete(limits = rev(iv_order), labels = wrap, expand = expansion(add = .65)) +
  labs(title = plot_title, y = NULL,
       x = percent_label, caption = source_note) + base_theme +
  theme(panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_blank())

# Connected dot plot
# Each row shows category percentages and their minimum-to-maximum range.
# The horizontal line is a descriptive range, not a confidence interval.
connected_data <- tab
connected_data$row <- match(as.character(connected_data$iv), rev(iv_order))
connected_data$offset <- 0
close_gap <- 8  # Percentage-point distance used to identify nearby markers.

# Stagger clusters of nearby markers within their own row.
for (row_index in seq_along(iv_order)) {
  indices <- which(connected_data$row == row_index)
  indices <- indices[order(connected_data$pct[indices])]
  clusters <- cumsum(c(TRUE, diff(connected_data$pct[indices]) >= close_gap))
  for (cluster in split(indices, clusters)) {
    if (length(cluster) > 1) {
      spread <- if (length(cluster) == 2) .2 else .33
      connected_data$offset[cluster] <- seq(spread, -spread, length.out = length(cluster))
    }
  }
}
connected_data$marker_y <- connected_data$row + connected_data$offset
connected_data$label_x <- connected_data$pct + ifelse(connected_data$pct > 90, -2.5, 2.5)
connected_ranges <- aggregate(pct ~ row, connected_data, range)
connected_ranges$lower <- connected_ranges$pct[, 1]
connected_ranges$upper <- connected_ranges$pct[, 2]
marker_shapes <- setNames(rep(c(21, 22, 24, 23, 25), length.out = length(dv_order)), dv_order)

p_connected <- ggplot(connected_data, aes(pct, marker_y)) +
  geom_segment(
    data = connected_ranges,
    aes(x = lower, xend = upper, y = row, yend = row),
    inherit.aes = FALSE, colour = "grey70", linewidth = .8
  ) +
  geom_segment(
    data = subset(connected_data, offset != 0),
    aes(x = pct, xend = pct, y = row, yend = marker_y),
    inherit.aes = FALSE, colour = "grey55", linewidth = .35
  ) +
  geom_point(aes(fill = dv, shape = dv), size = 3.2, colour = "black", stroke = .5) +
  geom_text(
    aes(x = label_x, label = label, hjust = ifelse(pct > 90, 1, 0)),
    size = 11 / .pt, family = "Times New Roman", show.legend = FALSE
  ) +
  scale_fill_manual(values = fills, breaks = dv_order, labels = wrap, name = NULL) +
  scale_shape_manual(values = marker_shapes, breaks = dv_order, labels = wrap, name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     expand = expansion(mult = .04)) +
  scale_y_continuous(breaks = seq_along(iv_order), labels = wrap(rev(iv_order)),
                     limits = c(.45, length(iv_order) + .55)) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE),
         shape = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title = plot_title, x = percent_label, y = NULL, caption = source_note) +
  base_theme +
  theme(panel.grid.major.x = element_line(colour = "grey90", linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 15, 10, 15))

# Preview in RStudio: run each print statement to view a plot.
print(p_stacked)
print(p_grouped)
print(p_dots)
print(p_connected)

# Export settings
# Edit output_dir to choose the export folder.
output_dir <- "export location"
file_prefix <- if (analysis_mode == "questions") {
  paste("survey_questions", question_categories, percentage_base, sep = "_")
} else paste(dv_column, iv_column, percentage_base, sep = "_")

figure_width <- max(6.5, length(iv_order) * 1.5)
figure_height <- 5

# Export figures
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
plots <- list(stacked = p_stacked, grouped = p_grouped, dots = p_dots,
              connected = p_connected)

for (nm in names(plots)) {
  filename <- paste0(file_prefix, "_", nm)
  
  #ggsave(
  #  file.path(output_dir, paste0(filename, ".pdf")),
  #  plot = plots[[nm]],
  #  width = figure_width, height = figure_height,
  #  units = "in", bg = "white"
  #)
  
  ggsave(
    file.path(output_dir, paste0(filename, ".png")),
    plot = plots[[nm]],
    width = figure_width, height = figure_height,
    units = "in", dpi = 600, bg = "white"
  )
}

# Export summary data
#write.csv(
#  tab,
#  file.path(output_dir, paste0(file_prefix, "_summary.csv")),
#  row.names = FALSE
#)