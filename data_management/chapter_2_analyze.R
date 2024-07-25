# Load required libraries
library(ggplot2)
library(stringr)  # For str_wrap

# Set working directory if necessary
setwd("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures")
theme_bw1 <- function(base_size = 18, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text.x = element_text(
        size = base_size, 
        color = "black",
        hjust = 0.5,
        vjust = 1
      ),  
      axis.text.y = element_text(
        size = base_size, 
        color = "black",
        hjust = 0.45, 
        vjust = 0.95,
        family = base_family
      ), 
      axis.title.y = element_text(
        size = base_size, 
        angle = 90,
        margin = margin(r = 25),
        family = base_family
      ), 
      axis.title.x = element_text(
        size = base_size,
        margin = margin(t = 25),
        family = base_family
      ),
      plot.title = element_text(
        size = base_size,
        face = "bold", 
        family = base_family
      ),
      axis.ticks = element_blank(), 
      legend.background = element_blank(), 
      legend.key = element_blank(),
      panel.background = element_blank(), 
      panel.border = element_blank(), 
      strip.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey87", size = 0.5)
    )
}
theme_set(theme_bw1())


# Define the base colors
base_colors <- c("darkseagreen4", "lightpink3", "navajowhite3", "sienna4", 
                 "steelblue", "coral", "goldenrod", "cadetblue", "tomato", "mediumorchid")

# Function to generate a color map based on unique terms
generate_color_map <- function(terms) {
    base_colors <- c("darkseagreen4", "lightpink3", "navajowhite3", "sienna4", 
                     "steelblue", "coral", "goldenrod", "cadetblue", "tomato", "mediumorchid")
    unique_terms <- unique(terms)
    if (length(unique_terms) > length(base_colors)) {
        stop("There are more terms than available colors. Please add more colors to the base_colors vector.")
    }
    colors <- setNames(base_colors[seq_along(unique_terms)], unique_terms)
    return(colors)
}


# Assuming bl_xp and lat_xp models are already created

extract_model_data <- function(model) {
    est_table <- summary(model)$coefficients
    ci_bounds <- confint(model, level = 0.95)
    df <- data.frame(
        Term = rownames(est_table),
        Estimate = est_table[, "Estimate"],
        `CI.lower` = ci_bounds[, 1],
        `CI.upper` = ci_bounds[, 2],
        stringsAsFactors = FALSE
    )
    return(df)
}

bl_xp_data <- extract_model_data(bl_xp)
lat_xp_data <- extract_model_data(lat_xp)


lat_xp_data$Term <- gsub("treat_cond", "", lat_xp_data$Term)
lat_xp_data$Term <- gsub("latino", "Latino", lat_xp_data$Term, ignore.case = TRUE)
lat_xp_data$Term <- gsub(":", ": ", lat_xp_data$Term)
lat_xp_data$Term <- gsub("_", " ", lat_xp_data$Term)
lat_xp_data <- lat_xp_data[lat_xp_data$Term != "(Intercept)", ]  # Remove the intercept

# Set the order with Latino at the top
lat_xp_data$Term <- factor(lat_xp_data$Term, levels = c("Latino: Cultural correction", "Latino: General correction", "Latino: No correction", "Cultural correction", "General correction", "No correction", "Latino"))

color_map_latino <- generate_color_map(lat_xp_data$Term)

plot_latino <- ggplot(lat_xp_data, aes(y = (Term), x = Estimate, xmin = `CI.lower`, xmax = `CI.upper`, color = Term)) +
    geom_point(size = 3) +  # Increase point size
    geom_errorbarh(height = 0.3, size = 1) +  # Increase error bar width and thickness
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +  # Dark grey dashed line at zero
    scale_color_manual(values = color_map_latino) +
    labs(title = str_wrap("", width = 60), 
         x = "Estimate", y = "Coefficient") +
    theme_bw1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Angle the x-axis text
          axis.text.y = element_text(angle = 0, hjust = 1), 
          legend.position = "none",
          plot.margin = margin(1, 1, 1, 1, "cm"))

plot_latino + xlim(-1, 1)

# Save the plot
ggsave(plot_latino + xlim(-1, 1), filename = "./chapter_2.1.1_plot.pdf", width = 15, height = 9, units = "in")



bl_xp_data$Term <- gsub("treat_cond", "", bl_xp_data$Term)
bl_xp_data$Term <- gsub("black", "Black", bl_xp_data$Term, ignore.case = TRUE)
bl_xp_data$Term <- gsub(":", ": ", bl_xp_data$Term)
bl_xp_data$Term <- gsub("_", " ", bl_xp_data$Term)
bl_xp_data <- bl_xp_data[bl_xp_data$Term != "(Intercept)", ]  # Remove the intercept

# Set the order with Black at the top
bl_xp_data$Term <- factor(bl_xp_data$Term, levels = c("Black: Cultural correction", "Black: General correction", "Black: No correction", "Cultural correction", "General correction", "No correction", "Black"))


color_map_black <- generate_color_map(bl_xp_data$Term)

# Generate the plot for Black participants
plot_black <- ggplot(bl_xp_data, aes(y = Term, x = Estimate, xmin = `CI.lower`, xmax = `CI.upper`, color = Term)) +
    geom_point(size = 3) +  # Increase point size
    geom_errorbarh(height = 0.3, size = 1) +  # Increase error bar width and thickness
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +  # Dark grey dashed line at zero
    scale_color_manual(values = color_map_black) +
    labs(title = str_wrap("", width = 60), 
         x = "Estimate", y = "Coefficient") +
    theme_bw1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Keep the x-axis text straight
          axis.text.y = element_text(angle = 0, hjust = 1),  # Slightly angle the y-axis text
          legend.position = "none",
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          plot.margin = margin(1, 1, 1, 1, "cm"))


plot_black + xlim(-1, 1)

# Save the plot
ggsave(plot_black + xlim(-1, 1), filename = "./chapter_2.1.2_plot.pdf", width = 15, height = 9, units = "in")


# Study 2

study2_data <- extract_model_data(f3)
# Define the proper term names

# Correct term labels based on the original names in study2_data
term_labels <- c(
  "treatmentBlack correction" = "Black correction",
  "treatmentWhite correction" = "White correction",
  "black" = "Black",
  "treatmentBlack correction:black" = "Black: Black correction",
  "treatmentWhite correction:black" = "Black: White correction"
)

# Apply the term labels to the study2_data
study2_data$Term <- sapply(study2_data$Term, function(x) term_labels[x])

# Check for any NA values introduced
print(study2_data)

# Remove the intercept and any NA values
study2_data <- study2_data[!is.na(study2_data$Term) & study2_data$Term != "(Intercept)", ]

# Set the order of the terms manually
study2_data$Term <- factor(study2_data$Term, levels = c(
  "Black: White correction", "Black: Black correction", "Black correction", "White correction", "Black"
))

generate_color_map <- function(terms) {
    base_colors <- c("darkseagreen4", "lightpink3", "navajowhite3", "sienna4", 
                     "steelblue", "coral", "goldenrod", "cadetblue", "tomato", "mediumorchid")
    unique_terms <- unique(terms)
    if (length(unique_terms) > length(base_colors)) {
        stop("There are more terms than available colors. Please add more colors to the base_colors vector.")
    }
    colors <- setNames(base_colors[seq_along(unique_terms)], unique_terms)
    return(colors)
}

color_map_study2 <- generate_color_map(study2_data$Term)
print(color_map_study2)

# Generate the plot for Study 2
plot_study2 <- ggplot(study2_data, aes(y = Term, x = Estimate, xmin = `CI.lower`, xmax = `CI.upper`, color = Term)) +
    geom_point(size = 3) +  # Increase point size
    geom_errorbarh(height = 0.3, size = 1) +  # Increase error bar width and thickness
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +  # Dark grey dashed line at zero
    scale_color_manual(values = color_map_study2) +
    labs(title = str_wrap("", width = 60), 
         x = "Estimate", y = "Coefficient") +
    theme_bw1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Keep the x-axis text straight
          axis.text.y = element_text(angle = 0, hjust = 1),  # Slightly angle the y-axis text
          legend.position = "none",
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          plot.margin = margin(1, 1, 1, 1, "cm"))

# Save the plot
ggsave(plot_study2 + xlim(-1, 1), filename = "./chapter2.2_plot.pdf")

plot_study2 + xlim(-1, 1)
