#1. IMPORTING THE DATASET AND VARIABLES PANORAMIC -------------------


data <- readRDS("data/cleaned/global_ads_performance_dataset_cleaned.rds")

View(data)

library(tidyverse)

# A simpple initial overview of the basic statistics about each variable
str(data) 
summary(data)

# 2. ANALYSIS OF THE VARIABLES --------------------------------------------------------

# 2.1 NUMERICAL VARIABLES DISTRIBUTION -----------------------------------------
# Goal: Visualize distribution shapes to detect skewness and outliers.

# Select only numeric metrics 
data |>
  select(impressions, clicks, ad_spend, revenue, conversions, CPC, CTR, ROAS)|>
  # Reshape data to 'long' format to plot all metrics at once using facets
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value")|>
  
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "white", alpha = 0.7) + # Histogram with 
  geom_density(color = "red", size = 1) + # density abline
  
  facet_wrap(~metric, scales = "free") + # separated panel for each metric
  
  theme_minimal() + # aesthetic choise 
  labs(title = "Distribution of Numeric Metrics", # descriptive labs
       subtitle = "Red line: density curve",
       x = "Value", y = "Density")


# COMMENT ON HISTOGRAMS (Revised Diagnosis):
# 1. Right-Skewed Distributions (Positively Skewed):
#    - 'Ad_Spend', 'Clicks', 'Revenue', 'Conversions', 'ROAS', and 'CPC'.
#    - These variables show a peak at lower values and a long tail extending to the right.
#    - This "Long Tail" behavior confirms the necessity of the Log-Transformation(see point 6)
#
# 2. Approximate Uniform Distribution:
#    - 'Impressions' is the only metric showing a relatively flat distribution across the range,
#      indicating the balanced presence of low, medium, and high visibility campaigns.
#
# 3. Normal Distribution:
#    - 'CTR' follows a classic symmetric Bell Curve centered around the mean.

# 2.2 CATEGORICAL VARIABLES FREQUENCY ------------------------------------------
# Goal: Check for class imbalance in categorical features.

# Select categorical dimensions
data |>
  select(platform, campaign_type, industry, country) |>
  # Reshape to long format for easier plotting
  pivot_longer(cols = everything(), names_to = "dimension", values_to = "category") |> 
  
  count(dimension, category) |>   # occurrences for each category
  ggplot(aes(x = reorder(category, n), y = n, fill = dimension)) +
  geom_col(show.legend = FALSE) + # bar chart
  
  facet_wrap(~dimension, scales = "free") + # Separate panels
  theme_minimal() +
  labs(title = "Frequency by Category", 
       x = "Category", y = "Count")

library(knitr)

#creating summary tables for the categories of the selected variables
summary_table <- data |>
  select(platform, campaign_type, industry, country) |>
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") |>
  
  count(Variable, Category) |>
  
  group_by(Variable) |>
  mutate(
    Percentage = n / sum(n) * 100,
    Percentage_Label = sprintf("%.1f%%", Percentage)
  ) |>
  ungroup() |>
  
  arrange(Variable, desc(n)) |>
  select(Variable, Category, Count = n, Percentage = Percentage_Label)

print(summary_table)
View(summary_table)

# COMMENT ON CATEGORICAL DISTRIBUTION:
# 1. Platform:
#    - Shows a slight imbalance with 'Google Ads' being the dominant channel 
#      and 'TikTok Ads' having the lowest frequency (25%).
#
# 2. Industry, Country and Campaign Type:
#    - Highly uniform distribution across all categories, almost perfecty balanced. 
#    - Absence of rare classes (e.g., no country with <200 obs, or industry with <300 obs).



library(reshape2)


#3. Multivariate Analysis --------------------------------------------


# 3.1 CORRELATION MATRIX -------------------------------------------------------
# Goal: Detect multicollinearity (redundant features) and identify strong predictors for ROAS.

# Select only numeric columns for correlation
num_data <- data |>
  select(impressions, clicks, ad_spend, revenue, conversions, CTR, CPC, ROAS)

cor_mat <- cor(num_data, method = "pearson") # corr matrix

melted_cor <- melt(cor_mat) # long format

ggplot(melted_cor, aes(x=Var1, y=Var2, fill=value)) + # Plot Heatmap
  geom_tile(color = "white") +
  
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + #correlation coefficients as text
  
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), name="Correlation") +
  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", 
       subtitle = "High correlation (> 0.8) indicates potential multicollinearity")

# NOTE ON THRESHOLD (0.8):
# A correlation > 0.8 implies that two variables share > 64% of their variance (corr^2 = R^2 > 0.64).
# This level of redundancy (Multicollinearity) causes instability in linear model coefficients
# and inflates standard errors (High VIF).

## IMPORTANT ##
# If r > 0.8, select only one feature for the model to avoid overfitting.


# 3.2 CATEGORICAL IMPACT ON PERFORMANCE (BOXPLOTS) -----------------------------
# Goal: Determine if Platform or Industry discriminates performance (ROAS).

# NOTE ON METRIC SELECTION FOR BOXPLOTS:
# We analyze ROAS and CPA (Efficiency Metrics) rather than Impressions or Clicks (Volume Metrics),
# because Volume metrics are biased by budget allocation (higher spend = more clicks).
# Efficiency metrics allow for a fair comparison of performance quality 
# across different platforms and industries, regardless of their size.


# A. ROAS Distribution by Platform
# filter out ROAS = 0 to focus on successful campaigns.
plot_platform <- data |>
  filter(ROAS > 0) |> 
  ggplot(aes(x = platform, y = ROAS, fill = platform)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(title = "ROAS by Platform", y = "ROAS") +
  theme(legend.position = "none") # No legend needed

# B. CPA Distribution by Industry
# filter out Conversions = 0 because CPA is infinite/undefined there.
plot_industry <- data |>
  filter(conversions > 0) |>
  ggplot(aes(x = industry, y = CPA, fill = industry)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "CPA (Cost per Acquisition) by Industry", y = "CPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate labels
  theme(legend.position = "none")


print(plot_platform)
print(plot_industry)

# COMMENTS:
# 1. ROAS by Platform:
#    - Clear distinction in performance: 'TikTok Ads' shows the highest median ROAS
#      and the widest interquartile range, indicating high potential returns but higher variance.
#    - 'Google Ads' appears to be the most conservative platform (lowest median, compacted box).
#
# 2. CPA by Industry:
#    - uniform distribution across all industries.
#    - Medians and variability are nearly identical.




# 3.3 MULTIVARIATE RELATIONSHIPS (SCATTERPLOTS) --------------------------------
# Goal: Visualize the direct relationship between Spend and Revenue across segments.

data |>
  filter(revenue > 0) |> # for a cleaner plot
  
  ggplot(aes(x = ad_spend, y = revenue, color = campaign_type)) +
  geom_point(alpha = 0.6, size = 2) + # points
  
  geom_smooth(method = "lm", se = FALSE) + #linear regression for each group
  
  theme_minimal() +
  labs(title = "Ad Spend vs. Revenue by Campaign Type",
       subtitle = "Slope of the line represents efficiency (Steeper = Better ROAS)",
       x = "Ad Spend",
       y = "Revenue")

# COMMENT ON MULTIVARIATE ANALYSIS (Spend vs Revenue):
# 1. Linearity:
#    - Strong positive linear relationship between Ad Spend and Revenue across all segments.
#
# 2. Efficiency by Campaign Type (Slope Analysis):
#    - The slopes (indicating ROAS efficiency) are relatively similar across types.
#    - 'Display' (Red) and 'Search' (Green) appear slightly more efficient (steeper slope)
#      compared to 'Video' and 'Shopping'.
#    - No distinct clusters are immediately visible since he data points overlap significantly, forming a single "cone",
#      implying that segmentation might rely on more complex combinations of variables rather than just Campaign Type alone.
#    - Since the points seems to overlap and the great part is about low values, could be visually usefull to scale the variables.





  theme(legend.position = "none") # Legend is redundant with x-axis labels

# 5. OUTLIERS DETECTION ------------------------------------------

# 5.1 OUTLIER DETECTION (Boxplots) ---------------------------------------------
# Goal: Identify extreme values in cost metrics that could skew models.

# NOTE: We focus on efficiency metrics (CPC, CPM) for outlier detection 
# because extreme values here often indicate technical errors, inefficiencies, frauds.
# We skip volume metrics (Revenue, Spend) as high values usually represent "Top Performers" not anomalies to be removed.


# Calculate CPM (Cost per Mille Impressions)
data2 <- data |>
  mutate(CPM = (ad_spend / impressions) * 1000)

# Visualize Distribution of Cost Metrics
data2 |>
  select(CPC, CPM) |>
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value") |>
  
  ggplot(aes(x = metric, y = value, fill = metric)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  
  theme_minimal() +
  facet_wrap(~metric, scales = "free") +
  labs(title = "Outlier Detection in Cost Metrics",
       subtitle = "Red stars indicate statistical outliers (1.5 * IQR)",
       y = "Value") +
  theme(legend.position = "none")

# COMMENT ON OUTLIERS (Visual Inspection):
# 1. Presence of High-Cost Outliers:
#    - Both CPC and CPM (especially CPM) exhibit a heavy tail distribution with significant outliers 
#      on the upper end (indicated by red stars), in fact CPM shows values reaching > 200 while the median is ~50.
#
# IMPORTANT!
#    - Apply log-transformation or scale to mitigate outlier's impact.



# 5.2 ZERO-REVENUE ANALYSIS (Budget Waste) -------------------------------------
# Goal: Quantify how many campaigns spent money but generated ZERO revenue.

waste_check <- data |>
  mutate(performance_status = ifelse(revenue == 0, "Zero Revenue (Waste)", "Profitable")) |>
  count(performance_status) |>
  mutate(percentage = n / sum(n) * 100)

print(waste_check)

# Result: 100% of campaigns are Profitable, no failed campaigns.

# 6.==============================================================================
# LOG-TRANSFORMATION CHECK
# log transformation is needed for skewed variables or at least scaling.

library(gridExtra) # to arrange plots side-by-side


# 6.1. AD SPEND COMPARISON (Handling Skewness) -----------------------------------
# Plot A: Original (Skewed)
p1_orig <- ggplot(data, aes(x = ad_spend)) + 
  geom_histogram(fill = "#e74c3c", bins = 30, color = "white", alpha = 0.8) +
  labs(title = "Original Ad Spend", 
       subtitle = "Problem: Right Skewed (Heavy Tail)",
       x = "Ad Spend ($)") +
  theme_minimal()

# Plot B: Log-Transformed (Normalized)
p1_log <- ggplot(data, aes(x = log1p(ad_spend))) + 
  geom_histogram(fill = "#2ecc71", bins = 30, color = "white", alpha = 0.8) +
  labs(title = "Log-Transformed Ad Spend", 
       subtitle = "Solution: Normal-like Distribution",
       x = "Log(Ad Spend + 1)") +
  theme_minimal()

# 6.2. CPC COMPARISON (Handling Outliers) ----------------------------------------
# Plot C: Original (Outliers)
p2_orig <- ggplot(data, aes(x = CPC)) + 
  geom_histogram(fill = "#e74c3c", bins = 30, color = "white", alpha = 0.8) +
  labs(title = "Original CPC", 
       subtitle = "Problem: Extreme Outliers",
       x = "CPC ($)") +
  theme_minimal()

# Plot D: Log-Transformed (Compressed)
p2_log <- ggplot(data, aes(x = log1p(CPC))) + 
  geom_histogram(fill = "#2ecc71", bins = 30, color = "white", alpha = 0.8) +
  labs(title = "Log-Transformed CPC", 
       subtitle = "Solution: Outliers Compressed",
       x = "Log(CPC + 1)") +
  theme_minimal()

grid.arrange(p1_orig, p1_log, p2_orig, p2_log, ncol = 2)

# FINAL CHECK: TRANSFORMATION VALIDATION
# The plots confirm the effectiveness of the Log-Transformation:
# 1. Ad Spend (Top Row): The distribution shifts from all left (plot on the left) to a near-perfect Gaussian curve (Right).
# 2. CPC (Bottom Row): The transformation successfully compresses the variance, 
#    pulling extreme outliers (Right Tail) closer to the center, stabilizing the data for Clustering. (in this case it's possible simply to scale the variable)




