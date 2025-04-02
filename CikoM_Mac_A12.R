# 1) Header---------------------------------------------------------------------
# MariaCiko_Unit1Part2_BIOL672
# MacOS Sonoma 14.6.1
# Libraries/Packages: ggplot2, grid, tidyverse, datasets, dplyr, MASS, mixtools
# Data files: transfusion_copy.data, iris_tab_bgnoise.txt, iris_purchase.txt, iris_csv_purchase.csv

library('ggplot2')
library('grid')
library('tidyverse')
library('datasets') #for built-in datasets like iris
library('dplyr')

#7) Dataset---------------------------------------------------------------------
# About the dataset: donor database of Blood Transfusion Service Center 
# in Hsin-Chu City in Taiwan. R (Recency - months since last donation), 
# F (Frequency - total number of donation), M (Monetary - total blood donated 
# in c.c.), T (Time - months since first donation), and a binary variable 
# representing whether they donated blood in March 2007 (1 stand for 
# donating blood; 0 stands for not donating blood) 
df <- read.table("transfusion_copy.data", sep = ",", header = TRUE)
str(df)
# cor(df[, c("Recency_months", "Frequency_times", "Monetary_ccblood", "Time_months")])


#8) MANOVA----------------------------------------------------------------------
# Perform MANOVA using cbind to combine the dependent variables
# NOTE: Only 3 of the 4 dependent variables were used because Monetary_ccblood 
# always gave error "residuals have rank 2 < 3"
fit <- manova(cbind(Recency_months, Frequency_times, Time_months) ~
                        whether_they_donated_blood_in_March_2007, data = df)
manova_summary <- summary(fit, test = "Pillai")  # Pillai's trace is a common test statistic for MANOVA
print(manova_summary)

# Interpretation of the MANOVA results
p_value <- manova_summary$stats[1, "Pr(>F)"]  # Extract p-value from summary output
if (p_value < 0.05) {
  cat("There is a statistically significant difference in the combined dependent variables across the groups.\n")
} else {
  cat("There is no statistically significant difference in the combined dependent variables across the groups.\n")
}

# Save the MANOVA results to a file (optional)
sink("manova_results.txt")
print(manova_summary)
sink()

# Visualize some of the data to look at relationships between variables
myOptionalPlot <- ggplot(df, aes(x = Recency_months, y = Frequency_times, color = as.factor(whether_they_donated_blood_in_March_2007))) +
  geom_point() +
  labs(title = "Recency vs Frequency by Blood Donation Status",
       x = "Recency (months)",
       y = "Frequency (times)",
       color = "Donation Status")
print(myOptionalPlot)


#9) Multiple regression---------------------------------------------------------
# Multiple regression across all categories
multiple_regression <- lm(whether_they_donated_blood_in_March_2007 ~ 
                            Recency_months + Frequency_times + Time_months, 
                          data = df)
summary_all <- summary(multiple_regression) # summary of the model

# Interpretation: Extracting the best predictor within the category
best_predictor_category <- 
  summary_all$coefficients[which.min(summary_all$coefficients[, "Pr(>|t|)"]), "Pr(>|t|)"]
best_predictor_name_category <- 
  rownames(summary_all$coefficients)[which.min(summary_all$coefficients[-1,4])]

cat("The best predictor of 'whether they donated blood in March 2007' is:", best_predictor_name_category, "\n")
cat("P-value for the best predictor is:", best_predictor_category, "\n")

# Save the regression results for the category to a file
sink("multiple_regression_results.txt")
print(summary_all)
cat("The best predictor of 'whether they donated blood in March 2007' is:", best_predictor_name_category, "\n")
sink()

# Conduct same regression but within 1 category
# Let's look within the category 'whether_they_donated_blood_in_March_2007' == 1
category_subset <- df |>
  filter(whether_they_donated_blood_in_March_2007 == 1)
multiple_regression_category <- lm(whether_they_donated_blood_in_March_2007 ~ 
                                     Recency_months + Frequency_times + Time_months, 
                                   data = category_subset)
summary_category <- summary(multiple_regression_category)
print(summary_category)

# Determine the best predictor within the category
best_predictor_category <- rownames(summary_category$coefficients)[which.min(summary_category$coefficients[-1, 4])]
cat("The best predictor of 'whether_they_donated_blood_in_March_2007' within the category is:", best_predictor_category, "\n")

# Save category regression results to a file
sink("category_results.txt")
print(summary_category)
cat("The best predictor of 'whether_they_donated_blood_in_March_2007' within the category is:", best_predictor_category, "\n")
sink()


#10) Composite variable and ANCOVA----------------------------------------------
# Define a composite variable that combines features in a meaningful way
# Multiplying Frequency by Recency allows us to emphasize donors who donate
# frequently but are less recent
# Dividing by Monetary adjusts the composite variable to account for the total 
# amount of blood donated (donors who have contributed significantly are 
# proportionally represented). Adding 1 prevents division by zero if a donor hasn't donated
CompositeVar <- (df$Frequency_times * (12-df$Recency_months)) / (df$Monetary_ccblood+1)
print(summary(CompositeVar))

# Conduct ANCOVA to see the effect of one variable on another while controlling for CompositeVar
# Let's say we want to see how 'whether_they_donated_blood_in_March_2007' is 
# predicted by 'Recency_months', controlling for 'CompositeVar'
ancova_model <- aov(whether_they_donated_blood_in_March_2007 ~ Recency_months + 
                      CompositeVar, data = df)
ancova_summary <- summary(ancova_model)
print(ancova_summary)

# Interpretation of the ANCOVA results
p_value_recency <- ancova_summary[[1]]["Recency_months", "Pr(>F)"]
if (p_value_recency < 0.05) {
  cat("There is a statistically significant effect of Recency_months on
      whether_they_donated_blood_in_March_2007 when controlling for
      CompositeVar (p =", round(p_value_recency, 4), ").\n")
} else {
  cat("There is no statistically significant effect of Recency_months on
      whether_they_donated_blood_in_March_2007 when controlling for
      CompositeVar (p =", round(p_value_recency, 4), ").\n")
}

# Save the ANCOVA results to a file
sink("ancova_results.txt")
print(ancova_summary)
if (p_value_recency < 0.05) {
  cat("There is a statistically significant effect of Recency_months on
      whether_they_donated_blood_in_March_2007 when controlling for
      CompositeVar (p =", round(p_value_recency, 4), ").\n")
} else {
  cat("There is no statistically significant effect of Recency_months on
      whether_they_donated_blood_in_March_2007 when controlling for
      CompositeVar (p =", round(p_value_recency, 4), ").\n")
}
sink()


#11) Iris-----------------------------------------------------------------------
# Choose a favorite hypothesis test or statistical method and compare it on the
# original iris data set vs three or the corrupted iris data sets
df_orig <- iris
# glimpse(df_orig)

df_corrupted <- read_table("iris_tab_bgnoise.txt")
# str(df_corrupted)

# Linear regression on original dataset
lm_orig <- lm(Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width,
              data = df_orig)

# Linear regression on corrupted dataset
lm_corrupted <- lm(petal_length ~ sepal_length + sepal_width + petal_width, 
                   data = df_corrupted)

# Compare R-squared values
r_squared_orig <- summary(lm_orig)$r.squared
r_squared_corrupted <- summary(lm_corrupted)$r.squared

# Compare coefficients
coefficients_orig <- summary(lm_orig)$coefficients
coefficients_corrupted <- summary(lm_corrupted)$coefficients

# ANOVA on both datasets
anova_orig <- anova(lm_orig)
anova_species_orig <- aov(Petal.Length ~ Species, data = df_orig)
anova_corrupted <- anova(lm_corrupted)
anova_species_corrupted <- aov(petal_length ~ species, data = df_corrupted)

# Save results to respective files
sink("iris_comparison_results.txt")
cat("R-squared (Original):", r_squared_orig, "\n")
cat("R-squared (Corrupted):", r_squared_corrupted, "\n")
if (r_squared_corrupted < r_squared_orig) {
  cat("The R-squared value is lower in the corrupted data, indicating that 
      the model's fit is reduced due to noise in the corrupted data.\n")
} else {
  cat("The R-squared value is similar or higher in the corrupted data, 
      suggesting that noise did not significantly impact the model's fit.\n")
}
sink()

sink("iris_coefficients.txt")
print("Coefficients - Original Data") #`cat` there'll be misalignment of column names
print(coefficients_orig)
cat("\n")
print("Coefficients - Corrupted Data")
print(coefficients_corrupted)
sink()

sink("iris_anova_results.txt")
cat("ANOVA (Original Data)\n")
print(summary(anova_species_orig))
anova_f_orig <- summary(anova_species_orig)[[1]][1, "F value"]
cat("F-statistic (Original Data):", anova_f_orig, "\n")
cat("\nANOVA (Corrupted Data)\n")
print(summary(anova_species_corrupted))
anova_f_corrupted <- summary(anova_species_corrupted)[[1]][1, "F value"]
cat("F-statistic (Corrupted Data):", anova_f_corrupted, "\n")
if (anova_f_corrupted < anova_f_orig) {
  cat("\nThe F-statistic is lower in the corrupted data, meaning that the group 
      differences are harder to detect due to the added noise.")
} else {
  cat("/nThe F-statistic is higher in the corrupted data, suggesting that noise 
      did not significantly affect the ability to detect group differences.")
}
sink()


#12) Extended Iris--------------------------------------------------------------
iris_purchase <- as_tibble(read.table("iris_purchase.txt")) |>
  dplyr::select(V6, V7, V10, V11, V13) |> # other packages mask select function
  # mutate(c(across(V6, V7, V10, V11, V13), as_factor)) |>
  print()

# Check for missing values
cat("missing values:", sum(is.na(iris_purchase)))

# Summarize categorical variables
species_table <- table(iris_purchase$V6)
color_table <- table(iris_purchase$V7)
sold_table <- table(iris_purchase$V10)
review_table <- table(iris_purchase$V11)
date_table <- table(iris_purchase$V13)

print(species_table)
print(color_table)
print(sold_table)
print(review_table)
print(date_table)


sink("chi_squared_species_sold.txt")
# Create contingency table between 'species' and 'sold'
species_sold_table <- table(iris_purchase$V6, iris_purchase$V10)
print(species_sold_table)
# Chi-squared test to check for association between 'species' and 'sold'
chisq_species_sold <- chisq.test(species_sold_table)
print(chisq_species_sold)
cat("Since the p-value is smaller than 0.05 there exists a statistically 
    significant relationship between species and sales.")
sink()

sink("chi_squared_species_color.txt")
# Create contingency table between 'species' and 'color'
species_color_table <- table(iris_purchase$V6, iris_purchase$V7)
print(species_color_table)
# Chi-Squared test for association between 'species' and 'color'
chisq_species_color <- chisq.test(species_color_table)
print(chisq_species_color)
cat("Since the p-value is smaller than 0.05 there exists a statistically 
    significant relationship between species and color")
sink()

# Ordinal Logistic Regression to predict purchase decision based on review ratings
library('MASS')
ordinal_reg_model <- polr(as.factor(V10) ~ as.factor(V11), 
                          data = iris_purchase, Hess=TRUE)
sink("ordinal_review_logreg.txt")
print(summary(ordinal_reg_model))
# Extract p-values
coefs <- summary(ordinal_reg_model)
pvals <- pnorm(abs(coefs$coefficients[, "t value"]), lower.tail=FALSE) * 2
print(pvals)
sink()

iris_purchase <- iris_purchase |>
  mutate(V13 = as.Date(V13, format = "%m/%d/%Y")) # format in file currently

# Plot purchases over time
purchase_Plot <- ggplot(iris_purchase, aes(x=V13)) + 
  geom_histogram(binwidth=30, position="dodge", fill="navy") +
  labs(title="Purchases Over Time", x="Date", y="Count of Purchases") +
  theme_minimal()
print(purchase_Plot)


#13) Principal Component Analysis-----------------------------------------------
numeric_iris <- read_csv("iris_csv_purchase.csv") |>
  dplyr::select(sepal_length:petal_width, attractiveness, likelytobuy, review) |>
  print()
anyNA(numeric_iris)
sum(is.nan(as.matrix(numeric_iris)))
sum(is.infinite(as.matrix(numeric_iris)))

# See how different variables contribute to variance
sink("pca.txt")
pca_result <- princomp(~ ., data = numeric_iris, cor = TRUE)  # Scale data with correlation matrix
cat("How much each original variable contributes to each Principal Component")
loadings_matrix = loadings(pca_result)
print(loadings_matrix)

positive_pc <- apply(loadings_matrix, 2, function(x) all(x > 0))
cat("\n PC that best describes overall size variation (all positive loadings):\n")
# print(which(positive_pc))
if (any(positive_pc)) {
  print(which(positive_pc))
} else {
  print("None found")
}
negative_pc <- apply(loadings_matrix, 2, function(x) all(x < 0))
cat("\n PC that best describes overall size variation (all negative loadings):\n")
if (any(negative_pc)) {
  print(which(negative_pc))
} else {
  print("None found")
}
# Check for PCs driven by one particular measurement (loadings close to ±1)
pos_threshold <- 0.7  # Define threshold for strong influence
neg_threshold <- -0.7
pos_influence_pc <- which(loadings_matrix > pos_threshold, arr.ind = TRUE)
neg_influence_pc <- which(loadings_matrix < neg_threshold, arr.ind = TRUE)

cat("\nPCs driven strongly by one particular measurement (loadings close to 1):\n")
print(pos_influence_pc)
cat("The 5th and 6th components are strongly positively influenced by sepal width and length.")
cat("\nPCs driven strongly by one particular measurement (loadings close to -1):\n")
print(neg_influence_pc)
cat("The 2nd, 3rd, 4th, and 7th components are strongly negatively influenced by 
    likeliness to buy, the review rating, attractiveness, and petal length, respectively.")

cat("\n")
cat("\n Summary: Amount of Variance explained by each PC \n")
summary(pca_result)
cat("\nProportion of Variance explained by each PC:\n")
print(summary(pca_result)$sdev^2 / sum(summary(pca_result)$sdev^2))
cat("The 1st PC contributes to the most variance in the dataset. This agrees with the scree plot.")

sink()

screeplot(pca_result, type = "lines", main = "Scree Plot of Principal Components")
cat("From the scree plot, we see that the variance explained starts to flatten 
    at Component 2, indicating that adding more components than 1 doesn't 
    significantly contribute to explaining the variance. Thus, dimensionality 
    reduction to 1 or 2 components seems effective.")


# 14) Factor analysis-----------------------------------------------------------
# Latent variables that influence customer decisions
glimpse(numeric_iris)
sink("factor_analysis.txt")
factanal_result <- factanal(numeric_iris, factors=3)
print(factanal_result)
cat("\n Sepal length, petal length, and petal width are strongly explained by factor 1.
    Sepal width is well explained by factor 2.\n
    I chose 3 factors because this is what gave me a p-value > 0.05, meaning 
    the model fits the data well or that there is not enough evidence to reject 
    the null hypothesis that the model fits the data well. 
    4 factors were too many for 7 variables and 2 factors had an insanely high 
    chi squared statistic as well as a small p-value (bad fit).
    Characteristics that cluster together may indicate underlying biological 
    relationships. For example, petal dimensions might reflect floral morphology.
    Traits close together along a significant factor axis suggest positive 
    relationships, indicating that they tend to increase or decrease together. 
    Conversely, traits that are far apart suggest a negative relationship.")
sink()

# Extract the factor loadings
loadings <- as.data.frame(factanal_result$loadings)

# Add row names as a new column
loadings$Variable <- rownames(loadings)

# Check the unique variable names in the loadings_long data frame
unique_variables <- unique(loadings_long$Variable)
print(unique_variables)

# Check for leading or trailing spaces in variable names
trimmed_variables <- trimws(unique_variables)  # Trim whitespace
print(trimmed_variables)

# Compare original and trimmed variables
identical(unique_variables, trimmed_variables)

# Convert the Variable names to numeric and back to factor for correct ordering
loadings_long$Variable <- as.numeric(as.character(loadings_long$Variable))
loadings_long$Variable <- factor(loadings_long$Variable, 
                                 levels = sort(unique(loadings_long$Variable)))

# Now, re-arrange the data frame to reflect this order
loadings_long <- loadings_long |>
  arrange(Variable)

# Create the heatmap
ggplot(loadings_long, aes(x = Factor, y = Variable, fill = Loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name="Factor Loading") +
  theme_minimal() +
  labs(title = "Factor Loadings Heatmap",
       x = "Factors",
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("The heat map illustrates the varying strengths of loadings, with different
    colors that equate to positive (red) and negative relationships (blue).
    The reason there are so many variables could be that our transformation of
    the data created additional variables.")


#15) k-means clustering---------------------------------------------------------
# Get the scores for PC1 and PC2
scores <- data.frame(pca_result$scores)

# Create a scatter plot of PC1 vs. PC2. Let's look at 'attractiveness'
ggplot(scores, aes(x = Comp.1, y = Comp.2, color = numeric_iris$attractiveness)) +
  geom_point() +
  labs(title = "Scatter Plot of PC1 vs. PC2", x = "Principal Component 1", 
       y = "Principal Component 2") +
  theme_minimal()

# Run K-means clustering
set.seed(42)
k <- 3 # saw 3 clusters of points
kmeans_result <- kmeans(scores[, 1:2], centers = k)

# Create a plot colored by cluster membership
scores$cluster <- as.factor(kmeans_result$cluster)  # Add cluster membership to the scores data frame

ggplot(scores, aes(x = Comp.1, y = Comp.2, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering Results", x = "Principal Component 1", 
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")


#16) PDFs and latent variability------------------------------------------------
library('MASS')    # For fitting normal and log-normal distributions
library('mixtools') # For GMM
library('ggplot2')  # For plotting
library('grid')     # For arranging multiple plots

# Assuming numeric_iris is loaded as you mentioned earlier
SZ <- (numeric_iris$sepal_length + numeric_iris$sepal_width + 
       numeric_iris$petal_length + numeric_iris$petal_width) / 4

# 1. Fit normal distribution
fitNORM <- fitdistr(SZ, densfun="normal")
print(fitNORM)

# 2. Fit log-normal distribution
fitLNORM <- fitdistr(SZ, densfun="log-normal")
print(fitLNORM)

# 3. Fit exponential distribution
myChoice <- fitdistr(SZ, densfun="exponential")
print(myChoice)

# 4. Fit Gaussian Mixture Model (GMM) with two components
fitGMM <- normalmixEM(SZ)  # default is 2 Gaussian components
print(fitGMM)

# Compute BIC for GMM
fitGMM_loglik <- fitGMM$loglik
BIC_GMM <- -2 * fitGMM_loglik + 4 * log(length(SZ))  # 4 parameters: 2 means + 2 sds
print(paste("BIC for GMM: ", BIC_GMM))

# Compare BIC values of all models
BICfit <- BIC(fitNORM, fitLNORM, myChoice)  # Compare normal, log-normal, exponential
print(BICfit)
print(paste("BIC for GMM: ", BIC_GMM))  # Include GMM manually since BIC doesn't handle mixtools directly

# Plotting histograms with fitted models
fitNORM_plot1 <- ggplot(data.frame(SZ), aes(x = SZ)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "grey") +
  stat_function(fun = dnorm, color = "red", args = list(mean = fitNORM$estimate[1], sd = fitNORM$estimate[2])) +
  ggtitle("Normal Distribution Fit")

fitLNORM_plot2 <- ggplot(data.frame(SZ), aes(x = SZ)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "grey") +
  stat_function(fun = dlnorm, color = "blue", args = list(meanlog = fitLNORM$estimate[1], sdlog = fitLNORM$estimate[2])) +
  ggtitle("Log-normal Distribution Fit")

myChoice_plot3 <- ggplot(data.frame(SZ), aes(x = SZ)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "grey") +
  stat_function(fun = dexp, color = "green", args = list(rate = myChoice$estimate[1])) +
  ggtitle("Exponential Distribution Fit")

# For GMM with two components
fitGMM_plot4 <- ggplot(data.frame(SZ), aes(x = SZ)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "grey") +
  stat_function(fun = dnorm, color = "red", args = list(mean = fitGMM$mu[1], sd = fitGMM$sigma[1])) +
  stat_function(fun = dnorm, color = "blue", args = list(mean = fitGMM$mu[2], sd = fitGMM$sigma[2])) +
  ggtitle("GMM Fit")

# Arrange plots in a 2x2 grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(fitNORM_plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(fitLNORM_plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myChoice_plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(fitGMM_plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# Verbal interpretation
if (BIC_GMM < min(BICfit)) {
  cat("GMM has the best fit according to BIC, suggesting underlying latent variability.")
} else {
  cat("The GMM did not perform better than the other models, indicating no strong evidence of latent variability.")
}

# Save results
sink("pdfs.txt")
print(fitGMM)
print(BICfit)
print(paste("BIC for GMM: ", BIC_GMM))
sink()
