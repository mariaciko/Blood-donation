## What variables in the dataset influence whether_they_donated_blood_in_March_2007?

The relevant [dataset](https://github.com/mariaciko/Blood-donation/blob/main/transfusion_copy.data) can be found in the UCI ML Repo at (https://archive.ics.uci.edu/dataset/176/blood+transfusion+service+center). The data comes from the donor database in the Blood Transfusion Service Center in Hsin-Chu City in Taiwan.
For quick access to the R script for analysis, follow [this](https://github.com/mariaciko/Blood-donation/blob/main/blood.R) link.

We first perfomed a MANOVA combining 3 of the 4 dependent variables (not all 4 due to errors) and visualized the data to look at relationships between variables. We then performed multiple regression across all categories and a single category to determine the best predictor of whether_they_donated_blood_in_March_2007. 

We then conducted an ANCOVA to see the effect of one variable on another while controlling for the composite variable, which we had previously defined as a meaningful combination of features.

## Other statistical tests with IRIS datasets
Sections #11 through #16, involve work with the Iris and extended/corrupted Iris datasets. Some of our analysis includes: 
  - ANOVa, linear regression
  - Chi-squared tests to check for association between variables
  - Ordinal Logistic Regression to predict purchase decision based on review ratings
  - PCA to check for those components driven by one particular measurement, scree plots
  - Factor analysis of latent variables on customer decision, heat maps
  - K-means clustering of the two components from the PCA
  - Evaluating different PDFs (i.e. normal, log-normal, exponential Gaussian Mixture Model (GMM) with two components) via hisotgrams with fitted models and comparing BIC values
