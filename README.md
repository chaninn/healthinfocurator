# HealthInfoCurator

To launch the application on your own computer, in an R environment, type the following:

```
library(shiny)
runGitHub("healthinfocurator","chaninn")
```
In a few moments, the application should launch from your web browser.

To use the application:

1. Upload your input file
2. Wait until you see a message in the Output text box
3. Click on the **Download** button to download the output ZIP file

In the *ZIP file* you should be able to find the following files:
- missing_data_removed.txt
- pca_variance.txt
- pca_loadings.txt
- pca_scores.txt
- rf_rule.txt
- decision_tree.pdf
