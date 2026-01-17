# doeapp
An interactive R Shiny app for designing and analyzing experiments, supporting CRD, RBD, factorial, split-plot, and confounded designs. It offers a simple web interface with no coding required, reproduces published results for validation, and enhances learning through guided analysis.
Guide for the app:
1. Data Upload
•	Format: Prepare your data in .csv or .xlsx (Excel) format.
•	Structure: Ensure your dataset is tidy (one observation per row, with columns for response variables and experimental factors).
Action: Use the "Upload Data" panel on the left sidebar to import your file.
2. Select Experimental Design
Choose the specific design that matches your experimental setup from the dropdown menu. The app supports:
Single Factor Designs: Completely Randomized Design (CRD), Randomized Complete Block Design (RCBD), Latin Square Design (LSD), and Graeco-Latin Square Design (GLSD).
Factorial Designs: Factorial CRD and Factorial RCBD (2 or 3 factors).
Specialized Designs: 2k Factorial and Split-Plot Designs.
3. Define Model Variables
Once the design is selected, the interface will dynamically update to request the relevant variables:
•	Response Variable: Select the numeric column representing your outcome.
•	Treatment Factors: Select the categorical columns for your main effects.
Blocking Factors: If using RCBD, LSD, or Split-Plot, select the columns representing blocks, rows, columns, or whole-plots.
4. Analysis & Diagnostics
The app automatically fits the General Linear Model (ANOVA) and generates the following tabs:
Results: Displays the ANOVA table and significance levels (p-values).
Visualization: Provides boxplots and interaction plots to visualize main effects and interactions.
•	Diagnostics: Automatically checks parametric assumptions using:
Plots: Residuals vs. Fitted, Normal Q-Q, and Ordered Residuals.
Formal Tests: Shapiro-Wilk (Normality), Breusch-Pagan (Homogeneity), and Durbin-Watson (Independence).
5. Remediation (If Assumptions Fail)
If diagnostic tests indicate violations (e.g., non-normality), you can apply remediation directly within the app:
Transformation: Apply Box-Cox, Log, Square Root, or Reciprocal transformations to the response variable and re-run the analysis.
Non-Parametric Analysis: Switch the analysis mode to perform non-parametric tests (e.g., Kruskal-Wallis) followed by appropriate post-hoc tests (e.g., Dunn’s Test).
6. Post-Hoc Analysis
If the ANOVA indicates significant effects, navigate to the post-Hoc tab to identify specific differences between groups.
Available Tests: Tukey’s HSD, Dunnett’s Test (vs. Control), Bonferroni, LSD, and Scheffé.
•	Output: The app generates comparison tables and confidence intervals for the selected method.

Note: You can also access the Help section in the app for better understanding on how to use the application. 
