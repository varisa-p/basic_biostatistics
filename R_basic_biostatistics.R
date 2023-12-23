# Import necessery library

library('tidyverse')
library(tableone)
library(dplyr)
library(ggplot2)
library(gtsummary)

# Import data
data <-  read_csv('data.csv')

# Descriptive analysis
# Convert disease column as a factor
data$disease_f =factor(data$disease)
# Create table compare baseline charateristic based on disease or no disease
# This table can show both categorical and numberic variables 
# The categorical should be add in factorVars
CreateTableOne(vars = column_variable,
               strata = c(disease_f), data = data, 
               factorVars = c(variable1,variable2))

# Evaluate correlation between numeric variables
# Calculate correlation matrix
correlation_matrix <- cor(data1[, numeric_columns])

# Visualize correlation matrix using a heatmap with numeric values

# Convert correlation matrix to a data frame with row and column indices
correlation_df <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_df) <- c("Variable1", "Variable2", "Correlation")

# Plot heatmap with numeric values
ggplot(data = correlation_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
#Univariate Analysis
#Example for one variable
glm_variable1 <- glm(disease~variable1,family="binomial",data = data)
glm_variable1
summary(glm_variable1)
## odds ratios and 95% CI
estimates_variable1=data.frame(exp(cbind(OR = coef(glm_variable1), confint(glm_variable1))))
estimates_variable1

# Multivariare Analysis
# Select proper viable to add in can based on univarite result or knowledge
mv_reg <- glm(disease ~ variable1 + variable2 + variable2 , family = "binomial", data = data)

summary(mv_reg)
mv_tab <- tbl_regression(mv_reg, exponentiate = TRUE)
mv_tab