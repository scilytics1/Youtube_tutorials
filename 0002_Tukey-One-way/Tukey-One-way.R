##################################
# Part 1: One-Way ANOVA
##################################

# We'll be using the PlantGrowth dataset, which comes built-in with R.

data(PlantGrowth)
library(ggplot2)
library(doBy)

# information on Plant Growth
??PlantGrowth

# Exploratory Data Analysis:
head(PlantGrowth)

# Visualize the data using boxplots.
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()

# Check for ANOVA assumptions:
# Normality:
shapiro.test(PlantGrowth$weight)

# histogram
hist(PlantGrowth$weight, main="Plant weights distribution", xlab = "weight",  col="lightblue" )

# Homogeneity of variances:
bartlett.test(weight ~ group, data=PlantGrowth)

#Running ANOVA with aov:
result <- aov(weight ~ group , data=PlantGrowth)
summary(result)

# obtain model coefficients
PlantGrowth$group <- as.factor(PlantGrowth$group)
mod <- lm(weight~group, data=PlantGrowth)
mod

# same as summary(result)
anova(mod)

# Compute summary statistics by group
summary_stats <- summaryBy(weight ~ group, data = PlantGrowth, FUN = c(mean, median, sd))

# Print the summary statistics
print(summary_stats)

##############################################
# Part 2: Tukey Test for Multiple Comparisons
##############################################

# library for  Tukey test
library(multcomp)

# Rum Tukey on reuslt object
tukey_result <- glht(result, linfct = mcp(group = "Tukey"))
summary(tukey_result)

# Visualize Tukey Results
plot(tukey_result, main="Tukey's HSD - Plant Growth", xlab="Difference in Means")


