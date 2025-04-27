## PREPARING PACKAGES

# installing packages
install.packages(c("ggplot2", "tidyverse", "ggpubr", "rstatix", "dplyr"))

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)

install.packages("car")
library(car)

## READING DATA

# loading the statistics experiment's results (from .csv)
stats <- read.csv("Stats.csv", header=T)

#set as experiment type as factor
stats$Experiment.Type=as.factor(stats$Experiment.Type)

# visually inspecting data
names(stats)
stats

## EXPLORATORY DATA ANALYSIS

#check missing values
colSums(is.na(stats))

# group the data by Experiment.Type and calculate the mean, median, and standard deviation
summary_stats <- stats %>%
  group_by(Experiment.Type) %>%
  summarise(mean = mean(Average.Recall),
            median = median(Average.Recall),
            std_dev = sd(Average.Recall))

# view the summary statistics
print(summary_stats)

# distribution
ggplot(stats, aes(x = Average.Recall)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(~ Experiment.Type, scales = "free") +
  labs(title = "Histogram of Average Recalls by Experiment Type", x = "Average Recall", y = "Frequency")

#see scatterplot by experiment type
colors <- c("lightblue", "red", "green")

plot(stats$Participant, stats$Average.Recall,
     xlab="Participant", ylab="Average Recall",
     main="Scatterplot of Average Recall",
     col=colors[stats$Experiment.Type],
     pch=19)

legend("topright", legend = levels(stats$Experiment.Type), fill = colors, title = "Experiment Type")

## ASSUMPTION CHECKS

#set factor levels and labels
levels(stats$Experiment.Type) <- c("Lyrical", "No Music", "Instrumental")

#check for outliers - method 1
boxplot(Average.Recall ~ Experiment.Type, data = stats,
        main = "Average Number of Objects Recalled by Experiment Type",
        xlab = "Experiment Type", ylab = "Number of Objects Recalled",
        col = "skyblue", ylim = c(0, max(stats$Average.Recall) + 1))

#test
print(levels(stats$Experiment.Type))

#outlier check method 2
outliers_NM <- stats %>%
  filter(Experiment.Type == "NM") %>%
  identify_outliers(Average.Recall)

outliers_L <- stats %>%
  filter(Experiment.Type == "L") %>%
  identify_outliers(Average.Recall)

outliers_I <- stats %>%
  filter(Experiment.Type == "I") %>%
  identify_outliers(Average.Recall)

# Output the identified outliers
outliers_NM
cat(" ")
outliers_I
cat(" ")
outliers_L

#normality check method 1 - each p > 0.05, thus normal
stats %>%
  group_by(Experiment.Type) %>%
  shapiro_test(Average.Recall)

#normality check method 2 - all plots fall along the reference line
ggqqplot(stats, x = "Average.Recall", facet.by = "Experiment.Type")

#equal variance check
levene_test <- leveneTest(Average.Recall ~ Experiment.Type, data = stats)
levene_test

#given that Pr(>F) < 0.05, we reject the hypothesis and conclude that the variances are NOT equal

variances <- tapply(stats$Average.Recall, stats$Experiment.Type, var, na.rm = TRUE)
variances

## ANOVA MODEL

# analysis of variance

# Fit the ANOVA model
anova_result <- aov(Average.Recall ~ Experiment.Type, data = stats)

# View the summary of the ANOVA
summary(anova_result)

# would perform Tukey's test if Pr(>F) was < 0.05 -- see example below
# Perform Tukey's test for multiple comparisons
#tukey_result <- TukeyHSD(anova_result)

# Print the Tukey's test results -- 0 is in the interval, and p > alpha, thus, FTR
#print(tukey_result)