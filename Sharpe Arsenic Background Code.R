
library(tidyverse)
library(readxl)
library(Kendall)
library(openxlsx)
library(zyp)

sharpe <- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930.xlsx")
sharpe$Group <- ifelse(startsWith(sharpe$`Location ID`, "DP"), "SB",
                   ifelse(startsWith(sharpe$`Location ID`, "SB"), "SB",
                          ifelse(startsWith(sharpe$`Location ID`, "SHAD"), "SHAD", NA)))
print(sharpe)
anova_result <- aov(Arsenic ~ Group, data = sharpe)
summary(anova_result)
TukeyHSD(anova_result)
print(sharpe)
sharpe$`Sample Depth` <- as.factor(sharpe$`Sample Depth`)

anova_depth <- aov(Arsenic ~ `Sample Depth`, data = sharpe)
ttest <- t.test(Arsenic ~ Group, data= sharpe)
ttest
summary(anova_depth)
TukeyHSD(anova_depth)
kt<- kruskal.test(Arsenic ~ Group, data= sharpe)
print(kt)
wt<- wilcox.test(Arsenic ~ Group, data= sharpe)
print(wt)

names(sharpe)[names(sharpe) == "Sample Depth"] <- "SampleDepth"
sharpe$SampleDepth <- as.factor(sharpe$SampleDepth)

anova_depth <- aov(Arsenic ~ SampleDepth, data = sharpe)
TukeyHSD(anova_depth)

sharpedepths <- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_depths fixed.xlsx")
names(sharpedepths)[names(sharpedepths) == "Sample Depth"] <- "SampleDepth"
sharpedepths$SampleDepth <- as.factor(sharpedepths$SampleDepth)

anova_depth <- aov(Arsenic ~ SampleDepth, data = sharpedepths)
TukeyHSD(anova_depth)
summary(sharpedepths)
summary(anova_depth)

ggplot(sharpe, aes( x=Group, y=Arsenic)) + geom_boxplot()
sharpeoutlier<- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_OutlierRemoved.xlsx")
sharpeoutlier$Group <- ifelse(startsWith(sharpeoutlier$`Location ID`, "DP"), "SB",
                       ifelse(startsWith(sharpeoutlier$`Location ID`, "SB"), "SB",
                              ifelse(startsWith(sharpeoutlier$`Location ID`, "SHAD"), "SHAD", NA)))


ggplot(sharpeoutlier, aes( x=Group, y=Arsenic)) + geom_boxplot()
ggplot(sharpeoutlier, aes(y=Arsenic)) + geom_boxplot()
names(sharpeoutlier)[names(sharpeoutlier) == "Sample Depth"] <- "SampleDepth"
sharpeoutlier$SampleDepth <- as.factor(sharpeoutlier$SampleDepth)

anova_outlier <- aov(Arsenic ~ Group, data = sharpeoutlier)
TukeyHSD(anova_outlier)
summary(sharpeoutlier)
summary(anova_outlier)
ttest_outlier<-t.test(Arsenic ~ Group, data = sharpeoutlier)
summary(ttest_outlier)
t.test()
table(sharpeoutlier$Group)
ttest_outlier <- t.test(Arsenic ~ Group, data = sharpeoutlier)
ttest_outlier
wtoutlier <- wilcox.test(Arsenic ~ Group, data = sharpeoutlier)
print(wtoutlier)

sharpeexccedances<- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_OutlierRemoved_separategroups.xlsx")
sharpeexccedances$Group <- ifelse(startsWith(sharpeexccedances$`Location ID`, "DP"), "SB",
                              ifelse(startsWith(sharpeexccedances$`Location ID`, "SB"), "SB",
                                     ifelse(startsWith(sharpeexccedances$`Location ID`, "SHAD"), "SHAD", NA)))
ggplot(sharpeexccedances, aes(x=Group, y=Arsenic)) + geom_boxplot()
sharpeexccedances %>%
  filter(!is.na(Arsenic), !is.na(Group)) %>%
  ggplot(aes(sample = Arsenic)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ Group) +
  labs(x= "Group", y= "Arsenic Concentration") +
  ggtitle("Cleaned Data with Separate Group")

sharpeexccedances %>%
  filter(!is.na(Arsenic), !is.na(Group)) %>%
  ggplot(aes(sample = Arsenic)) +
  geom_qq() +
  geom_qq_line() +
  labs(x= "Group", y= "Arsenic Concentration") +
  ggtitle("Cleaned Data with Groups Combined")

wtexceedances <- wilcox.test(Arsenic ~ Group, data = sharpeexccedances)
print(wtexceedances)

ttestexccedances<- t.test(Arsenic ~ Group, data = sharpeexccedances)
print(ttestexccedances)
