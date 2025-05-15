
library(tidyverse)
library(readxl)
library(Kendall)
library(openxlsx)
library(zyp)
#load file of full data set
sharpe <- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930.xlsx")
#create a Group column to make it easy to sort by group (DP and SB are combined based on location in the map)
sharpe$Group <- ifelse(startsWith(sharpe$`Location ID`, "DP"), "SB",
                   ifelse(startsWith(sharpe$`Location ID`, "SB"), "SB",
                          ifelse(startsWith(sharpe$`Location ID`, "SHAD"), "SHAD", NA)))
print(sharpe) #check to make sure everything loaded
anova_result <- aov(Arsenic ~ Group, data = sharpe) #run ANOVA
summary(anova_result) #Check ANOVA output
TukeyHSD(anova_result) #Post hoc test
print(sharpe)
sharpe$`Sample Depth` <- as.factor(sharpe$`Sample Depth`) #added for future analysis 'sample depth' needs to be categorized as a factor

anova_depth <- aov(Arsenic ~ `Sample Depth`, data = sharpe) #this is out of order it is ran after the sharpedepths need to be loaded
ttest <- t.test(Arsenic ~ Group, data= sharpe) #ttest with the two sample groups. ANOVA is  not approriate for only 2 groups
ttest #ttest output
summary(anova_depth) #Not needed ANOVA output
TukeyHSD(anova_depth) #Not needed post hoc output
kt<- kruskal.test(Arsenic ~ Group, data= sharpe) #Not needed non parametric ANOVA
print(kt) #not needed output
wt<- wilcox.test(Arsenic ~ Group, data= sharpe) #non parametric ttest
print(wt) #wilcox test output

names(sharpe)[names(sharpe) == "Sample Depth"] <- "SampleDepth" #sample depths name change to run smoother
sharpe$SampleDepth <- as.factor(sharpe$SampleDepth) #sample depth turned into factors (same as above)

anova_depth <- aov(Arsenic ~ SampleDepth, data = sharpe) #ANOVA by sample depths
TukeyHSD(anova_depth)#post hoc of ANOVA comparing sample depths

sharpedepths <- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_depths fixed.xlsx") #updated sample depths that group similar depth ranges
names(sharpedepths)[names(sharpedepths) == "Sample Depth"] <- "SampleDepth" #fix sample depth catergory name
sharpedepths$SampleDepth <- as.factor(sharpedepths$SampleDepth)

anova_depth <- aov(Arsenic ~ SampleDepth, data = sharpedepths) #ANOVA based on sample depths with the new name
TukeyHSD(anova_depth)#Post Hoc sample depths
summary(sharpedepths)
summary(anova_depth)

ggplot(sharpe, aes( x=Group, y=Arsenic)) + geom_boxplot() #boxplot
sharpeoutlier<- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_OutlierRemoved.xlsx") #load with outlier removed
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
ttest_outlier<-t.test(Arsenic ~ Group, data = sharpeoutlier) #Outlier removed t.test
summary(ttest_outlier)
t.test()
table(sharpeoutlier$Group)
ttest_outlier <- t.test(Arsenic ~ Group, data = sharpeoutlier)
ttest_outlier
wtoutlier <- wilcox.test(Arsenic ~ Group, data = sharpeoutlier) #non parametric comparison
print(wtoutlier)

sharpeexccedances<- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_OutlierRemoved_separategroups.xlsx")
sharpeexccedances$Group <- ifelse(startsWith(sharpeexccedances$`Location ID`, "DP"), "SB",
                              ifelse(startsWith(sharpeexccedances$`Location ID`, "SB"), "SB",
                                     ifelse(startsWith(sharpeexccedances$`Location ID`, "SHAD"), "SHAD", NA)))

##QQ Plot
ggplot(sharpeexccedances, aes(x=Group, y=Arsenic)) + geom_boxplot()
sharpeexccedances %>%
  filter(!is.na(Arsenic), !is.na(Group)) %>%
  ggplot(aes(sample = Arsenic)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ Group) +
  labs(x= "Group", y= "Arsenic Concentration") +
  ggtitle("Cleaned Data with Separate Group")

##QQplot

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


###post meeting Code###

#run the difference with one population outliers
sharpeexccedances<- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_OutlierRemoved_separategroups.xlsx")
sharpeexccedances$Group <- ifelse(startsWith(sharpeexccedances$`Location ID`, "DP"), "SB",
                                  ifelse(startsWith(sharpeexccedances$`Location ID`, "SB"), "SB",
                                         ifelse(startsWith(sharpeexccedances$`Location ID`, "SHAD"), "SHAD", NA)))
ttestexccedances<- t.test(Arsenic ~ Group, data = sharpeexccedances)
wtexceedances <- wilcox.test(Arsenic ~ Group, data = sharpeexccedances)
print(wtexceedances)

sharpeexccedances %>%
  filter(!is.na(Arsenic), !is.na(Group)) %>%
  ggplot(aes(sample = Arsenic)) +
  geom_qq() +
  geom_qq_line() +
  labs(x= "Group", y= "Arsenic Concentration") +
  ggtitle("Cleaned Data with Groups Combined")

ggplot(sharpeexccedances, aes(y=Arsenic)) + geom_boxplot()

##looking at 0-10 as one group and if different than greater than 10

sharpeexccedances<- read_excel("/Users/eousley/OneDrive - beringstraits.com/Documents/Sharpe Arsenic Background/ProUCL_data_SHAD_20250417014930_OutlierRemoved_separategroups_0to10.xlsx")
names(sharpeexccedances)[names(sharpeexccedances) == "Sample Depth"] <- "SampleDepth" #fix sample depth catergory name
sharpeexccedances$SampleDepth <- as.factor(sharpeexccedances$SampleDepth)
wtexceedances <- wilcox.test(Arsenic ~ SampleDepth, data = sharpeexccedances)
print(wtexceedances)
print(sharpe)

sharpeexccedances$Group <- ifelse(startsWith(sharpeexccedances$`Location ID`, "DP"), "SB",
                                  ifelse(startsWith(sharpeexccedances$`Location ID`, "SB"), "SB",
                                         ifelse(startsWith(sharpeexccedances$`Location ID`, "SHAD"), "SHAD", NA)))
sharpeexccedances$Arsenic <- as.numeric(sharpeexccedances$Arsenic)

sharpegroup <- sharpeexccedances %>%
  group_split(Group) %>%
  wilcox.test(Arsenic ~ SampleDepth)
  
wtgroup <- wilcox.test(Arsenic ~ SampleDepth, data = sharpegroup)
print(wtgroup)
summary(wtgroup)

sharpegroup
