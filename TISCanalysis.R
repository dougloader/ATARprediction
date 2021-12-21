rm(list = ls())

library(tidyverse)
library(readxl)
library(scales)
library(magrittr)
library(broom)
#library(tibble)
#library(rlang)

#Check CSV source file is in home directory
c("TISCMaster.xlsx") %in% dir()
c("CurrentData11_12.xlsx") %in% dir()

# Load data from excel files
TiscMaster <- read_xlsx("TISCMaster.xlsx")
CurrentAssessmentData <- read_xlsx("CurrentData11_12.xlsx")

# Review data for sanity and outcomes
head(TiscMaster)
dim(TiscMaster)
summary(TiscMaster)
summary(CurrentAssessmentData)

# Check column names
names(TiscMaster)

# Select only columns
TiscMaster %>%
  filter(Year > 2014) %>%
  select(`Surname - Given Names`, Appnum, Year, Course, `Sch Assess`, `Raw Exam`,
         `Mod/Std Sch Assess`, `Comb Mark`, `Final Scaled`, ATAR) %>% 
  rename(Student = "Surname - Given Names", School_Assessment = "Sch Assess",
         Final_Scaled = "Final Scaled", ModSchool_Assessment = "Mod/Std Sch Assess") -> TiscMaster

CurrentAssessmentData %>% 
  rename(StudentName = "Student Name", GovID = "Student Government ID", TTPcode = "Timetable Period Code",
         Course = "Course Code", School_Assessment = "Moderated Score#", '2021Year_Group' = "Year Group Code") -> CurrentAssessmentData

# Aggregate by mulitple subsets
aggregate(ATAR ~ Student + Year, TiscMaster, FUN = "median")

TiscMaster %>% 
  group_by(Student, Year) %>% 
  summarise(
    ATAR_Outcome = median(ATAR)
  ) -> TiscGrouped

TiscGrouped %>% 
  group_by(Year) %>%
  summarise(
    ATAR_Student_Count = n(),
    Median_Year = median(ATAR_Outcome, na.rm = T)
  ) %>% ungroup()

median(TiscMaster$ATAR, na.rm = T)

TiscGrouped %>% 
  ggplot(aes(ATAR_Outcome)) +
  geom_histogram(binwidth = 1, col = "black",
                 fill = "grey", alpha = .6) +
  scale_x_continuous(limits = c(30, 110), breaks = round(seq(30, 100, by = 5),0)) +
  geom_vline(xintercept = mean(TiscGrouped$ATAR_Outcome, na.rm = T), colour = "blue", size = 1) +
  geom_vline(xintercept = mean(TiscGrouped$ATAR_Outcome, na.rm = T) - (2 * sd(TiscGrouped$ATAR_Outcome, na.rm = T)),
             colour = "red", size = 1, linetype="dashed") +
  geom_vline(xintercept = mean(TiscGrouped$ATAR_Outcome, na.rm = T) + (2 * sd(TiscGrouped$ATAR_Outcome, na.rm = T)),
             colour = "red", size = 1, linetype="dashed") +
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(TiscGrouped$ATAR_Outcome, na.rm=TRUE), sd = sd(TiscGrouped$ATAR_Outcome, na.rm=TRUE)) * 1 * sum(!is.na(TiscGrouped$ATAR_Outcome)),
    colour = "green", size = 1) +
  # Add the below if you want - it will slow up the print time though.
  #geom_label(label = "Average", x = 81, y = 30, colour = "blue") +
  #geom_label(label = "-2SD", x = 65, y = 25, colour = "red") +
  #geom_label(label = "+2SD", x = 105, y = 25, colour = "red") +
  labs(title = "Add your school name. ATAR 2015 - 2020 \nScore Frequency", x = "ATAR score", y = "Count")

TiscGrouped %>% 
  ggplot(aes(x = Year, y = ATAR_Outcome,
             group = Year,
             fill = factor(Year))) +
  geom_boxplot() +
  scale_x_discrete(limits=c(2015, 2016, 2017, 2018, 2019, 2020)) +
  ylab("ATAR Outcome") +
  theme(legend.title=element_blank()) +
  geom_jitter(width = 0.050, alpha = 0.5)


summary(TiscMaster$ATAR)
e1071::skewness(TiscMaster$ATAR, na.rm = T)
e1071::kurtosis(TiscMaster$ATAR, na.rm = T)
sd(TiscMaster$ATAR, na.rm = T)
IQR(TiscMaster$ATAR, na.rm = T)

table(TiscMaster$Course)

TiscMaster %>% 
  ggplot(aes(x = Course)) +
  geom_bar(aes(y=(..count..)), col = "black",
           fill = "grey", alpha = .6) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Add your school name. ATAR 2015 - 2020 \nSubject Frequency", x = "Course", y = "Frequency") 


# Correlation Analysis
# Remove nas
TiscMasterLess <- na.omit(TiscMaster)

TiscMasterLess %>% 
  ggplot(aes(x = School_Assessment, y = Final_Scaled)) +
  geom_point() +
  xlab("School Assessment Score") + ylab("Final Scaled") +
  ggtitle("School Assessment by Final Scaled") +
  geom_smooth(method = lm) +
  facet_wrap(~Course) # We can see that the graphs are good positive linear looking.
# Definitely a cause and effect relationship between these 2 variables.
  
# Establish Pearson correlation values
TiscMasterLess %>% 
  group_by(Course) %>% 
  summarise(Student_Count = n(),
            COR = cor(School_Assessment, Final_Scaled)
            ) %>% ungroup() -> PearsonCorrelationValues

# Quick review of the correlation coefficient. r = . Positive - linear relationship

# Run prediction for just ENG ########################
TiscMasterLess %>% 
  filter(Course == "ENG") %>% 
  ggplot(aes(x = School_Assessment, y = Final_Scaled)) + geom_point() + 
  geom_smooth(method = lm) +
  ggtitle("School Assessment VS Final Scaled score")

# Notice the strong positive correlation
TiscMasterLess %>% 
  filter(Course == "ENG") -> ENG

# Pearson correlation value
ENG %>% 
  select(School_Assessment, Final_Scaled) %>% cor()

# Log data check
ENG %>% 
  select(School_Assessment, Final_Scaled) %>%
  mutate(School_Assessment = log(School_Assessment), Final_Scaled = log(Final_Scaled)) %>% 
  cor() # 0.866. this is just less than so stick with original values.

# R can create the Linear Regression model for us.

# y = a + bx - where y is our predicted value
# a is the intercept (when school assessment is 0, our final scaled will be the intercept) 
# value and b is the slope (the rate of change - add an extra 1 point of school assessmnet 
# & the Final scaled score will increase by the slope value) and x is the cause value
fit <- lm(Final_Scaled ~ School_Assessment, data = ENG)

summary(fit)
# Slope coefficient = 1.38. For each 1point increase in School Assessment score
# Intercept - If the School Assessment = 0 then Final Scaled will be -28.3
# R-Squared the Linear Model explains 76% of all information in the data.

fit$coefficients
# plot(fit)

# Filter ENG for current student data
CurrentAssessmentData %>% 
  filter(Course == "ENG") -> ENG_predict

predict(object = fit, newdata = data.frame(School_Assessment = ENG_predict$School_Assessment))

ENG_predict %>% 
  mutate(PredictedScaledScore = predict(fit, newdata = .)) %>%
  mutate(across(where(is.numeric), round, 1)) -> ENG_predict

############ Now group and loop through coeffiecients

# Try it with NESTING
TiscMasterLess %>% 
  group_by(Course) %>% 
  nest() -> TiscNested

# Mutate with prediction - Each course now has its own coefficient model, we can use these later.
TiscNested %>% 
  mutate(models = map(data, function(df) lm(Final_Scaled ~ School_Assessment, data = df))) -> TiscNested

TiscNested %>% unnest(data) -> TiscNested

# For analysis purposes
# Take the nested data, add the glance, tidy(coefficient values) & augment columns. Then unpack the tidy() values.
regressions <- TiscMasterLess %>%
  nest(data = -Course) %>% 
  mutate(
    fit = map(data, ~ lm(Final_Scaled ~ School_Assessment, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  ) 

# View p.value
regressions %>% 
  unnest(tidied)

# View r.squared value
regressions %>% 
  unnest(glanced)
# Mulitple R squared tells us how well the model is going to perform.
# How much of the dependent Final Scaled can be explained by the independent Sch_Assess
# In other words: Sch_Ass score explains n% of the variation in Final Scaled

# These predictions use a value inside the limits of the observed x-values. This is called interpolation
# and it is a safe prediction to make.

# Create a LIST of the linear models for easy evaluation.
linear_model <- function(df) lm(Final_Scaled ~ School_Assessment, data = df)

listOfCoefficients <- lapply(split(TiscNested, TiscNested$Course),linear_model)

# We now have a list of coefficient values.
# We'll use it to make predictions for all models:
# A new dataframe of names from the list of coefficients
namedList <- data.frame(names(listOfCoefficients))

# Loop through list of coefficient values and student Course scores 
# This will work no matter how many items in your list.
# But what if you are to make a prediction on an item that is not in your list? (needs alternative)
x = 1
for (i in listOfCoefficients) {
  
  print(x)
  
  varName = paste(as.character(namedList$names.listOfCoefficients.[x]))
  
  CurrentAssessmentData %>%
    mutate({{varName}} := ifelse(Course == namedList$names.listOfCoefficients.[x], predict(i, newdata = .), NA)) -> CurrentAssessmentData
  
  
  print(i)
  
  x <- x+1
  
}

CurrentAssessmentData %>% 
  select(namedList$names.listOfCoefficients.[[1]]:namedList$names.listOfCoefficients.[[nrow(namedList)]]) %>% 
  mutate(Predicted_Scaled_Score = coalesce(!!!.)) %>% select(Predicted_Scaled_Score) -> predictedCol

CurrentAssessmentData %>% 
  select(StudentName:School_Assessment) %>% 
  cbind(predictedCol) -> CurrentAssessmentData
                      
## Change NA values to Course outcome values in the case of no previous course data.
CurrentAssessmentData %>% 
  mutate(Predicted_Scaled_Score = coalesce(Predicted_Scaled_Score,School_Assessment)) -> CurrentAssessmentData


#######################


## Add 10% bonus for the correct courses - there are many others - add them here.
CurrentAssessmentData %>% 
  group_by(TTPcode, StudentName, `2021Year_Group`, GovID, Course, `Subject Name`) %>% 
  summarise(
    Sch_Assess = mean(School_Assessment),
    PredictedScaled = mean(Predicted_Scaled_Score),
    SubjectCount = n(),
    Ten_Percent_Bonus = ifelse(Course == "MAS", Predicted_Scaled_Score/10,
                               ifelse(Course == "MAM", Predicted_Scaled_Score/10,
                                      ifelse(Course == "ISL", Predicted_Scaled_Score/10,
                                             ifelse(Course == "IND", Predicted_Scaled_Score/10,
                                                    ifelse(Course == "FSL", Predicted_Scaled_Score/10, 0)))))) %>% 
  ungroup() -> CurrentAssessmentData
  
## Sort descending and calculate top 4
CurrentAssessmentData %>% 
  arrange(desc(PredictedScaled)) %>% 
  group_by(TTPcode, StudentName, `2021Year_Group`, GovID) %>% 
  summarise(CountOfCourses = n(),
            TEA_top4 = round(sum(head(PredictedScaled, 4)),1),
            bonus = round(sum(Ten_Percent_Bonus),1),
            TEA = round(sum(TEA_top4, bonus),1),
            ATAR = round(ifelse(CountOfCourses >= 4 && TEA * TEA * -0.0013 + 0.9662 * TEA - 78.311 <= 99.95, TEA * TEA * -0.0013 + 0.9662 * TEA - 78.311,
                                ifelse (CountOfCourses >= 4 && TEA * TEA * -0.0013 + 0.9662 * TEA - 78.311 > 99.95,99.95, 0)),2)) -> ATARprediction

write_csv(ATARprediction, "ATAR_Predictions_by_semester.csv")

write_csv(CurrentAssessmentData, "CurrentAssessmentData_by_semester.csv")
