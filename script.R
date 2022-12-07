######################################################################
### Title: "Final Project"
### Course: STA 235H
### Semester: Fall 2022
### Name: Manoj Singireddy, Surya Chokkar, Pranav Mereddy, Cameron Wee
#######################################################################
# Clears memory
rm(list = ls())
# Clears console
cat("\014")

### Load libraries
library(tidyverse)
library(modelsummary)
library(estimatr)
library(jtools)
library(rsample)
library(lattice)
library(caret)
library(modelr)

### Read in Data
fake <- read.csv("https://raw.githubusercontent.com/Manojrsingireddy/DS-Final-Project/main/fake_job_postings.csv")

### Create Additional Variables to notify if location, department, etc. have values
fake$hasLocation <- ifelse(fake$location == "",0,1)
#fake$hasTitle <- ifelse(fake$title == "",0,1)
fake$hasDepartment <- ifelse(fake$department == "",0,1)
fake$hasSalary_range <- ifelse(fake$salary_range == "",0,1)
fake$hasCompany_profile <- ifelse(fake$company_profile == "",0,1)
#fake$hasDescription <- ifelse(fake$description == "",0,1)
fake$hasRequirements <- ifelse(fake$requirements == "",0,1)
fake$hasBenefits <- ifelse(fake$benefits == "",0,1)
fake$hasIndustry <- ifelse(fake$industry == "",0,1)
fake$hasFunction <- ifelse(fake$function. == "",0,1)


### Create Factor Variables
fake$employment_type <- ifelse(fake$employment_type == "","None Given", fake$employment_type)
emp_type_num = ifelse(fake$employment_type == 'Full-time',1,ifelse(fake$employment_type == 'Part-time',2,ifelse(fake$employment_type == 'Contract',3,
ifelse(fake$employment_type == 'Temporary',4,ifelse(fake$employment_type == 'Other',5,6)))))
fake$employment_type <- factor(emp_type_num, labels = c("Full-Time","Part-Time","Contract","Temporary","Other","None Given"))      


fake$required_experience <- ifelse(fake$required_experience == "","None Given", fake$required_experience)
req_exp_num = ifelse(fake$required_experience == 'Executive',1,ifelse(fake$required_experience == 'Director',2,ifelse(fake$required_experience == 'Mid-Senior level',3,
ifelse(fake$required_experience == 'Associate',4,ifelse(fake$required_experience == 'Entry level',5,ifelse(fake$required_experience == 'Internship',6,
                                                                                                                                                                                                                                 ifelse(fake$required_experience == 'Not Applicable',7,8)))))))
fake$required_experience <- factor(req_exp_num, labels = c("Executive","Director","Mid-Senior","Associate","Entry","Internship", "Not Applicable", "None Given"))   


### Drop Unnecessary Variables and na
fake <- fake %>% select(-c(job_id, title, location, department, salary_range, company_profile, description, requirements, benefits, required_education, industry, function.))
fake <- fake %>% drop_na()


### Create Balance Table on the Result to See Mean Differences
fake_bin <- fake %>% select (hasLocation, hasDepartment, hasSalary_range, hasCompany_profile, hasRequirements,
                             hasBenefits, hasIndustry, hasFunction, telecommuting, has_company_logo, has_questions, fraudulent)
fake_bin$fraudulent <- factor(fake_bin$fraudulent, labels = c("Not Fraudulent", "Fraudulent"))
datasummary_balance(~fraudulent, data=fake_bin, dinm_statistic = "std.error", fmt=5, title = "Data Summary BT on Fraudulent")

### Create histograms to analyze correlation between company profile and logo with fraudulent job postings
profiles = c('0' = 'No Company Profile',
             '1' = 'Has Company Profile')

ggplot(data = fake, aes(x = fraudulent)) +
  geom_bar(width = 0.5) +
  facet_wrap(~hasCompany_profile, labeller = as_labeller(profiles)) +
  xlab("Fraudulent job posting (1 = Fraudulent, 0 = Not Fraudulent)") +
  ylab("Count")

logos = c('0' = 'No Company Logo',
          '1' = 'Has Company Logo')

ggplot(data = fake, aes(x = fraudulent)) +
  geom_bar(width = 0.5) +
  facet_wrap(~has_company_logo, labeller = as_labeller(logos)) +
  xlab("Fraudulent job posting (1 = Fraudulent, 0 = Not Fraudulent)") +
  ylab("Count")

##### Regression Variations

### Simple Regression to see relationship of variables with outcome

### Create Model to Examine Effect of binary Variables
summary(lm_robust(fraudulent ~ required_experience + employment_type + hasLocation + hasDepartment + hasSalary_range + hasCompany_profile + hasRequirements +
                    hasBenefits + hasIndustry + hasFunction + telecommuting + has_company_logo + has_questions + employment_type , data=fake))

### Split Data
set.seed(420)
split <- initial_split(fake, prop = 0.75, strata = "fraudulent")
train.data  <- training(split)
test.data   <- testing(split)

### Count variable num
lm_num <- lm(fraudulent ~ . , data = fake)
nvars <- length(lm_num$coefficients)-1

### Forward Stepwise
set.seed(420)
lm.front <- train(fraudulent ~ ., data = train.data, method = "leapForward",
                 tuneGrid = data.frame(nvmax = 1:nvars), 
                 trControl = trainControl(method = "cv", number = 10))
lm.front$bestTune
rmse(lm.front,test.data)

### Backward Stepwise
set.seed(420)
lm.back <- train(fraudulent ~ ., data = train.data, method = "leapBackward",
                  tuneGrid = data.frame(nvmax = 1:nvars), 
                  trControl = trainControl(method = "cv", number = 10))
lm.back$bestTune
rmse(lm.back,test.data)

### Ridge Regression
lambda_seq <- seq(0,0.5,length = 50) 

set.seed(420)

ridge <- train(factor(fraudulent) ~ ., data = train.data, 
               method = "glmnet",
               preProcess = "scale", 
               trControl = trainControl("cv", number = 10), 
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = lambda_seq)
)

ridge$bestTune$lambda

ridge.pred.values <- ridge %>% predict(test.data)
mean(ridge.pred.values == test.data$fraudulent)

### Lasso Regression
lambda_seq <- seq(0,0.005,length = 50) 

set.seed(420)
lasso <- train(factor(fraudulent) ~ ., data = train.data, 
               method = "glmnet",
               preProcess = "scale", 
               trControl = trainControl("cv", number = 10), 
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = lambda_seq)
)
lasso$bestTune$lambda
lasso.pred.values <- lasso %>% predict(test.data)
mean(lasso.pred.values == test.data$fraudulent)
coefs <- coef(lasso$finalModel, lasso$bestTune$lambda)
coefs@p[2] - 1


### Machine Learning Variations

### Bagging Tree

### Random Forest

### Gradient Boosting


