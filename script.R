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
library(rpart)
library(iterators)
library(foreach)
library(parallel)
library(doParallel)

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

test.data$ridge.pred.values <- ridge %>% predict(test.data)
mean(test.data$ridge.pred.values == test.data$fraudulent)




### Machine Learning Variations

# Parallelization Setup

cl <- makePSOCKcluster(detectCores() - 1) 
registerDoParallel(cl)

### Bagging Tree
set.seed(420)
bt <- train(factor(fraudulent) ~ ., data = train.data,
            method = "treebag", trControl = trainControl("cv", number = 10),
            nbagg = 100,
            allowParallel = TRUE,
            control = rpart.control(cp = 0))

test.data$bt.pred.values <- bt %>% predict(test.data)
mean(test.data$bt.pred.values == test.data$fraudulent)

plot(varImp(bt, scale = TRUE))


### Random Forest
set.seed(420)
tuneGrid <- expand.grid(
  mtry = 1:14, 
  splitrule = "variance", 
  min.node.size = 5
)

rf <- train(fraudulent ~ ., data = train.data,
            method = "ranger", # You can also use "rf", but "ranger" is faster!
            trControl = trainControl("cv", number = 10),
            importance = "permutation",
            tuneGrid = tuneGrid,
            num.trees = 150,
            allowParallel = TRUE)
test.data$rf.pred.valeus <- rf %>% predict(test.data)
mean(test.data$rf.pred.values == test.data$fraudulent)

rf$bestTune
plot(varImp(rf, scale = TRUE))


### Gradient Boosting
set.seed(420)

gbm <- train(fraudulent ~ ., data = train.data,
             method = "gbm",                          # We are using gradient boosting
             trControl = trainControl("cv", number = 10),
             tuneLength = 20) # Play around with this parameter!

# Final Model information
gbm$finalModel

# Best Tuning parameters?
gbm$bestTune

test.data$gb.pred.valeus <- gb %>% predict(test.data)
mean(test.data$gb.pred.values == test.data$fraudulent)


# Detach Parallelization
stopCluster(cl) 
registerDoSEQ() 

# Compare Models

ggplot(data = test.data, aes(x = cnt)) +
  geom_histogram(aes(y = ..density..), color = "grey", fill = alpha("grey", 0.5), lwd = 1, bins = 20) +
  geom_density(aes(x = bt.pred.values, color = "Bagged Trees"), lty = 2, lwd = 1) +
  geom_density(aes(x = rf.pred.values, color = "Random Forest"), lty = 1, lwd = 1) +
  geom_density(aes(x = gb.pred.values, color = "Gradient Boosting"), lty = 4, lwd = 1) +
  scale_color_manual(name = "", values = c("#0F6AF8","#8108AB","#05C166")) +
  theme_minimal() + theme(legend.position = c(0.85,0.9)) +
  xlab("Is Fraudulent or not") + ylab("Density")
