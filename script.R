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
library(cvms)
library(tibble)
library(NeuralNetTools)


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

# Reduce number of positive observations
set.seed(420)
fake1 <- fake %>% filter(fraudulent == 1)
fake0 <- fake %>% filter(fraudulent == 0) %>% slice_sample(prop = 0.1)
fake <- rbind(fake1,fake0)

### Create Balance Table on the Result to See Mean Differences
fake_bin <- fake %>% select (-c(required_experience,employment_type))
fake_bin$fraudulent <- factor(fake_bin$fraudulent, labels = c("Not Fraudulent", "Fraudulent"))
colnames(fake_bin) = c('Telecommuting', 'Company Logo', 'Questions', 'fraudulent', 'Location', 'Department', 'Salary Range', 'Company Profile', 'Requirements', 'Benefits', 'Industry', 'Function')
datasummary_balance(~fraudulent, data=fake_bin, dinm_statistic = "std.error", fmt=5, title = "Data Summary BT on Fraudulent")

### Create histograms to analyze correlation between company profile and logo with fraudulent job postings
profiles = c('0' = 'No Company Profile',
             '1' = 'Has Company Profile')

ggplot(data = fake, aes(x = fraudulent)) +
  geom_bar(width = 0.5) +
  facet_wrap(~hasCompany_profile, labeller = as_labeller(profiles)) +
  xlab("Fraudulent job posting (1 = Fraudulent, 0 = Not Fraudulent)") +
  ylab("Count") +
  theme(strip.background = element_rect(fill = 'blue')) +
  theme(strip.text = element_text(color = 'white'))

logos = c('0' = 'No Company Logo',
          '1' = 'Has Company Logo')

ggplot(data = fake, aes(x = fraudulent)) +
  geom_bar(width = 0.5) +
  facet_wrap(~has_company_logo, labeller = as_labeller(logos)) +
  xlab("Fraudulent job posting (1 = Fraudulent, 0 = Not Fraudulent)") +
  ylab("Count") +
  theme(strip.background = element_rect(fill = 'light blue')) +
  theme(strip.text = element_text(color = 'black'))

### Simple Regression to see relationship of variables with outcome

### Create Model to Examine Effect of Variables
library(jtools)
library(kableExtra)
fake_lm_data <- fake %>% rename("Telecommuting Available" = telecommuting, 
                                "Company Logo Present" = has_company_logo,
                                "Questions Present" = has_questions,
                                "Location Present" = hasLocation,
                                "Department Present" = hasDepartment,
                                "Salary Range Present" = hasSalary_range,
                                "Company Profile Present" = hasCompany_profile,
                                "Requirements Present" = hasRequirements,
                                "Benefits Present" = hasBenefits,
                                "Industry Present" = hasIndustry,
                                "Function Present" = hasFunction,
                                "Required Experience: " = required_experience,
                                "Employment Type: " = employment_type)
summary(lm_robust(fraudulent ~ ., data=fake_lm_data))


### Split Data
set.seed(420)
split <- initial_split(fake, prop = 0.75, strata = "fraudulent")
train.data  <- training(split)
test.data   <- testing(split)

### Count variable num
lm_num <- lm(fraudulent ~ . , data = fake)
nvars <- length(lm_num$coefficients)-1

### Machine Learning Variations

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
test.data$ridge.pred.values <- ridge %>% predict(test.data)

### Lasso Regression
lambda_seq <- seq(0,0.5,length = 50) 

set.seed(420)

lasso <- train(factor(fraudulent) ~ ., data = train.data, 
               method = "glmnet",
               preProcess = "scale", 
               trControl = trainControl("cv", number = 10), 
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = lambda_seq)
)
test.data$lasso.pred.values <- lasso %>% predict(test.data)
coefs <- coef(lasso$finalModel, lasso$bestTune$lambda)
coefs@p[2] - 1
plot(varImp(lasso, scale = TRUE),main="Most Important Variables for Lasso Regression")

### Tree Bagging
set.seed(420)
bt <- train(factor(fraudulent) ~ ., data = train.data,
            method = "treebag", trControl = trainControl("cv", number = 10),
            nbagg = 150,  
            metric = "Accuracy")
test.data$bt.pred.values <- bt %>% predict(test.data)
plot(varImp(bt, scale = TRUE),main="Most Important Variables for Tree Bagging")

### Neural Network
nnet <- train(factor(fraudulent) ~ ., 
                     data = train.data, 
                     method = "nnet", 
                     trControl = trainControl(method = "cv", number = 10),
                     trace = FALSE,
                     learningrate = 0.25,
                     tuneGrid = expand.grid(size = seq(2, 15), decay=10^seq(-4,0,by=1)),
                     metric = "Accuracy"
                     )
test.data$nnet.pred.values <- nnet %>% predict(test.data)
nnet$finalModel
nnet$bestTune
plotnet(nnet$finalModel) #Plot the network

# Compare Models

# Used this Resource: https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
draw_confusion_matrix <- function(cm, title) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(title, cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#59a7ff')
  text(195, 435, 'Not Fraudulent', cex=1.2)
  rect(250, 430, 340, 370, col='#1e3fa1')
  text(295, 435, 'Fraudulent', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#1e3fa1')
  rect(250, 305, 340, 365, col='#59a7ff')
  text(140, 400, 'Not Fraudulent', cex=1.2, srt=90)
  text(140, 335, 'Frauduelent', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  



draw_confusion_matrix(caret::confusionMatrix(data = factor(test.data$fraudulent), reference = test.data$ridge.pred.values), 
                      "Confusion Matrix for Ridge Regression") # Did for fun (no need to use)
draw_confusion_matrix(caret::confusionMatrix(data = factor(test.data$fraudulent), reference = test.data$lasso.pred.values), 
                      "Confusion Matrix for Lasso Regression") # Use Lasso
draw_confusion_matrix(caret::confusionMatrix(data = factor(test.data$fraudulent), reference = test.data$bt.pred.values), 
                      "Confusion Matrix for Tree Bagging") # Use Tree Bagging
draw_confusion_matrix(caret::confusionMatrix(data = factor(test.data$fraudulent), reference = test.data$nnet.pred.values), 
                      "Confusion Matrix for Neural Net") # Use Neural Net





