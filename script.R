
# Clears memory
rm(list = ls())
# Clears console
cat("\014")

### Load libraries
library(tidyverse)
library(modelsummary)
library(estimatr)

### Read in Data
fake <- read.csv("/Users/cameronwee/Documents/fake_job_postings.csv") #Adjust for your computer

### Create Additional Variables to notify if location, department, etc. have values
fake$hasLocation <- ifelse(fake$location == "",0,1)
fake$hasTitle <- ifelse(fake$title == "",0,1)
fake$hasDepartment <- ifelse(fake$department == "",0,1)
fake$hasSalary_range <- ifelse(fake$salary_range == "",0,1)
fake$hasCompany_profile <- ifelse(fake$company_profile == "",0,1)
fake$hasDescription <- ifelse(fake$description == "",0,1)
fake$hasRequirements <- ifelse(fake$requirements == "",0,1)
fake$hasBenefits <- ifelse(fake$benefits == "",0,1)
fake$hasIndustry <- ifelse(fake$industry == "",0,1)
fake$hasFunction <- ifelse(fake$function. == "",0,1)

### Create Factor Variables
fake$employment_type <- ifelse(fake$employment_type == "","None Given", fake$employment_type)
emp_type_num = ifelse(fake$employment_type == 'Full-time',1,ifelse(fake$employment_type == 'Part-time',2,ifelse(fake$employment_type == 'Contract',3,
                                                                                                                ifelse(fake$employment_type == 'Temporary',4,ifelse(fake$employment_type == 'Other',5,6)))))
fake$emp_typ_fact <- factor(emp_type_num, labels = c("Full-Time","Part-Time","Contract","Temporary","Other","None Given"))      


fake$required_experience <- ifelse(fake$required_experience == "","None Given", fake$required_experience)
req_exp_num = ifelse(fake$required_experience == 'Executive',1,ifelse(fake$required_experience == 'Director',2,ifelse(fake$required_experience == 'Mid-Senior level',3,
                                                                                                                      ifelse(fake$required_experience == 'Associate',4,ifelse(fake$required_experience == 'Entry level',5,ifelse(fake$required_experience == 'Internship',6,
                                                                                                                                                                                                                                 ifelse(fake$required_experience == 'Not Applicable',7,8)))))))
fake$req_exp_fact <- factor(req_exp_num, labels = c("Executive","Director","Mid-Senior","Associate","Entry","Internship", "Not Applicable", "None Given"))   


### Create Balance Table on the Result to See Mean Differences
fake_bin <- fake %>% select (hasLocation, hasTitle, hasDepartment, hasSalary_range, hasCompany_profile, hasDescription, hasRequirements,
                             hasBenefits, hasIndustry, hasFunction, telecommuting, has_company_logo, has_questions,fraudulent)
datasummary_balance(~fraudulent, data=fake_bin, dinm_statistic = "p.value", fmt=3, title = "Data Summary BT on Fraudulent")

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


### Create Model to Examine Effect of Required Experience and Employment Type on Fraudulent
summary(lm_robust(fraudulent ~ req_exp_fact + emp_typ_fact, data=fake))

### Create Model to Examine Effect of binary Variables
summary(lm_robust(fraudulent ~ hasLocation + hasTitle + hasDepartment + hasSalary_range + hasCompany_profile+ hasDescription + hasRequirements +
                    hasBenefits + hasIndustry + hasFunction + telecommuting + has_company_logo + has_questions + emp_typ_fact , data=fake))