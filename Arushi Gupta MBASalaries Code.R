# Analysis of MBA Salaries
# NAME: ARUSHI GUPTA
# EMAIL: arushigupta148@gmail.com
# COLLEGE: Manipal Institute of Technology


#1) Read the data
mba<-read.csv(paste("MBA Starting Salaries Data.csv",sep=""))

#2) View the data frame
View(mba)

#3) Summarize the data
library(psych)
View(describe(mba))

#4) Boxplots to show the distribution of all variables independently
library(car)
Boxplot(mba$age)
Boxplot(mba$gmat_tot)
Boxplot(mba$gmat_qpc)
Boxplot(mba$gmat_vpc)
Boxplot(mba$gmat_tpc)
Boxplot(mba$s_avg)
Boxplot(mba$f_avg)
Boxplot(mba$quarter)
Boxplot(mba$work_yrs)
Boxplot(mba$frstlang)
Boxplot(mba$salary)
Boxplot(mba$satis)

#5) Barplots to show the distribution of all variables independently
barplot(mba$age)
barplot(mba$gmat_tot)
barplot(mba$gmat_qpc)
barplot(mba$gmat_vpc)
barplot(mba$gmat_tpc)
barplot(mba$s_avg)
barplot(mba$f_avg)
barplot(mba$quarter)
barplot(mba$work_yrs)
barplot(mba$frstlang)
barplot(mba$salary)
barplot(mba$satis)

#6) Correlation between each value using corrgram
library(corrgram)
corrgram(mba, upper.panel = panel.cor)

#7)	Scatter Plots to understand how the are variables correlated pair-wise

scatterplot(mba$work_yrs~mba$salary) 
#People with work experience greater than 5 years get higher salaries

scatterplot(mba$age,mba$gmat_tot) 
#Younger people tend to score a little better in GMAT

scatterplot(jitter(mba$salary)~mba$frstlang)
#People having english as their first lang get paid more than others

scatterplot(jitter(mba$salary)~mba$f_avg)
#Starting salary is higher for people whose fall MBA performance was better.

scatterplot(mba$f_avg~mba$s_avg)
#People who scored well during spring MBA also scored well during fall MBA. People who did not score well during spring, did not score well during fall either.

scatterplotMatrix(mba)

#8) Create Variance and Covariance matrices of the dataset
var(mba)
cov(mba)

#9) subset of the dataset consisting of only those people who actually got a job
subs<- mba[ which(mba$salary>0 & mba$salary!=999 & mba$salary!=998) , ] 

#10) Contingency tables

#Hypothesis 1: The starting salary of MBA students is independent of the spring MBA average
mytable <- xtabs(~s_avg+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.9524, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 2: The starting salary of MBA students is independent of the fall MBA average
mytable <- xtabs(~f_avg+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.2518, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 3: The starting salary of MBA students is independent of the GMAT Total score
mytable <- xtabs(~gmat_tot+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.005279, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 4: The starting salary of MBA students is independent of the Work experience
mytable <- xtabs(~work_yrs+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.003809, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 5: The starting salary of MBA students is independent of the First Language
mytable <- xtabs(~frstlang+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.003296, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 6: The starting salary is independent of quartile ranking of students
mytable <- xtabs(~quarter+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value = 0.3186, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 7: The starting salary is independent of age
mytable <- xtabs(~age+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value = 3.929e-05, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 8: The starting salary is independent of sex
mytable <- xtabs(~sex+salary,data=subs)
addmargins(mytable)

#Chi square test for the above table
#p-value = 0.1045, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 9: Both males and females get similar starting salaries
#Perform t-test, p-value = 0.1809, Do not reject null hypothesis
t.test(sub_job$salary~sub_job$sex)



#10) Linear model
#It is found from the above hypotheses that age, first language, total GMAT score and work experience are the variables on which starting salary is dependent mostly on.
lin_dependent <- lm(salary~age+work_yrs+frstlang+gmat_tot,data=subs)
coefficients(lin_dependent)

#Creating a linear model based on every variable in the dataset
lin_all <- lm(salary~sex+age+work_yrs+gmat_tot+gmat_tpc+gmat_qpc+gmat_vpc+s_avg
                    +f_avg+quarter+frstlang+satis,data=subs)
coefficients(lin_all)

#11) Comparing  the remaining subset of those people who did not get a job and compare them with those people who got a job.
new_mba = data.frame(mba)
new_mba$salary[new_mba$salary<1000] <-0 #indicating unemployed
new_mba$salary[new_mba$salary>999] <-1 #indicating employed


#12) Create contingency tables
#Hypothesis 1: The starting salary of MBA students is independent of the spring MBA average
mytable <- xtabs(~s_avg+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.5283, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 2: The starting salary of MBA students is independent of the fall MBA average
mytable <- xtabs(~f_avg+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value= 0.4368, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 3: The starting salary of MBA students is independent of the GMAT Total score
mytable <- xtabs(~gmat_tot+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.5112, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 4: The starting salary of MBA students is independent of the Work experience
mytable <- xtabs(~work_yrs+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.1742, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 5: The starting salary of MBA students is independent of the First Language
mytable <- xtabs(~frstlang+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.07859, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 6: The starting salary is independent of quartile ranking of students
mytable <- xtabs(~quarter+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value = 0.0591, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 7: The starting salary is independent of age
mytable <- xtabs(~age+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value = 0.3877, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 8: The starting salary is independent of sex
mytable <- xtabs(~sex+salary,data=new_mba)
addmargins(mytable)

#Chi square test for the above table
#p-value = 0.1539, Do not reject null hypothesis
chisq.test(mytable)

smp_size <- floor(0.8 * nrow(new_mba))
set.seed(123)
train_ind <- sample(seq_len(nrow(new_mba)), size = smp_size)

train <- new_mba[train_ind, ]
test <- new_mba[-train_ind, ]

#13) Logistic Regression model
logistic_model <- glm(salary~sex+age+work_yrs+gmat_tot+gmat_tpc+gmat_qpc+gmat_vpc+s_avg
                     +f_avg+quarter+frstlang+satis,data=train)
coefficients(logistic_model)

fitted.results <- predict(logistic_model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$salary)
print(paste('Accuracy',1-misClasificError))






