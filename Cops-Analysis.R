library(tidyverse)
library(readxl)
library(ggalt)
library(scales)
library(ineq)

# Read in the data
SA_police.data = read_excel("San Antonio Police.xlsx")
Houston_police.data = read_excel("Houston Police.xlsx") 
Austin_police.data = read_excel("Austin Police.xlsx")
Dallas_police.data = read_excel("Dallas Police.xlsx")
FW_police.data = read_excel("Fort Worth Police.xlsx")


# Data Cleaning


# Grabs just the salary and job title
variables_sa = list(salary = sym("FY16 ANNUAL SALARY2"), jobtitle = sym("JOB TITLE"))
SA_police.data %>%
    select(., !!!variables_sa) -> SA_salary.only


variables_houston = list(salary = sym("Annual Salary"), jobtitle = sym("Title"))
Houston_police.data %>%
    select(., !!!variables_houston) -> Houston_salary.only


variables_austin = list(salary = sym("Annual Salary"), jobtitle = sym("Title"))
Austin_police.data %>%
    select(., !!!variables_austin) -> Austin_salary.only


variables_dallas = list(salary = sym("Annual Salary"), jobtitle = sym("Job Code Description"))
Dallas_police.data %>%
    select(., !!!variables_dallas) -> Dallas_salary.only


variables_fw = list(salary = sym("Annual Rt"), jobtitle = sym("Job"))
FW_police.data %>%
    select(., !!!variables_fw) -> FW_salary.only


# Adds a column to identify the city
SA_salary.only <- mutate(SA_salary.only, city = "San Antonio")
Houston_salary.only <- mutate(Houston_salary.only, city = "Houston")
Austin_salary.only <- mutate(Austin_salary.only, city = "Austin")
Dallas_salary.only <- mutate(Dallas_salary.only, city = "Dallas")
FW_salary.only <- mutate(FW_salary.only, city = "Fort Worth")

# Combines the individual city data sets into one
Cities_salary <- bind_rows(SA_salary.only, Houston_salary.only, Austin_salary.only, 
                           Dallas_salary.only, FW_salary.only)


# Data Exploration


# Mean salary
Cities_salary %>%
    group_by(., city) %>%
    summarise(., jobtitle = n(), MeanAnnual=mean(salary, na.rm = TRUE)) %>%
    print.data.frame(., digits = 3)

# Salary distribution
Cities_salary %>%
    ggplot(., aes(city, salary)) +
    geom_boxplot() +
    coord_flip() +
    labs(title ="Texas Police Department Salary Distributions")

# Calculates Gini coefficients
ineq(SA_salary.only$salary, type = "Gini")
ineq(Houston_salary.only$salary, type = "Gini")
ineq(Austin_salary.only$salary, type = "Gini")
ineq(Dallas_salary.only$salary, type = "Gini")
ineq(FW_salary.only$salary, type = "Gini")

# Create table to show Gini coeffecients for each city
City_gini = matrix(c(0.1507, 0.1486, 0.1509, 0.1779, 0.2310), ncol = 5, byrow = T)
colnames(City_gini) = c("San Antonio ", "Dallas ", "Houston ", "Austin ", "Fort Worth")
rownames(City_gini) = c("Gini")
City_gini = as.table(City_gini)
City_gini

# Lollipop graph of Gini coeffecients
Cities_salary %>%
    group_by(., city) %>%
    summarise(., Gini=ineq(salary, type="Gini")) %>%
    ggplot(., aes(reorder(city, Gini), Gini)) +
    geom_lollipop(point.colour="blue", point.size=3) +
    coord_flip() +
    labs(title="Texas Police Department Salary Inequity") 

# Lorenz curves
plot(Lc(SA_salary.only$salary), col = "blue", lwd = 2, sub = "San Antonio")
par(mfrow = c(2,2))
plot(Lc(Houston_salary.only$salary), col = "blue", lwd = 2, sub = "Houston")
plot(Lc(Austin_salary.only$salary), col = "blue", lwd = 2, sub = "Austin")
plot(Lc(Dallas_salary.only$salary), col = "blue", lwd = 2, sub = "Dallas")
plot(Lc(FW_salary.only$salary), col = "blue", lwd = 2, sub = "Fort Worth")
