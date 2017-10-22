#set workspace
setwd("F:/EikDenYeoh/Documents/Research Paper/R")

#load readxl package for import excel file
library(readxl)

#read sheet and assign to variable
gdp <- read_excel("RealEstateRawData-MultipleRegression.xlsx", sheet=1)
population <- read_excel("RealEstateRawData-MultipleRegression.xlsx", sheet=2)
gdp_capita <- read_excel("RealEstateRawData-MultipleRegression.xlsx", sheet=3)
unemployment <- read_excel("RealEstateRawData-MultipleRegression.xlsx", sheet=4)
interest <- read_excel("RealEstateRawData-MultipleRegression.xlsx", sheet=5)

#linear test & correlation
lmgdp <- lm(`Real GDP (%)`~`Properties Stock Index ('000)`+`Housing Price Index`, data=gdp)
summary(lmgdp)
cor(gdp$`Real GDP (%)`, gdp$`Properties Stock Index ('000)`)
cor(gdp$`Real GDP (%)`, gdp$`Housing Price Index`)

lmpopulation <- lm(`Population (Millions)`~`Properties Stock Index ('000)`+`Housing Price Index`, data=population)
summary(lmpopulation)
cor(population$`Population (Millions)`, population$`Properties Stock Index ('000)`)
cor(population$`Population (Millions)`, population$`Housing Price Index`)

lmgdp_capita <- lm(`GDP per capita in USD ('000)`~`Properties Stock Index ('000)`+`Housing Price Index`, data=gdp_capita)
summary(lmgdp_capita)
cor(gdp_capita$`GDP per capita in USD ('000)`, gdp_capita$`Properties Stock Index ('000)`)
cor(gdp_capita$`GDP per capita in USD ('000)`, gdp_capita$`Housing Price Index`)

lmunemployment <- lm(`Unemployment Rate (%)`~`Properties Stock Index ('000)`+`Housing Price Index`, data=unemployment)
summary(lmunemployment)
cor(unemployment$`Unemployment Rate (%)`, unemployment$`Properties Stock Index ('000)`)
cor(unemployment$`Unemployment Rate (%)`, unemployment$`Housing Price Index`)

lmginterest <- lm(`Interest Lending Rate (%)`~`Properties Stock Index ('000)`+`Housing Price Index`, data=interest)
summary(lmginterest)
cor(interest$`Interest Lending Rate (%)`, interest$`Properties Stock Index ('000)`)
cor(interest$`Interest Lending Rate (%)`, interest$`Housing Price Index`)

