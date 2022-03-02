###########################################################
########    Choose Your Own Capstone      ################
##########################################################

#Load libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(knitr)
library(RColorBrewer)
library(rmarkdown)
library(pdftools)
library(kableExtra)
library(rpart)
library(rpart.plot)
library(randomForest)
library(purrr)
library(e1071)
library(scales)
library(ggpubr)
library(ggrepel)
library(parameters)
library(insight)

##########################################################
# Data Preparation
##########################################################

# openning the data set
energy<-read.csv("energy.csv")
str(energy)

#removing useless variables and the ones with many NAs.
energy = subset(energy, select = -c(GenSolarBinary, GenTotalRenewableBinary, AllSourcesCO2, AllSourcesSO2, AllSourcesNOx, EsalesOther, Import))
str(energy) 

#averaging variables
avgEPR<- energy%>% group_by(YEAR) %>% summarise(mean(EPriceResidential))
avgEPC<- energy%>% group_by(YEAR) %>% summarise(mean(EPriceCommercial))
avgEPI<- energy%>% group_by(YEAR) %>% summarise(mean(EPriceIndustrial))
avgEPT<- energy%>% group_by(YEAR) %>% summarise(mean(EPriceTotal))
avgGenTotal<- energy%>% group_by(YEAR) %>% summarise(mean(GenTotal))
avgGenHydro<- energy%>% group_by(YEAR) %>% summarise(mean(GenHydro))
avgGenSolar<- energy%>% group_by(YEAR) %>% summarise(mean(GenSolar))
avgGenTotalRenewable<- energy%>% group_by(YEAR) %>% summarise(mean(GenTotalRenewable))
avgCumlFinancial<- energy%>% group_by(YEAR) %>% summarise(mean(CumlFinancial))
avgCumlRegulatory<- energy%>% group_by(YEAR) %>% summarise(mean(CumlRegulatory))
avgESR<- energy%>% group_by(YEAR) %>% summarise(mean(EsalesResidential))
avgESC<- energy%>% group_by(YEAR) %>% summarise(mean(EsalesCommercial))
avgESI<- energy%>% group_by(YEAR) %>% summarise(mean(EsalesIndustrial))
avgEST<- energy%>% group_by(YEAR) %>% summarise(mean(EsalesTotal))
avgTotalSalary<- energy%>% group_by(YEAR) %>% summarise(mean(Total.salary))
year<- energy%>% group_by(YEAR) %>% summarise(mean(YEAR))

#creating averaged data frame
avgenergy <- data.frame(avgCumlFinancial, avgCumlRegulatory, avgEPC, avgEPI, avgEPR, avgEPT, avgESC, avgESI, avgESR, avgEST, avgGenHydro, avgGenSolar, avgGenTotal, avgGenTotalRenewable, avgTotalSalary, year)
avgenergy = subset(avgenergy, select = -c(YEAR, YEAR.1, YEAR.2,YEAR.3, YEAR.4, YEAR.5, YEAR.6,YEAR.7, YEAR.8, YEAR.9, YEAR.10, YEAR.11, YEAR.12, YEAR.13, YEAR.14, YEAR.15))
str(avgenergy)

##########################################################
# Data Exploration
##########################################################

#2.1 Prices
#plotting prices
plot(avgEPT)

#plotting prices in residential, commercial and industrial zones
ggplot(avgenergy, aes(x=mean.YEAR.)) + 
  geom_line(aes(y=mean.EPriceResidential., color="Residential"), size=2) + 
  geom_line(aes(y=mean.EPriceCommercial., color="Commercial"), size=2) + 
  geom_line(aes(y=mean.EPriceIndustrial., color="Industrial"), size=2) + 
  geom_line(aes(y=mean.EPriceTotal., color="Total"), linetype="twodash", size=2) + 
  scale_color_manual(values = c("darkred", "steelblue", "darkolivegreen4", "black")) + 
  scale_y_continuous(name= "Energy Prices", limits=c(5,13), breaks= c(5,7,9,11,13)) + 
  scale_x_continuous(name= "Year", limits=c(2000,2013), breaks= c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ggtitle("Energy Prices 2000-2013") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x=element_text(angle = -270, hjust = 1)) + 
  labs(color="Zone")

#2.2 Government Incentives
#plotting government incentives
ggplot(avgenergy, aes(x=mean.YEAR.)) + 
  geom_line(aes(y=mean.CumlRegulatory., color="Regulatory"), size=2) + 
  geom_line(aes(y=mean.CumlFinancial., color="Financial"), size=2)  + 
  scale_color_manual(values = c("darkorange", "cyan3")) + 
  scale_y_continuous(name= "Government Incentives", limits=c(0,40), breaks= c(0,10,20,30,40)) + 
  scale_x_continuous(name= "Year", limits=c(2000,2013), breaks= c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) + 
  ggtitle("Government Incentives 2000-2013") +  
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_text(angle = -270, hjust = 1)) + 
  labs(color="Type of Incentive")

#2.3 Energy Generation
#plotting energy generation
ggplot(avgenergy, aes(x=mean.YEAR.)) +
  geom_line(aes(y=mean.GenHydro., color="Hydro"), size=2) + 
  geom_line(aes(y=mean.GenSolar., color="Solar"), size=2)  + 
  geom_line(aes(y=mean.GenTotalRenewable., color="Renewable"), size=2)  + 
  scale_color_manual(values = c("darkslategrey", "sienna4", "coral1")) + 
  scale_y_continuous(name= "Energy Generation", limits=c(0,.16), breaks= c(0.04,0.08,0.12,0.16)) + 
  scale_x_continuous(name= "Year", limits=c(2000,2013), breaks= c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) + 
  ggtitle("Energy Generation 2000-2013") +  
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_text(angle = -270, hjust = 1)) + 
  labs(color="Type of Energy Generation")

#2.4 Energy consumption
#plotting energy consumption
ggplot(avgenergy, aes(x=mean.YEAR.)) + 
  geom_line(aes(y=mean.EsalesResidential., color="Residential"), size=2) + 
  geom_line(aes(y=mean.EsalesCommercial., color="Commercial"), size=2)  + 
  geom_line(aes(y=mean.EsalesIndustrial., color="Industrial"), size=2)  + 
  scale_color_manual(values = c("darkred", "steelblue", "darkolivegreen4")) + 
  scale_y_continuous(name= "Energy Consumption", limits=c(0.25,0.4), breaks= c(0.25,0.30,0.35,0.40)) + 
  scale_x_continuous(name= "Year", limits=c(2000,2013), breaks= c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) + 
  ggtitle("Energy Consumption 2000-2013") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_text(angle = -270, hjust = 1)) + 
  labs(color="Zone")

#2.5 Population Income
#plotting population income
ggplot(avgenergy, aes(x=mean.YEAR.)) + 
  geom_line(aes(y=mean.Total.salary., color="Income"), size=2) + 
  geom_line(aes(y=mean.EPriceTotal., color="Energy Prices"), size=2)  + 
  geom_line(aes(y=mean.EsalesTotal., color="Energy Consumption"), size=2) + 
  scale_color_manual(values = c("darkmagenta", "brown2", "aquamarine2")) + 
  scale_y_continuous(name= "Currency", limits=c(5,30), breaks= c(5,10,15,20,30,25,30)) + 
  scale_x_continuous(name= "Year", limits=c(2000,2013), breaks= c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) + 
  ggtitle("Energy Trends 2000-2013") + theme_classic() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_text(angle = -270, hjust = 1)) + 
  labs(color="Trend")
 

##3. MODELLING ##

#removing NAs
energy<- drop_na(energy)

#checking that there are no more NAs left
sum(is.na(energy)) 

#data Partition into train and test sets
set.seed(1, sample.kind="Rounding")
test_index<-createDataPartition(y=energy$GenTotalRenewable, times=1, p=0.2, list=FALSE)
train_set<-energy %>% slice(-test_index)
test_set<-energy %>% slice (test_index)

#checking the size of each set
dim(train_set) 
dim(test_set)

#3.1 Linear Regression
#setting seed and first model with all variables
set.seed(1, sample.kind="Rounding")
lm1<-lm(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceResidential +EPriceCommercial + EPriceIndustrial + EPriceTransportation + EPriceTotal + EsalesResidential + EsalesCommercial + EsalesIndustrial +EsalesTransportation + EsalesTotal + CumlFinancial + CumlRegulatory + Total.salary + presidential.results, data=energy)
table1<-tab_model(lm1)
table1

#second model with only 9 variables, the significant ones and the important ones according to theory.
lm2<-lm(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceTransportation + EPriceTotal + EsalesTransportation + EsalesTotal + CumlFinancial + Total.salary, data=energy)
table2<-tab_model(lm2)
table2

#third model with only the 6 significant variables
lm3<-lm(GenTotalRenewable~ GenHydro + GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial + Total.salary, data=energy)
table3<-tab_model(lm3)
table3

#training the models in the training set, testing them in the test set, and obtaining the RMSE.
#first model with all variables
set.seed(1, sample.kind="Rounding")
train_lm1<-lm(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceResidential +EPriceCommercial + EPriceIndustrial + EPriceTransportation + EPriceTotal + EsalesResidential + EsalesCommercial + EsalesIndustrial +EsalesTransportation + EsalesTotal + CumlRegulatory + CumlFinancial + Total.salary + presidential.results, data=train_set)
yhat_trainlm1<-predict(train_lm1, newdata=test_set)
rmse_lm1<-sqrt(mean(yhat_trainlm1-test_set$GenTotalRenewable)^2)
rmse_lm1 

#second model with 9 variables
train_lm2<-lm(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceTransportation + EPriceTotal + EsalesTransportation + EsalesTotal + CumlFinancial + Total.salary, data=train_set)
yhat_trainlm2<-predict(train_lm2, newdata=test_set)
rmse_lm2<-sqrt(mean(yhat_trainlm2-test_set$GenTotalRenewable)^2)
rmse_lm2 

#third model with 6 variables
train_lm3<-lm(GenTotalRenewable~ GenHydro + GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, data=train_set)
yhat_trainlm3<-predict(train_lm3, newdata=test_set)
rmse_lm3<-sqrt(mean(yhat_trainlm3-test_set$GenTotalRenewable)^2)
rmse_lm3 

#3.2 Regression Trees
#visualizing the three models with its most important variables
#first model with all variables
set.seed(1, sample.kind="Rounding")
tree1<-rpart(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceResidential +EPriceCommercial + EPriceIndustrial + EPriceTransportation + EPriceTotal + EsalesResidential + EsalesCommercial + EsalesIndustrial +EsalesTransportation + EsalesTotal + CumlRegulatory + CumlFinancial + Total.salary, data=energy)
rpart.plot(tree1, type=3, digits=2, fallen.leaves=TRUE)
table_tree1<-varImp(tree1)
kable(table_tree1) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)

#second model with 9 variables
tree2<-rpart(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceTransportation + EPriceTotal + EsalesTransportation + EsalesTotal + CumlFinancial + Total.salary, data=energy)
rpart.plot(tree1, type=3, digits=2, fallen.leaves=TRUE)
table_tree2<-varImp(tree2)
kable(table_tree2) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)

#third model with 6 variables
tree3<-rpart(GenTotalRenewable~ GenHydro+ GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, data=energy)
rpart.plot(tree3, type=3, digits=2, fallen.leaves=TRUE)
table_tree3<-varImp(tree3)
kable(table_tree3) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)

#fourth model replacing GenHydro with GenTotal
tree4<-rpart(GenTotalRenewable~ GenTotal+ GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, data=energy)
rpart.plot(tree4, type=3, digits=2, fallen.leaves=TRUE)
table_tree4<-varImp(tree4)
kable(table_tree4) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)

#training the models in the training set, testing them in the test set, and obtaining the RMSE.
#first model with all variables
set.seed(1, sample.kind="Rounding")
train_tree1<-train(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceResidential +EPriceCommercial + EPriceIndustrial + EPriceTransportation + EPriceTotal + EsalesResidential + EsalesCommercial + EsalesIndustrial +EsalesTransportation + EsalesTotal + CumlRegulatory + CumlFinancial + Total.salary, method="rpart", data=train_set, na.action=na.roughfix)
yhat_train_tree1<-predict(train_tree1, newdata=test_set)
rmse_tree1<-sqrt(mean((yhat_train_tree1-test_set$GenTotalRenewable)^2))
rmse_tree1 

#second model with 9 variables
train_tree2<-train(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceTransportation + EPriceTotal + EsalesTransportation + EsalesTotal + CumlFinancial + Total.salary, method="rpart", data=train_set, na.action=na.roughfix)
yhat_train_tree2<-predict(train_tree2, newdata=test_set)
rmse_tree2<-sqrt(mean((yhat_train_tree2-test_set$GenTotalRenewable)^2))
rmse_tree2 

#third model with 6 variables
train_tree3<-train(GenTotalRenewable~ GenHydro+ GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, method="rpart", data=train_set, na.action=na.roughfix)
yhat_train_tree3<-predict(train_tree3, newdata=test_set)
rmse_tree3<-sqrt(mean((yhat_train_tree3-test_set$GenTotalRenewable)^2))
rmse_tree3 

#fourth model replacing GenHydro with GenTotal
train_tree4<-train(GenTotalRenewable~ GenTotal+ GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, method="rpart", data=train_set, na.action=na.roughfix)
yhat_train_tree4<-predict(train_tree4, newdata=test_set)
rmse_tree4<-sqrt(mean((yhat_train_tree4-test_set$GenTotalRenewable)^2))
rmse_tree4 

#3.3 Random Forest
#setting seed
set.seed(1, sample.kind="Rounding")

#first model with all variables
train_rf1<-train(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceResidential +EPriceCommercial + EPriceIndustrial + EPriceTransportation + EPriceTotal + EsalesResidential + EsalesCommercial + EsalesIndustrial +EsalesTransportation + EsalesTotal + CumlRegulatory + CumlFinancial + Total.salary, method="rf", data=train_set, na.action=na.roughfix)
yhat_train_rf1<-predict(train_rf1, newdata=test_set)
rmse_rf1<-sqrt(mean((yhat_train_rf1-test_set$GenTotalRenewable)^2))
rmse_rf1 

#second model with 9 variables
train_rf2<-train(GenTotalRenewable~GenTotal + GenHydro + GenSolar + EPriceTransportation + EPriceTotal + EsalesTransportation + EsalesTotal + CumlFinancial + Total.salary, method="rf", data=train_set, na.action=na.roughfix)
yhat_train_rf2<-predict(train_rf2, newdata=test_set)
rmse_rf2<-sqrt(mean(yhat_train_rf2-test_set$GenTotalRenewable)^2)
rmse_rf2 # 0.001517161

#third model with 6 variables
train_rf3<-train(GenTotalRenewable~ GenHydro+ GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, method="rf", data=train_set, na.action=na.roughfix)
yhat_train_rf3<-predict(train_rf3, newdata=test_set)
rmse_rf3<-sqrt(mean(yhat_train_rf3-test_set$GenTotalRenewable)^2)
rmse_rf3 #0.0009524423

#fourth model replacing GenHydro with GenTotal
train_rf4<-train(GenTotalRenewable~ GenTotal+ GenSolar + EPriceTransportation + EsalesTransportation + CumlFinancial+ Total.salary, method="rf", data=train_set, na.action=na.roughfix)
yhat_train_rf4<-predict(train_rf4, newdata=test_set)
rmse_rf4<-sqrt(mean(yhat_train_rf4-test_set$GenTotalRenewable)^2)
rmse_rf4 #0.002519054


##########################################################
# Results
##########################################################
#creating list with all RMSE's
Results <- tibble(Model="Linear Regression All Variables", RMSE= rmse_lm1)
Results<-bind_rows(Results, data_frame(Model="Linear Regression 9 Variables", RMSE=rmse_lm2))
Results<-bind_rows(Results, data_frame(Model="Linear Regression 6 Variables", RMSE=rmse_lm3))
Results<-bind_rows(Results, data_frame(Model="Regression Tree All Variabless", RMSE=rmse_tree1))
Results<-bind_rows(Results, data_frame(Model="Regression Tree 9 Variables", RMSE=rmse_tree2))
Results<-bind_rows(Results, data_frame(Model="Regression Tree 6 Variables", RMSE=rmse_tree3))
Results<-bind_rows(Results, data_frame(Model="Regression Tree No GenHydro",  RMSE=rmse_tree4))
Results<-bind_rows(Results, data_frame(Model="Random Forest All Variables", RMSE=rmse_rf1))
Results<-bind_rows(Results, data_frame(Model="Random Forest 9 Variables", RMSE=rmse_rf2))
Results<-bind_rows(Results, data_frame(Model="Random Forest 6 Variables", RMSE=rmse_rf3))
Results<-bind_rows(Results, data_frame(Model="Random Forest No GenHydro", RMSE=rmse_rf4))
kable(Results) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)

##########################################################