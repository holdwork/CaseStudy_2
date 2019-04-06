library(readxl)
library(ggplot2)
library(dplyr)
library(psych)
library(Hmisc)
library(mlr)
data1 <- read_excel("C:/Users/jhold/Desktop/case study 2/CaseStudy2-data.xlsx")
str(data1)

tbl_1 <- table(data1$Age)
cbind( Freq=tbl_1, Cumul=cumsum(tbl_1), relative=prop.table(tbl_1))

FreqTable <- function(X){ 
  Table <- data.frame( table(X) ) 
  Table$CumFreq <- cumsum(Table$Freq)
  Table$Prop <- prop.table( Table$Freq ) 
  Table$CumProp <-  cumsum( Table$Prop ) 
  Table 
} 
ft1 <- FreqTable(data1$Age)
head(ft1)
ggplot(data=ft1, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("Age") +ggtitle("Age Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft1, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("Age") +ggtitle("Age Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$Age)
agNAcnt <- sum(is.na(data1$DailyRate))

ft2 <- FreqTable(data1$DailyRate)
hist(data1$DailyRate, main="Distribution of DailyRate", breaks=20, xlab="DailyRate", border="black", col="blue", xlim=c(100,1500)) 
ggplot(data=ft2, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("DailyRate") +ggtitle("DailyRate Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$DailyRate)
drNAcnt <- sum(is.na(data1$DailyRate))
 
ft3 <- FreqTable(data1$DistanceFromHome)
hist(data1$DistanceFromHome, main="Distribution of DistanceFromHome", breaks=10, xlab="DistanceFromHome", border="black", col="blue", xlim=c(1,29)) 
ggplot(data=ft3, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("DistanceFromHome") +ggtitle("DistanceFromHome  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$DistanceFromHome )
dfhNAcnt <- sum(is.na(data1$DistanceFromHome ))

ft4 <- FreqTable(data1$Education)
ggplot(data=ft4, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("Education") +ggtitle("Education  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft4, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("Education") +ggtitle("Education  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$Education )
edNAcnt <- sum(is.na(data1$Education ))

ft5 <- FreqTable(data1$EmployeeCount )
ggplot(data=ft5, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("EmployeeCount ") +ggtitle("EmployeeCount   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft5, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("EmployeeCount ") +ggtitle("EmployeeCount   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$EmployeeCount  )
empcNAcnt <- sum(is.na(data1$EmployeeCount  ))

ft6 <- FreqTable(data1$EnvironmentSatisfaction)
ggplot(data=ft6, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("EnvironmentSatisfaction") +ggtitle("EnvironmentSatisfaction  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft6, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("EnvironmentSatisfaction") +ggtitle("EnvironmentSatisfaction  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$EnvironmentSatisfaction )
envsNAcnt <- sum(is.na(data1$EnvironmentSatisfaction ))

ft7 <- FreqTable(data1$HourlyRate)
hist(data1$HourlyRate, main="Distribution of HourlyRate", breaks=10, xlab="HourlyRate", border="black", col="blue", xlim=c(30,100)) 
ggplot(data=ft7, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("HourlyRate") +ggtitle("HourlyRate  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$HourlyRate )
hrNAcnt <- sum(is.na(data1$HourlyRate ))

ft8 <- FreqTable(data1$JobInvolvement)
ggplot(data=ft8, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("JobInvolvement") +ggtitle("JobInvolvement  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft8, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("JobInvolvement") +ggtitle("JobInvolvement  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$JobInvolvement )
jobinNAcnt <- sum(is.na(data1$JobInvolvement ))

ft9 <- FreqTable(data1$JobLevel)
ggplot(data=ft9, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("JobLevel") +ggtitle("JobLevel  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft9, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("JobLevel") +ggtitle("JobLevel  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$JobLevel )
joblNAcnt <- sum(is.na(data1$JobLevel ))

ft10 <- FreqTable(data1$JobSatisfaction)
ggplot(data=ft10, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("JobSatisfaction") +ggtitle("JobSatisfaction  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft10, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("JobSatisfaction") +ggtitle("JobSatisfaction  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$JobSatisfaction )
jobsaNAcnt <- sum(is.na(data1$JobSatisfaction ))

ft11 <- FreqTable(data1$MonthlyIncome  )
hist(data1$MonthlyIncome, main="Distribution of MonthlyIncome", breaks=50, xlab="MonthlyIncome", border="black", col="blue", xlim=c(1000,20000)) 
ggplot(data=ft11, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("MonthlyIncome  ") +ggtitle("MonthlyIncome    Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$MonthlyIncome   )
miNAcnt <- sum(is.na(data1$MonthlyIncome   ))

ft12 <- FreqTable(data1$MonthlyRate)
hist(data1$MonthlyRate, main="Distribution of MonthlyRate", breaks=50, xlab="MonthlyRate", border="black", col="blue", xlim=c(2090,27000)) 
ggplot(data=ft12, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("MonthlyRate") +ggtitle("MonthlyRate  Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$MonthlyRate )
monrNAcnt <- sum(is.na(data1$MonthlyRate ))

ft13 <- FreqTable(data1$NumCompaniesWorked )
ggplot(data=ft13, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("NumCompaniesWorked ") +ggtitle("NumCompaniesWorked   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft13, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("NumCompaniesWorked ") +ggtitle("NumCompaniesWorked   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$NumCompaniesWorked  )
ncwNAcnt <- sum(is.na(data1$NumCompaniesWorked))

ft14 <- FreqTable(data1$PercentSalaryHike )
ggplot(data=ft14, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("PercentSalaryHike ") +ggtitle("PercentSalaryHike   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft14, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("PercentSalaryHike ") +ggtitle("PercentSalaryHike   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$PercentSalaryHike  )
pshNAcnt <- sum(is.na(data1$PercentSalaryHike))

ft15 <- FreqTable(data1$PerformanceRating)
ggplot(data=ft15, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("PerformanceRating ") +ggtitle("PerformanceRating   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft15, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("PerformanceRating ") +ggtitle("PerformanceRating   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$PerformanceRating  )
prNAcnt <- sum(is.na(data1$PerformanceRating))

ft16 <- FreqTable(data1$RelationshipSatisfaction )
ggplot(data=ft16, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("RelationshipSatisfaction ") +ggtitle("RelationshipSatisfaction   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft16, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("RelationshipSatisfaction ") +ggtitle("RelationshipSatisfaction   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$RelationshipSatisfaction  )
relNAcnt <- sum(is.na(data1$RelationshipSatisfaction  ))

ft17 <- FreqTable(data1$StandardHours)
ggplot(data=ft17, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("StandardHours ") +ggtitle("StandardHours   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft17, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("StandardHours ") +ggtitle("StandardHours   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$StandardHours)
shNAcnt <- sum(is.na(data1$StandardHours))

ft18 <- FreqTable(data1$StockOptionLevel)
ggplot(data=ft18, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("StockOptionLevel ") +ggtitle("StockOptionLevel   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft18, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("StockOptionLevel ") +ggtitle("StockOptionLevel   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$StockOptionLevel)
sopNAcnt <- sum(is.na(data1$StockOptionLevel))

ft19 <- FreqTable(data1$TotalWorkingYears)
hist(data1$TotalWorkingYears, main="TotalWorkingYears", breaks=15, xlab="TotalWorkingYears", border="black", col="blue", xlim=c(0,40)) 
ggplot(data=ft19, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("TotalWorkingYears ") +ggtitle("TotalWorkingYears   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$TotalWorkingYears)
twyNAcnt <- sum(is.na(data1$TotalWorkingYears))

ft20 <- FreqTable(data1$TrainingTimesLastYear)
ggplot(data=ft20, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("TrainingTimesLastYear ") +ggtitle("TrainingTimesLastYear   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft20, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("TrainingTimesLastYear ") +ggtitle("TrainingTimesLastYear   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$TrainingTimesLastYear)
ttlyNAcnt <- sum(is.na(data1$TrainingTimesLastYear))

ft21 <- FreqTable(data1$WorkLifeBalance)
ggplot(data=ft21, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("WorkLifeBalance ") +ggtitle("WorkLifeBalance   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft21, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("WorkLifeBalance ") +ggtitle("WorkLifeBalance   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$WorkLifeBalance)
wlbNAcnt <- sum(is.na(data1$WorkLifeBalance))

ft22 <- FreqTable(data1$YearsAtCompany)
hist(data1$YearsAtCompany, main="YearsAtCompany", breaks=15, xlab="YearsAtCompany", border="black", col="blue", xlim=c(0,40)) 
ggplot(data=ft22, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsAtCompany ") +ggtitle("YearsAtCompany   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$YearsAtCompany)
yacNAcnt <- sum(is.na(data1$YearsAtCompany))

ft23 <- FreqTable(data1$YearsInCurrentRole)
ggplot(data=ft23, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsInCurrentRole ") +ggtitle("YearsInCurrentRole   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft23, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsInCurrentRole ") +ggtitle("YearsInCurrentRole   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$YearsInCurrentRole)
ycrNAcnt <- sum(is.na(data1$YearsInCurrentRole))

ft24 <- FreqTable(data1$YearsSinceLastPromotion)
ggplot(data=ft24, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsSinceLastPromotion ") +ggtitle("YearsSinceLastPromotion   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft24, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsSinceLastPromotion ") +ggtitle("YearsSinceLastPromotion   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$YearsSinceLastPromotion)
yspNAcnt <- sum(is.na(data1$YearsSinceLastPromotion))

ft25 <- FreqTable(data1$YearsWithCurrManager)
ggplot(data=ft25, aes(x=X, y=Freq)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsWithCurrManager ") +ggtitle("YearsWithCurrManager   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
ggplot(data=ft25, aes(x=X, y=CumProp)) +geom_bar(stat="identity") +ylab("Freq") + xlab("YearsWithCurrManager ") +ggtitle("YearsWithCurrManager   Histogram")+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
describe(data1$YearsWithCurrManager)
ywcmNAcnt <- sum(is.na(data1$YearsWithCurrManager))

ft26 <- FreqTable(data1$Attrition)
ft27 <- FreqTable(data1$BusinessTravel)
ft28 <- FreqTable(data1$Department)
ft29 <- FreqTable(data1$EducationField)
ft30 <- FreqTable(data1$Gender)
ft31 <- FreqTable(data1$JobRole)
ft32 <- FreqTable(data1$MaritalStatus)
ft33 <- FreqTable(data1$Over18)
ft34 <- FreqTable(data1$OverTime)

t.test(data1$Age~data1$Attrition)
t.test(data1$DailyRate~data1$Attrition)
t.test(data1$DistanceFromHome~data1$Attrition)
t.test( data1$Education~data1$Attrition)
t.test( data1$EmployeeCount~data1$Attrition)
t.test( data1$EnvironmentSatisfaction~data1$Attrition)
t.test( data1$HourlyRate~data1$Attrition)
t.test( data1$JobInvolvement~data1$Attrition)
t.test( data1$JobLevel~data1$Attrition)
t.test( data1$JobSatisfaction~data1$Attrition)
t.test( data1$MonthlyIncome~data1$Attrition)
t.test( data1$MonthlyRate~data1$Attrition)
t.test( data1$NumCompaniesWorked~data1$Attrition)
t.test( data1$PercentSalaryHike~data1$Attrition)
t.test( data1$PerformanceRating~data1$Attrition)
t.test( data1$RelationshipSatisfaction~data1$Attrition)
t.test( data1$StandardHours~data1$Attrition)
t.test( data1$StockOptionLevel~data1$Attrition)
t.test( data1$TotalWorkingYears~data1$Attrition)
t.test( data1$TrainingTimesLastYear~data1$Attrition)
t.test( data1$WorkLifeBalance~data1$Attrition)
t.test( data1$YearsAtCompany~data1$Attrition)
t.test( data1$YearsInCurrentRole~data1$Attrition)
t.test( data1$YearsSinceLastPromotion~data1$Attrition)
t.test( data1$YearsWithCurrManager~data1$Attrition)

ct1 <- table(data1$RelationshipSatisfaction,data1$Attrition)
ct2 <- table(data1$BusinessTravel,data1$Attrition)
ct3 <- table(data1$Department,data1$Attrition)
ct4 <- table(data1$EducationField,data1$Attrition)
ct5 <- table(data1$Gender,data1$Attrition)
ct6 <- table(data1$JobRole,data1$Attrition)
ct7 <- table(data1$MaritalStatus,data1$Attrition)
ct8 <- table(data1$Over18,data1$Attrition)
ct9 <- table(data1$OverTime,data1$Attrition)
ct10 <- table(data1$JobSatisfaction,data1$Attrition)
ct11 <- table(data1$EnvironmentSatisfaction,data1$Attrition)
ct12 <- table(data1$Education,data1$Attrition)
chisq.test(ct1)
chisq.test(ct2)
chisq.test(ct3)
chisq.test(ct4)
chisq.test(ct5)
chisq.test(ct6)
chisq.test(ct7)
chisq.test(ct8)
chisq.test(ct9)
chisq.test(ct10)
chisq.test(ct11)
chisq.test(ct12)


ct2 
ct3 
ct4 
ct6 
ct7 
ct9 
ct10
ct11 

str(data1)

cor.test( ~ Age + DailyRate,data=data1,method = "pearson")
cor.test( ~ Age + DistanceFromHome ,data=data1,method = "pearson")
cor.test( ~ Age + EnvironmentSatisfaction,data=data1,method = "pearson")
cor.test( ~ Age + JobInvolvement, data=data1,method = "pearson")
cor.test( ~ Age + JobLevel ,data=data1,method = "pearson")
cor.test( ~ Age + JobSatisfaction ,data=data1,method = "pearson")
cor.test( ~ Age + MonthlyIncome,data=data1,method = "pearson")
cor.test( ~ Age + JobInvolvement, data=data1,method = "pearson")

x <- data1[,c(1,4,6,11,14,15,17,19,24,28,29,30,31,32,33,35)]
y <- data1[,c(1,4,6,11,14,15,17,19,24,28,29,30,31,32,33,35)]
head(x)

corm <-cor(x)
round(corm,2)
pc1 <- princomp(x, cor=TRUE)
summary(pc1) # print variance accounted for 
loadings(pc1) # pc loadings 
plot(pc1,type="lines") # scree plot 

pc2 <- principal(x, nfactors=4, rotate="varimax")
pc3 <- principal(x, nfactors=3, rotate="varimax")
pc4 <- principal(x, nfactors=3, rotate="promax")
pc5 <- principal(x, nfactors=2, rotate="promax")

data1$sex[data1$Gender=="Female"]  = 2
data1$sex[data1$Gender=="Male"]  = 1
data1$sex <- as.factor(data1$sex)
FreqTable(data1$sex)
data1$AttNum[data1$Attrition=="Yes"]  = 1
data1$AttNum[data1$Attrition=="No"]  = 0
data1$AttNum <- as.factor(data1$AttNum)
FreqTable(data1$AttNum)

data1$FreqTravel[data1$BusinessTravel=="Travel_Frequently"]  = 1
data1$FreqTravel[data1$BusinessTravel=="Travel_Rarely"]  = 0
data1$FreqTravel[data1$BusinessTravel=="Non-Travel"]  = 0
data1$FreqTravel <- as.factor(data1$FreqTravel)
data1$NonTravel[data1$BusinessTravel=="Travel_Frequently"]  = 0
data1$NonTravel[data1$BusinessTravel=="Travel_Rarely"]  = 0
data1$NonTravel[data1$BusinessTravel=="Non-Travel"]  = 1
data1$NonTravel <- as.factor(data1$NonTravel)
data1$RDDept[data1$Department=="Human Resources"]  = 0
data1$RDDept[data1$Department=="Research & Development"]  = 1
data1$RDDept[data1$Department=="Sales"]  = 0
data1$RDDept <- as.factor(data1$RDDept)
data1$HRdegree[data1$EducationField=="Life Sciences"]  = 0
data1$HRdegree[data1$EducationField=="Human Resources"]  = 1
data1$HRdegree[data1$EducationField=="Marketing"]  = 0
data1$HRdegree[data1$EducationField=="Other"]  = 0
data1$HRdegree[data1$EducationField=="Medical"]  = 0
data1$HRdegree[data1$EducationField=="Technical Degree"]  = 0
data1$HRdegree <- as.factor(data1$HRdegree)
data1$LSdegree[data1$EducationField=="Life Sciences"]  = 1
data1$LSdegree[data1$EducationField=="Human Resources"]  = 0
data1$LSdegree[data1$EducationField=="Marketing"]  = 0
data1$LSdegree[data1$EducationField=="Other"]  = 0
data1$LSdegree[data1$EducationField=="Medical"]  = 0
data1$LSdegree[data1$EducationField=="Technical Degree"]  = 0
data1$LSdegree <- as.factor(data1$LSdegree)
data1$MKdegree[data1$EducationField=="Life Sciences"]  = 0
data1$MKdegree[data1$EducationField=="Human Resources"]  = 0
data1$MKdegree[data1$EducationField=="Marketing"]  = 1
data1$MKdegree[data1$EducationField=="Other"]  = 0
data1$MKdegree[data1$EducationField=="Medical"]  = 0
data1$MKdegree[data1$EducationField=="Technical Degree"]  = 0
data1$MKdegree <- as.factor(data1$MKdegree)
data1$MDdegree[data1$EducationField=="Life Sciences"]  = 0
data1$MDdegree[data1$EducationField=="Human Resources"]  = 0
data1$MDdegree[data1$EducationField=="Marketing"]  = 0
data1$MDdegree[data1$EducationField=="Other"]  = 0
data1$MDdegree[data1$EducationField=="Medical"]  = 1
data1$MDdegree[data1$EducationField=="Technical Degree"]  = 0
data1$MDdegree <- as.factor(data1$MDdegree)
data1$TDdegree[data1$EducationField=="Life Sciences"]  = 0
data1$TDdegree[data1$EducationField=="Human Resources"]  = 0
data1$TDdegree[data1$EducationField=="Marketing"]  = 0
data1$TDdegree[data1$EducationField=="Other"]  = 0
data1$TDdegree[data1$EducationField=="Medical"]  = 0
data1$TDdegree[data1$EducationField=="Technical Degree"]  = 1
data1$TDdegree <- as.factor(data1$TDdegree)
data1$HeathREPjob[data1$JobRole=="Healthcare Representative"]  = 1
data1$HeathREPjob[data1$JobRole=="Human Resources"]  = 0
data1$HeathREPjob[data1$JobRole=="Laboratory Technician"]  = 0
data1$HeathREPjob[data1$JobRole=="Manager"]  = 0
data1$HeathREPjob[data1$JobRole=="Manufacturing Director"]  = 0
data1$HeathREPjob[data1$JobRole=="Research Director"]  = 0
data1$HeathREPjob[data1$JobRole=="Research Scientist"]  = 0
data1$HeathREPjob[data1$JobRole=="Sales Executive"]  = 0
data1$HeathREPjob[data1$JobRole=="Sales Representative"]  = 0
data1$HeathREPjob <- as.factor(data1$HeathREPjob)
data1$HRjob[data1$JobRole=="Healthcare Representative"]  = 0
data1$HRjob[data1$JobRole=="Human Resources"]  = 1
data1$HRjob[data1$JobRole=="Laboratory Technician"]  = 0
data1$HRjob[data1$JobRole=="Manager"]  = 0
data1$HRjob[data1$JobRole=="Manufacturing Director"]  = 0
data1$HRjob[data1$JobRole=="Research Director"]  = 0
data1$HRjob[data1$JobRole=="Research Scientist"]  = 0
data1$HRjob[data1$JobRole=="Sales Executive"]  = 0
data1$HRjob[data1$JobRole=="Sales Representative"]  = 0
data1$HRjob <- as.factor(data1$HRjob)
data1$LabTechjob[data1$JobRole=="Healthcare Representative"]  = 0
data1$LabTechjob[data1$JobRole=="Human Resources"]  = 0
data1$LabTechjob[data1$JobRole=="Laboratory Technician"]  = 1
data1$LabTechjob[data1$JobRole=="Manager"]  = 0
data1$LabTechjob[data1$JobRole=="Manufacturing Director"]  = 0
data1$LabTechjob[data1$JobRole=="Research Director"]  = 0
data1$LabTechjob[data1$JobRole=="Research Scientist"]  = 0
data1$LabTechjob[data1$JobRole=="Sales Executive"]  = 0
data1$LabTechjob[data1$JobRole=="Sales Representative"]  = 0
data1$LabTechjob <- as.factor(data1$LabTechjob)
data1$Managerjob[data1$JobRole=="Healthcare Representative"]  = 0
data1$Managerjob[data1$JobRole=="Human Resources"]  = 0
data1$Managerjob[data1$JobRole=="Laboratory Technician"]  = 0
data1$Managerjob[data1$JobRole=="Manager"]  = 1
data1$Managerjob[data1$JobRole=="Manufacturing Director"]  = 0
data1$Managerjob[data1$JobRole=="Research Director"]  = 0
data1$Managerjob[data1$JobRole=="Research Scientist"]  = 0
data1$Managerjob[data1$JobRole=="Sales Executive"]  = 0
data1$Managerjob[data1$JobRole=="Sales Representative"]  = 0
data1$Managerjob <- as.factor(data1$Managerjob)

data1$ManDirjob <- 0
data1$ManDirjob[data1$JobRole=="Manufacturing Director"]  = 1
data1$ManDirjob <- as.factor(data1$ManDirjob)

data1$ResDirjob <- 0
data1$ResDirjob[data1$JobRole=="Research Director"]  = 1
data1$ResDirjob <- as.factor(data1$ResDirjob)

data1$ResScijob <- 0
data1$ResScijob[data1$JobRole=="Research Scientist"]  = 1
data1$ResScijob <- as.factor(data1$ResScijob)

data1$SalExecjob <- 0
data1$SalExecjob[data1$JobRole=="Sales Executive"]  = 1
data1$SalExecjob <- as.factor(data1$SalExecjob)

data1$SalRepjob <- 0
data1$SalRepjob[data1$JobRole=="Sales Representative"]  = 1
data1$SalRepjob <- as.factor(data1$SalRepjob)

data1$Managejob <- 0
data1$Managejob[data1$JobRole=="Sales Executive"]  = 1
data1$Managejob[data1$JobRole=="Research Director"]  = 1
data1$Managejob[data1$JobRole=="Manufacturing Director"]  = 1
data1$Managejob[data1$JobRole=="Manager"]  = 1
data1$Managejob <- as.factor(data1$Managejob)



data1$Married <- 0
data1$Married[data1$MaritalStatus=="Married"]  = 1
data1$Married<- as.factor(data1$Married)

data1$Divorced <- 0
data1$Divorced[data1$MaritalStatus=="Divorced"]  = 1
data1$Divorced<- as.factor(data1$Divorced)

data1$Single <- 0
data1$Single[data1$MaritalStatus=="Single"]  = 1
data1$Single<- as.factor(data1$Single)

data1$OT <- 0
data1$OT[data1$OverTime=="Yes"]  = 1
data1$OT<- as.factor(data1$OT)

FreqTable(data1$OT)
table(data1$OT,data1$OverTime)


data1$id<- 1:nrow(data1)
data1train <- data1 %>% dplyr::sample_frac(.75)
data1test  <- dplyr::anti_join(data1, data1train, by = 'id')

str(data1train)
str(data1test)

logout1 <- glm(AttNum ~ OT, data=data1, family="binomial")
summary(logout1)

trainTask <- makeClassifTask(data = data1train,target = "data1$AttNum ")
testTask <- makeClassifTask(data = data1test, target = "data1$AttNum ")
