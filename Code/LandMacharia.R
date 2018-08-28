
#(STEP 1)
#Read the csv file into R 
gesareton <- read.csv("Gesareton.csv") #import the csv file into a dataframe in R
gesareton #display the dataframe

str(gesareton)
summary(gesareton)

#(STEP 2)
# Recoding values to missing 

gesareton$PolioVaccination[gesareton$PolioVaccination == 9] <- NA
gesareton$Sex[gesareton$Sex == 3] <- NA
gesareton$PolioDiagnosis[gesareton$PolioDiagnosis == 0] <- NA
gesareton$Age[gesareton$Age == 330] <- NA
gesareton$Age[gesareton$Age == 999] <- NA
gesareton$Age[gesareton$Age == 337] <- NA
gesareton$Age[gesareton$Age == 223] <- NA


#Check dataframe
newgesareton
summary(newgesareton)

#(STEP 3)
#Remove rows with missing data with na.omit() function 
newgesareton <- na.omit(gesareton)

#Specifying the elements of the dataframe
newgesareton[7:10] #indexing
newgesareton[c("Age", "Sex", "PolioVaccination")]
gesareton$PolioDiagnosis #Alternatives while plotting: attach,detach and with functions - Kabacoff pg 27
table(newgesareton$PolioDiagnosis, newgesareton$RespiratoryResults) #cross-tabulation

#Factors -- coercing values to factors. (STEP 3)
newgesareton$Sex <- factor(newgesareton$Sex, labels = c("Male", "Female"), levels = c(1, 2))
newgesareton$RespiratoryResults <- factor(newgesareton$RespiratoryResults, labels = c("MildARI","SevereLowerARI",
                                                                                      "Asthma"), levels = c(1, 2, 3))
newgesareton$PolioDiagnosis <- factor(newgesareton$PolioDiagnosis, labels = c("Yes","No"), levels = c(1, 2))
newgesareton$PolioVaccination <- factor(newgesareton$PolioVaccination, labels = c("None","1Dose","2Doses","3Doses"), levels = c(0, 1, 2, 3))


newgesareton$GivenName <- as.character(newgesareton$GivenName) # coerce GivenName into character mode
newgesareton$PolioDiagnosisDate <- as.character(newgesareton$PolioDiagnosisDate)
newgesareton$FamilyName <- as.character(newgesareton$FamilyName)

#SKIP THIS PART
# Creating bar plots
library(vcd)
counts <- table(newgesareton$PolioDiagnosis)
myplot <- barplot(counts,
                  main = "Polio Diagnosis Plot",
                  xlab = "Diagnosis",
                  ylab = "Frequency",
                  horiz = "FALSE")

#Creating a class interval for Age
attach(newgesareton)
breaks <- seq(from = 0, to = 20, by = 4)
pop <- cut(Age, breaks = breaks, right = FALSE, include.lowest = TRUE)
table(pop)
cbind(table(pop))
title <- cbind(table(pop))
colnames(title) <- c("Counts")
title
detach(newgesareton)

#Measuring skewness in Age
#Requires "moments" package
library(moments)
skewness(newgesareton$Age)

#Plotting Age frequency ## Has been re-done below...better.
library(ggplot2)
quickplot(newgesareton$Age, geom = "histogram",binwidth = 2, xlim = c(0,20)) + xlab('Age (yrs)') + ylab('No. of residents')


#(STEP 4)
#Frequency Tables 
ftSex <- with(newgesareton, table(Sex))
ftPolioVaccination <- with(newgesareton, table(PolioVaccination))
ftRespiratoryResults <- with(newgesareton, table(RespiratoryResults))
ftPolioDiagnosis <- with(newgesareton, table(PolioDiagnosis))

#Turn the frequencies into propotions or percentages with prop.table() function
pctftSex <- prop.table(ftSex)*100
pctftpolDia <- prop.table(ftPolioDiagnosis)*100
pctftrespRes <- prop.table(ftRespiratoryResults)*100
prop.table(ftPolioVaccination)*100

#Two way contingency tables using xtabs() function
sexResp <- xtabs(~Sex + RespiratoryResults, data = newgesareton)
sexPolDia <- xtabs(~Sex+PolioDiagnosis, data = newgesareton) #fisher's exact
sexPolVac <- xtabs(~Sex+PolioVaccination, data = newgesareton)
polVacResp <- xtabs(~PolioVaccination+RespiratoryResults, data = newgesareton)
polDiaPolVac <- xtabs(~PolioDiagnosis+PolioVaccination, data = newgesareton)
respResPolioVac <- xtabs(~RespiratoryResults+PolioVaccination, data = newgesareton) #better than polVacResp
polDiaResp <- xtabs(~PolioDiagnosis+RespiratoryResults, data = newgesareton)


#Marginal frequencies in contigency tables through margin.table() function
margin.table(sexResp, 1)
margin.table(sexPolDia, 2)
margin.table(polDiaPolVac, 2)
margin.table(title, 1) #Age margin.

#Proportions from contigency tables using prop.table() function
prop.table(polDiaPolVac, 1)*100
prop.table(polDiaResp, 1)*100
prop.table(polVacResp, 2)*100
prop.table(sexPolVac, 2)*100
propsexPolDia <- prop.table(sexPolDia, 2)*100
prop.table(sexResp, 1)*100
prop.table(margin.table(title, 1))*100 #Age proportion

#Add marginal sums to the tables using the addmargins() function
addmargins(sexPolVac)
addmargins(polVacResp)

addmargins(prop.table(polDiaPolVac, 1)*100, 1) #in terms of proportion
addmargins(prop.table(polVacResp, 2)*100, 1)

#Two way tables using CrossTable() function from gmodels package
CrossTable(newgesareton$PolioVaccination, newgesareton$RespiratoryResults, chisq = TRUE) #set argument chisq = TRUE for chisq statistic

#multidimensional tables
#three way contigency tables
twContTable <- xtabs(~Sex+RespiratoryResults+PolioVaccination, data = newgesareton)

#to print multidimensional tables in a compact,attractive manner.
ftable(xtabs(~Sex+RespiratoryResults+PolioVaccination, data = newgesareton))

margin.table(twContTable, 1) #marginal frequencies
margin.table(twContTable, c(1, 3)) #Sex X Polio vaccination marginal frequencies

ftable(prop.table(twContTable, c(1, 3))*100) #improved proportions for Sex X Respiratory results

ftable(addmargins(prop.table(twContTable, c(1, 2))*100, 3)) #sum of proportions

#(STEP 5)
#Tests for independence of the variables
#Chi-square test for independence
library(vcd)
chisq.test(sexResp) #some columns (eg NoSymptoms) with 0 observations hence chi-sq = NaN
chisq.test(xtabs(~PolioVaccination+RespiratoryResults, data = newgesareton))

#Fisher's Exact test
fisher.test(sexResp)
fisher.test(xtabs(~RespiratoryResults+PolioVaccination, data = newgesareton))

#Cochran-Mantel-Haenszel Test
#test of the null hypothesis that two nominal variables are conditionally independent in each stratum of a third variable.
mantelhaen.test(xtabs(~PolioVaccination+RespiratoryResults+Sex, data = newgesareton))

#Maximum-likelihood chi square test
library("DescTools")
GTest(polDiaResp)

#Measures of association between the variables
#assocstats() function in the vcd package
library(vcd)
assocstats(xtabs(~PolioDiagnosis+RespiratoryResults, data = newgesareton)) # includes pearson's chis-quare


#(STEP 6)
#Graphs
#Bar plots
#Respiratory Results on Polio Vaccination
barplot(respResPolioVac,
        #main="Respiratory Results on Polio Vaccination",
        xlab = "Polio Vaccination (No. of Doses)", ylab = "No. of Residents",
        ylim = c(0, 25),
        col = c("red", "green", "blue", "gray"),
        legend = rownames(respResPolioVac), beside = TRUE,
        legend.text = c("No Symptoms", "Mild ARI", "Severe Lower ARI", "Asthma"),
        args.legend = list(x = "topleft", inset = .05, title = "Respiratory Results"),
        names.arg = c("0 Dose", "1 Dose", "2 Dose", "3 Dose"))


#Polio Diagnosis with Respiratory infection
barplot(polDiaResp,
        #main="Polio Diagnosis on Respiratory infection",
        xlab = "Respiratory Results", ylab = "No. of Residents",
        ylim = c(0, 50),
        col = c("red", "blue"),
        legend = rownames(polDiaResp), beside = TRUE,
        legend.text = c("Positive", "Negative"),
        args.legend = list(x = "topleft", inset = .05, title = "Polio Diagnosis"),
        names.arg = c("No Symptoms", "Mild ARI", "Severe Lower ARI", "Asthma"))

#Respiratory results on Gender
barplot(sexResp,
        main = "Respiratory results on Gender",
        xlab = "Respiratory Results", ylab = "Frequency",
        ylim = c(0, 30),
        col = c("blue", "black"),
        legend = rownames(sexResp), beside = TRUE,
        legend.text = c("Male", "Female"),
        args.legend = list(x = "topleft", inset = .05, title = "Respiratory Results"),
        names.arg = c("No Symptoms", "Mild ARI", "Severe Lower ARI", "Asthma"))

#Polio diagnosis on Gender
barplot(sexPolDia,
        main="Polio Diagnosis on Gender",
        xlab = "Polio Diagnosis", ylab = "No. of Residents",
        ylim = c(0, 55),
        col = c("red", "green"),
        legend = rownames(sexPolDia), beside = TRUE,
        legend.text = c("Male", "Female"),
        args.legend = list(x = "topright", inset = .05, title = "Polio Diagnosis"),
        names.arg = c("Yes", "No"))

#Polio diagnosis on Polio Vaccination
barplot(polDiaPolVac,
        #main="Polio Diagnosis on Polio Vaccination",
        xlab = "No. of Doses", ylab = "No. of Residents",
        ylim = c(0, 35),
        col = c("purple", "black"),
        legend = rownames(polDiaPolVac), beside = TRUE,
        legend.text = c("Yes", "No"),
        args.legend = list(x = "topleft", inset = .05, title = "Polio Diagnosis"),
        names.arg = c("No Dose","1 Dose","2 Doses","3 Doses"))

#Polio vaccination frequency
barplot(prop.table(ftPolioVaccination)*100,
        main = "Polio Vaccination Frequency",
        xlab = "No. of Doses", ylab = "Vaccinated Residents (%)",
        ylim = c(0, 40),
        col = ("blue"),
        names.arg = c("None", "1 Dose", "2 Doses", "3 Doses"))

#Age frequency
barplot(title,
        main = "Age distribution",
        xlab = "Age Categories (yrs)", ylab = "No. of Residents",
        ylim = c(0, 40),
        beside = TRUE,
        col = c("black", "light gray", "dark blue", "magenta", "brown"),
        names.arg = c("0-3", "4-7", "8-11", "12-15", "16-19"))

#Pie Charts
#Gender distribution
lbls1 <- paste(names(pctftSex), "\n", pctftSex, sep = "")
pie(pctftSex, labels= lbls1,
    main = "Gender distribution in Gesareton")


#Polio Diagnosis frequency pie chart
lbls2 <- paste(names(pctftpolDia), "\n", pctftpolDia, sep = "")
pie(pctftpolDia, labels= lbls2,
    #main = "Polio Diagnosis frequency in Gesareton",
    col = c("blue", "red"))

#Respiratory results distribution Pie chart
lbls3 <- paste(names(pctftrespRes), "\n", pctftrespRes, sep = "")
pie(pctftrespRes, labels= lbls3,
    #main = "Respiratory results distribution",
    col = c("Blue", "Purple", "orange", "Red"))











