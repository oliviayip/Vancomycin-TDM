#################################################################################
# Title:          Model-Informed Precision Dosing of Vancomycin                 #
# Programmer:     Olivia Yip                                                    #
# Date:           05 May 2023                                                  #
# Updated:        NA                                                            #
# Updated by:     NA                                                            #
# Notes:                                                                        #
#################################################################################

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)

vancoTDM <- read_excel("/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Vancomycin/TDMvancomycin.xlsx")

#Load Rdata file (after first time executing code)-------------------------------------------------

save(vancoTDM, file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Vancomycin/TDMvancomycin.Rdata")
load(file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Vancomycin/VancoTDM.Rdata")

vancoTDM = data.frame(vancoTDM)

#Convert all variable names to lowercase 
names(vancoTDM) <- tolower(names(vancoTDM))
vancoTDM$weight <- as.numeric(vancoTDM$weight)
vancoTDM$scr <- as.numeric(vancoTDM$scr)
vancoTDM$auc <- as.numeric(vancoTDM$auc)
vancoTDM$clind_precisepk <- as.numeric(vancoTDM$clind_precisepk)
vancoTDM$clind_shinyapps <- as.numeric(vancoTDM$clind_shinyapps)

#AUC count
auccount <- subset(vancoTDM, auc != "")

str(vancoTDM)
summary(vancoTDM)

#Names: 
names(vancoTDM)

#subset of patients with ID (first collection/ first row)
patientcount <- subset(vancoTDM, id != "")

#Patient Demographics
summary(vancoTDM$age, na.rm = TRUE)
sd(vancoTDM$age,na.rm = TRUE)
summary(vancoTDM$weight, na.rm = TRUE)
sd(vancoTDM$weight,na.rm = TRUE)
summary(vancoTDM$height, na.rm = TRUE)
sd(vancoTDM$height, na.rm = TRUE)
summary(patientcount$scr, na.rm = TRUE)
sd(patientcount$scr, na.rm = TRUE)
summary(vancoTDM$auc, na.rm = TRUE)
sd(vancoTDM$auc, na.rm = TRUE)

#Comparing Precise PK and Shinyapps - CL

summary(vancoTDM$clind_precisepk, na.rm = TRUE)
sd(vancoTDM$clind_precisepk,na.rm = TRUE)
summary(vancoTDM$clind_shinyapps, na.rm = TRUE)
sd(vancoTDM$clind_shinyapps, na.rm = TRUE)
t.test(vancoTDM$clind_precisepk, vancoTDM$clind_shinyapps, paired = TRUE, alternative = "two.sided", na.rm = TRUE)

#Comparing Precise PK and Shinyapps - Vd

summary(vancoTDM$vdind_precisepk, na.rm = TRUE)
sd(vancoTDM$vdind_precisepk, na.rm = TRUE)
summary(vancoTDM$vdind_shinyapps, na.rm = TRUE)
sd(vancoTDM$vdind_shinyapps, na.rm = TRUE)
t.test(vancoTDM$vdind_precisepk, vancoTDM$vdind_shinyapps, paired = TRUE, alternative = "two.sided", na.rm = TRUE)

#Comparing Precise PK and Shinyapps - AUCss

summary(vancoTDM$aucss, na.rm = TRUE)
sd(vancoTDM$aucss, na.rm = TRUE)
summary(vancoTDM$auc_shinyapps, na.rm = TRUE)
sd(vancoTDM$auc_shinyapps, na.rm = TRUE)
t.test(vancoTDM$aucss, vancoTDM$auc_shinyapps, paired = TRUE, alternative = "two.sided", na.rm = TRUE)


# #Comparing AUC nonbayesian and AUC  precise PK
# summary(vancoTDM$auc_precisepk, na.rm = TRUE)
# sd(vancoTDM$auc_precisepk, na.rm = TRUE)
# summary(vancoTDM$auc_nonbayesian, na.rm = TRUE)
# sd(vancoTDM$auc_nonbayesian, na.rm = TRUE)
# t.test(vancoTDM$auc_precisepk, vancoTDM$auc_nonbayesian, paired = TRUE, alternative = "two.sided", na.rm = TRUE)
# 
# 
# #Comparing AUC nonbayesian and AUC  precise PK
# summary(vancoTDM$auc_nonbayesian, na.rm = TRUE)
# sd(vancoTDM$auc_nonbayesian, na.rm = TRUE)
# summary(vancoTDM$aucss_shinyapps, na.rm = TRUE)
# sd(vancoTDM$aucss_shinyapps, na.rm = TRUE)
# t.test(vancoTDM$aucss_shinyapps, vancoTDM$auc_nonbayesian, paired = TRUE, alternative = "two.sided", na.rm = TRUE)


#Comparing shinyapp AUC with precise PK (standard) 

accuracy_shinyapps = 0
n_ashiny = 0
for (x in 1:nrow(vancoTDM)) {
  if(!is.na((vancoTDM$aucss[x])) && !is.na((vancoTDM$auc_shinyapps[x]))) {
    accuracy_shinyapps = accuracy_shinyapps+ ((vancoTDM$auc_shinyapps [x] - vancoTDM$aucss [x])/ vancoTDM$aucss[x])
    n_ashiny=n_ashiny+1
    }
}
accuracy_shinyapps = accuracy_shinyapps/n_ashiny

#BOXPLOT - accuracy
vancoTDM$accuracy = 0
for (x in 1:nrow(vancoTDM)) {
  if(!is.na((vancoTDM$aucss[x])) && !is.na((vancoTDM$auc_shinyapps[x]))) {
    vancoTDM$accuracy[x] = vancoTDM$accuracy[x]+ ((vancoTDM$auc_shinyapps [x] - vancoTDM$aucss [x])/ vancoTDM$aucss[x])
  }
}
vancoTDM$accuracy[vancoTDM$accuracy == 0] <- NA


precision_shinyapps = 0
n_pshiny = 0
for (x in 1:nrow(vancoTDM)) {
  if((!is.na(vancoTDM$aucss[x])) && (!is.na(vancoTDM$auc_shinyapps[x]))) {
    precision_shinyapps = precision_shinyapps + abs((vancoTDM$auc_shinyapps[x] - vancoTDM$aucss[x])/vancoTDM$aucss[x])
    n_pshiny = n_pshiny +1 
    }
}
precision_shinyapps = precision_shinyapps/ n_pshiny
#BOXPLOT - precision
vancoTDM$precision = 0


for (x in 1:nrow(vancoTDM)) {
  if((!is.na(vancoTDM$aucss[x])) && (!is.na(vancoTDM$auc_shinyapps[x]))) {
    vancoTDM$precision[x] = vancoTDM$precision[x] + abs((vancoTDM$auc_shinyapps[x] - vancoTDM$aucss[x])/vancoTDM$aucss[x])
    
  }
}
vancoTDM$precision[vancoTDM$precision == 0] <- NA


# #Comparing nonbayesian AUC with precise PK (standard) 
# accuracy_nonbayesian = 0
# n_anonbay = 0
# 
# for (x in 1:nrow(vancoTDM)) {
#   if(!is.na((vancoTDM$auc_precisepk[x])) && !is.na((vancoTDM$auc_nonbayesian[x]))) {
#     accuracy_nonbayesian  = accuracy_nonbayesian  + ((vancoTDM$auc_nonbayesian [x] - vancoTDM$auc_precisepk [x])/ vancoTDM$auc_precisepk[x])
#     n_anonbay = n_anonbay + 1
#     }
# }
# accuracy_nonbayesian = accuracy_nonbayesian/n_anonbay
# 
# 
# precision_nonbayesian = 0
# n_pnonbay = 0
# for (x in 1:nrow(vancoTDM)) {
#   if(!is.na((vancoTDM$auc_precisepk[x])) && !is.na((vancoTDM$auc_nonbayesian[x]))) {
#     precision_nonbayesian = precision_nonbayesian + abs(((vancoTDM$auc_nonbayesian [x] - vancoTDM$auc_precisepk [x]))/ vancoTDM$auc_precisepk [x])
#     n_pnonbay = n_pnonbay + 1
#     }
# }
# precision_nonbayesian = precision_nonbayesian/ n_pnonbay
# 
# 
# #using shinyapp as the gold standard
# 
# accuracy = 0
# n_acc = 0
# for (x in 1:nrow(vancoTDM)) {
#   if(!is.na((vancoTDM$aucss_shinyapps[x])) && !is.na((vancoTDM$auc_nonbayesian[x]))) {
#     accuracy  = accuracy  + ((vancoTDM$auc_nonbayesian [x] - vancoTDM$aucss_shinyapps [x])/ vancoTDM$aucss_shinyapps[x])
#     n_acc = n_acc +1
#     }
# }
# accuracy = accuracy/n_acc
# 
# 
# precision = 0
# n_pre = 0
# for (x in 1:nrow(vancoTDM)) {
#   if(!is.na((vancoTDM$aucss_shinyapps[x])) && !is.na((vancoTDM$auc_nonbayesian[x]))) {
#     precision = precision + abs(((vancoTDM$auc_nonbayesian [x] - vancoTDM$aucss_shinyapps [x]))/ vancoTDM$aucss_shinyapps [x])
#     n_pre = n_pre + 1
#     }
# }
# precision = precision/n_pre

#CREATING BOXPLOTS FOR COMPARISON 


#Clearance
 boxplot(vancoTDM$clind_precisepk,vancoTDM$clind_shinyapps,
        names=c("Precise PK","ShinyApps"),
        col=c("turquoise","tomato"),
        ylab="Clearance (L/h/kg)",
        main="Clearance",
        outline=FALSE,
        range = 0.0 )


#Volume of Distribution (L/kg)
 boxplot(vancoTDM$vdind_precisepk,vancoTDM$vdind_shinyapps,
         names=c("Precise PK","ShinyApps"),
         col=c("turquoise","tomato"),
         ylab="Volume of Distribution (L/kg)",
         main="Volume of Distribution",
         outline=FALSE,
         range = 0.0 )

 
#Area Under the Curve
 boxplot(vancoTDM$aucss,vancoTDM$auc_shinyapps,
         names=c("Precise PK","ShinyApps"),
         col=c("turquoise","tomato"),
         ylab="Area-Under-the-Curve (AUC, mg/L/h)",
         main="Area Under the Curve",
         outline=FALSE,
         range = 0.0 )
 
 #Accuracy and Precision of Shinyapps
 boxplot(vancoTDM$aucss,vancoTDM$auc_shinyapps,
         names=c("Precise PK","ShinyApps"),
         col=c("turquoise","tomato"),
         ylab="Area-Under-the-Curve (AUC, mg/L/h)",
         main="Area Under the Curve",
         outline=FALSE,
         range = 0.0 )
 
 
 