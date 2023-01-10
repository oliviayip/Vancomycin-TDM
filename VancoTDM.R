library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

vancoTDM <- read_excel("/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Vancomycin/TDMvancomycin_NhiTH_Thu_thap_thong_tin.xlsx")

#Load Rdata file (after first time executing code)-------------------------------------------------

save(vancoTDM, file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Vancomycin/VancoTDM.Rdata")
load(file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Vancomycin/VancoTDM.Rdata")

vancoTDM = data.frame(vancoTDM)

str(vancoTDM)
summary(vancoTDM)

#Convert all variable names to lowercase 
names(vancoTDM) <- tolower(names(vancoTDM))

#Names: 
names(vancoTDM)

#Comparing Precise PK and Shinyapps - CL

summary(vancoTDM$clind_precisepk, na.rm = TRUE)
sd(vancoTDM$clind_precisepk)
summary(vancoTDM$clind_shinyapps, na.rm = TRUE)
t.test(vancoTDM$clind_precisepk, vancoTDM$clind_shinyapps, paired = TRUE, alternative = "two.sided", na.rm = TRUE)

#Comparing Precise PK and Shinyapps - Vd

summary(vancoTDM$vdind_precisepk, na.rm = TRUE)
summary(vancoTDM$vdind_shinyapps, na.rm = TRUE)
t.test(vancoTDM$vdind_precisepk, vancoTDM$vdind_shinyapps, paired = TRUE, alternative = "two.sided", na.rm = TRUE)

#Comparing Precise PK and Shinyapps - AUCss

summary(vancoTDM$auc_precisepk, na.rm = TRUE)
summary(vancoTDM$aucss_shinyapps, na.rm = TRUE)
t.test(vancoTDM$auc_precisepk, vancoTDM$aucss_shinyapps, paired = TRUE, alternative = "two.sided", na.rm = TRUE)

