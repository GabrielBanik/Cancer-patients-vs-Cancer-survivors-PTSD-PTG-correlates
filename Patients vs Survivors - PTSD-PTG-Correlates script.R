#install required packages
packages <- c("readr","tidyverse", "psych", "data.table", "qgraph", 
              "bootnet", "networktools", "knitr", "kableExtra", 
              "NetworkComparisonTest", "patchwork", "mice", "miceadds",
              "MKmisc", "scales", "BayesFactor", "effectsize", "psychometric",
              "ppcor", "corrplot") 
# in the latest version of R might it be necessary install package limma by applying this approach - 1. install.packages("BiocManager"); 2. BiocManager::install("limma")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages
invisible(lapply(packages, library, character.only = TRUE))


#load subsetted mids object for group of patients
#load list - patients
data_listPatients <- readRDS("dataimpPatients.RDs") 
#creating new groups and transform education to numeric for correlation
data_listPatients <- data_listPatients %>% 
  map(~ .x %>% mutate_at("educationGroup", funs(recode(., `high school`=1, `university`=2, `elementary`=0))))

data_listPatients <- data_listPatients %>% 
  map(~ .x %>% mutate(educationGroup = as.numeric(educationGroup)))

data_listPatients <- data_listPatients %>% 
  map(~ .x %>% mutate(partnership = case_when(marital_status == "single" ~ "no relationship",
                                              marital_status == "divorced" ~ "no relationship",
                                              marital_status == "widowed" ~ "no relationship",
                                              marital_status == "married" ~ "in relationship",
                                              marital_status == "cohabited" ~ "in relationship",
                                              marital_status == "in relationship-not same living" ~ "in relationship",
                                              marital_status == "other" ~ "no relationship",)))

data_listPatients <- data_listPatients %>% 
  map(~ .x %>% mutate(withCancerFamilyAnamnenis = case_when(cancer_family_anamnesis == "yes" ~ "yes",
                                                            cancer_family_anamnesis == "no" ~ "no")))
dataimpPatients <- miceadds::datlist2mids(data_listPatients)

#load list - survivors
data_listSurvivors <- readRDS("dataimpSurvivors.RDs") 

data_listSurvivors <- data_listSurvivors %>% 
  map(~ .x %>% mutate_at("educationGroup", funs(recode(., `high school`=1, `university`=2, `elementary`=0))))

data_listSurvivors <- data_listSurvivors %>% 
  map(~ .x %>% mutate(educationGroup = as.numeric(educationGroup)))

data_listSurvivors <- data_listSurvivors %>% 
  map(~ .x %>% mutate(partnership = case_when(marital_status == "single" ~ "no relationship",
                                              marital_status == "divorced" ~ "no relationship",
                                              marital_status == "widowed" ~ "no relationship",
                                              marital_status == "married" ~ "in relationship",
                                              marital_status == "cohabited" ~ "in relationship",
                                              marital_status == "in relationship-not same living" ~ "in relationship",
                                              marital_status == "other" ~ "no relationship",)))

data_listSurvivors <- data_listSurvivors %>% 
  map(~ .x %>% mutate(withCancerFamilyAnamnenis = case_when(cancer_family_anamnesis == "yes" ~ "yes",
                                                            cancer_family_anamnesis == "no" ~ "no")))

dataimpSurvivors <- miceadds::datlist2mids(data_listSurvivors)


#PTG and PTSD comparison based of group - patients vs survivors-----------------------------------------------------------------
#maikng together mids object from patients mids and survivors mids objects

data_listPatients <- data_listPatients %>% #create new column
  map(~ .x %>% add_column(survivors = "no", .before = "gender"))

dataimpPatients <- miceadds::datlist2mids(data_listPatients)

data_listSurvivors <- data_listSurvivors %>% #create new column
  map(~ .x %>% add_column(survivors = "yes", .before = "gender"))

dataimpSurvivors <- miceadds::datlist2mids(data_listSurvivors)

impTogether <- rbind(dataimpPatients, dataimpSurvivors)

data_listTogether <- miceadds::mids2datlist(impTogether)

mi.t.test(data_listTogether, x = "ptg", y = "survivors", var.equal = FALSE) #overall PTG (Patients - M = 59.624, SD = 24.278, Survivors - M = 63.745, SD = 23.645; t = -1.923, df = 330.97, p = 0.055)
mi.t.test(data_listTogether, x = "ptgRO", y = "survivors", var.equal = FALSE) #relation to others (Patients - M = 21.589, SD = 9.108, Survivors - M = 21.657, SD = 8.633; t = -0.086, df = 323.01, p = 0.932)
mi.t.test(data_listTogether, x = "ptgNP", y = "survivors", var.equal = FALSE) #new possibilities (Patients - M = 11.598, SD = 6.374, Survivors - M = 13.513, SD = 6.425; t = -3.371, df = 341.72, p = 0.0008)
mi.t.test(data_listTogether, x = "ptgPS", y = "survivors", var.equal = FALSE) #personal strength (Patients - M = 11.316, SD = 5.527, Survivors - M = 12.264, SD = 5.107; t = -1.973, df = 314.64, p = 0.049)
mi.t.test(data_listTogether, x = "ptgSCH", y = "survivors", var.equal = FALSE) #spiitual change (Patients - M = 4.656, SD = 3.220, Survivors - M = 5.339, SD = 3.495; t = -2.321, df = 366.31, p = 0.021)
mi.t.test(data_listTogether, x = "ptgAL", y = "survivors", var.equal = FALSE) #appreciation of life (Patients - M = 10.465, SD = 3.994, Survivors - M = 10.972, SD = 3.788; t = -1.448, df = 323.12, p = 0.149)

mi.t.test(data_listTogether, x = "ptsd", y = "survivors", var.equal = FALSE) #overall PTSD (Patients - M = 7.503, SD = 6.043, Survivors - M = 8.518, SD = 5.790; t = -1.913, df = 326.47, p = 0.057)
mi.t.test(data_listTogether, x = "ptsd1", y = "survivors", var.equal = FALSE) #Upseting dreams (Patients - M = 1.02, SD = 1.206, Survivors - M = 1.031, SD = 1.214; t = -0.078, df = 341.33, p = 0.938)
mi.t.test(data_listTogether, x = "ptsd2", y = "survivors", var.equal = FALSE) #Vivid memories and flashbacks (Patients - M = 1.216, SD = 1.226, Survivors - M = 1.550, SD = 1.282; t = -3.020, df = 353.66, p = 0.003)
mi.t.test(data_listTogether, x = "ptsd3", y = "survivors", var.equal = FALSE) #Internal avoidance (Patients - M = 1.481, SD = 1.351, Survivors - M = 1.578, SD = 1.353; t = -0.806, df = 335.52, p = 0.421)
mi.t.test(data_listTogether, x = "ptsd4", y = "survivors", var.equal = FALSE) #External avoidance (Patients - M = 1.166, SD = 1.302, Survivors - M = 1.174, SD = 1.309; t = -0.069, df = 341.02, p = 0.945)
mi.t.test(data_listTogether, x = "ptsd5", y = "survivors", var.equal = FALSE) #Being on guard (Patients - M = 1.315, SD = 1.272, Survivors - M = 1.642, SD = 1.283; t = -2.882, df = 342.06, p = 0.004)
mi.t.test(data_listTogether, x = "ptsd6", y = "survivors", var.equal = FALSE) #Jumpy/startled (Patients - M = 1.304, SD = 1.292, Survivors - M = 1.545, SD = 1.286; t = -2.104, df = 337.98, p = 0.036)


#PTG a PTSD correlates in patients---------------------------------------------------------------------------------------

####correlations for Patients vs Survivors study#### (pooled)
dataComplete <- complete(dataimpPatients, "long")
dataComplete <- dataComplete[,-c(1:2)]
match(c("ptsd", "ptg", 
        "age", "diagnosis_year",                
        "spirituality_importance", "spirituality_practice",
        "neuroticism", "optimism", "pesimism", "selfefficacy", "resilience",
        "helplessnessHopelessness", "anxiousPreoccupation", "avoidance", "fatalism", "fightingSpirit", 
        "socialSupport", "social_support_medical", 
        "satisfactionWithLife",  
        "cognitiveMeaningInLife", "motivateMeaningInLife", "afectiveMeaningInLife", 
        "anxiety", "depression", "pain"), names(dataComplete))
miceadds::micombine.cor(dataimpPatients, 
                        variables = c(12, 19,  3,  6, 10, 11, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44), 
                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)
a <- miceadds::micombine.cor(dataimpPatients, 
                             variables = c(12, 19,  3,  6, 10, 11, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44), 
                             conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

a <- a[c(1:45),c(1:3,9:11)]
a %>% 
  kbl() %>% 
  kable_material()

#match variables order in dataset for correlations plot
match(c("neuroticism", "optimism", "pesimism", "selfefficacy", "resilience",
        "helplessnessHopelessness", "anxiousPreoccupation", "avoidance", "fatalism", "fightingSpirit"), names(dataComplete)) #personality_coping

match(c("socialSupport", "social_support_medical"), names(dataComplete)) #social_support
match(c("cognitiveMeaningInLife", "motivateMeaningInLife", "afectiveMeaningInLife","satisfactionWithLife"), names(dataComplete)) #existential 
match(c("anxiety", "depression", "pain"), names(dataComplete)) #affective
match(c("spirituality_importance", "spirituality_practice"), names(dataComplete)) #spirituality


#PTG
mi.t.test(data_listPatients, x = "ptg", y = "gender", var.equal = FALSE) #m1 = 61.07747; sd1 = 24.85929; m2 = 57.59206; sd2 = 23.47493; (109 females 70 males)
d1 <- (57.59206-61.07747)/sqrt(((24.85929)^2+(23.47493)^2/2)) # d = -0.117
rd1 <- d_to_r(d1) # r = -0.058 (convert d to r effect size)
ci1 <- CIr(r = rd1, n = 179, level = .95)
ci1lower <- -0.20313975
ci1upper <- 0.08923243
#female coded as 0, males coded as 1
#recoding order for better interpretation in plot (female will be 1 and male 0)
rd1 <- rd1*-1
ci1lower <- -0.20313975*-1
ci1upper <- 0.08923243*-1

mi.t.test(data_listPatients, x = "ptg", y = "partnership", var.equal = FALSE) #m1 = 61.57048 ; sd1 = 23.60935; m2 = 56.51282; sd2 = 25.17494 (134 in relationship, 26 no relationship)
d2 <- (56.51282-61.57048)/sqrt(((23.60935)^2+(25.17494)^2/2)) # d = -0.171
rd2 <- d_to_r(d2) # r = -0.085
ci2 <- CIr(r = rd2, n = 160, level = .95)
ci2lower <- -0.23723577
ci2upper <- 0.07088225
#in partnership coded as 0, no relationship = 1
#recoding order for better interpretation in plot (in relationship will be 1 and wihtout relationship will be 0)
rd2 <- rd2*-1
ci2lower <- -0.23723577*-1
ci2upper <- 0.07088225*-1

mi.t.test(data_listPatients, x = "ptg", y = "withCancerFamilyAnamnenis", var.equal = FALSE) #m1 = 57.51020 ; sd1 = 27.45389; m2 = 60.88982; sd2 = 23.21199 (119 yes, 49 no)
d3 <- (60.88982-57.51020)/sqrt(((27.45389)^2+(23.21199)^2/2)) # d = 0.106
rd3 <- d_to_r(d3) # r = 0.053
ci3 <- CIr(r = rd3, n = 168, level = .95)
ci3lower <- -0.09944831
ci3upper <- 0.20254773
#without cancer anamnesis in family coded as 0, with coded as 1

mi.t.test(data_listPatients, x = "ptg", y = "relaps", var.equal = FALSE) #m1 = 59.97071; sd1 = 24.91675; m2 = 61.00000; sd2 = 21.22429 (35 relapse, 129 no relapse)
d4 <- (61.00000-59.97071)/sqrt(((24.91675)^2+(21.22429)^2/2)) # d = 0.035
rd4 <- d_to_r(d4) # r = 0.018
ci4 <- CIr(r = rd4, n = 164, level = .95)
ci4lower <- -0.1359281
ci4upper <- 0.1704780
#without cancer relapse coded as 0, with coded as 1

mi.t.test(data_listPatients, x = "ptg", y = "treatment_late_impact", var.equal = FALSE) #m1 = 59.61570; sd1 = 23.32771; m2 = 64.25532; sd2 = 24.50007 (47 yes, 109 no)
d5 <- (64.25532-59.61570)/sqrt(((23.32771)^2+(24.50007)^2/2)) # d = 0.160
rd5 <- d_to_r(d5) # r = 0.080
ci5 <- CIr(r = rd5, n = 156, level = .95)
ci5lower <- -0.07853962
ci5upper <- 0.23380022
#without cancer treatment late impact coded as 0, with coded as 1

mi.t.test(data_listPatients, x = "ptg", y = "cancerSupportGroup", var.equal = FALSE) #m1 = 58.57849; sd1 = 24.25735; m2 = 69.63158; sd2 = 26.18781 (19 yes, 155 no)
d11 <- (69.63158-58.57849)/sqrt(((24.25735)^2+(26.18781)^2/2)) # d = 0.362
rd11 <- d_to_r(d11) # r = 0.178
ci11 <- CIr(r = rd11, n = 174, level = .95)
ci11lower <- 0.0302272
ci11upper <- 0.3185215
#without attendance of cancer support group coded as 0, with coded as 1

rcat1 <- c(rd1, rd2, rd3, rd4, rd5, rd11) #correlations vector for PTG and factor variables
rcatlower1 <- c(ci1lower, ci2lower, ci3lower, ci4lower, ci5lower, ci11lower)
rcatupper1 <- c(ci1upper, ci2upper, ci3upper, ci4upper, ci5upper, ci11upper)

#PTSD
mi.t.test(data_listPatients, x = "ptsd", y = "gender", var.equal = FALSE) #m1 = 7.753313; sd1 = 6.098716; m2 = 7.155556; sd2 = 6.057256; (109 females 70 males)
d6 <- (7.155556-7.753313)/sqrt(((6.098716)^2+(6.057256)^2/2)) # d = -0.080
rd6 <- d_to_r(d6) # r = -0.040 (convert d to r effect size)
ci6 <- CIr(r = rd6, n = 179, level = .95)
ci6lower <- -0.1856534
ci6upper <- 0.1072302
#female coded as 0, males coded as 1
#recoding order for better interpretation in plot (female will be 1 and male 0)
rd6 <- rd6*-1
ci6lower <- -0.1856534*-1
ci6upper <- 0.1072302*-1

mi.t.test(data_listPatients, x = "ptsd", y = "partnership", var.equal = FALSE) #m1 = 7.426202; sd1 = 6.034683; m2 = 8.653846; sd2 = 6.626868 (134 in relationship, 26 no relationship)
d7 <- (8.653846-7.426202)/sqrt(((6.034683)^2+(6.626868)^2/2)) # d = 0.161
rd7 <- d_to_r(d7) # r = 0.080
ci7 <- CIr(r = rd7, n = 160, level = .95)
ci7lower <- -0.07602195 
ci7upper <- 0.23235307
#in partnership coded as 0, no relationship = 1
#recoding order for better interpretation in plot (in relationship will be 1 and wihtout relationship will be 0)
rd7 <- rd7*-1
ci7lower <- -0.07602195*-1 
ci7upper <- 0.23235307*-1

mi.t.test(data_listPatients, x = "ptsd", y = "withCancerFamilyAnamnenis", var.equal = FALSE) #m1 = 8.941043; sd1 = 6.186611; m2 = 7.108310; sd2 = 5.845424 (49 no, 119 yes)
d8 <- (7.108310-8.941043)/sqrt(((6.186611)^2+(5.845424)^2/2)) # d = -0.246
rd8 <- d_to_r(d8) # r = -0.122
ci8 <- CIr(r = rd8, n = 168, level = .95)
ci8lower <- -0.26867543
ci8upper <- 0.02972157
#without cancer anamnesis in family coded as 0, with coded as 1

mi.t.test(data_listPatients, x = "ptsd", y = "relaps", var.equal = FALSE) #m1 = 6.954350; sd1 = 6.026364; m2 = 9.685714; sd2 = 5.949649 (129 no relapse, 35 relapse)
d9 <- (9.685714-6.954350)/sqrt(((6.026364)^2+(5.949649)^2/2)) # d = 0.372
rd9 <- d_to_r(d9) # r = 0.183
ci9 <- CIr(r = rd9, n = 164, level = .95)
ci9lower <- 0.03028875
ci9upper <- 0.32679125
#without cancer relapse coded as 0, with coded as 1

mi.t.test(data_listPatients, x = "ptsd", y = "treatment_late_impact", var.equal = FALSE) #m1 = 6.091743; sd1 = 5.674997; m2 = 9.702128; sd2 = 5.559617 (109 no 47 yes)
d10 <- (9.702128-6.091743)/sqrt(((5.674997)^2+(5.559617)^2/2)) # d = 0.523
rd10 <- d_to_r(d10) # r = 0.253
ci10 <- CIr(r = rd10, n = 156, level = .95)
ci10lower <- 0.09980537
ci10upper <- 0.39443905
#without cancer treatment late impact coded as 0, with coded as 1

mi.t.test(data_listPatients, x = "ptsd", y = "cancerSupportGroup", var.equal = FALSE) #m1 = 7.089606; sd1 = 6.030815; m2 = 9.842105; sd2 = 5.620035 (19 yes, 155 no)
d12 <- (9.842105-7.089606)/sqrt(((6.030815)^2+(5.620035)^2/2)) # d = 0.381
rd12 <- d_to_r(d12) # r = 0.187
ci12 <- CIr(r = rd12, n = 174, level = .95)
ci12lower <- 0.03951531
ci12upper <- 0.32685257
#without attendance of cancer support group coded as 0, with coded as 1

rcat2 <- c(rd6, rd7, rd8, rd9, rd10, rd12) #correlations vector for PTSD and factor variables
rcatlower2 <- c(ci6lower, ci7lower, ci8lower, ci9lower, ci10lower, ci12lower)
rcatupper2 <- c(ci6upper, ci7upper, ci8upper, ci9upper, ci10upper, ci12upper)

##correlations-------------------------------
dataComplete <- complete(dataimpPatients, "long")
dataComplete <- dataComplete[,-c(1:2)]

match(c("ptsd", "ptg", 
       "educationGroup"), names(dataComplete))

correlations <- miceadds::micombine.cor(dataimpPatients, 
                        variables = c(19, 12,  3,  6, 10, 11, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44), 
                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

#making correlation groups for plot
age <- miceadds::micombine.cor(dataimpPatients, #age
                                        variables = c(19, 12,  3), 
                                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

education <- miceadds::micombine.cor(dataimpPatients, #education
                             variables = c(19, 12, 4), 
                             conf.level=0.95, method="spearman", nested=FALSE, partial=NULL)

diagnosis_time <- miceadds::micombine.cor(dataimpPatients, #age
                                          variables = c(19, 12,  6), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

personality <- miceadds::micombine.cor(dataimpPatients, #personality 
                                              variables = c(19, 12, 25, 26, 27, 28, 29), 
                                              conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

coping <- miceadds::micombine.cor(dataimpPatients, #personality 
                                       variables = c(19, 12, 30, 31, 32, 33, 34), 
                                       conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

social_support <- miceadds::micombine.cor(dataimpPatients, #social support
                                          variables = c(19,12,36,37), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

eudaimonic <- miceadds::micombine.cor(dataimpPatients, #existential
                                          variables = c(19,12,39,40,41,38), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

emotional <- miceadds::micombine.cor(dataimpPatients, #emotional
                                          variables = c(19,12,42,43,44), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

spiritual <- miceadds::micombine.cor(dataimpPatients, #spirituality
                                          variables = c(19,12,10,11), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 


#PTG and PTSD correlates
variables <- c("gender", "age", "education", "partnership",
               "cancer family anamnesis", "cancer relapse", "treatment late impact", "diagnosis time",                 
               "neuroticism", "optimism", "pesimism", "selfefficacy", "resilience",
               "helplessness-hopelessness", "anxious preoccupation", "cognitive avoidance", "fatalism", "fighting spirit", 
               "cancer support group", "social support", "medical social support", 
               "cognitive meaningfulness", "motivational meaningfulness", "afective meaningfulness", "satisfaction with life",
               "spirituality importance", "spirituality practice",
               "anxiety", "depression", "pain")

groups <- c("socio-demographic","socio-demographic","socio-demographic", "socio-demographic",
            "cancer-related", "cancer-related", "cancer-related", "cancer-related", 
            "personality", "personality", "personality", "personality", "personality", 
            "coping", "coping", "coping", "coping", "coping",
            "social support", "social support", "social support",
            "eudaimonic", "eudaimonic", "eudaimonic", "eudaimonic",
            "spiritual", "spiritual",
            "emotional", "emotional", "emotional")

subgroups1 <- rep("PTG", 30)
subgroups2 <- rep("PTSD", 30)


order1 <- seq(1,60, by=2)
order2 <- seq(2,60, by=2)

#making variables block for correlations plot
#education
educationcor <- education$r
education1 <- educationcor[2]
education2 <- educationcor[3]

educationlower <- education$lower95
educationlower1 <- educationlower[2]
educationlower2 <- educationlower[3]

educationupper <- education$upper95 
educationupper1 <- educationupper[2]
educationupper2 <- educationupper[3]
#age
agecor <- age$r
age1 <- agecor[2]
age2 <- agecor[3]

agelower <- age$lower95
agelower1 <- agelower[2]
agelower2 <- agelower[3]

ageupper <- age$upper95 
ageupper1 <- ageupper[2]
ageupper2 <- ageupper[3]

#sociodemographic correlations vector = personal group
socdem1 <- c(rd1, age1, education1,rd2) #gender, age, education, partnership PTG
socdemlower1 <- c(ci1lower,agelower1, educationlower1,ci2lower) 
socdemupper1 <- c(ci1upper,ageupper1, educationupper1,ci2upper) 

socdem2 <- c(rd6,age2, education2,rd7) #gender, age, education, partnership PTSD
socdemlower2 <- c(ci6lower,agelower2, educationlower2,ci7lower) 
socdemupper2 <- c(ci6upper,ageupper2, educationupper2,ci7upper) 

#correlations vector for cancer-related group
diagnosis_timecor <- diagnosis_time$r
diagnosis_time1 <- diagnosis_timecor[2]
diagnosis_time2 <- diagnosis_timecor[3]

diagnosis_timelower <- diagnosis_time$lower95
diagnosis_timelower1 <- diagnosis_timelower[2]
diagnosis_timelower2 <- diagnosis_timelower[3]

diagnosis_timeupper <- diagnosis_time$upper95 
diagnosis_timeupper1 <- diagnosis_timeupper[2]
diagnosis_timeupper2 <- diagnosis_timeupper[3]

cancer1 <- c(rd3, rd4, rd5, diagnosis_time1) #cancer in family anamnesis, cancer relapse, treatmet late impact, time since diagnosis PTG
cancerlower1 <- c(ci3lower,ci4lower,ci5lower, diagnosis_timelower1) 
cancerupper1 <- c(ci3upper,ci4upper,ci5upper, diagnosis_timeupper1) 

cancer2 <- c(rd8, rd9, rd10, diagnosis_time2) #cancer in family anamnesis, cancer relapse, treatmet late impact, time since diagnosis PTSD
cancerlower2 <- c(ci8lower,ci9lower,ci10lower, diagnosis_timelower2) 
cancerupper2 <- c(ci8upper,ci9upper,ci10upper, diagnosis_timeupper2) 

#personality
personalitycor <- personality$r
personality1 <- personalitycor[2:6]
personality2 <- personalitycor[7:11]

personalitylower <- personality$lower95
personalitylower1 <- personalitylower[2:6]
personalitylower2 <- personalitylower[7:11]

personalityupper <- personality$upper95 
personalityupper1 <- personalityupper[2:6]
personalityupper2 <- personalityupper[7:11]

#coping
copingcor <- coping$r
coping1 <- copingcor[2:6]
coping2 <- copingcor[7:11]

copinglower <- coping$lower95
copinglower1 <- copinglower[2:6]
copinglower2 <- copinglower[7:11]

copingupper <- coping$upper95 
copingupper1 <- copingupper[2:6]
copingupper2 <- copingupper[7:11]

#social support
social_supportcor <- social_support$r
social_support1 <- social_supportcor[2:3]
social_support2 <- social_supportcor[4:5]

social_supportlower <- social_support$lower95
social_supportlower1 <- social_supportlower[2:3]
social_supportlower2 <- social_supportlower[4:5]

social_supportupper <- social_support$upper95 
social_supportupper1 <- social_supportupper[2:3]
social_supportupper2 <- social_supportupper[4:5]

social_support1 <- c(social_support1, rd11) #social support, medical social support, cancer support group PTG
social_supportlower1 <- c(social_supportlower1, ci11lower) 
social_supportupper1 <- c(social_supportupper1, ci11upper) 

social_support2 <- c(social_support2, rd12) #social support, medical social support, cancer support group  PTSD
social_supportlower2 <- c(social_supportlower2, ci12lower) 
social_supportupper2 <- c(social_supportupper2, ci12upper) 

#eudaimonic
eudaimoniccor <- eudaimonic$r
eudaimonic1 <- eudaimoniccor[2:5]
eudaimonic2 <- eudaimoniccor[6:9]

eudaimoniclower <- eudaimonic$lower95
eudaimoniclower1 <- eudaimoniclower[2:5]
eudaimoniclower2 <- eudaimoniclower[6:9]

eudaimonicupper <- eudaimonic$upper95 
eudaimonicupper1 <- eudaimonicupper[2:5]
eudaimonicupper2 <- eudaimonicupper[6:9]

#spiritual
spiritualcor <- spiritual$r
spiritual1 <- spiritualcor[2:3]
spiritual2 <- spiritualcor[4:5]

spirituallower <- spiritual$lower95
spirituallower1 <- spirituallower[2:3]
spirituallower2 <- spirituallower[4:5]

spiritualupper <- spiritual$upper95 
spiritualupper1 <- spiritualupper[2:3]
spiritualupper2 <- spiritualupper[4:5]

#emotional
emotionalcor <- emotional$r
emotional1 <- emotionalcor[2:4]
emotional2 <- emotionalcor[5:7]

emotionallower <- emotional$lower95
emotionallower1 <- emotionallower[2:4]
emotionallower2 <- emotionallower[5:7]

emotionalupper <- emotional$upper95 
emotionalupper1 <- emotionalupper[2:4]
emotionalupper2 <- emotionalupper[5:7]

r1 <- c(socdem1, cancer1, personality1, coping1, social_support1, eudaimonic1, spiritual1, emotional1)
r2 <- c(socdem2, cancer2, personality2, coping2, social_support2, eudaimonic2, spiritual2, emotional2)
lower1 <- c(socdemlower1, cancerlower1, personalitylower1, copinglower1, social_supportlower1, eudaimoniclower1, spirituallower1, emotionallower1)
lower2 <- c(socdemlower2, cancerlower2, personalitylower2, copinglower2, social_supportlower2, eudaimoniclower2, spirituallower2, emotionallower2)
upper1 <- c(socdemupper1, cancerupper1, personalityupper1, copingupper1, social_supportupper1, eudaimonicupper1, spiritualupper1, emotionalupper1)
upper2 <- c(socdemupper2, cancerupper2, personalityupper2, copingupper2, social_supportupper2, eudaimonicupper2, spiritualupper2, emotionalupper2)

ptg <- tibble(order1, variables, r1, lower1, upper1, groups, subgroups1)
names(ptg) <- c("order", "variables", "r", "lower", "upper", "groups", "subgroups")
ptsd <- tibble(order2, variables, r2, lower2, upper2, groups, subgroups2)
names(ptsd) <- c("order", "variables", "r", "lower", "upper", "groups", "subgroups")

ptgptsd <- rbind(ptg,ptsd)

ptgptsd <- ptgptsd %>% 
  arrange(order)

#plots for PTG and PTSD correlates 

#plot subgroups (with different color for PTG and PTSD and with differet shapes for correlates group)
pdf("PTG and PTSD correlates in cancer patients.pdf",height=12,width=10)
ggplot(data=ptgptsd, aes(x=variables, y=r, ymin=lower, ymax=upper, color = subgroups, shape = groups)) +
  ylim(-0.75,0.75) +
  geom_pointrange(position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  facet_grid(groups~., scales= "free", space="free") + # add grouping based of defined categories of variables
  theme_bw() + # use a white background
  theme(strip.background =element_rect(fill="white")) +  # use white background in facets
  theme_minimal() + # minimal theme for plot
  xlab("correlates") + ylab("Pearson's r") +
  theme(axis.title.x = element_text(hjust= 0.5)) + # adjust axis title position
  theme(axis.title.y = element_text(size = 14)) + # adjust axis title size
  theme(plot.title = element_text(hjust = 0.5)) + # adjust title position
  labs(title = "Posttraumatic growth (PTG) and PTSD correlates in patients with cancer in cancer treatment",
       subtitle = "Pearson Correlation between PTG and PTSD: 0.071 [-.08,.21]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9, face = "bold.italic", color = "red")) +
  labs(col="Posttraumatic\nreactions") + # change name of legend 1 (\n) in two lines 
  scale_shape_manual(values = c(8,19,17,15,4,5,13,3), name = "Correlates\ncategories") + # change name of legend 2 (\n) in two lines 
  theme(legend.title.align = 0.5) + # centering of legend titles
  scale_color_manual(values = c("#66CCCC", "#660033")) # apply different colors for points (colorblind friendly)

dev.off()

#correlations table
#working table for values from correlation matrix
z <- miceadds::micombine.cor(dataimpPatients, 
                             variables = c(19, 12,  3,  6, 10, 11, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44), 
                             conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)
v <- z$variable1
v2 <- z$variable2
a <- z$r
b <- z$p
c <- z$lower95
d <- z$upper95

o <- tibble(v,v2,a,b,c,d)
o$a <- round(o$a, 2)
o$c <- round(o$c, 2)
o$d <- round(o$d, 2)

o$x <- symnum(o$b, corr = FALSE, na = FALSE, 
              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
              symbols = c("***", "**", "*", ",", " "))
o$b <- o$x
o$x <- NULL

o %>% 
  kbl() %>% 
  kable_material

#Bayesian correlation

dat <- data_listPatients

bf1 <- lapply(dat, function(x){correlationBF(x$ptg,x$ptsd)})
bf1mean <- lapply(bf1, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf1mean, function(x){mean(x)}))) #0.266

#PTG
bf3 <- lapply(dat, function(x){correlationBF(x$ptg,x$age)})
bf3mean <- lapply(bf3, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf3mean, function(x){mean(x)}))) #699.0339

bf2 <- lapply(dat, function(x){correlationBF(x$ptg,x$diagnosis_year)})
bf2mean <- lapply(bf2, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf2mean, function(x){mean(x)}))) #0.9627733

bf5 <- lapply(dat, function(x){correlationBF(x$ptg,x$spirituality_importance)})
bf5mean <- lapply(bf5, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf5mean, function(x){mean(x)}))) #0.4289542

bf7 <- lapply(dat, function(x){correlationBF(x$ptg,x$spirituality_practice)})
bf7mean <- lapply(bf7, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf7mean, function(x){mean(x)}))) #0.7086697

bf4 <- lapply(dat, function(x){correlationBF(x$ptg,x$neuroticism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #0.2785411

bf8 <- lapply(dat, function(x){correlationBF(x$ptg,x$optimism)})
bf8mean <- lapply(bf8, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf8mean, function(x){mean(x)}))) #0.8722609

bf4 <- lapply(dat, function(x){correlationBF(x$ptg,x$pesimism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #0.1860906

bf6 <- lapply(dat, function(x){correlationBF(x$ptg,x$selfefficacy)})
bf6mean <- lapply(bf6, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf6mean, function(x){mean(x)}))) #1.082036

bf10 <- lapply(dat, function(x){correlationBF(x$ptg,x$resilience)})
bf10mean <- lapply(bf10, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf10mean, function(x){mean(x)}))) #0.2061681

bf13 <- lapply(dat, function(x){correlationBF(x$ptg,x$helplessnessHopelessness)})
bf13mean <- lapply(bf13, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf13mean, function(x){mean(x)}))) #0.1737242

bf16 <- lapply(dat, function(x){correlationBF(x$ptg,x$anxiousPreoccupation)})
bf16mean <- lapply(bf16, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf16mean, function(x){mean(x)}))) #0.2640679

bf12 <- lapply(dat, function(x){correlationBF(x$ptg,x$avoidance)})
bf12mean <- lapply(bf12, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf12mean, function(x){mean(x)}))) #0.2195415

bf18 <- lapply(dat, function(x){correlationBF(x$ptg,x$fatalism)})
bf18mean <- lapply(bf18, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf18mean, function(x){mean(x)}))) #0.3461599

bf14 <- lapply(dat, function(x){correlationBF(x$ptg,x$fightingSpirit)})
bf14mean <- lapply(bf14, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf14mean, function(x){mean(x)}))) #0.3473368

bf15 <- lapply(dat, function(x){correlationBF(x$ptg,x$socialSupport)})
bf15mean <- lapply(bf15, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf15mean, function(x){mean(x)}))) #0.4195907

bf17 <- lapply(dat, function(x){correlationBF(x$ptg,x$social_support_medical)})
bf17mean <- lapply(bf17, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf17mean, function(x){mean(x)}))) #0.2778977

bf9 <- lapply(dat, function(x){correlationBF(x$ptg,x$satisfactionWithLife)})
bf9mean <- lapply(bf9, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf9mean, function(x){mean(x)}))) #0.3860905

bf11 <- lapply(dat, function(x){correlationBF(x$ptg,x$cognitiveMeaningInLife)})
bf11mean <- lapply(bf11, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf11mean, function(x){mean(x)}))) #2.697491

bf19 <- lapply(dat, function(x){correlationBF(x$ptg,x$motivateMeaningInLife)})
bf19mean <- lapply(bf19, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf19mean, function(x){mean(x)}))) #91.58173

bf20 <- lapply(dat, function(x){correlationBF(x$ptg,x$afectiveMeaningInLife)})
bf20mean <- lapply(bf20, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf20mean, function(x){mean(x)}))) #11.39633

bf21 <- lapply(dat, function(x){correlationBF(x$ptg,x$anxiety)})
bf21mean <- lapply(bf21, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf21mean, function(x){mean(x)}))) #0.1764971

bf22 <- lapply(dat, function(x){correlationBF(x$ptg,x$depression)})
bf22mean <- lapply(bf22, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf22mean, function(x){mean(x)}))) #1.946558

bf23 <- lapply(dat, function(x){correlationBF(x$ptg,x$pain)})
bf23mean <- lapply(bf23, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf23mean, function(x){mean(x)}))) #0.1939956

#PTSD
bf3 <- lapply(dat, function(x){correlationBF(x$ptsd,x$age)})
bf3mean <- lapply(bf3, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf3mean, function(x){mean(x)}))) #0.1782295

bf2 <- lapply(dat, function(x){correlationBF(x$ptsd,x$diagnosis_year)})
bf2mean <- lapply(bf2, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf2mean, function(x){mean(x)}))) #0.4465938

bf5 <- lapply(dat, function(x){correlationBF(x$ptsd,x$spirituality_importance)})
bf5mean <- lapply(bf5, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf5mean, function(x){mean(x)}))) #0.2020117

bf7 <- lapply(dat, function(x){correlationBF(x$ptsd,x$spirituality_practice)})
bf7mean <- lapply(bf7, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf7mean, function(x){mean(x)}))) #0.4230429

bf4 <- lapply(dat, function(x){correlationBF(x$ptsd,x$neuroticism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #32366643450

bf8 <- lapply(dat, function(x){correlationBF(x$ptsd,x$optimism)})
bf8mean <- lapply(bf8, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf8mean, function(x){mean(x)}))) #0.1993152

bf4 <- lapply(dat, function(x){correlationBF(x$ptsd,x$pesimism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #0.1960898

bf6 <- lapply(dat, function(x){correlationBF(x$ptsd,x$selfefficacy)})
bf6mean <- lapply(bf6, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf6mean, function(x){mean(x)}))) #13093.83

bf10 <- lapply(dat, function(x){correlationBF(x$ptsd,x$resilience)})
bf10mean <- lapply(bf10, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf10mean, function(x){mean(x)}))) #3274563

bf13 <- lapply(dat, function(x){correlationBF(x$ptsd,x$helplessnessHopelessness)})
bf13mean <- lapply(bf13, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf13mean, function(x){mean(x)}))) #4089.471

bf16 <- lapply(dat, function(x){correlationBF(x$ptsd,x$anxiousPreoccupation)})
bf16mean <- lapply(bf16, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf16mean, function(x){mean(x)}))) #1800225857

bf12 <- lapply(dat, function(x){correlationBF(x$ptsd,x$avoidance)})
bf12mean <- lapply(bf12, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf12mean, function(x){mean(x)}))) #0.2045305

bf18 <- lapply(dat, function(x){correlationBF(x$ptsd,x$fatalism)})
bf18mean <- lapply(bf18, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf18mean, function(x){mean(x)}))) #0.2856321

bf14 <- lapply(dat, function(x){correlationBF(x$ptsd,x$fightingSpirit)})
bf14mean <- lapply(bf14, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf14mean, function(x){mean(x)}))) #1.211616

bf15 <- lapply(dat, function(x){correlationBF(x$ptsd,x$socialSupport)})
bf15mean <- lapply(bf15, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf15mean, function(x){mean(x)}))) #93.27744

bf17 <- lapply(dat, function(x){correlationBF(x$ptsd,x$social_support_medical)})
bf17mean <- lapply(bf17, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf17mean, function(x){mean(x)}))) #25.39742

bf9 <- lapply(dat, function(x){correlationBF(x$ptsd,x$satisfactionWithLife)})
bf9mean <- lapply(bf9, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf9mean, function(x){mean(x)}))) #7449182

bf11 <- lapply(dat, function(x){correlationBF(x$ptsd,x$cognitiveMeaningInLife)})
bf11mean <- lapply(bf11, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf11mean, function(x){mean(x)}))) #466.5973

bf19 <- lapply(dat, function(x){correlationBF(x$ptsd,x$motivateMeaningInLife)})
bf19mean <- lapply(bf19, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf19mean, function(x){mean(x)}))) #3183185

bf20 <- lapply(dat, function(x){correlationBF(x$ptsd,x$afectiveMeaningInLife)})
bf20mean <- lapply(bf20, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf20mean, function(x){mean(x)}))) #1283084546

bf21 <- lapply(dat, function(x){correlationBF(x$ptsd,x$anxiety)})
bf21mean <- lapply(bf21, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf21mean, function(x){mean(x)}))) #10143.04

bf22 <- lapply(dat, function(x){correlationBF(x$ptsd,x$depression)})
bf22mean <- lapply(bf22, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf22mean, function(x){mean(x)}))) #2.260805

bf23 <- lapply(dat, function(x){correlationBF(x$ptsd,x$pain)})
bf23mean <- lapply(bf23, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf23mean, function(x){mean(x)}))) #14980080612



#-------------------------------------------------------------------------------------------------------------
#PTG and PTSD correlates in survivors-------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

dataSComplete <- complete(dataimpSurvivors, "long")
dataSComplete <- dataSComplete[,-c(1:2)]
#match variables order in dataset for correlations plot
match(c("neuroticism", "optimism", "pesimism", "selfefficacy", "resilience",
        "helplessnessHopelessness", "anxiousPreoccupation", "avoidance", "fatalism", "fightingSpirit"), names(dataSComplete)) #personality_coping

match(c("socialSupport", "social_support_medical"), names(dataSComplete)) #social_support
match(c("cognitiveMeaningInLife", "motivateMeaningInLife", "afectiveMeaningInLife","satisfactionWithLife"), names(dataSComplete)) #existential 
match(c("anxiety", "depression", "pain"), names(dataSComplete)) #affective
match(c("spirituality_importance", "spirituality_practice"), names(dataSComplete)) #spirituality


#PTG
mi.t.test(data_listSurvivors, x = "ptg", y = "gender", var.equal = FALSE) #m1 = 66.10185; sd1 = 22.44469; m2 = 57.97076; sd2 = 25.64509; (300 females 114 males)
d1S <- (57.97076-66.10185)/sqrt(((22.44469)^2+(25.64509)^2/2)) # d = -0.282
rd1S <- d_to_r(d1S) # r = -0.14 (convert d to r effect size)
ci1S <- CIr(r = rd1S, n = 414, level = .95)
ci1lowerS <- -0.23276654
ci1upperS <- -0.04372881
#female coded as 0, males coded as 1
#recoding order for better interpretation in plot (female will be 1 and male 0)
rd1S <- rd1S*-1
ci1lowerS <- -0.23276654*-1
ci1upperS <- -0.04372881*-1


mi.t.test(data_listSurvivors, x = "ptg", y = "partnership", var.equal = FALSE) #m1 = 63.92989 ; sd1 = 22.99003; m2 = 63.12302; sd2 = 25.17494 (290 in relationship, 84 no relationship)
d2S <- (63.12302-63.92989)/sqrt(((22.99003)^2+(27.06192)^2/2)) # d = -0.027
rd2S <- d_to_r(d2S) # r = -0.014
ci2S <- CIr(r = rd2S, n = 374, level = .95)
ci2lowerS <- -0.11473585
ci2upperS <- 0.08804064
#in partnership coded as 0, no relationship = 1
#recoding order for better interpretation in plot (in relationship will be 1 and wihtout relationship will be 0)
rd2S <- rd2S*-1
ci2lowerS <- -0.11473585*-1
ci2upperS <- 0.08804064*-1

mi.t.test(data_listSurvivors, x = "ptg", y = "withCancerFamilyAnamnenis", var.equal = FALSE) #m1 = 63.25899 ; sd1 = 22.77605; m2 = 60.88982; sd2 = 23.41422 (256 yes, 136 no)
d3S <- (64.93403-63.25899)/sqrt(((22.77605)^2+(23.41422)^2/2)) # d = 0.06
rd3S <- d_to_r(d3S) # r = 0.03
ci3S <- CIr(r = rd3S, n = 392, level = .95)
ci3lowerS <- -0.06952236
ci3upperS <- 0.12840084
#without cancer anamnesis in family coded as 0, with coded as 1

mi.t.test(data_listSurvivors, x = "ptg", y = "relaps", var.equal = FALSE) #m1 = 64.37526; sd1 = 23.97776; m2 = 61.00000; sd2 = 23.49206 (79 yes, 326 no)
d4S <- (60.40928-64.37526)/sqrt(((23.97776)^2+(23.49206)^2/2)) # d = -0.136
rd4S <- d_to_r(d4S) # r = -0.068
ci4S <- CIr(r = rd4S, n = 405, level = .95)
ci4lowerS <- -0.16418344
ci4upperS <- 0.02981637
#without cancer relapse coded as 0, with coded as 1

mi.t.test(data_listSurvivors, x = "ptg", y = "treatment_late_impact", var.equal = FALSE) #m1 = 63.32625; sd1 = 23.21039; m2 = 67.96559; sd2 = 22.77726 (155 yes, 204 no)
d5S <- (67.96559-63.32625)/sqrt(((23.21039)^2+(22.77726)^2/2)) # d = 0.164
rd5S <- d_to_r(d5S) # r = 0.082
ci5S <- CIr(r = rd5S, n = 359, level = .95)
ci5lowerS <- -0.02185734
ci5upperS <- 0.18378282
#without cancer treatment late impact coded as 0, with coded as 1

mi.t.test(data_listSurvivors, x = "ptg", y = "cancerSupportGroup", var.equal = FALSE) #m1 = 61.89867; sd1 = 23.65821; m2 = 71.06692; sd2 = 26.18781 (88 yes, 318 no)
d11S <- (71.06692-61.89867)/sqrt(((23.65821)^2+(22.15265)^2/2)) # d = 0.323
rd11S <- d_to_r(d11S) # r = 0.159
ci11S <- CIr(r = rd11S, n = 406, level = .95)
ci11lowerS <- 0.06314944
ci11upperS <- 0.25289116
#without attendance of cancer support group coded as 0, with coded as 1

rcat1S <- c(rd1S, rd2S, rd3S, rd4S, rd5S, rd11S) #correlations vector for PTG and factor variables
rcatlower1S <- c(ci1lowerS, ci2lowerS, ci3lowerS, ci4lowerS, ci5lowerS, ci11lowerS)
rcatupper1S <- c(ci1upperS, ci2upperS, ci3upperS, ci4upperS, ci5upperS, ci11upperS)

#PTSD
mi.t.test(data_listSurvivors, x = "ptsd", y = "gender", var.equal = FALSE) #m1 = 8.603333; sd1 = 5.569205; m2 = 7.155556; sd2 = 6.356634; (300 females 114 males)
d6S <- (8.281676-8.603333)/sqrt(((5.569205)^2+(6.356634)^2/2)) # d = -0.045
rd6S <- d_to_r(d6S) # r = -0.023 (convert d to r effect size)
ci6S <- CIr(r = rd6S, n = 414, level = .95)
ci6lowerS <- -0.11858757
ci6upperS <- 0.07407167
#female coded as 0, males coded as 1
#recoding order for better interpretation in plot (female will be 1 and male 0)
rd6S <- rd6S*-1
ci6lowerS <- -0.11858757*-1
ci6upperS <- 0.07407167*-1

mi.t.test(data_listSurvivors, x = "ptsd", y = "partnership", var.equal = FALSE) #m1 = 8.324521; sd1 = 5.729365; m2 = 8.869048; sd2 = 5.926822 (290 in relationship, 84 no relationship)
d7S <- (8.869048-8.324521)/sqrt(((5.729365)^2+(5.926822)^2/2)) # d = 0.077
rd7S <- d_to_r(d7S) # r = 0.038
ci7S <- CIr(r = rd7S, n = 374, level = .95)
ci7lowerS <- -0.06332593 
ci7upperS <- 0.13919221
#in partnership coded as 0, no relationship = 1
#recoding order for better interpretation in plot (in relationship will be 1 and wihtout relationship will be 0)
rd7S <- rd7S*-1
ci7lowerS <- -0.06332593*-1
ci7upperS <- 0.13919221*-1

mi.t.test(data_listSurvivors, x = "ptsd", y = "withCancerFamilyAnamnenis", var.equal = FALSE) #m1 = 9.036765; sd1 = 6.015299; m2 = 8.289497; sd2 = 5.636603 (256 yes, 136 no)
d8S <- (8.289497-9.036765)/sqrt(((6.015299)^2+(5.636603)^2/2)) # d = -0.104
rd8S <- d_to_r(d8S) # r = -0.052
ci8S <- CIr(r = rd8S, n = 392, level = .95)
ci8lowerS <- -0.14998994
ci8upperS <- 0.04758213
#without cancer anamnesis in family coded as 0, with coded as 1

mi.t.test(data_listSurvivors, x = "ptsd", y = "relaps", var.equal = FALSE) #m1 = 8.208930; sd1 = 5.648942; m2 = 9.685714; sd2 = 6.293843 (79 yes, 326 no)
d9S <- (9.658228-8.208930)/sqrt(((5.648942)^2+(6.293843)^2/2)) # d = 0.202
rd9S <- d_to_r(d9S) # r = 0.101
ci9S <- CIr(r = rd9S, n = 345, level = .95)
ci9lowerS <- -0.005386885
ci9upperS <- 0.203689244
#without cancer relapse coded as 0, with coded as 1

mi.t.test(data_listSurvivors, x = "ptsd", y = "treatment_late_impact", var.equal = FALSE) #m1 = 7.613290; sd1 = 5.507542; m2 = 10.090323; sd2 = 5.690466  (155 yes, 204 no)
d10S <- (10.090323-7.613290)/sqrt(((5.507542)^2+(5.690466 )^2/2)) # d = 0.363
rd10S <- d_to_r(d10S) # r = 0.179
ci10S <- CIr(r = rd10S, n = 359, level = .95)
ci10lowerS <- 0.07656725
ci10upperS <- 0.27704003
#without cancer treatment late impact coded as 0, with coded as 1

mi.t.test(data_listSurvivors, x = "ptsd", y = "cancerSupportGroup", var.equal = FALSE) #m1 = 8.484626; sd1 = 5.823500; m2 = 8.909091; sd2 = 5.871490 (88 yes, 318 no)
d12S <- (8.909091-8.484626)/sqrt(((5.823500)^2+(5.871490)^2/2)) # d = 0.059
rd12S <- d_to_r(d12S) # r = 0.030
ci12S <- CIr(r = rd12S, n = 406, level = .95)
ci12lowerS <- -0.06785788
ci12upperS <- 0.12661992
#without attendance of cancer support group coded as 0, with coded as 1

rcat2S <- c(rd6S, rd7S, rd8S, rd9S, rd10S, rd12S) #correlations vector for PTSD and factor variables
rcatlower2S <- c(ci6lowerS, ci7lowerS, ci8lowerS, ci9lowerS, ci10lowerS, ci12lowerS)
rcatupper2S <- c(ci6upperS, ci7upperS, ci8upperS, ci9upperS, ci10upperS, ci12upperS)

##correlations-------------------------------
correlations <- miceadds::micombine.cor(dataimpSurvivors, 
                                        variables = c(19, 12,  3,  6, 10, 11, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44), 
                                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

#making correlation groups for plot
ageS <- miceadds::micombine.cor(dataimpSurvivors, #age
                               variables = c(19, 12,  3), 
                               conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

educationS <- miceadds::micombine.cor(dataimpSurvivors, #education
                                     variables = c(19, 12, 4), 
                                     conf.level=0.95, method="spearman", nested=FALSE, partial=NULL)

diagnosis_timeS <- miceadds::micombine.cor(dataimpSurvivors, #age
                                          variables = c(19, 12,  6), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

personalityS <- miceadds::micombine.cor(dataimpSurvivors, #personality 
                                       variables = c(19, 12, 25, 26, 27, 28, 29), 
                                       conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

copingS <- miceadds::micombine.cor(dataimpSurvivors, #personality 
                                  variables = c(19, 12, 30, 31, 32, 33, 34), 
                                  conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

social_supportS <- miceadds::micombine.cor(dataimpSurvivors, #social support
                                          variables = c(19,12,36,37), 
                                          conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

eudaimonicS <- miceadds::micombine.cor(dataimpSurvivors, #existential
                                      variables = c(19,12,39,40,41,38), 
                                      conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

emotionalS <- miceadds::micombine.cor(dataimpSurvivors, #emotional
                                     variables = c(19,12,42,43,44), 
                                     conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 

spiritualS <- miceadds::micombine.cor(dataimpSurvivors, #spirituality
                                     variables = c(19,12,10,11), 
                                     conf.level=0.95, method="pearson", nested=FALSE, partial=NULL) 


#PTG and PTSD correlates
variables <- c("gender", "age", "education", "partnership",
               "cancer family anamnesis", "cancer relapse", "treatment late impact", "diagnosis time",                 
               "neuroticism", "optimism", "pesimism", "selfefficacy", "resilience",
               "helplessness-hopelessness", "anxious preoccupation", "cognitive avoidance", "fatalism", "fighting spirit", 
               "cancer support group", "social support", "medical social support", 
               "cognitive meaningfulness", "motivational meaningfulness", "afective meaningfulness", "satisfaction with life",
               "spirituality importance", "spirituality practice",
               "anxiety", "depression", "pain")

groups <- c("socio-demographic","socio-demographic","socio-demographic", "socio-demographic",
            "cancer-related", "cancer-related", "cancer-related", "cancer-related", 
            "personality", "personality", "personality", "personality", "personality", 
            "coping", "coping", "coping", "coping", "coping",
            "social support", "social support", "social support",
            "eudaimonic", "eudaimonic", "eudaimonic", "eudaimonic",
            "spiritual", "spiritual",
            "emotional", "emotional", "emotional")

subgroups1 <- rep("PTG", 30)
subgroups2 <- rep("PTSD", 30)


order1 <- seq(1,60, by=2)
order2 <- seq(2,60, by=2)

#making variables block for correlations plot
#education
educationcorS <- educationS$r
education1S <- educationcorS[2]
education2S <- educationcorS[3]

educationlowerS <- educationS$lower95
educationlower1S <- educationlowerS[2]
educationlower2S <- educationlowerS[3]

educationupperS <- educationS$upper95 
educationupper1S <- educationupperS[2]
educationupper2S <- educationupperS[3]
#age
agecorS <- ageS$r
age1S <- agecorS[2]
age2S <- agecorS[3]

agelowerS <- ageS$lower95
agelower1S <- agelowerS[2]
agelower2S <- agelowerS[3]

ageupperS <- ageS$upper95 
ageupper1S <- ageupperS[2]
ageupper2S <- ageupperS[3]

#sociodemographic correlations vector = personal group
socdem1S <- c(rd1S, age1S, education1S,rd2S) #gender, age, education, partnership PTG
socdemlower1S <- c(ci1lowerS,agelower1S, educationlower1S,ci2lowerS) 
socdemupper1S <- c(ci1upperS,ageupper1S, educationupper1S,ci2upperS) 

socdem2S <- c(rd6S,age2S, education2S,rd7S) #gender, age, education, partnership PTSD
socdemlower2S <- c(ci6lowerS,agelower2S, educationlower2S,ci7lowerS) 
socdemupper2S <- c(ci6upperS,ageupper2S, educationupper2S,ci7upperS) 

#correlations vector for cancer-related group
diagnosis_timecorS <- diagnosis_timeS$r
diagnosis_time1S <- diagnosis_timecorS[2]
diagnosis_time2S <- diagnosis_timecorS[3]

diagnosis_timelowerS <- diagnosis_timeS$lower95
diagnosis_timelower1S <- diagnosis_timelowerS[2]
diagnosis_timelower2S <- diagnosis_timelowerS[3]

diagnosis_timeupperS <- diagnosis_timeS$upper95 
diagnosis_timeupper1S <- diagnosis_timeupperS[2]
diagnosis_timeupper2S <- diagnosis_timeupperS[3]

cancer1S <- c(rd3S, rd4S, rd5S, diagnosis_time1S) #cancer in family anamnesis, cancer relapse, treatmet late impact, time since diagnosis PTG
cancerlower1S <- c(ci3lowerS,ci4lowerS,ci5lowerS, diagnosis_timelower1S) 
cancerupper1S <- c(ci3upperS,ci4upperS,ci5upperS, diagnosis_timeupper1S) 

cancer2S <- c(rd8S, rd9S, rd10S, diagnosis_time2S) #cancer in family anamnesis, cancer relapse, treatmet late impact, time since diagnosis PTSD
cancerlower2S <- c(ci8lowerS,ci9lowerS,ci10lowerS, diagnosis_timelower2S) 
cancerupper2S <- c(ci8upperS,ci9upperS,ci10upperS, diagnosis_timeupper2S) 

#personality
personalitycorS <- personalityS$r
personality1S <- personalitycorS[2:6]
personality2S <- personalitycorS[7:11]

personalitylowerS <- personalityS$lower95
personalitylower1S <- personalitylowerS[2:6]
personalitylower2S <- personalitylowerS[7:11]

personalityupperS <- personalityS$upper95 
personalityupper1S <- personalityupperS[2:6]
personalityupper2S <- personalityupperS[7:11]

#coping
copingcorS <- copingS$r
coping1S <- copingcorS[2:6]
coping2S <- copingcorS[7:11]

copinglowerS <- copingS$lower95
copinglower1S <- copinglowerS[2:6]
copinglower2S <- copinglowerS[7:11]

copingupperS <- copingS$upper95 
copingupper1S <- copingupperS[2:6]
copingupper2S <- copingupperS[7:11]

#social support
social_supportcorS <- social_supportS$r
social_support1S <- social_supportcorS[2:3]
social_support2S <- social_supportcorS[4:5]

social_supportlowerS <- social_supportS$lower95
social_supportlower1S <- social_supportlowerS[2:3]
social_supportlower2S <- social_supportlowerS[4:5]

social_supportupperS <- social_supportS$upper95 
social_supportupper1S <- social_supportupperS[2:3]
social_supportupper2S <- social_supportupperS[4:5]

social_support1S <- c(social_support1S, rd11S) #social support, medical social support, cancer support group PTG
social_supportlower1S <- c(social_supportlower1S, ci11lowerS) 
social_supportupper1S <- c(social_supportupper1S, ci11upperS) 

social_support2S <- c(social_support2S, rd12S) #social support, medical social support, cancer support group  PTSD
social_supportlower2S <- c(social_supportlower2S, ci12lowerS) 
social_supportupper2S <- c(social_supportupper2S, ci12upperS) 

#eudaimonic
eudaimoniccorS <- eudaimonicS$r
eudaimonic1S <- eudaimoniccorS[2:5]
eudaimonic2S <- eudaimoniccorS[6:9]

eudaimoniclowerS <- eudaimonicS$lower95
eudaimoniclower1S <- eudaimoniclowerS[2:5]
eudaimoniclower2S <- eudaimoniclowerS[6:9]

eudaimonicupperS <- eudaimonicS$upper95 
eudaimonicupper1S <- eudaimonicupperS[2:5]
eudaimonicupper2S <- eudaimonicupperS[6:9]

#spiritual
spiritualcorS <- spiritualS$r
spiritual1S <- spiritualcorS[2:3]
spiritual2S <- spiritualcorS[4:5]

spirituallowerS <- spiritualS$lower95
spirituallower1S <- spirituallowerS[2:3]
spirituallower2S <- spirituallowerS[4:5]

spiritualupperS <- spiritualS$upper95 
spiritualupper1S <- spiritualupperS[2:3]
spiritualupper2S <- spiritualupperS[4:5]

#emotional
emotionalcorS <- emotionalS$r
emotional1S <- emotionalcorS[2:4]
emotional2S <- emotionalcorS[5:7]

emotionallowerS <- emotionalS$lower95
emotionallower1S <- emotionallowerS[2:4]
emotionallower2S <- emotionallowerS[5:7]

emotionalupperS <- emotionalS$upper95 
emotionalupper1S <- emotionalupperS[2:4]
emotionalupper2S <- emotionalupperS[5:7]

r1S <- c(socdem1S, cancer1S, personality1S, coping1S, social_support1S, eudaimonic1S, spiritual1S, emotional1S)
r2S <- c(socdem2S, cancer2S, personality2S, coping2S, social_support2S, eudaimonic2S, spiritual2S, emotional2S)
lower1S <- c(socdemlower1S, cancerlower1S, personalitylower1S, copinglower1S, social_supportlower1S, eudaimoniclower1S, spirituallower1S, emotionallower1S)
lower2S <- c(socdemlower2S, cancerlower2S, personalitylower2S, copinglower2S, social_supportlower2S, eudaimoniclower2S, spirituallower2S, emotionallower2S)
upper1S <- c(socdemupper1S, cancerupper1S, personalityupper1S, copingupper1S, social_supportupper1S, eudaimonicupper1S, spiritualupper1S, emotionalupper1S)
upper2S <- c(socdemupper2S, cancerupper2S, personalityupper2S, copingupper2S, social_supportupper2S, eudaimonicupper2S, spiritualupper2S, emotionalupper2S)

ptgS <- tibble(order1, variables, r1S, lower1S, upper1S, groups, subgroups1)
names(ptgS) <- c("order", "variables", "r", "lower", "upper", "groups", "subgroups")
ptsdS <- tibble(order2, variables, r2S, lower2S, upper2S, groups, subgroups2)
names(ptsdS) <- c("order", "variables", "r", "lower", "upper", "groups", "subgroups")

ptgptsdS <- rbind(ptgS,ptsdS)

ptgptsdS <- ptgptsdS %>% 
  arrange(order)

#plots for PTG and PTSD correlates 

#plot subgroups (with different color for PTG and PTSD and with differet shapes for correlates group)
pdf("PTG and PTSD correlates in cancer survivors.pdf",height=12,width=10)
ggplot(data=ptgptsdS, aes(x=variables, y=r, ymin=lower, ymax=upper, color = subgroups, shape = groups)) +
  ylim(-0.75,0.75) +
  geom_pointrange(position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  facet_grid(groups~., scales= "free", space="free") + # add grouping based of defined categories of variables
  theme_bw() + # use a white background
  theme(strip.background =element_rect(fill="white")) +  # use white background in facets
  theme_minimal() + # minimal theme for plot
  xlab("correlates") + ylab("Pearson's r") +
  theme(axis.title.x = element_text(hjust= 0.5)) + # adjust axis title position
  theme(axis.title.y = element_text(size = 14)) + # adjust axis title size
  theme(plot.title = element_text(hjust = 0.5)) + # adjust title position
  labs(title = "Posttraumatic growth (PTG) and PTSD correlates in cancer survivors",
       subtitle = "Pearson Correlation between PTG and PTSD: 0.17 [.08,.26]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9, face = "bold.italic", color = "red")) +
  labs(col="Posttraumatic\nreactions") + # change name of legend 1 (\n) in two lines 
  scale_shape_manual(values = c(8,19,17,15,4,5,13,3), name = "Correlates\ncategories") + # change name of legend 2 (\n) in two lines 
  theme(legend.title.align = 0.5) + # centering of legend titles
  scale_color_manual(values = c("#66CCCC", "#660033")) # apply different colors for points (colorblind friendly)

dev.off()

#correlations table
#working table for values from correlation matrix
z <- miceadds::micombine.cor(dataimpSurvivors, 
                             variables = c(19, 12,  3,  6, 10, 11, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44), 
                             conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)
v <- z$variable1
v2 <- z$variable2
a <- z$r
b <- z$p
c <- z$lower95
d <- z$upper95

o <- tibble(v,v2,a,b,c,d)
o$a <- round(o$a, 2)
o$c <- round(o$c, 2)
o$d <- round(o$d, 2)

o$x <- symnum(o$b, corr = FALSE, na = FALSE, 
              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
              symbols = c("***", "**", "*", ",", " "))
o$b <- o$x
o$x <- NULL

o %>% 
  kbl() %>% 
  kable_material

#Bayesian correlation

dat <- data_listSurvivors

bf1 <- lapply(dat, function(x){correlationBF(x$ptg,x$ptsd)})
bf1mean <- lapply(bf1, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf1mean, function(x){mean(x)}))) #47.68313

#PTG
bf3 <- lapply(dat, function(x){correlationBF(x$ptg,x$age)})
bf3mean <- lapply(bf3, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf3mean, function(x){mean(x)}))) #1.147974

bf2 <- lapply(dat, function(x){correlationBF(x$ptg,x$diagnosis_year)})
bf2mean <- lapply(bf2, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf2mean, function(x){mean(x)}))) #2.245123

bf5 <- lapply(dat, function(x){correlationBF(x$ptg,x$spirituality_importance)})
bf5mean <- lapply(bf5, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf5mean, function(x){mean(x)}))) #255406252

bf7 <- lapply(dat, function(x){correlationBF(x$ptg,x$spirituality_practice)})
bf7mean <- lapply(bf7, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf7mean, function(x){mean(x)}))) #21850608

bf4 <- lapply(dat, function(x){correlationBF(x$ptg,x$neuroticism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #0.2702865

bf8 <- lapply(dat, function(x){correlationBF(x$ptg,x$optimism)})
bf8mean <- lapply(bf8, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf8mean, function(x){mean(x)}))) #120.2913

bf4 <- lapply(dat, function(x){correlationBF(x$ptg,x$pesimism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #0.1252609

bf6 <- lapply(dat, function(x){correlationBF(x$ptg,x$selfefficacy)})
bf6mean <- lapply(bf6, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf6mean, function(x){mean(x)}))) #1.25448

bf10 <- lapply(dat, function(x){correlationBF(x$ptg,x$resilience)})
bf10mean <- lapply(bf10, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf10mean, function(x){mean(x)}))) #0.1579644

bf13 <- lapply(dat, function(x){correlationBF(x$ptg,x$helplessnessHopelessness)})
bf13mean <- lapply(bf13, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf13mean, function(x){mean(x)}))) #34.56954

bf16 <- lapply(dat, function(x){correlationBF(x$ptg,x$anxiousPreoccupation)})
bf16mean <- lapply(bf16, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf16mean, function(x){mean(x)}))) #11.62948

bf12 <- lapply(dat, function(x){correlationBF(x$ptg,x$avoidance)})
bf12mean <- lapply(bf12, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf12mean, function(x){mean(x)}))) #2123.427

bf18 <- lapply(dat, function(x){correlationBF(x$ptg,x$fatalism)})
bf18mean <- lapply(bf18, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf18mean, function(x){mean(x)}))) #5827280362

bf14 <- lapply(dat, function(x){correlationBF(x$ptg,x$fightingSpirit)})
bf14mean <- lapply(bf14, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf14mean, function(x){mean(x)}))) #1.019769e+13

bf15 <- lapply(dat, function(x){correlationBF(x$ptg,x$socialSupport)})
bf15mean <- lapply(bf15, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf15mean, function(x){mean(x)}))) #9264.729

bf17 <- lapply(dat, function(x){correlationBF(x$ptg,x$social_support_medical)})
bf17mean <- lapply(bf17, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf17mean, function(x){mean(x)}))) #4.949023

bf9 <- lapply(dat, function(x){correlationBF(x$ptg,x$satisfactionWithLife)})
bf9mean <- lapply(bf9, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf9mean, function(x){mean(x)}))) #2.002899

bf11 <- lapply(dat, function(x){correlationBF(x$ptg,x$cognitiveMeaningInLife)})
bf11mean <- lapply(bf11, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf11mean, function(x){mean(x)}))) #15238889800

bf19 <- lapply(dat, function(x){correlationBF(x$ptg,x$motivateMeaningInLife)})
bf19mean <- lapply(bf19, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf19mean, function(x){mean(x)}))) #356331480

bf20 <- lapply(dat, function(x){correlationBF(x$ptg,x$afectiveMeaningInLife)})
bf20mean <- lapply(bf20, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf20mean, function(x){mean(x)}))) #491976.9

bf21 <- lapply(dat, function(x){correlationBF(x$ptg,x$anxiety)})
bf21mean <- lapply(bf21, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf21mean, function(x){mean(x)}))) #0.2172117

bf22 <- lapply(dat, function(x){correlationBF(x$ptg,x$depression)})
bf22mean <- lapply(bf22, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf22mean, function(x){mean(x)}))) #2.334639

bf23 <- lapply(dat, function(x){correlationBF(x$ptg,x$pain)})
bf23mean <- lapply(bf23, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf23mean, function(x){mean(x)}))) #5.823233

#PTSD
bf3 <- lapply(dat, function(x){correlationBF(x$ptsd,x$age)})
bf3mean <- lapply(bf3, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf3mean, function(x){mean(x)}))) #0.1357068

bf2 <- lapply(dat, function(x){correlationBF(x$ptsd,x$diagnosis_year)})
bf2mean <- lapply(bf2, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf2mean, function(x){mean(x)}))) #1.18445

bf5 <- lapply(dat, function(x){correlationBF(x$ptsd,x$spirituality_importance)})
bf5mean <- lapply(bf5, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf5mean, function(x){mean(x)}))) #0.166683

bf7 <- lapply(dat, function(x){correlationBF(x$ptsd,x$spirituality_practice)})
bf7mean <- lapply(bf7, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf7mean, function(x){mean(x)}))) #0.1198323

bf4 <- lapply(dat, function(x){correlationBF(x$ptsd,x$neuroticism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #2.180962e+24

bf8 <- lapply(dat, function(x){correlationBF(x$ptsd,x$optimism)})
bf8mean <- lapply(bf8, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf8mean, function(x){mean(x)}))) #445877.5

bf4 <- lapply(dat, function(x){correlationBF(x$ptsd,x$pesimism)})
bf4mean <- lapply(bf4, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf4mean, function(x){mean(x)}))) #12889202

bf6 <- lapply(dat, function(x){correlationBF(x$ptsd,x$selfefficacy)})
bf6mean <- lapply(bf6, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf6mean, function(x){mean(x)}))) #279125700

bf10 <- lapply(dat, function(x){correlationBF(x$ptsd,x$resilience)})
bf10mean <- lapply(bf10, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf10mean, function(x){mean(x)}))) #4.76052e+20

bf13 <- lapply(dat, function(x){correlationBF(x$ptsd,x$helplessnessHopelessness)})
bf13mean <- lapply(bf13, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf13mean, function(x){mean(x)}))) #131482.2

bf16 <- lapply(dat, function(x){correlationBF(x$ptsd,x$anxiousPreoccupation)})
bf16mean <- lapply(bf16, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf16mean, function(x){mean(x)}))) #3.10975e+18

bf12 <- lapply(dat, function(x){correlationBF(x$ptsd,x$avoidance)})
bf12mean <- lapply(bf12, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf12mean, function(x){mean(x)}))) #0.1341284

bf18 <- lapply(dat, function(x){correlationBF(x$ptsd,x$fatalism)})
bf18mean <- lapply(bf18, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf18mean, function(x){mean(x)}))) #0.1489708

bf14 <- lapply(dat, function(x){correlationBF(x$ptsd,x$fightingSpirit)})
bf14mean <- lapply(bf14, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf14mean, function(x){mean(x)}))) #0.1293076

bf15 <- lapply(dat, function(x){correlationBF(x$ptsd,x$socialSupport)})
bf15mean <- lapply(bf15, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf15mean, function(x){mean(x)}))) #34.02567

bf17 <- lapply(dat, function(x){correlationBF(x$ptsd,x$social_support_medical)})
bf17mean <- lapply(bf17, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf17mean, function(x){mean(x)}))) #0.5534779

bf9 <- lapply(dat, function(x){correlationBF(x$ptsd,x$satisfactionWithLife)})
bf9mean <- lapply(bf9, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf9mean, function(x){mean(x)}))) #136789.6

bf11 <- lapply(dat, function(x){correlationBF(x$ptsd,x$cognitiveMeaningInLife)})
bf11mean <- lapply(bf11, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf11mean, function(x){mean(x)}))) #1463535

bf19 <- lapply(dat, function(x){correlationBF(x$ptsd,x$motivateMeaningInLife)})
bf19mean <- lapply(bf19, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf19mean, function(x){mean(x)}))) #15428961

bf20 <- lapply(dat, function(x){correlationBF(x$ptsd,x$afectiveMeaningInLife)})
bf20mean <- lapply(bf20, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf20mean, function(x){mean(x)}))) #1279180185

bf21 <- lapply(dat, function(x){correlationBF(x$ptsd,x$anxiety)})
bf21mean <- lapply(bf21, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf21mean, function(x){mean(x)}))) #955315665

bf22 <- lapply(dat, function(x){correlationBF(x$ptsd,x$depression)})
bf22mean <- lapply(bf22, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf22mean, function(x){mean(x)}))) #110.7505

bf23 <- lapply(dat, function(x){correlationBF(x$ptsd,x$pain)})
bf23mean <- lapply(bf23, function(x){extractBF(x)$bf})
mean(unlist(lapply(bf23mean, function(x){mean(x)}))) #1.628301e+18

###########################################################--------------------------------------------------------------------------
####PTSD and PTG comparison in cancer survivors vs patients--------------------------------------------------------------------------
###########################################################--------------------------------------------------------------------------

data_listPvsS <- readRDS("dataimpPvsS.RDs") 

mi.t.test(data_listPvsS, x = "ptg", y = "in_treatment", var.equal = FALSE) 
mi.t.test(data_listPvsS, x = "ptgRO", y = "in_treatment", var.equal = FALSE)
mi.t.test(data_listPvsS, x = "ptgNP", y = "in_treatment", var.equal = FALSE)
mi.t.test(data_listPvsS, x = "ptgPS", y = "in_treatment", var.equal = FALSE)
mi.t.test(data_listPvsS, x = "ptgSCH", y = "in_treatment", var.equal = FALSE)
mi.t.test(data_listPvsS, x = "ptgAL", y = "in_treatment", var.equal = FALSE)
mi.t.test(data_listPvsS, x = "ptsd", y = "in_treatment", var.equal = FALSE)

#descriptives for groups
#patients
data_listP <- lapply(data_listPvsS, function(x){x %>% filter(in_treatment == "in treatment")}) #N = 181
data_impP <- miceadds::datlist2mids(data_listP)
#survivors
data_listS <- lapply(data_listPvsS, function(x){x %>% filter(in_treatment != "in treatment")}) #N = 419
data_impS <- miceadds::datlist2mids(data_listS)

#ptsd patients
ptsd_descriptive <- with(data_impP, expr=c("ptsd(mean)"=mean(ptsd), 
                                          "ptsd(SD)"=stats::sd(ptsd), 
                                          "ptsd(S.E)"=sd(ptsd)/sqrt(length(ptsd)), 
                                          "ptsd(min)"=min(ptsd), 
                                          "ptsd(max)"=max(ptsd)))
# pool estimates
withPool_MI(ptsd_descriptive)

#ptsd survivors
ptsd_descriptive <- with(data_impS, expr=c("ptsd(mean)"=mean(ptsd), 
                                           "ptsd(SD)"=stats::sd(ptsd), 
                                           "ptsd(S.E)"=sd(ptsd)/sqrt(length(ptsd)), 
                                           "ptsd(min)"=min(ptsd), 
                                           "ptsd(max)"=max(ptsd)))
# pool estimates
withPool_MI(ptsd_descriptive)


#posttraumatic growth patients
ptg_descriptive <- with(data_impP, expr=c("ptg(mean)"=mean(ptg), 
                                         "ptg(SD)"=stats::sd(ptg), 
                                         "ptg(S.E)"=sd(ptg)/sqrt(length(ptg)), 
                                         "ptg(min)"=min(ptg), 
                                         "ptg(max)"=max(ptg)))
# pool estimates
withPool_MI(ptg_descriptive)

#posttraumatic growth survivors
ptg_descriptive <- with(data_impS, expr=c("ptg(mean)"=mean(ptg), 
                                           "ptg(SD)"=stats::sd(ptg), 
                                           "ptg(S.E)"=sd(ptg)/sqrt(length(ptg)), 
                                           "ptg(min)"=min(ptg), 
                                           "ptg(max)"=max(ptg)))
# pool estimates
withPool_MI(ptg_descriptive)

#posttraumatic growth relation to others patients
ptgRO_descriptive <- with(data_impP, expr=c("ptgRO(mean)"=mean(ptgRO), 
                                          "ptgRO(SD)"=stats::sd(ptgRO), 
                                          "ptgRO(S.E)"=sd(ptgRO)/sqrt(length(ptgRO)), 
                                          "ptgRO(min)"=min(ptgRO), 
                                          "ptgRO(max)"=max(ptgRO)))
# pool estimates
withPool_MI(ptgRO_descriptive)

#posttraumatic growth relation to others survivors
ptgRO_descriptive <- with(data_impS, expr=c("ptgRO(mean)"=mean(ptgRO), 
                                            "ptgRO(SD)"=stats::sd(ptgRO), 
                                            "ptgRO(S.E)"=sd(ptgRO)/sqrt(length(ptgRO)), 
                                            "ptgRO(min)"=min(ptgRO), 
                                            "ptgRO(max)"=max(ptgRO)))
# pool estimates
withPool_MI(ptgRO_descriptive)

#posttraumatic growth new possibilities patiens
ptgNP_descriptive <- with(data_impP, expr=c("ptgNP(mean)"=mean(ptgNP), 
                                            "ptgNP(SD)"=stats::sd(ptgNP), 
                                            "ptgNP(S.E)"=sd(ptgNP)/sqrt(length(ptgNP)), 
                                            "ptgNP(min)"=min(ptgNP), 
                                            "ptgNP(max)"=max(ptgNP)))
# pool estimates
withPool_MI(ptgNP_descriptive)

#posttraumatic growth new possibilities survivors
ptgNP_descriptive <- with(data_impS, expr=c("ptgNP(mean)"=mean(ptgNP), 
                                            "ptgNP(SD)"=stats::sd(ptgNP), 
                                            "ptgNP(S.E)"=sd(ptgNP)/sqrt(length(ptgNP)), 
                                            "ptgNP(min)"=min(ptgNP), 
                                            "ptgNP(max)"=max(ptgNP)))
# pool estimates
withPool_MI(ptgNP_descriptive)

#posttraumatic growth personal strenght patiens
ptgPS_descriptive <- with(data_impP, expr=c("ptgPS(mean)"=mean(ptgPS), 
                                            "ptgPS(SD)"=stats::sd(ptgPS), 
                                            "ptgPS(S.E)"=sd(ptgPS)/sqrt(length(ptgPS)), 
                                            "ptgPS(min)"=min(ptgPS), 
                                            "ptgPS(max)"=max(ptgPS)))
# pool estimates
withPool_MI(ptgPS_descriptive)

#posttraumatic growth personal strength survivors
ptgPS_descriptive <- with(data_impS, expr=c("ptgPS(mean)"=mean(ptgPS), 
                                            "ptgPS(SD)"=stats::sd(ptgPS), 
                                            "ptgPS(S.E)"=sd(ptgPS)/sqrt(length(ptgPS)), 
                                            "ptgPS(min)"=min(ptgPS), 
                                            "ptgPS(max)"=max(ptgPS)))
# pool estimates
withPool_MI(ptgPS_descriptive)

#posttraumatic growth spiritual changes patiens
ptgSCH_descriptive <- with(data_impP, expr=c("ptgSCH(mean)"=mean(ptgSCH), 
                                            "ptgSCH(SD)"=stats::sd(ptgSCH), 
                                            "ptgSCH(S.E)"=sd(ptgSCH)/sqrt(length(ptgSCH)), 
                                            "ptgSCH(min)"=min(ptgSCH), 
                                            "ptgSCH(max)"=max(ptgSCH)))
# pool estimates
withPool_MI(ptgSCH_descriptive)

#posttraumatic growth spiritual changes survivors
ptgSCH_descriptive <- with(data_impS, expr=c("ptgSCH(mean)"=mean(ptgSCH), 
                                             "ptgSCH(SD)"=stats::sd(ptgSCH), 
                                             "ptgSCH(S.E)"=sd(ptgSCH)/sqrt(length(ptgSCH)), 
                                             "ptgSCH(min)"=min(ptgSCH), 
                                             "ptgSCH(max)"=max(ptgSCH)))
# pool estimates
withPool_MI(ptgSCH_descriptive)

#posttraumatic growth appreciation of life patiens
ptgAL_descriptive <- with(data_impP, expr=c("ptgAL(mean)"=mean(ptgAL), 
                                             "ptgAL(SD)"=stats::sd(ptgAL), 
                                             "ptgAL(S.E)"=sd(ptgAL)/sqrt(length(ptgAL)), 
                                             "ptgAL(min)"=min(ptgAL), 
                                             "ptgAL(max)"=max(ptgAL)))
# pool estimates
withPool_MI(ptgAL_descriptive)

#posttraumatic growth appreciation of life survivors
ptgAL_descriptive <- with(data_impS, expr=c("ptgAL(mean)"=mean(ptgAL), 
                                             "ptgAL(SD)"=stats::sd(ptgAL), 
                                             "ptgAL(S.E)"=sd(ptgAL)/sqrt(length(ptgAL)), 
                                             "ptgAL(min)"=min(ptgAL), 
                                             "ptgAL(max)"=max(ptgAL)))
# pool estimates
withPool_MI(ptgAL_descriptive)

#-----------------------------------------------------------------------------------------------------------------------
###NETWORKS and Networks comparison-------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

#select patients

dataP <- read.csv("data_averageNetworkP.csv")

ptgPtsdDataP <- setnames(dataP, 
                        old=c("ptgi6","ptgi8", "ptgi9","ptgi15", "ptgi16",
                              "ptgi20", "ptgi21",
                              "ptgi3","ptgi7", "ptgi11", "ptgi14", "ptgi17",
                              "ptgi4","ptgi10", "ptgi12", "ptgi19", 
                              "ptgi5", "ptgi18", 
                              "ptgi1", "ptgi2", "ptgi13",
                              "ptsd1", "ptsd2", "ptsd3", "ptsd4", "ptsd5", "ptsd6"), 
                        new=c("OTPEOP", "CLOSE", "EMOT", "COMP", "EFFOR", 
                              "WOND", "NEEDS",
                              "NINTER", "PATH", "BETTR", "NOPP", "CHANG",
                              "SREL", "DIFF", "ACCPT","STRG", 
                              "SPIR", "FAITH", 
                              "PRIOR", "VALUE", "APPR",
                              "Nightm", "Flash", "AvThought", "AvSit", "Hyperv", "Startl"))

###Polychoric and partial correlations

#correlations
correlations <- polychoric(ptgPtsdDataP)

rho <- correlations$rho
rho[upper.tri(rho)] <- NA
rho <- round(rho, 2)

tab02 <- data.frame(
  variables = c("1.OTEOP", "2.CLOSE", "3.EMOT", "4.COMP","5.EFFORT", "6.WOND","7.NEEDS","8.NINTER","9.PATH","10.BETTR","11.NOPP","12.CHANG","13.SREL","14.DIFF","15.ACCPT","16.STGR","17.SPIR","18.FAITH","19.PRIOR","20.VALUE","21.APPR","22.Nightm","23.Flash","24.AvThought","25.AvSit","26.Hyper","27.Startl"),
  a = c("",rho[2:27,1]),
  b = c("","",rho[3:27,2]),
  c = c("","","",rho[4:27,3]),
  d = c("","","","",rho[5:27,4]),
  e = c("","","","","",rho[6:27,5]),
  f = c("","","","","","",rho[7:27,6]),
  g = c("","","","","","","",rho[8:27,7]),
  h = c("","","","","","","","",rho[9:27,8]),
  i = c("","","","","","","","","",rho[10:27,9]),
  j = c("","","","","","","","","","",rho[11:27,10]),
  k = c("","","","","","","","","","","",rho[12:27,11]),
  l = c("","","","","","","","","","","","",rho[13:27,12]),
  m = c("","","","","","","","","","","","","",rho[14:27,13]),
  n = c("","","","","","","","","","","","","","",rho[15:27,14]),
  o = c("","","","","","","","","","","","","","","",rho[16:27,15]),
  p = c("","","","","","","","","","","","","","","","",rho[17:27,16]),
  r = c("","","","","","","","","","","","","","","","","",rho[18:27,17]),
  s = c("","","","","","","","","","","","","","","","","","",rho[19:27,18]),
  t = c("","","","","","","","","","","","","","","","","","","",rho[20:27,19]),
  q = c("","","","","","","","","","","","","","","","","","","","",rho[21:27,20]),
  u = c("","","","","","","","","","","","","","","","","","","","","",rho[22:27,21]),
  v = c("","","","","","","","","","","","","","","","","","","","","","",rho[23:27,22]),
  w = c("","","","","","","","","","","","","","","","","","","","","","","",rho[24:27,23]),
  x = c("","","","","","","","","","","","","","","","","","","","","","","","",rho[25:27,24]),
  y = c("","","","","","","","","","","","","","","","","","","","","","","","","",rho[26:27,25]),
  z = c("","","","","","","","","","","","","","","","","","","","","","","","","","",rho[27,26])
)

rownames(tab02) <- NULL

kable(
  tab02,
  knitr.table.format = "latex",
  longtable = TRUE,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26"),
  align = c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l"),
  caption = "Polychoric correlations between Posttraumatic growth indicators and PTSD symptoms"
) %>% 
  row_spec(row = 0, align = "c") %>% 
  kable_styling(full_width = TRUE) %>% 
  footnote(
    general_title = "Note.",
    general = "1-21 represents indicators of Posttraumatic growth, and 22-27 represents symptoms of PTSD according to ICD-11.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) 

# average Polychoric correlation between PTG a PTSD, average PTG, and PTSD correlation
rho[rho == 1] <- NA
round(mean(rho[1:21,1:20], na.rm = TRUE), 2) #PTG average correlation .49
round(mean(rho[22:27,22:26], na.rm = TRUE), 2) #PTSD average corr .60
round(mean(rho[22:27,1:21], na.rm = TRUE), 2) #PTSD-PTG average corr .03
max(rho[22:27,1:21], na.rm = TRUE) #highest positive correlation between PTSD sx and PTG ind .32 (flashback and spirituality) 
min(rho[22:27,1:21], na.rm = TRUE) #highest negative correlation between PTSD sx and PTG ind -.21 (external avoidance and handling difficulties) 

#partial correlation
partialCor <- pcor(ptgPtsdDataP)
partialCorrelations <- partialCor$estimate
partialCorrelations[upper.tri(partialCorrelations)] <- NA
partialCorrelations <- round(partialCorrelations, 2)

tab02 <- data.frame(
  variables = c("1.OTEOP", "2.CLOSE", "3.EMOT", "4.COMP","5.EFFORT", "6.WOND","7.NEEDS","8.NINTER","9.PATH","10.BETTR","11.NOPP","12.CHANG","13.SREL","14.DIFF","15.ACCPT","16.STGR","17.SPIR","18.FAITH","19.PRIOR","20.VALUE","21.APPR","22.Nightm","23.Flash","24.AvThought","25.AvSit","26.Hyper","27.Startl"),
  a = c("",partialCorrelations[2:27,1]),
  b = c("","",partialCorrelations[3:27,2]),
  c = c("","","",partialCorrelations[4:27,3]),
  d = c("","","","",partialCorrelations[5:27,4]),
  e = c("","","","","",partialCorrelations[6:27,5]),
  f = c("","","","","","",partialCorrelations[7:27,6]),
  g = c("","","","","","","",partialCorrelations[8:27,7]),
  h = c("","","","","","","","",partialCorrelations[9:27,8]),
  i = c("","","","","","","","","",partialCorrelations[10:27,9]),
  j = c("","","","","","","","","","",partialCorrelations[11:27,10]),
  k = c("","","","","","","","","","","",partialCorrelations[12:27,11]),
  l = c("","","","","","","","","","","","",partialCorrelations[13:27,12]),
  m = c("","","","","","","","","","","","","",partialCorrelations[14:27,13]),
  n = c("","","","","","","","","","","","","","",partialCorrelations[15:27,14]),
  o = c("","","","","","","","","","","","","","","",partialCorrelations[16:27,15]),
  p = c("","","","","","","","","","","","","","","","",partialCorrelations[17:27,16]),
  r = c("","","","","","","","","","","","","","","","","",partialCorrelations[18:27,17]),
  s = c("","","","","","","","","","","","","","","","","","",partialCorrelations[19:27,18]),
  t = c("","","","","","","","","","","","","","","","","","","",partialCorrelations[20:27,19]),
  q = c("","","","","","","","","","","","","","","","","","","","",partialCorrelations[21:27,20]),
  u = c("","","","","","","","","","","","","","","","","","","","","",partialCorrelations[22:27,21]),
  v = c("","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[23:27,22]),
  w = c("","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[24:27,23]),
  x = c("","","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[25:27,24]),
  y = c("","","","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[26:27,25]),
  z = c("","","","","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[27,26])
)

rownames(tab02) <- NULL

kable(
  tab02,
  knitr.table.format = "latex",
  longtable = TRUE,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26"),
  align = c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l"),
  caption = "Partial correlations between Posttraumatic growth indicators and PTSD symptoms"
) %>% 
  row_spec(row = 0, align = "c") %>% 
  kable_styling(full_width = TRUE) %>% 
  footnote(
    general_title = "Note.",
    general = "1-21 represents indicators of Posttraumatic growth, and 22-27 represents symptoms of PTSD according to ICD-11.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) 

#average partial correlation
partialCorrelations[partialCorrelations == 1] <- NA
round(mean(partialCorrelations[1:21,1:20], na.rm = TRUE), 2) #PTG average partial correlation .05
round(mean(partialCorrelations[22:27,22:26], na.rm = TRUE), 2) #PTSD average partial corr .17
round(mean(partialCorrelations[22:27,1:21], na.rm = TRUE), 5) #PTG-PTSD average corr .002
max(partialCorrelations[22:27,1:21], na.rm = TRUE) #highest positive correlation between PTG indicators and PTSD sx .22 (FAITH and Hypervigilance)
min(partialCorrelations[22:27,1:21], na.rm = TRUE) #highest negative correlation between PTG indicators and PTSD sx -.18 (Spirituality and Hypervigilance)

#polychoric correlation plot
pdf("Polychoric corr_P.pdf",height=10,width=16)
rhoMatrix <- correlations$rho #matrix of polychoric correlation
corrplot(rhoMatrix,method="color", type = "upper", addCoef.col="black", outline=F, diag=T, 
         col=colorRampPalette(c("deepskyblue1","white","indianred3")) (200), 
         tl.cex = 1, number.cex = 0.6,  cl.cex = 1.1, tl.pos = "td")
dev.off()

#partial correlation plot
partialCor <- partialCor$estimate #matrix of partial correlation
pdf("Partial corr_P.pdf",height=10,width=16)
corrplot(partialCor,method="color", type = "upper", addCoef.col="black", outline=F, diag=T, 
         col=colorRampPalette(c("deepskyblue1","white","indianred3")) (200), 
         tl.cex = 1, number.cex = 0.6,  cl.cex = 1.1, tl.pos = "td")
dev.off()


#PTSD network-----------------------------------------------

ptsdDataP <- ptgPtsdDataP[,22:27]

Factors <- c(rep("Reexperiencing", 2),
             rep("Avoidance", 2),
             rep("Sense of Threat", 2)) 

namenet <- c("Nightmares",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Hypervigilance",
             "Startle response")


##ptsd graph

ptsdNetP <- estimateNetwork(ptsdDataP, default = "EBICglasso")

pdf("PTSD_P.pdf",height=10,width=16)
PTSD_P <- plot(ptsdNetP,
             layout = "spring",
             cut = 0,
             theme = "colorblind",
             groups = Factors,
             nodeNames = namenet,
             legend.cex = 0.7, legend.mode = "style1",
             title = "PTSD symptoms network in patients with cancer (in treatment)", 
             title.cex = 1.6)
dev.off()


##ptsd network accurancy and stability

NetworkPTSD_P <- estimateNetwork(ptsdDataP, default = "EBICglasso")
boot1PTSD_P <- bootnet(NetworkPTSD_P, nBoots = 2500, nCores = 1)
pdf("PTSD_P_accurancy.pdf",height=10,width=16)
plot(boot1PTSD_P, labels = FALSE, order = "sample")
dev.off()

set.seed(8)
boot2PTSD_P <- bootnet(ptsdNetP, nBoots = 2500, type = "case", 
                     nCores = 1, statistics = c("strength", "closeness", "betweenness", 
                                                "expectedInfluence", "edge"))

pdf("PTSD_P_centrality_stability.pdf",height=10,width=16)
plot(boot2PTSD_P, statistics = c("strength", "closeness", "betweenness", "expectedInfluence"))
dev.off()

corStability(boot2PTSD_P, cor = 0.7, statistics = "all", verbose = TRUE) #edge = 0.674 
#strength = 0.127
#EI = 0.204
#closeness = 0.127
#betweenness = 0

##centrality plot ptsd

pdf("PTSD_P_centrality.pdf",height=12,width=10)
centralityPlot(PTSD_P, include = c("ExpectedInfluence"))
dev.off()

#edge difference plot
pdf("PTSD_edges_difference_P.pdf",height=12,width=10)
plot(boot1PTSD_P, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

#strength difference plot
pdf("PTSD_strength_difference_P.pdf",height=12,width=10)
plot(boot1PTSD_P, "strength")
dev.off()

## network simulation

simnetworkP <- netSimulator(input = NetworkPTSD_P, dataGenerator = ggmGenerator(), 
                           default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                           nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD_P_simulation.pdf",height=10,width=16)
plot(simnetworkP)
dev.off()

repilcatenetworkP <- replicationSimulator(input = NetworkPTSD_P, dataGenerator = ggmGenerator(), 
                                         default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                         nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD_P_replication.pdf",height=10,width=16)
plot(repilcatenetworkP)
dev.off()

#PTG network-------------------------------------------

ptgDataP <- dataP[,1:21]

Factors <- c(rep("Relating to others", 7), 
             rep("New possibilities", 5), 
             rep("Personal strength", 4), 
             rep("Spiritual change", 2),
             rep("Appreciation of life", 3)) 

namenet <- c("Count on people in time of troubles",
             "Sense of closeness with others",
             "Willingness to express emotions",
             "Compassion for others",
             "Effort into relationships",
             "How wonderful people are",
             "Accept needs of others",
             "New interests",
             "New path for life",
             "Better things with life",
             "New opportunities",
             "Openess to do changes",
             "Self-reliance",
             "Handling difficulties",
             "Accept the way things work out",
             "Personal strength",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Changing life priorities", 
             "Appreciation of life",
             "Appreciation of each day")

ptgNetP <- estimateNetwork(ptgDataP, default = "EBICglasso")

pdf("PTG_P.pdf",height=10,width=16)
PTG_P <- plot(ptgNetP,
            layout = "spring",
            cut = 0,
            theme = "colorblind",
            groups = Factors,
            nodeNames = namenet,
            legend.cex = 0.5, legend.mode = "style1",
            title = "Posttraumatic growth indicators network in patients with cancer (in treatment)",
            title.cex = 2)
dev.off()

##ptg network accurancy and stability

NetworkPTG_P <- estimateNetwork(ptgDataP, default = "EBICglasso")
boot1_P <- bootnet(NetworkPTG_P, nBoots = 2500, nCores = 1)

pdf("PTG_P_accurancy.pdf",height=10,width=16)
plot(boot1_P, labels = FALSE, order = "sample")
dev.off()

set.seed(8)
boot2PTG_P <- bootnet(ptgNetP, nBoots = 2500, type = "case", caseN = 50,
                    statistics = c("strength", "closeness", "betweenness", 
                                               "expectedInfluence", "edge"))

pdf("PTG_P_centrality_stability.pdf",height=10,width=16)
plot(boot2PTG_P, statistics = c("strength", "closeness", "betweenness", "expectedInfluence"))
dev.off()

corStability(boot2PTG_P, cor = 0.7, statistics = "all", verbose = TRUE) #edge = 0.552
#strength = 0.365
#EI = 0.448
#between = 0.077
#close = 0.204

##centrality plot ptg

pdf("PTG_P_centrality.pdf",height=12,width=10)
centralityPlot(PTG_P, include = "ExpectedInfluence")
dev.off()

#edge difference plot
pdf("PTG_edges_difference_P.pdf",height=12,width=10)
plot(boot1_P, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

#strength difference plot
pdf("PTG_strength_difference_P.pdf",height=12,width=10)
plot(boot1_P, "strength")
dev.off()


## network simulation

PTG_P_simnetwork <- netSimulator(input = NetworkPTG_P, dataGenerator = ggmGenerator(), 
                               default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                               nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTG_P_simulation.pdf",height=10,width=16)
plot(PTG_P_simnetwork)
dev.off()

PTG_repilcatenetworkP <- replicationSimulator(input = NetworkPTG_P, dataGenerator = ggmGenerator(), 
                                             default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                             nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTG_P_replication.pdf",height=10,width=16)
plot(PTG_repilcatenetworkP)
dev.off()

################-----------------------------------------------------------------------------------------------------
#select survivors----------------------------------------------------------------------------------------------------
################-----------------------------------------------------------------------------------------------------

dataS <- read.csv("data_averageNetworkS.csv")

ptgPtsdDataS <- setnames(dataS, 
                         old=c("ptgi6","ptgi8", "ptgi9","ptgi15", "ptgi16",
                               "ptgi20", "ptgi21",
                               "ptgi3","ptgi7", "ptgi11", "ptgi14", "ptgi17",
                               "ptgi4","ptgi10", "ptgi12", "ptgi19", 
                               "ptgi5", "ptgi18", 
                               "ptgi1", "ptgi2", "ptgi13",
                               "ptsd1", "ptsd2", "ptsd3", "ptsd4", "ptsd5", "ptsd6"), 
                         new=c("OTPEOP", "CLOSE", "EMOT", "COMP", "EFFOR", 
                               "WOND", "NEEDS",
                               "NINTER", "PATH", "BETTR", "NOPP", "CHANG",
                               "SREL", "DIFF", "ACCPT","STRG", 
                               "SPIR", "FAITH", 
                               "PRIOR", "VALUE", "APPR",
                               "Nightm", "Flash", "AvThought", "AvSit", "Hyperv", "Startl"))

###Polychoric and partial correlations

#correlations
correlations <- polychoric(ptgPtsdDataS)

rho <- correlations$rho
rho[upper.tri(rho)] <- NA
rho <- round(rho, 2)

tab04 <- data.frame(
  variables = c("1.OTEOP", "2.CLOSE", "3.EMOT", "4.COMP","5.EFFORT", "6.WOND","7.NEEDS","8.NINTER","9.PATH","10.BETTR","11.NOPP","12.CHANG","13.SREL","14.DIFF","15.ACCPT","16.STGR","17.SPIR","18.FAITH","19.PRIOR","20.VALUE","21.APPR","22.Nightm","23.Flash","24.AvThought","25.AvSit","26.Hyper","27.Startl"),
  a = c("",rho[2:27,1]),
  b = c("","",rho[3:27,2]),
  c = c("","","",rho[4:27,3]),
  d = c("","","","",rho[5:27,4]),
  e = c("","","","","",rho[6:27,5]),
  f = c("","","","","","",rho[7:27,6]),
  g = c("","","","","","","",rho[8:27,7]),
  h = c("","","","","","","","",rho[9:27,8]),
  i = c("","","","","","","","","",rho[10:27,9]),
  j = c("","","","","","","","","","",rho[11:27,10]),
  k = c("","","","","","","","","","","",rho[12:27,11]),
  l = c("","","","","","","","","","","","",rho[13:27,12]),
  m = c("","","","","","","","","","","","","",rho[14:27,13]),
  n = c("","","","","","","","","","","","","","",rho[15:27,14]),
  o = c("","","","","","","","","","","","","","","",rho[16:27,15]),
  p = c("","","","","","","","","","","","","","","","",rho[17:27,16]),
  r = c("","","","","","","","","","","","","","","","","",rho[18:27,17]),
  s = c("","","","","","","","","","","","","","","","","","",rho[19:27,18]),
  t = c("","","","","","","","","","","","","","","","","","","",rho[20:27,19]),
  q = c("","","","","","","","","","","","","","","","","","","","",rho[21:27,20]),
  u = c("","","","","","","","","","","","","","","","","","","","","",rho[22:27,21]),
  v = c("","","","","","","","","","","","","","","","","","","","","","",rho[23:27,22]),
  w = c("","","","","","","","","","","","","","","","","","","","","","","",rho[24:27,23]),
  x = c("","","","","","","","","","","","","","","","","","","","","","","","",rho[25:27,24]),
  y = c("","","","","","","","","","","","","","","","","","","","","","","","","",rho[26:27,25]),
  z = c("","","","","","","","","","","","","","","","","","","","","","","","","","",rho[27,26])
)

rownames(tab04) <- NULL

kable(
  tab02,
  knitr.table.format = "latex",
  longtable = TRUE,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26"),
  align = c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l"),
  caption = "Polychoric correlations between Posttraumatic growth indicators and PTSD symptoms"
) %>% 
  row_spec(row = 0, align = "c") %>% 
  kable_styling(full_width = TRUE) %>% 
  footnote(
    general_title = "Note.",
    general = "1-21 represents indicators of Posttraumatic growth, and 22-27 represents symptoms of PTSD according to ICD-11.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) 

# average Polychoric correlation between PTG a PTSD, average PTG, and PTSD correlation
rho[rho == 1] <- NA
round(mean(rho[1:21,1:20], na.rm = TRUE), 2) #PTG average correlation .53
round(mean(rho[22:27,22:26], na.rm = TRUE), 2) #PTSD average corr .54
round(mean(rho[22:27,1:21], na.rm = TRUE), 2) #PTSD-PTG average corr .11
max(rho[22:27,1:21], na.rm = TRUE) #highest positive correlation between PTSD sx and PTG ind .31 (hypervigilance and priority) 
min(rho[22:27,1:21], na.rm = TRUE) #highest negative correlation between PTSD sx and PTG ind -.09 (external avoidance and dslcosure emotions) 

#partial correlation
partialCor <- pcor(ptgPtsdDataS)
partialCorrelations <- partialCor$estimate
partialCorrelations[upper.tri(partialCorrelations)] <- NA
partialCorrelations <- round(partialCorrelations, 2)

tab05 <- data.frame(
  variables = c("1.OTEOP", "2.CLOSE", "3.EMOT", "4.COMP","5.EFFORT", "6.WOND","7.NEEDS","8.NINTER","9.PATH","10.BETTR","11.NOPP","12.CHANG","13.SREL","14.DIFF","15.ACCPT","16.STGR","17.SPIR","18.FAITH","19.PRIOR","20.VALUE","21.APPR","22.Nightm","23.Flash","24.AvThought","25.AvSit","26.Hyper","27.Startl"),
  a = c("",partialCorrelations[2:27,1]),
  b = c("","",partialCorrelations[3:27,2]),
  c = c("","","",partialCorrelations[4:27,3]),
  d = c("","","","",partialCorrelations[5:27,4]),
  e = c("","","","","",partialCorrelations[6:27,5]),
  f = c("","","","","","",partialCorrelations[7:27,6]),
  g = c("","","","","","","",partialCorrelations[8:27,7]),
  h = c("","","","","","","","",partialCorrelations[9:27,8]),
  i = c("","","","","","","","","",partialCorrelations[10:27,9]),
  j = c("","","","","","","","","","",partialCorrelations[11:27,10]),
  k = c("","","","","","","","","","","",partialCorrelations[12:27,11]),
  l = c("","","","","","","","","","","","",partialCorrelations[13:27,12]),
  m = c("","","","","","","","","","","","","",partialCorrelations[14:27,13]),
  n = c("","","","","","","","","","","","","","",partialCorrelations[15:27,14]),
  o = c("","","","","","","","","","","","","","","",partialCorrelations[16:27,15]),
  p = c("","","","","","","","","","","","","","","","",partialCorrelations[17:27,16]),
  r = c("","","","","","","","","","","","","","","","","",partialCorrelations[18:27,17]),
  s = c("","","","","","","","","","","","","","","","","","",partialCorrelations[19:27,18]),
  t = c("","","","","","","","","","","","","","","","","","","",partialCorrelations[20:27,19]),
  q = c("","","","","","","","","","","","","","","","","","","","",partialCorrelations[21:27,20]),
  u = c("","","","","","","","","","","","","","","","","","","","","",partialCorrelations[22:27,21]),
  v = c("","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[23:27,22]),
  w = c("","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[24:27,23]),
  x = c("","","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[25:27,24]),
  y = c("","","","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[26:27,25]),
  z = c("","","","","","","","","","","","","","","","","","","","","","","","","","",partialCorrelations[27,26])
)

rownames(tab05) <- NULL

kable(
  tab02,
  knitr.table.format = "latex",
  longtable = TRUE,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26"),
  align = c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l"),
  caption = "Partial correlations between Posttraumatic growth indicators and PTSD symptoms"
) %>% 
  row_spec(row = 0, align = "c") %>% 
  kable_styling(full_width = TRUE) %>% 
  footnote(
    general_title = "Note.",
    general = "1-21 represents indicators of Posttraumatic growth, and 22-27 represents symptoms of PTSD according to ICD-11.",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) 

#average partial correlation
partialCorrelations[partialCorrelations == 1] <- NA
round(mean(partialCorrelations[1:21,1:20], na.rm = TRUE), 2) #PTG average partial correlation .05
round(mean(partialCorrelations[22:27,22:26], na.rm = TRUE), 2) #PTSD average partial corr .16
round(mean(partialCorrelations[22:27,1:21], na.rm = TRUE), 5) #PTG-PTSD average corr .001
max(partialCorrelations[22:27,1:21], na.rm = TRUE) #highest positive correlation between PTG indicators and PTSD sx .14 (Putting effort to relationships and Hypervigilance)
min(partialCorrelations[22:27,1:21], na.rm = TRUE) #highest negative correlation between PTG indicators and PTSD sx -.13 (external avoidance and dslcosure emotions)

#polychoric correlation plot
pdf("Polychoric corr_S.pdf",height=10,width=16)
rhoMatrix <- correlations$rho #matrix of polychoric correlation
corrplot(rhoMatrix,method="color", type = "upper", addCoef.col="black", outline=F, diag=T, 
         col=colorRampPalette(c("deepskyblue1","white","indianred3")) (200), 
         tl.cex = 1, number.cex = 0.6,  cl.cex = 1.1, tl.pos = "td")
dev.off()

#partial correlation plot
partialCor <- partialCor$estimate #matrix of partial correlation
pdf("Partial corr_S.pdf",height=10,width=16)
corrplot(partialCor,method="color", type = "upper", addCoef.col="black", outline=F, diag=T, 
         col=colorRampPalette(c("deepskyblue1","white","indianred3")) (200), 
         tl.cex = 1, number.cex = 0.6,  cl.cex = 1.1, tl.pos = "td")
dev.off()

#PTSD network-----------------------------------------------

ptsdDataS <- ptgPtsdDataS[,22:27]

Factors <- c(rep("Reexperiencing", 2),
             rep("Avoidance", 2),
             rep("Sense of Threat", 2)) 

namenet <- c("Nightmares",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Hypervigilance",
             "Startle response")


##ptgnet graph

ptsdNetS <- estimateNetwork(ptsdDataS, default = "EBICglasso")

pdf("PTSD_S.pdf",height=10,width=16)
PTSD_S <- plot(ptsdNetS,
               layout = PTSD_P$layout,
               cut = 0,
               theme = "colorblind",
               groups = Factors,
               nodeNames = namenet,
               legend.cex = 0.7, legend.mode = "style1",
               title = "PTSD symptoms network in cancer survivors", 
               title.cex = 1.6)
dev.off()


##ptsd network accurancy and stability

NetworkPTSD_S <- estimateNetwork(ptsdDataS, default = "EBICglasso")
boot1PTSD_S <- bootnet(NetworkPTSD_S, nBoots = 2500, nCores = 1)
pdf("PTSD_S_accurancy.pdf",height=10,width=16)
plot(boot1PTSD_S, labels = FALSE, order = "sample")
dev.off()

set.seed(8)
boot2PTSD_S <- bootnet(ptsdNetS, nBoots = 2500, type = "case", caseN = 50,
                       statistics = c("strength", "closeness", "betweenness", 
                                                  "expectedInfluence", "edge"))

pdf("PTSD_S_centrality_stability.pdf",height=10,width=16)
plot(boot2PTSD_S, statistics = c("strength", "closeness", "betweenness", "expectedInfluence"))
dev.off()

corStability(boot2PTSD_S, cor = 0.7, statistics = "all", verbose = TRUE) #edge = 0.75 
#strength = 0.55
#EI = 0.55
#closeness = 0.265
#betweenness = 0.236

##centrality plot ptsd

pdf("PTSD_S_centrality.pdf",height=12,width=10)
centralityPlot(PTSD_S, include = c("ExpectedInfluence"))
dev.off()

pdf("PTSD_S_edge_difference.pdf",height=12,width=10)
plot(boot1PTSD_S, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

pdf("PTSD_S_strength_difference.pdf",height=12,width=10)
plot(boot1PTSD_S, "strength")
dev.off()

## network simulation

simnetworkS <- netSimulator(input = NetworkPTSD_S, dataGenerator = ggmGenerator(), 
                            default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                            nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD_S_simulation.pdf",height=10,width=16)
plot(simnetworkS)
dev.off()

repilcatenetworkS <- replicationSimulator(input = NetworkPTSD_S, dataGenerator = ggmGenerator(), 
                                          default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                          nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD_S_replication.pdf",height=10,width=16)
plot(repilcatenetworkS)
dev.off()

#PTG network-------------------------------------------

ptgDataS <- dataS[,1:21]

Factors <- c(rep("Relating to others", 7), 
             rep("New possibilities", 5), 
             rep("Personal strength", 4), 
             rep("Spiritual change", 2),
             rep("Appreciation of life", 3)) 

namenet <- c("Count on people in time of troubles",
             "Sense of closeness with others",
             "Willingness to express emotions",
             "Compassion for others",
             "Effort into relationships",
             "How wonderful people are",
             "Accept needs of others",
             "New interests",
             "New path for life",
             "Better things with life",
             "New opportunities",
             "Openess to do changes",
             "Self-reliance",
             "Handling difficulties",
             "Accept the way things work out",
             "Personal strength",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Changing life priorities", 
             "Appreciation of life",
             "Appreciation of each day")

ptgNetS <- estimateNetwork(ptgDataS, default = "EBICglasso")

pdf("PTG_S.pdf",height=10,width=16)
PTG_S <- plot(ptgNetS,
              layout = PTG_P$layout,
              cut = 0,
              theme = "colorblind",
              groups = Factors,
              nodeNames = namenet,
              legend.cex = 0.5, legend.mode = "style1",
              title = "Posttraumatic growth indicators network in cancer survivors",
              title.cex = 2)
dev.off()

##ptg network accurancy and stability

NetworkPTG_S <- estimateNetwork(ptgDataS, default = "EBICglasso")
boot1_S <- bootnet(NetworkPTG_S, nBoots = 2500, nCores = 1)

pdf("PTG_S_accurancy.pdf",height=10,width=16)
plot(boot1_S, labels = FALSE, order = "sample")
dev.off()

set.seed(8)
boot2PTG_S <- bootnet(ptgNetS, nBoots = 2500, type = "case", caseN = 50,
                      statistics = c("strength", "closeness", "betweenness", 
                                                 "expectedInfluence", "edge"))

pdf("PTG_S_centrality_stability.pdf",height=10,width=16)
plot(boot2PTG_S, statistics = c("strength", "closeness", "betweenness", "expectedInfluence"))
dev.off()

corStability(boot2PTG_S, cor = 0.7, statistics = "all", verbose = TRUE) #edge = 0.75
#strength = 0.58
#EI = 0.66
#between = 0
#close = 0.09

##centrality plot ptg

pdf("PTG_S_centrality.pdf",height=12,width=10)
centralityPlot(PTG_S, include = "ExpectedInfluence")
dev.off()

pdf("PTG_S_edge_difference.pdf",height=12,width=10)
plot(boot1_S, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

pdf("PTG_S_strength_difference.pdf",height=12,width=10)
plot(boot1_S, "strength")
dev.off()


## network simulation

PTG_S_simnetwork <- netSimulator(input = NetworkPTG_S, dataGenerator = ggmGenerator(), 
                                 default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                 nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTG_S_simulation.pdf",height=10,width=16)
plot(PTG_S_simnetwork)
dev.off()

PTG_repilcatenetworkS <- replicationSimulator(input = NetworkPTG_S, dataGenerator = ggmGenerator(), 
                                              default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                              nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTG_S_replication.pdf",height=10,width=16)
plot(PTG_repilcatenetworkS)
dev.off()

#PTSD and PTG bridge symptoms--------------------------------Patients-------------------------------------------------
dataPF <- dataP[c("PRIOR", "APPR",
                  "PATH", "BETTR",
                  "SPIR", "FAITH",
                  "CLOSE", "WOND",
                  "DIFF","STRG",
                  "Nightm", "Flash", "AvThought", "AvSit", "Hyperv", "Startl")]

Factors <- c(rep("Posttraumatic growth", 10), 
             rep("PTSD", 6)) 

namenet <- c("Changing life priorities",
             "Appreciation of life",
             "New path for life",
             "Better things with life",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Sense of closeness with others",
             "How wonderful people are",
             "Handling difficulties",
             "Personal strength",
             "Nightmares",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Hypervigilance",
             "Startle response")

ptsdPtgNetP <- estimateNetwork(dataPF , default = "EBICglasso")

#accurancy and stabilty
boot1PTSDPTG_P <- bootnet(ptsdPtgNetP, nBoots = 2500, nCores = 1)
pdf("PTSD-PTG_P_accurancy.pdf",height=10,width=16)
plot(boot1PTSDPTG_P , labels = FALSE, order = "sample")
dev.off()

set.seed(8)
boot2PtgPtsdP <- bootnet(ptsdPtgNetP, nBoots = 2500, type = "case", caseN = 50, 
                        statistics = c("strength", "closeness", "betweenness", 
                                       "expectedInfluence", "edge"))

pdf("PTSD_PTG_centrality_stabilityP.pdf", height = 10, width = 16)
PTSD_PTG_centrality_stability <- plot(boot2PtgPtsdP, statistics = c("strength", "closeness","expectedInfluence"))
dev.off()

corStability(boot2PtgPtsdP, cor = 0.7, statistics = "all", verbose = TRUE) #edge = 0.56, closeness = 0.077, betweenn = 0, strength = 0.265, EI = 0.221

set.seed(7)
bridge_centrality_stabilityP <- bootnet(ptsdPtgNetP, statistics=c("edge", "expectedInfluence", "bridgeExpectedInfluence", "bridgeStrength"), communities=c(rep("Growth", 10), rep("PTSD", 6)),
                                       nBoots=2500, type="case", caseN=50)
corStability(bridge_centrality_stabilityP, statistics=c("edge", "expectedInfluence", "bridgeExpectedInfluence", "bridgeStrength")) # BEI = .293, BS = .149

pdf("PTSD_PTG_centrality_bridge_stabilityP.pdf", height = 10, width = 16)
plot(bridge_centrality_stabilityP, statistics = c("bridgeExpectedInfluence", "bridgeStrength"))
dev.off()

#edge difference plot
pdf("PTSD-PTG_edges_difference_P.pdf",height=12,width=10)
plot(boot1PTSDPTG_P, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

#strength difference plot
pdf("PTSD-PTG_strength_difference_P.pdf",height=12,width=10)
plot(boot1PTSDPTG_P, "strength")
dev.off()


## network simulation

PTSD_PTG_P_simnetwork <- netSimulator(input = ptsdPtgNetP, dataGenerator = ggmGenerator(), 
                                 default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                 nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD-PTG_P_simulation.pdf",height=10,width=16)
plot(PTSD_PTG_P_simnetwork)
dev.off()

PTSD_PTG_repilcatenetworkP <- replicationSimulator(input = ptsdPtgNetP, dataGenerator = ggmGenerator(), 
                                              default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                              nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD-PTG_P_replication.pdf",height=10,width=16)
plot(PTSD_PTG_repilcatenetworkP)
dev.off()

#bridge

myNetworkP <- EBICglasso(cor(dataPF, use = "pairwise"),
                         n=696, gamma = 0.5)

communityStructure <- Factors
## Let’s plot our network with qgraph:
qgraph(myNetworkP, groups = communityStructure, layout="spring", color=c("black", "grey"))
## Now we are ready to look at bridge centrality!
## It’s a one-liner with the bridge() function:
bridgeCentrality <- bridge(myNetworkP, communities=communityStructure)
bridgeCentrality
## It’s very easy to get a nice plot of this:
plot(bridgeCentrality) ## We are missing a couple of values because a couple nodes are completely unconnected
## We can narrow down the type of plots to display using the “include” argument
plot(bridgeCentrality, include="Bridge Strength")
## We can also change how the nodes are ordered in the plot, using the “order” argument
pdf("PTSD_PTGSFBridges_centrality_P.pdf", height = 12, width = 10)
bridgeHigherCenter <- plot(bridgeCentrality, include=c("Bridge Strength"), order="value")
dev.off()
## You can also plot z-values instead of raw centrality scores:
plot(bridgeCentrality, zscore=TRUE, order = "value")
## We can extract each type of bridge centrality easily:
bridgeCentrality$`Bridge Strength`
bridgeCentrality$`Bridge Betweenness`
bridgeCentrality$`Bridge Closeness`
bridgeCentrality$`Bridge Expected Influence (1-step)`
bridgeCentrality$`Bridge Expected Influence (2-step)`
## What about those nice plots where bridges are colored?
## It’s a little more tricky, but we can do it
## Select the top 80th percentile bridge strength:
bridgeStrength <- bridgeCentrality$`Bridge Strength`
topBridges <- names(bridgeStrength[bridgeStrength>quantile(bridgeStrength, probs=0.79, na.rm=TRUE)])
## Now create a new community vector where bridges are their own “community”
bridgeNumW1 <- which(names(bridgeStrength) %in% topBridges)
newCommunities <- vector()
for(i in 1:length(bridgeStrength)) {
  if(i %in% bridgeNumW1) {
    newCommunities[i] <- "Bridge symptoms"
  } else {newCommunities[i] <- communityStructure[i]}
}
## And now use that community vector as your “groups” in qgraph!
qgraph(myNetworkP, layout="spring", groups=newCommunities, color=c("white", "grey", "black"))
## Check out ?bridge and ?plot.bridge for extra information. Good luck!

#bridge plot
dataPF <- dataP[c("DIFF", "APPR",
                 "PATH", 
                 "SPIR", "FAITH",
                 "CLOSE", "WOND",
                 "STRG",
                 "Flash", "Hyperv","AvSit", "Startl",
                 "PRIOR", "BETTR", "Nightm", "AvThought")]

Factors <- c(rep("Posttraumatic growth", 8), 
             rep("PTSD", 4),
             rep("Bridge symptoms", 4)) 

namenet <- c("Handling difficulties",
             "Appreciation of life",
             "New path for life",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Sense of closeness with others",
             "How wonderful people are",
             "Personal strength",
             "Flashbacks",
             "Hypervigilance",
             "Avoidance of situations",
             "Startle response",
             "Changing life priorities",
             "Better things with life",
             "Nightmares",
             "Avoidance of thoughts")

ptsdPtgBridgeP <- estimateNetwork(dataPF, default = "EBICglasso")

pdf("PTSD_PTG_bridges_P.pdf", height = 10, width = 16)
bridgePatientsNet <- plot(ptsdPtgBridgeP,
                        layout = "spring",
                        cut = 0,
                        theme = "colorblind",
                        color = c("#F0E442", "#56B4E9", "#009E73"),
                        groups = Factors,
                        nodeNames = namenet,
                        legend.cex = 0.5, legend.mode = "style1",
                        title = "PTSD and PTG with bridge symptoms in patients with cancer")
dev.off()


#PTSD and PTG bridge symptoms--------------------------------SURVIVORS-------------------------------------------------
dataSF <- dataS[c("PRIOR", "APPR",
                  "PATH", "BETTR",
                  "SPIR", "FAITH",
                  "CLOSE", "WOND",
                  "DIFF","STRG",
                  "Nightm", "Flash", "AvThought", "AvSit", "Hyperv", "Startl")]

Factors <- c(rep("Posttraumatic growth", 10), 
             rep("PTSD", 6)) 

namenet <- c("Changing life priorities",
             "Appreciation of life",
             "New path for life",
             "Better things with life",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Sense of closeness with others",
             "How wonderful people are",
             "Handling difficulties",
             "Personal strength",
             "Nightmares",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Hypervigilance",
             "Startle response")

ptsdPtgNetS <- estimateNetwork(dataSF , default = "EBICglasso")

#accurancy and stabilty
boot1PTSDPTG_S <- bootnet(ptsdPtgNetS, nBoots = 2500, nCores = 1)
pdf("PTSD-PTG_S_accurancy.pdf",height=10,width=16)
plot(boot1PTSDPTG_S , labels = FALSE, order = "sample")
dev.off()

set.seed(8)
boot2PtgPtsdS <- bootnet(ptsdPtgNetS, nBoots = 2500, type = "case", caseN = 50, 
                        statistics = c("strength", "closeness", "betweenness", 
                                       "expectedInfluence", "edge"))

pdf("PTSD_PTG_centrality_stabilityS.pdf", height = 10, width = 16)
PTSD_PTG_centrality_stabilityS <- plot(boot2PtgPtsdS, statistics = c("strength", "closeness","expectedInfluence"))
dev.off()

corStability(boot2PtgPtsdS, cor = 0.7, statistics = "all", verbose = TRUE) #edge = 0.75, closeness = 0.093, betweenn = 0, strength = 0.635, EI = 0.635

set.seed(7)
bridge_centrality_stabilityS <- bootnet(ptsdPtgNetS, statistics=c("edge", "expectedInfluence", "bridgeExpectedInfluence", "bridgeStrength"), communities=c(rep("Growth", 10), rep("PTSD", 6)),
                                       nBoots=2500, type="case", caseN=50)
corStability(bridge_centrality_stabilityS, statistics=c("edge", "expectedInfluence", "bridgeExpectedInfluence", "bridgeStrength")) # BEI = .535, BS = .193

pdf("PTSD_PTG_centrality_bridge_stabilityS.pdf", height = 10, width = 16)
plot(bridge_centrality_stabilityS, statistics = c("bridgeExpectedInfluence", "bridgeStrength"))
dev.off()


#edge difference plot
pdf("PTSD-PTG_edges_difference_S.pdf",height=12,width=10)
plot(boot1PTSDPTG_S, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

#strength difference plot
pdf("PTSD-PTG_strength_difference_S.pdf",height=12,width=10)
plot(boot1PTSDPTG_S, "strength")
dev.off()


## network simulation

PTSD_PTG_S_simnetwork <- netSimulator(input = ptsdPtgNetS, dataGenerator = ggmGenerator(), 
                                      default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                      nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD-PTG_S_simulation.pdf",height=10,width=16)
plot(PTSD_PTG_S_simnetwork)
dev.off()

PTSD_PTG_repilcatenetworkS <- replicationSimulator(input = ptsdPtgNetP, dataGenerator = ggmGenerator(), 
                                                   default = "ggmModSelect", stepwise = FALSE, corMethod = "cor",
                                                   nCores = 1, nCases = c(250, 350, 500, 1000, 2500))

pdf("PTSD-PTG_S_replication.pdf",height=10,width=16)
plot(PTSD_PTG_repilcatenetworkS)
dev.off()

#bridge

myNetworkS <- EBICglasso(cor(dataSF, use = "pairwise"),
                              n=696, gamma = 0.5)

communityStructure <- Factors
## Let’s plot our network with qgraph:
q <- qgraph(myNetworkS, groups = communityStructure, layout="spring", color=c("black", "grey"))
## Now we are ready to look at bridge centrality!
## It’s a one-liner with the bridge() function:
bridgeCentrality <- bridge(myNetworkS, communities=communityStructure)
bridgeCentrality
## It’s very easy to get a nice plot of this:
plot(bridgeCentrality) ## We are missing a couple of values because a couple nodes are completely unconnected
## We can narrow down the type of plots to display using the “include” argument
plot(bridgeCentrality, include="Bridge Strength")
## We can also change how the nodes are ordered in the plot, using the “order” argument
pdf("PTSD_PTGSFBridges_centrality_S.pdf", height = 12, width = 10)
bridgeHigherCenter <- plot(bridgeCentrality, include=c("Bridge Strength"), order="value")
dev.off()
## You can also plot z-values instead of raw centrality scores:
plot(bridgeCentrality, zscore=TRUE, order = "value")
## We can extract each type of bridge centrality easily:
bridgeCentrality$`Bridge Strength`
bridgeCentrality$`Bridge Betweenness`
bridgeCentrality$`Bridge Closeness`
bridgeCentrality$`Bridge Expected Influence (1-step)`
bridgeCentrality$`Bridge Expected Influence (2-step)`
## What about those nice plots where bridges are colored?
## It’s a little more tricky, but we can do it
## Select the top 80th percentile bridge strength:
bridgeStrength <- bridgeCentrality$`Bridge Strength`
topBridges <- names(bridgeStrength[bridgeStrength>quantile(bridgeStrength, probs=0.79, na.rm=TRUE)])
## Now create a new community vector where bridges are their own “community”
bridgeNumW1 <- which(names(bridgeStrength) %in% topBridges)
newCommunities <- vector()
for(i in 1:length(bridgeStrength)) {
  if(i %in% bridgeNumW1) {
    newCommunities[i] <- "Bridge symptoms"
  } else {newCommunities[i] <- communityStructure[i]}
}
## And now use that community vector as your “groups” in qgraph!
qgraph(myNetworkS, layout="spring", groups=newCommunities, color=c("white", "grey", "black"))
## Check out ?bridge and ?plot.bridge for extra information. Good luck!

#bridge plot
dataSF <- dataS[c("DIFF", "APPR",
                 "PATH", "BETTR",
                 "SPIR", "FAITH",
                 "CLOSE", "WOND",
                 "STRG",
                 "Flash", "AvThought","AvSit",
                 "PRIOR", "Nightm", "Hyperv", "Startl")]

Factors <- c(rep("Posttraumatic growth", 9), 
             rep("PTSD", 3),
             rep("Bridge symptoms", 4)) 

namenet <- c("Handling difficulties",
             "Appreciation of life",
             "New path for life",
             "Better things with life",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Sense of closeness with others",
             "How wonderful people are",
             "Personal strength",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Changing life priorities",
             "Nightmares",
             "Hypervigilance",
             "Startle response")

ptsdPtgBridgeS <- estimateNetwork(dataSF, default = "EBICglasso")

pdf("PTSD_PTG_bridges_S.pdf", height = 10, width = 16)
bridgeSurvivorsNet <- plot(ptsdPtgBridgeS,
                        layout = "spring",
                        cut = 0,
                        theme = "colorblind",
                        color = c("#F0E442", "#56B4E9", "#009E73"),
                        groups = Factors,
                        nodeNames = namenet,
                        legend.cex = 0.5, legend.mode = "style1",
                        title = "PTSD and PTG with bridge symptoms in cancer survivors")
dev.off()

#network comparison##################################################################################################

#PTSD
Factors <- c(rep("Reexperiencing", 2),
             rep("Avoidance", 2),
             rep("Sense of Threat", 2)) 

ptsdComparePS <- NCT(ptsdDataP, ptsdDataS, it = 100, binary.data = FALSE, weighted = TRUE, communities = Factors,
                        test.edges = TRUE, edges = "all", p.adjust.methods = "bonferroni", gamma = 0.5,
                        test.centrality = TRUE, progressbar = TRUE)

ptsdComparePS
plot(ptsdComparePS, what="network")
plot(ptsdComparePS, what="strength")
plot(ptsdComparePS, what="edge")

pdf("PTSD_SP.pdf", height = 12, width = 10)
centralityPlot(
  list(patients = ptsdNetP,
       survivors = ptsdNetS), 
  include = c("Strength"),
  decreasing = TRUE)
dev.off()

#PTG
Factors <- c(rep("Relating to others", 7), 
             rep("New possibilities", 5), 
             rep("Personal strength", 4), 
             rep("Spiritual change", 2),
             rep("Appreciation of life", 3)) 

ptgComparePS <- NCT(ptgDataP, ptgDataS, it = 100, binary.data = FALSE, weighted = TRUE, communities = Factors,
                    test.edges = TRUE, edges = "all", p.adjust.methods = "bonferroni", gamma = 0.5,
                    test.centrality = TRUE, progressbar = TRUE)

ptgComparePS
plot(ptgComparePS, what="network")
plot(ptgComparePS, what="strength")
plot(ptgComparePS, what="edge")

pdf("PTG_SP.pdf", height = 12, width = 10)
centralityPlot(
  list(patients = ptgNetP,
       survivors = ptgNetS), 
  include = c("Strength"),
  decreasing = TRUE)
dev.off()

#PTSD-PTG
Factors <- c(rep("Posttraumatic growth", 10), 
             rep("PTSD", 6)) 

ptsdPtgComparePS <- NCT(dataPF, dataSF, it = 100, binary.data = FALSE, weighted = TRUE, communities = Factors,
                      test.edges = TRUE, edges = "all", p.adjust.methods = "bonferroni", gamma = 0.5,
                      test.centrality = TRUE, progressbar = TRUE)

ptsdPtgComparePS
plot(ptsdPtgComparePS, what="network")
plot(ptsdPtgComparePS, what="strength")
plot(ptsdPtgComparePS, what="edge")

#PTSD-PTG centrality plots
#patients
dataPF <- dataP[c("PRIOR", "APPR",
                  "PATH", "BETTR",
                  "SPIR", "FAITH",
                  "CLOSE", "WOND",
                  "DIFF","STRG",
                  "Nightm", "Flash", "AvThought", "AvSit", "Hyperv", "Startl")]

Factors <- c(rep("Posttraumatic growth", 10), 
             rep("PTSD", 6)) 

namenet <- c("Changing life priorities",
             "Appreciation of life",
             "New path for life",
             "Better things with life",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Sense of closeness with others",
             "How wonderful people are",
             "Handling difficulties",
             "Personal strength",
             "Nightmares",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Hypervigilance",
             "Startle response")

communityStructure <- Factors

myNetworkP <- EBICglasso(cor(dataPF, use = "pairwise"),
                         n=419, gamma = 0.5)

bridgeCentralityP <- bridge(myNetworkP, communities=communityStructure)

pdf("PTSD_PTGSF_bridgeP.pdf", height = 12, width = 10)
plot(bridgeCentralityP, color = TRUE, colpalette = "Dark2", include=c("Bridge Strength", "Bridge Expected Influence (1-step)"))
dev.off()

#survivors
dataSF <- dataS[c("PRIOR", "APPR",
                  "PATH", "BETTR",
                  "SPIR", "FAITH",
                  "CLOSE", "WOND",
                  "DIFF","STRG",
                  "Nightm", "Flash", "AvThought", "AvSit", "Hyperv", "Startl")]

Factors <- c(rep("Posttraumatic growth", 10), 
             rep("PTSD", 6)) 

namenet <- c("Changing life priorities",
             "Appreciation of life",
             "New path for life",
             "Better things with life",
             "Understanding of spiritual matters",
             "Stronger religious faith",
             "Sense of closeness with others",
             "How wonderful people are",
             "Handling difficulties",
             "Personal strength",
             "Nightmares",
             "Flashbacks",
             "Avoidance of thoughts",
             "Avoidance of situations",
             "Hypervigilance",
             "Startle response")

communityStructure <- Factors

myNetworkS <- EBICglasso(cor(dataSF, use = "pairwise"),
                              n=419, gamma = 0.5)

bridgeCentralityS <- bridge(myNetworkS, communities=communityStructure)

pdf("PTSD_PTGSF_bridgeS.pdf", height = 12, width = 10)
plot(bridgeCentralityS, color = TRUE, colpalette = "Dark2", include=c("Bridge Strength", "Bridge Expected Influence (1-step)"))
dev.off()
