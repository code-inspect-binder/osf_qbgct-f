#========================================================#
#       OA Survey: Attitudes towards OA Practices        #
#========================================================#

#By Daniel Toribio-Florez, June 2020
#Codebook with variables labelling is accesible at: https://osf.io/qbgct/

#============================== #
#           PACKAGES            #
#============================== #
library(tidyverse)
library(cowplot)
library(psych)
library(Rmisc)
library(Hmisc)
library(apaTables)
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(effects)
library(emmeans)
options(scipen=99)    


#### DEMOGRAPHICS ####
# Gender.
count(df$Gender)
#Position.
count(df$Position)
#Age.
count(df$Age) # Three participants reported to be underage, but this is not possible.
# We assume it is a mistake in the report of their age, and force to NA.
df$Age <- ifelse(df$Age < 18, NA, df$Age)
summary(df$Age) #Mean = 28.09.
sd(df$Age, na.rm = TRUE) #SD = 2.91.
#Center Age.
df$Age.c <- scale(df$Age, center=TRUE, scale=FALSE)

#Section.
count_sec <- count(df$Section)%>% filter(!is.na(x))
count_sec$x <- recode(as.factor(count_sec$x),
                      "BM" = "Biomedical\nSciences",
                      "CPT" = "Chemistry,\nPhysics and,\nTechnology",
                      "HUM" = "Humanities")


#### VARIABLES AND DESCRIPTIVE GRAPHS ####
#============================== #
      #### Knowledge ####
#============================== #
#Correlation Current / Desired across OA Practices.
rcorr(as.matrix(df[,c('P1Know.P1Know01.','P1Know.P1Know02.',
                      'P2Know.P2Know01.','P2Know.P2Know02.',
                      'P3Know.P3Know01.','P3Know.P3Know02.',
                      'P4Know.P4Know01.','P4Know.P4Know02.',
                      'P5Know.P5Know01.','P5Know.P5Know02.')]))

#Long Format data set: Knowledge.
df_know <- df %>%
  select(Responder_ID,Position,MPI,Section,Gender,Age.c,
         P1Know.P1Know01.,P2Know.P2Know01.,P3Know.P3Know01.,P4Know.P4Know01.,P5Know.P5Know01.,
         P1Know.P1Know02.,P2Know.P2Know02.,P3Know.P3Know02.,P4Know.P4Know02.,P5Know.P5Know02.) %>%
  pivot_longer(cols = P1Know.P1Know01.:P5Know.P5Know02.,
               names_to = "Item",
               values_to = "Knowledge")
#Create variable OAPractice from Item.
df_know$OAPractice <- recode(df_know$Item,
                             P1Know.P1Know01. = "OA Publications",P1Know.P1Know02. = "OA Publications",
                             P2Know.P2Know01. = "Open Data",P2Know.P2Know02. = "Open Data",
                             P3Know.P3Know01. = "Pre-registrations",P3Know.P3Know02. = "Pre-registrations",
                             P4Know.P4Know01. = "Registered Reports",P4Know.P4Know02. = "Registered Reports",
                             P5Know.P5Know01. = "Replication Studies",P5Know.P5Know02. = "Replication Studies")
#Create variable Current_Desired Knowledge from Item.
df_know$Current_Desired <- recode(df_know$Item,
                                  P1Know.P1Know01. = "Are you knowledgeable about...",P1Know.P1Know02. = "Would you like to know more about...",
                                  P2Know.P2Know01. = "Are you knowledgeable about...",P2Know.P2Know02. = "Would you like to know more about...",
                                  P3Know.P3Know01. = "Are you knowledgeable about...",P3Know.P3Know02. = "Would you like to know more about...",
                                  P4Know.P4Know01. = "Are you knowledgeable about...",P4Know.P4Know02. = "Would you like to know more about...",
                                  P5Know.P5Know01. = "Are you knowledgeable about...",P5Know.P5Know02. = "Would you like to know more about...")


#Calculate KNOWLEDGE GAP: Difference between DESIRED and CURRENT Knowledge for each OAPractice.
df$KnowledgeGap1 <- df$P1Know.P1Know02.-df$P1Know.P1Know01.
df$KnowledgeGap2 <- df$P2Know.P2Know02.-df$P2Know.P2Know01.
df$KnowledgeGap3 <- df$P3Know.P3Know02.-df$P3Know.P3Know01.
df$KnowledgeGap4 <- df$P4Know.P4Know02.-df$P4Know.P4Know01.
df$KnowledgeGap5 <- df$P5Know.P5Know02.-df$P5Know.P5Know01.

#Long Format data set: KnowledgeGAP.
df_knowgap <- df %>%
  select(Responder_ID,Position,MPI,Section,Gender,Age.c,
         KnowledgeGap1,KnowledgeGap2,KnowledgeGap3,KnowledgeGap4,KnowledgeGap5) %>%
  pivot_longer(cols = KnowledgeGap1:KnowledgeGap5,
               names_to = "Item",
               values_to = "KnowledgeGap")
#Create variable OAPractice from Item.
df_knowgap$OAPractice <- recode(df_knowgap$Item,
                             KnowledgeGap1 = "OA Publications",
                             KnowledgeGap2 = "Open Data",
                             KnowledgeGap3 = "Pre-registrations",
                             KnowledgeGap4 = "Registered Reports",
                             KnowledgeGap5 = "Replication Studies")

### ANALYSES ###
#Does Knowledge Gap about OA Practices differ across Sections?
mod_knowgap1 <- lmer(KnowledgeGap~Section*OAPractice+(1|Responder_ID),
                     df_knowgap)
summary(mod_knowgap1)
anova(mod_knowgap1) # Omnibus test.
eta_sq(mod_knowgap1, partial= TRUE)
# For investigating the Section*OAPractice significant interaction
# we compare Estimated Marginal means between Section across OAPractices.
emm_knowgap_SOAPractice <- emmeans(mod_knowgap1, specs = ~ Section|OAPractice)
pairs(emm_knowgap_SOAPractice, simple = "Section", adjust = "bonferroni")

#Do demographics affect results (i.e., Age, Gender)?
mod_knowgap2 <- lmer(KnowledgeGap~Age.c+Gender*Section*OAPractice+(1|Responder_ID),
                     filter(df_knowgap, Gender != "Other" | is.na(Gender)))
summary(mod_knowgap2)
anova(mod_knowgap2) # Omnibus test.
eta_sq(mod_knowgap2, partial = TRUE)
# Difference between Males and Females.
df_knowgap %>% group_by(Gender) %>% summarise(mean = mean(KnowledgeGap, na.rm=TRUE)) # Observed Means.
emm_knowgap_Gender <- emmeans(mod_knowgap2, specs = ~ Gender,
                              data = filter(df_knowgap, Gender != "Other" | is.na(Gender)))  #Marginal means.
pairs(emm_knowgap_Gender, simple = "Gender", adjust = "bonferroni")

# Across OA Practices and Sections, FEMALE PHDs have a bigger gap between Current and Desired KnowledgeGap.
# Is this due to LOWER CURRENT KNOWLEDGE or because of HIGHER DESIRED KNOWLEDGE or BOTH?

#Current Knowledge. Filter from df_know which included also Desired Knowledge.
mod_know1 <- lmer(Knowledge~Section*OAPractice+(1|Responder_ID),
                  filter(df_know,Current_Desired == "Are you knowledgeable about..."))
summary(mod_know1)
anova(mod_know1) # Omnibus test.
eta_sq(mod_know1, partial = TRUE)
#Differences across Sections per OA Practice.
emm_know1_SOAPractice <- emmeans(mod_know1, specs = ~ Section|OAPractice,
                                 data = filter(df_know,Current_Desired == "Are you knowledgeable about..."))
pairs(emm_know1_SOAPractice, simple = "Section", adjust = "bonferroni")

#Do demographics affect results (i.e., Age, Gender)?
mod_know1b <- lmer(Knowledge~Age.c+Gender*Section*OAPractice+(1|Responder_ID),
                   filter(df_know,Current_Desired == "Are you knowledgeable about..." &
                            (Gender != "Other" | is.na(Gender))))
anova(mod_know1b)

#Desired Knowledge. Filter from df_know which included also Current Knowledge.
mod_know2 <- lmer(Knowledge~Section*OAPractice+(1|Responder_ID),
                   filter(df_know,Current_Desired == "Would you like to know more about..."))
summary(mod_know2)
anova(mod_know2)# Omnibus test.
eta_sq(mod_know2, partial = TRUE)
#Differences between Sections across OA practices.
emm_know2_SOAPractice <- emmeans(mod_know2, specs = ~ Section|OAPractice,
                                 data=filter(df_know,Current_Desired == "Would you like to know more about..."))
pairs(emm_know2_SOAPractice, simple = "Section", adjust = "bonferroni")

#Do demographics affect results (i.e., Age, Gender)?
mod_know2 <- lmer(Knowledge~Age.c+Gender*Section*OAPractice+(1|Responder_ID),
                  data=filter(df_know,Current_Desired == "Would you like to know more about..."&
                    (Gender != "Other" | is.na(Gender))))
anova(mod_know2)
eta_sq(mod_know2, partial = TRUE)
#Differences between Males and Females.
emm_know2_Gender <- emmeans(mod_know2, specs = ~ Gender)
pairs(emm_know2_Gender, adjust = "bonferroni")
#Differences across OA practices..
emm_know2_OAPractice <- emmeans(mod_know2, specs = ~ OAPractice)
pairs(emm_know2_OAPractice, adjust = "bonferroni")
# Across OA Practices and Sections, FEMALE PhDs seem to report higher DESIRED KNOWLEDGE than MALES.

#NOTE: These results can imply:
  # - Males tended to respond the survey in a more socially desirable way (i.e., pretend to be smart)
  #   or females were more humble in their responses.

#-------------------------------------------------------------------------#

#============================== #
    #### Attitudes ####
#============================== #
# Calculate Attitudinal Valence (Positive - Negative Attitudes) for each OA Practice and Level.
## OA Publications.
# LEVEL A_Daily Research Life.
# Useful-Useless.
df$P1AttValenceA01 <- df$P1AttitudesA.P1AttitudesA01.-df$P1AttitudesA.P1AttitudesA02.
# Advantageous - Disadvantageous.
df$P1AttValenceA02 <- df$P1AttitudesA.P1AttitudesA03.-df$P1AttitudesA.P1AttitudesA04.
# Beneficial-Harmful.
df$P1AttValenceA03 <- df$P1AttitudesA.P1AttitudesA05.-df$P1AttitudesA.P1AttitudesA06.
# LEVEL B_Research Field.
# Useful-Useless.
df$P1AttValenceB01 <- df$P1AttitudesB.P1AttitudesB01.-df$P1AttitudesB.P1AttitudesB02.
# Advantageous - Disadvantageous.
df$P1AttValenceB02 <- df$P1AttitudesB.P1AttitudesB03.-df$P1AttitudesB.P1AttitudesB04.
# Beneficial-Harmful.
df$P1AttValenceB03 <- df$P1AttitudesB.P1AttitudesB05.-df$P1AttitudesB.P1AttitudesB06.
# LEVEL C_Research Field.
# Useful-Useless.
df$P1AttValenceC01 <- df$P1AttitudesC.P1AttitudesC01.-df$P1AttitudesC.P1AttitudesC02.
# Advantageous - Disadvantageous.
df$P1AttValenceC02 <- df$P1AttitudesC.P1AttitudesC03.-df$P1AttitudesC.P1AttitudesC04.
# Beneficial-Harmful.
df$P1AttValenceC03 <- df$P1AttitudesC.P1AttitudesC05.-df$P1AttitudesC.P1AttitudesC06.
## Open Data.
# LEVEL A_Daily Research Life.
# Useful-Useless.
df$P2AttValenceA01 <- df$P2AttitudesA.P2AttitudesA01.-df$P2AttitudesA.P2AttitudesA02.
# Advantageous - Disadvantageous.
df$P2AttValenceA02 <- df$P2AttitudesA.P2AttitudesA03.-df$P2AttitudesA.P2AttitudesA04.
# Beneficial-Harmful.
df$P2AttValenceA03 <- df$P2AttitudesA.P2AttitudesA05.-df$P2AttitudesA.P2AttitudesA06.
# LEVEL B_Research Field.
# Useful-Useless.
df$P2AttValenceB01 <- df$P2AttitudesB.P2AttitudesB01.-df$P2AttitudesB.P2AttitudesB02.
# Advantageous - Disadvantageous.
df$P2AttValenceB02 <- df$P2AttitudesB.P2AttitudesB03.-df$P2AttitudesB.P2AttitudesB04.
# Beneficial-Harmful.
df$P2AttValenceB03 <- df$P2AttitudesB.P2AttitudesB05.-df$P2AttitudesB.P2AttitudesB06.
# LEVEL C_Research Field.
# Useful-Useless.
df$P2AttValenceC01 <- df$P2AttitudesC.P2AttitudesC01.-df$P2AttitudesC.P2AttitudesC02.
# Advantageous - Disadvantageous.
df$P2AttValenceC02 <- df$P2AttitudesC.P2AttitudesC03.-df$P2AttitudesC.P2AttitudesC04.
# Beneficial-Harmful.
df$P2AttValenceC03 <- df$P2AttitudesC.P2AttitudesC05.-df$P2AttitudesC.P2AttitudesC06.
## Pre-Registrations.
# LEVEL A_Daily Research Life.
# Useful-Useless.
df$P3AttValenceA01 <- df$P3AttitudesA.P3AttitudesA01.-df$P3AttitudesA.P3AttitudesA02.
# Advantageous - Disadvantageous.
df$P3AttValenceA02 <- df$P3AttitudesA.P3AttitudesA03.-df$P3AttitudesA.P3AttitudesA04.
# Beneficial-Harmful.
df$P3AttValenceA03 <- df$P3AttitudesA.P3AttitudesA05.-df$P3AttitudesA.P3AttitudesA06.
# LEVEL B_Research Field.
# Useful-Useless.
df$P3AttValenceB01 <- df$P3AttitudesB.P3AttitudesB01.-df$P3AttitudesB.P3AttitudesB02.
# Advantageous - Disadvantageous.
df$P3AttValenceB02 <- df$P3AttitudesB.P3AttitudesB03.-df$P3AttitudesB.P3AttitudesB04.
# Beneficial-Harmful.
df$P3AttValenceB03 <- df$P3AttitudesB.P3AttitudesB05.-df$P3AttitudesB.P3AttitudesB06.
# LEVEL C_Research Field.
# Useful-Useless.
df$P3AttValenceC01 <- df$P3AttitudesC.P3AttitudesC01.-df$P3AttitudesC.P3AttitudesC02.
# Advantageous - Disadvantageous.
df$P3AttValenceC02 <- df$P3AttitudesC.P3AttitudesC03.-df$P3AttitudesC.P3AttitudesC04.
# Beneficial-Harmful.
df$P3AttValenceC03 <- df$P3AttitudesC.P3AttitudesC05.-df$P3AttitudesC.P3AttitudesC06.
## Registered-Reports.
# LEVEL A_Daily Research Life.
# Useful-Useless.
df$P4AttValenceA01 <- df$P4AttitudesA.P4AttitudesA01.-df$P4AttitudesA.P4AttitudesA02.
# Advantageous - Disadvantageous.
df$P4AttValenceA02 <- df$P4AttitudesA.P4AttitudesA03.-df$P4AttitudesA.P4AttitudesA04.
# Beneficial-Harmful.
df$P4AttValenceA03 <- df$P4AttitudesA.P4AttitudesA05.-df$P4AttitudesA.P4AttitudesA06.
# LEVEL B_Research Field.
# Useful-Useless.
df$P4AttValenceB01 <- df$P4AttitudesB.P4AttitudesB01.-df$P4AttitudesB.P4AttitudesB02.
# Advantageous - Disadvantageous.
df$P4AttValenceB02 <- df$P4AttitudesB.P4AttitudesB03.-df$P4AttitudesB.P4AttitudesB04.
# Beneficial-Harmful.
df$P4AttValenceB03 <- df$P4AttitudesB.P4AttitudesB05.-df$P4AttitudesB.P4AttitudesB06.
# LEVEL C_Research Field.
# Useful-Useless.
df$P4AttValenceC01 <- df$P4AttitudesC.P4AttitudesC01.-df$P4AttitudesC.P4AttitudesC02.
# Advantageous - Disadvantageous.
df$P4AttValenceC02 <- df$P4AttitudesC.P4AttitudesC03.-df$P4AttitudesC.P4AttitudesC04.
# Beneficial-Harmful.
df$P4AttValenceC03 <- df$P4AttitudesC.P4AttitudesC05.-df$P4AttitudesC.P4AttitudesC06.
## Open Data.
# LEVEL A_Daily Research Life.
# Useful-Useless.
df$P5AttValenceA01 <- df$P5AttitudesA.P5AttitudesA01.-df$P5AttitudesA.P5AttitudesA02.
# Advantageous - Disadvantageous.
df$P5AttValenceA02 <- df$P5AttitudesA.P5AttitudesA03.-df$P5AttitudesA.P5AttitudesA04.
# Beneficial-Harmful.
df$P5AttValenceA03 <- df$P5AttitudesA.P5AttitudesA05.-df$P5AttitudesA.P5AttitudesA06.
# LEVEL B_Research Field.
# Useful-Useless.
df$P5AttValenceB01 <- df$P5AttitudesB.P5AttitudesB01.-df$P5AttitudesB.P5AttitudesB02.
# Advantageous - Disadvantageous.
df$P5AttValenceB02 <- df$P5AttitudesB.P5AttitudesB03.-df$P5AttitudesB.P5AttitudesB04.
# Beneficial-Harmful.
df$P5AttValenceB03 <- df$P5AttitudesB.P5AttitudesB05.-df$P5AttitudesB.P5AttitudesB06.
# LEVEL C_Research Field.
# Useful-Useless.
df$P5AttValenceC01 <- df$P5AttitudesC.P5AttitudesC01.-df$P5AttitudesC.P5AttitudesC02.
# Advantageous - Disadvantageous.
df$P5AttValenceC02 <- df$P5AttitudesC.P5AttitudesC03.-df$P5AttitudesC.P5AttitudesC04.
# Beneficial-Harmful.
df$P5AttValenceC03 <- df$P5AttitudesC.P5AttitudesC05.-df$P5AttitudesC.P5AttitudesC06.

#RELIABILITY OF Univalence Scales.
#OA Publications.
alpha(as.matrix(df[,c("P1AttValenceA01","P1AttValenceA02","P1AttValenceA03")]))
# Alpha .88
alpha(as.matrix(df[,c("P1AttValenceB01","P1AttValenceB02","P1AttValenceB03")]))
# Alpha .93
alpha(as.matrix(df[,c("P1AttValenceC01","P1AttValenceC02","P1AttValenceC03")]))
# Alpha .92

#Open Data.
alpha(as.matrix(df[,c("P2AttValenceA01","P2AttValenceA02","P2AttValenceA03")]))
# Alpha .91
alpha(as.matrix(df[,c("P2AttValenceB01","P2AttValenceB02","P2AttValenceB03")]))
# Alpha .93
alpha(as.matrix(df[,c("P2AttValenceC01","P2AttValenceC02","P2AttValenceC03")]))
# Alpha .93

#Pre-registrations.
alpha(as.matrix(df[,c("P3AttValenceA01","P3AttValenceA02","P3AttValenceA03")]))
# Alpha .95
alpha(as.matrix(df[,c("P3AttValenceB01","P3AttValenceB02","P3AttValenceB03")]))
# Alpha .96
alpha(as.matrix(df[,c("P3AttValenceC01","P3AttValenceC02","P3AttValenceC03")]))
# Alpha .94

#Registered-Reports.
alpha(as.matrix(df[,c("P4AttValenceA01","P4AttValenceA02","P4AttValenceA03")]))
# Alpha .96
alpha(as.matrix(df[,c("P4AttValenceB01","P4AttValenceB02","P4AttValenceB03")]))
# Alpha .97
alpha(as.matrix(df[,c("P4AttValenceC01","P4AttValenceC02","P4AttValenceC03")]))
# Alpha .96

#OA Publications.
alpha(as.matrix(df[,c("P5AttValenceA01","P5AttValenceA02","P5AttValenceA03")]))
# Alpha .93
alpha(as.matrix(df[,c("P5AttValenceB01","P5AttValenceB02","P5AttValenceB03")]))
# Alpha .94
alpha(as.matrix(df[,c("P5AttValenceC01","P5AttValenceC02","P5AttValenceC03")]))
# Alpha .96

# Compute a mean score of Attitudinal Valence for each OA Practice.
#OA Publications.
df$AttValenceA1 <- rowMeans(df[c("P1AttValenceA01","P1AttValenceA02","P1AttValenceA03")],na.rm=TRUE) 
df$AttValenceB1 <- rowMeans(df[c("P1AttValenceB01","P1AttValenceB02","P1AttValenceB03")],na.rm=TRUE)
df$AttValenceC1 <- rowMeans(df[c("P1AttValenceC01","P1AttValenceC02","P1AttValenceC03")],na.rm=TRUE)
#Open Data.
df$AttValenceA2 <- rowMeans(df[c("P2AttValenceA01","P2AttValenceA02","P2AttValenceA03")],na.rm=TRUE) 
df$AttValenceB2 <- rowMeans(df[c("P2AttValenceB01","P2AttValenceB02","P2AttValenceB03")],na.rm=TRUE)
df$AttValenceC2 <- rowMeans(df[c("P2AttValenceC01","P2AttValenceC02","P2AttValenceC03")],na.rm=TRUE)
#Pre-registrations.
df$AttValenceA3 <- rowMeans(df[c("P3AttValenceA01","P3AttValenceA02","P3AttValenceA03")],na.rm=TRUE) 
df$AttValenceB3 <- rowMeans(df[c("P3AttValenceB01","P3AttValenceB02","P3AttValenceB03")],na.rm=TRUE)
df$AttValenceC3 <- rowMeans(df[c("P3AttValenceC01","P3AttValenceC02","P3AttValenceC03")],na.rm=TRUE)
#Registered-Reports.
df$AttValenceA4 <- rowMeans(df[c("P4AttValenceA01","P4AttValenceA02","P4AttValenceA03")],na.rm=TRUE) 
df$AttValenceB4 <- rowMeans(df[c("P4AttValenceB01","P4AttValenceB02","P4AttValenceB03")],na.rm=TRUE)
df$AttValenceC4 <- rowMeans(df[c("P4AttValenceC01","P4AttValenceC02","P4AttValenceC03")],na.rm=TRUE)
#Replication Studies.
df$AttValenceA5 <- rowMeans(df[c("P5AttValenceA01","P5AttValenceA02","P5AttValenceA03")],na.rm=TRUE) 
df$AttValenceB5 <- rowMeans(df[c("P5AttValenceB01","P5AttValenceB02","P5AttValenceB03")],na.rm=TRUE)
df$AttValenceC5 <- rowMeans(df[c("P5AttValenceC01","P5AttValenceC02","P5AttValenceC03")],na.rm=TRUE)

#Long Format Data Set: Attitude Valence.
df_attvalence <- df %>%
  select(Responder_ID,Position,MPI,Section,Age.c,Gender,
         AttValenceA1, AttValenceB1, AttValenceC1,
         AttValenceA2, AttValenceB2, AttValenceC2,
         AttValenceA3, AttValenceB3, AttValenceC3,
         AttValenceA4, AttValenceB4, AttValenceC4,
         AttValenceA5, AttValenceB5, AttValenceC5) %>%
  pivot_longer(cols = AttValenceA1:AttValenceC5,
               names_to = "Item",
               values_to = "Att")
#Create variable OAPractice from Item.
df_attvalence$OAPractice <- recode(df_attvalence$Item,
                                   AttValenceA1 = "OA Publications", AttValenceB1 = "OA Publications", AttValenceC1 = "OA Publications",
                                   AttValenceA2 = "Open Data", AttValenceB2 = "Open Data", AttValenceC2 = "Open Data",
                                   AttValenceA3 = "Pre-registrations", AttValenceB3 = "Pre-registrations", AttValenceC3 = "Pre-registrations",
                                   AttValenceA4 = "Registered Reports", AttValenceB4 = "Registered Reports", AttValenceC4 = "Registered Reports",
                                   AttValenceA5 = "Replication Studies", AttValenceB5 = "Replication Studies", AttValenceC5 = "Replication Studies")
#Create variable Level from Item. 
#(in main manuscript this refers to variable "Perspective")
df_attvalence$Level <- recode(df_attvalence$Item,
                              AttValenceA1 = "Daily Research Life", AttValenceB1 = "Research Field", AttValenceC1 = "Public Society",
                              AttValenceA2 = "Daily Research Life", AttValenceB2 = "Research Field", AttValenceC2 = "Public Society",
                              AttValenceA3 = "Daily Research Life", AttValenceB3 = "Research Field", AttValenceC3 = "Public Society",
                              AttValenceA4 = "Daily Research Life", AttValenceB4 = "Research Field", AttValenceC4 = "Public Society",
                              AttValenceA5 = "Daily Research Life", AttValenceB5 = "Research Field", AttValenceC5 = "Public Society")

### ANALYSES ###
#Does Att Valence differ across Sections, OA Practice, and Level?
mod_attvalence <- lmer(Att~Level*Section*OAPractice+(1|Responder_ID),
                       df_attvalence)
summary(mod_attvalence)
anova(mod_attvalence)
eta_sq(mod_attvalence, partial = TRUE)
#Differences between Sections across OA Practices.
emm_attvalence_Section <- emmeans(mod_attvalence, specs = ~ Section | OAPractice)
pairs(emm_attvalence_Section, adjust = "bonferroni")
#Differences between Perspectives across OA Practices.
emm_attvalence_Level <- emmeans(mod_attvalence, specs = ~ Level | OAPractice)
pairs(emm_attvalence_Level, adjust = "bonferroni")

#Do demographics affect results (i.e., Age, Gender)?
mod_attvalence_dem <- lmer(Att~Age.c+Gender*Level*Section*OAPractice+(1|Responder_ID),
                              filter(df_attvalence, Gender == "Male" | Gender == "Female" | is.na(Gender)))
anova(mod_attvalence_dem)
eta_sq(mod_attvalence_dem, partial = TRUE)
#Differences between Males and Females across OA Practices.
emm_attvalence_dem1<- emmeans(mod_attvalence_dem, specs = ~ Gender | OAPractice,
                              data=filter(df_attvalence, Gender == "Male" | Gender == "Female" | is.na(Gender)))
pairs(emm_attvalence_dem1, adjust = "bonferroni")
#Differences between Males and Females across Perspectives.
emm_attvalence_dem2 <- emmeans(mod_attvalence_dem, specs = ~ Gender | Level,
                               data=filter(df_attvalence, Gender == "Male" | Gender == "Female" | is.na(Gender)))
pairs(emm_attvalence_dem2, adjust = "bonferroni")

# -------------------------------------------------------------------------#

#============================== #
      #### Need ####
#============================== #
#Calculate NEED score (Necessary - Unnecessary).
#Therefore, scale changes to -6 (completely Unnecesary) to 6 (Completely Necessary).
#Daily life.
df$NeedA1 <- df$P1AttitudesA.P1NeedA01.-df$P1AttitudesA.P1NeedA02.
df$NeedA2 <- df$P2AttitudesA.P2NeedA01.-df$P2AttitudesA.P2NeedA02.
df$NeedA3 <- df$P3AttitudesA.P3NeedA01.-df$P3AttitudesA.P3NeedA02.
df$NeedA4 <- df$P4AttitudesA.P4NeedA01.-df$P4AttitudesA.P4NeedA02.
df$NeedA5 <- df$P5AttitudesA.P5NeedA01.-df$P5AttitudesA.P5NeedA02.
#Research field.
df$NeedB1 <- df$P1AttitudesB.P1NeedB01.-df$P1AttitudesB.P1NeedB02.
df$NeedB2 <- df$P2AttitudesB.P2NeedB01.-df$P2AttitudesB.P2NeedB02.
df$NeedB3 <- df$P3AttitudesB.P3NeedB01.-df$P3AttitudesB.P3NeedB02.
df$NeedB4 <- df$P4AttitudesB.P4NeedB01.-df$P4AttitudesB.P4NeedB02.
df$NeedB5 <- df$P5AttitudesB.P5NeedB01.-df$P5AttitudesB.P5NeedB02.
#Society.
df$NeedC1 <- df$P1AttitudesC.P1NeedC01.-df$P1AttitudesC.P1NeedC02.
df$NeedC2 <- df$P2AttitudesC.P2NeedC01.-df$P2AttitudesC.P2NeedC02.
df$NeedC3 <- df$P3AttitudesC.P3NeedC01.-df$P3AttitudesC.P3NeedC02.
df$NeedC4 <- df$P4AttitudesC.P4NeedC01.-df$P4AttitudesC.P4NeedC02.
df$NeedC5 <- df$P5AttitudesC.P5NeedC01.-df$P5AttitudesC.P5NeedC02.

#Long Format data set: Need.
df_need <- df %>%
  select(Responder_ID,Position,MPI,Section,Age.c,Gender,
         NeedA1, NeedA2, NeedA3, NeedA4, NeedA5,
         NeedB1, NeedB2, NeedB3, NeedB4, NeedB5,
         NeedC1, NeedC2, NeedC3, NeedC4, NeedC5) %>%
  pivot_longer(cols = NeedA1:NeedC5,
               names_to = "Item",
               values_to = "Need")
#Create variable OAPractice from Item.
df_need$OAPractice <- recode(df_need$Item,
                             NeedA1 = "OA Publications",NeedB1 = "OA Publications",NeedC1 = "OA Publications",
                             NeedA2 = "Open Data",NeedB2 = "Open Data",NeedC2 = "Open Data",
                             NeedA3 = "Pre-registrations",NeedB3 = "Pre-registrations",NeedC3 = "Pre-registrations",
                             NeedA4 = "Registered Reports",NeedB4 = "Registered Reports",NeedC4 = "Registered Reports",
                             NeedA5 = "Replication Studies",NeedB5 = "Replication Studies",NeedC5 = "Replication Studies")
#Create variable Level from Item.
#(in main manuscript this refers to variable "Perspective")
df_need$Level <- recode(df_need$Item,
                             NeedA1 = "Daily Research Life",NeedB1 = "Research Field",NeedC1 = "Public Society",
                             NeedA2 = "Daily Research Life",NeedB2 = "Research Field",NeedC2 = "Public Society",
                             NeedA3 = "Daily Research Life",NeedB3 = "Research Field",NeedC3 = "Public Society",
                             NeedA4 = "Daily Research Life",NeedB4 = "Research Field",NeedC4 = "Public Society",
                             NeedA5 = "Daily Research Life",NeedB5 = "Research Field",NeedC5 = "Public Society")

### ANALYSES ###
#Do Need differ between Levels and across Sections?
mod_need <- lmer(Need~Level*OAPractice*Section+(1|Responder_ID),
                 df_need)
anova(mod_need)
eta_sq(mod_need, partial = TRUE)
#Differences between Perspectives across OA Practices.
emm_need_LOAPractice <- emmeans(mod_need, specs = ~ Level*OAPractice)
pairs(emm_need_LOAPractice, simple = "Level", adjust = "bonferroni")
#Differences between Sections across OA Practices.
emm_need_SOApractice <- emmeans(mod_need, specs = ~ Section*OAPractice)
pairs(emm_need_SOApractice, simple = "Section", adjust = "bonferroni")

#Do demographics affect these effects?
mod_need_dem <- lmer(Need ~ Age.c+Gender*Level*OAPractice*Section+(1|Responder_ID),
                  filter(df_need, Gender != "Other" | is.na(Gender)))
anova(mod_need_dem)
eta_sq(mod_need_dem, partial = TRUE)
#Differences between Males and Females across OA Practices.
emm_need_Gender2 <- emmeans(mod_need_dem, specs = ~ Gender*OAPractice,
                            data = filter(df_need, Gender != "Other" | is.na(Gender)))
pairs(emm_need_Gender2, simple = "Gender", adjust = "bonferroni")
#Differences between Males and Females across Perpectives and Sections.
emm_need_Gender3 <- emmeans(mod_need2, specs = ~ Gender*Level*Section,
                            data = filter(df_need, Gender != "Other" | is.na(Gender)))
pairs(emm_need_Gender3, simple = "Gender", adjust = "bonferroni")


# -------------------------------------------------------------------------#

#============================== #
    #### Implementation ####
#============================== #
#Long Format data set: USE/IMPLEMENTATION.
df_use <- df %>%
  select(Responder_ID,Position,MPI,Section,Gender,Age.c,
         P1Use01,P1Use02,P2Use01,P2Use02,P3Use01,P3Use02,P4Use01,P4Use02,P5Use01,P5Use02,
         P1Use01b, P1Use02b) %>%
  pivot_longer(cols = P1Use01:P5Use02,
               names_to = "Item",
               values_to = "Use")
#Create variable OAPractice from Item.
df_use$OAPractice <- recode(df_use$Item,
                             P1Use01 = "OA Publications",P1Use02 = "OA Publications",
                             P2Use01 = "Open Data",P2Use02 = "Open Data",
                             P3Use01 = "Pre-registrations",P3Use02 = "Pre-registrations",
                             P4Use01 = "Registered Reports",P4Use02 = "Registered Reports",
                             P5Use01 = "Replication Studies",P5Use02 = "Replication Studies")
#Create variable Past_Future from Item.
df_use$Past_Future <- recode(df_use$Item,
                             P1Use01 = "In the past 12 months",P1Use02 = "In the following 12 months",
                             P2Use01 = "In the past 12 months",P2Use02 = "In the following 12 months",
                             P3Use01 = "In the past 12 months",P3Use02 = "In the following 12 months",
                             P4Use01 = "In the past 12 months",P4Use02 = "In the following 12 months",
                             P5Use01 = "In the past 12 months",P5Use02 = "In the following 12 months")


### ANALYSES ### PROBLEMS OF CONVERGENCE when including interaction terms.
#Also, the interpretation of interaction terms in logistic regression is complex, not recommended.
df_use$OAPractice <- relevel(as.factor(df_use$OAPractice), "OA Publications")
#Past Use across OA Practices.
mod_use1 <- glmer(Use~OAPractice+(1|Responder_ID),
                  filter(df_use, Past_Future == "In the past 12 months"),
                  family = binomial(link = logit))
summary(mod_use1)
cc_mod_use1 <- confint(mod_use1,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod_use1 <- cbind(Coeff.=fixef(mod_use1),
                             OR=exp(fixef(mod_use1)),CIs=exp(cc_mod_use1)) #Transform into ORs.
round(ctab_mod_use1,2)

#Future Use across OA Practices.
mod_use2 <- glmer(Use~OAPractice+(1|Responder_ID),
                  filter(df_use, Past_Future == "In the following 12 months"),
                  family = binomial(link = logit))
summary(mod_use2)

#Solution: Check differences between sections for each OS practice.
df_use$Section <- relevel(as.factor(df_use$Section), "HUM")

##OA Publications.
#Past Use.
mod_OS1_use1 <- glm(Use~Section,
                  filter(df_use, OAPractice == "OA Publications" &
                           Past_Future == "In the past 12 months"),
                  family = binomial(link = logit))
summary(mod_OS1_use1)
model_parameters(mod_OS1_use1, exponentiate = TRUE)

#Future Use.
mod_OS1_use2 <- glm(Use~Section,
                filter(df_use, OAPractice == "OA Publications" &
                         Past_Future == "In the following 12 months"),
                family = binomial(link = logit))
summary(mod_OS1_use2)

##Aggregated Percentages.
summarySE(filter(df_use,OAPractice=="OA Publications"),
          "Use", groupvars=c("Past_Future"),na.rm = TRUE, conf.interval =0.95)
#Splitted across Sections.
summ_use_OAPublication<-summarySE(filter(df_use,OAPractice=="OA Publications"),
                                  "Use", groupvars=c("Section","Past_Future"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
summ_use_OAPublication #% Use per Section.
# Where DRs have published their OA Publications in the past 12 months? (Self-Archive vs. OA Publisher.)
df_use$P1Use01b <- as.factor(df_use$P1Use01b)
past_OAPublications <- count(filter
                             (df_use,OAPractice=="OA Publications"&Past_Future=="In the past 12 months"&Use==1)
                             $P1Use01b)
past_OAPublications$percentage <- past_OAPublications$freq/sum(past_OAPublications$freq)
past_OAPublications
# Types of OA Publications per Section in the past 12 months.
past_OAPublications_BM <- df_use %>%
  filter(OAPractice=="OA Publications"&Past_Future=="In the past 12 months"&Use==1&Section=="BM")
count(past_OAPublications_BM$P1Use01b)$freq/sum(count(past_OAPublications_BM$P1Use01b)$freq)  
#BM - 80% OA Publisher, 10% Other means, 6% Self-archiving.
past_OAPublications_CPT <- df_use %>%
  filter(OAPractice=="OA Publications"&Past_Future=="In the past 12 months"&Use==1&Section=="CPT")
count(past_OAPublications_CPT$P1Use01b)$freq/sum(count(past_OAPublications_CPT$P1Use01b)$freq) 
#CPT - 53.57% OA Publisher, 20.24% Other Means, 25% Self-Archiving.
past_OAPublications_HUM <- df_use %>%
  filter(OAPractice=="OA Publications"&Past_Future=="In the past 12 months"&Use==1&Section=="HUM")
count(past_OAPublications_HUM$P1Use01b)$freq/sum(count(past_OAPublications_HUM$P1Use01b)$freq)
#HUM - 63% oA Publisher, 11.11% Other means, 18.52% Self-Archiving.

# Where DRs plan to publish published their OA Publications in the following 12 monhts? (Self-Archive vs. OA Publisher.)
df_use$P1Use02b <- as.factor(df_use$P1Use02b)
future_OAPublications <- count(filter
                               (df_use,OAPractice=="OA Publications"&Past_Future=="In the following 12 months"&Use==1)
                               $P1Use02b)
future_OAPublications$percentage <- future_OAPublications$freq/sum(future_OAPublications$freq)
future_OAPublications
#Types of OA publications per Section in the following 12 months.
future_OAPublications_BM <- df_use %>%
  filter(OAPractice=="OA Publications"&Past_Future=="In the following 12 months"&Use==1&Section=="BM")
count(future_OAPublications_BM$P1Use02b)$freq/sum(count(future_OAPublications_BM$P1Use02b)$freq)  
#BM - 66.36% OA Publisher, 8.18% Other means, 13.64% Self-archiving.
future_OAPublications_CPT <- df_use %>%
  filter(OAPractice=="OA Publications"&Past_Future=="In the following 12 months"&Use==1&Section=="CPT")
count(future_OAPublications_CPT$P1Use02b)$freq/sum(count(future_OAPublications_CPT$P1Use02b)$freq)  
#CPT - 56.41% OA Publisher, 12.18% Other means, 19.87% Self-archiving.
future_OAPublications_HUM <- df_use %>%
  filter(OAPractice=="OA Publications"&Past_Future=="In the following 12 months"&Use==1&Section=="HUM")
count(future_OAPublications_HUM$P1Use02b)$freq/sum(count(future_OAPublications_HUM$P1Use02b)$freq)  
#HUM - 61.67% OA Publisher, 5% Other means, 23.33% Self-archiving.

# Open Data.
#Past Use.
mod_OS2_use1 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Open Data" &
                             Past_Future == "In the past 12 months"),
                    family = binomial(link = logit))
summary(mod_OS2_use1)
model_parameters(mod_OS2_use1, exponentiate = TRUE)

#Future Use.
mod_OS2_use2 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Open Data" &
                             Past_Future == "In the following 12 months"),
                    family = binomial(link = logit))
summary(mod_OS2_use2)
model_parameters(mod_OS2_use2, exponentiate = TRUE)
#Aggregated Percentages.
summarySE(filter(df_use,OAPractice=="Open Data"),
          "Use", groupvars=c("Past_Future"),na.rm = TRUE, conf.interval =0.95)
#Splitted per Section.
summ_use_OpenData<-summarySE(filter(df_use,OAPractice=="Open Data"),
                             "Use", groupvars=c("Section","Past_Future"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
summ_use_OpenData #% Use per Section.

# Pre-registrations.
#Past Use.
mod_OS3_use1 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Pre-registrations" &
                             Past_Future == "In the past 12 months"),
                    family = binomial(link = logit))
summary(mod_OS3_use1)
model_parameters(mod_OS3_use1, exponentiate = TRUE)

#Future Use.
mod_OS3_use2 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Pre-registrations" &
                             Past_Future == "In the following 12 months"),
                    family = binomial(link = logit))
summary(mod_OS3_use2)
model_parameters(mod_OS3_use2, exponentiate = TRUE)
#Aggregated Percentages.
summarySE(filter(df_use,OAPractice=="Pre-registrations"),
          "Use", groupvars=c("Past_Future"),na.rm = TRUE, conf.interval =0.95)
#Splitted per Section.
summ_use_Prereg<-summarySE(filter(df_use,OAPractice=="Pre-registrations"),
                           "Use", groupvars=c("Section","Past_Future"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
summ_use_Prereg #% Use per Section.

# Registered Reports.
#Past Use.
mod_OS4_use1 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Registered Reports" &
                             Past_Future == "In the past 12 months"),
                    family = binomial(link = logit))
summary(mod_OS4_use1)
model_parameters(mod_OS4_use1, exponentiate = TRUE)

#Future Use.
mod_OS4_use2 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Registered Reports" &
                             Past_Future == "In the following 12 months"),
                    family = binomial(link = logit))
summary(mod_OS4_use2)
model_parameters(mod_OS4_use2, exponentiate = TRUE)
#Aggregated Percentages.
summarySE(filter(df_use,OAPractice=="Registered Reports"),
          "Use", groupvars=c("Past_Future"),na.rm = TRUE, conf.interval =0.95)
#Splitted per Section.
summ_use_RegRep<-summarySE(filter(df_use,OAPractice=="Registered Reports"),
                           "Use", groupvars=c("Section","Past_Future"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
summ_use_RegRep #% Use per Section.

# Replication Studies.
#Past Use.
mod_OS5_use1 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Replication Studies" &
                             Past_Future == "In the past 12 months"),
                    family = binomial(link = logit))
summary(mod_OS5_use1)
model_parameters(mod_OS5_use1, exponentiate = TRUE)

#Future Use.
mod_OS5_use2 <- glm(Use~Section,
                    filter(df_use, OAPractice == "Replication Studies" &
                             Past_Future == "In the following 12 months"),
                    family = binomial(link = logit))
summary(mod_OS5_use2)
model_parameters(mod_OS5_use2, exponentiate = TRUE)
#Aggregated Percentages.
summarySE(filter(df_use,OAPractice=="Replication Studies"),
          "Use", groupvars=c("Past_Future"),na.rm = TRUE, conf.interval =0.95)
#Splitted per Section.
summ_use_RepStudies<-summarySE(filter(df_use,OAPractice=="Replication Studies"),
                               "Use", groupvars=c("Section","Past_Future"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
summ_use_RepStudies #% Use per Section.
# -------------------------------------------------------------------------#



#### RELATIONSHIPS BETWEEN VARIABLES ####
# -------------------------------------------------------------------------#
#============================== #
####  Knowledge ~  Attitudes ####
#============================== #
df_know_att <- left_join(df_know, df_attvalence, by = c("Responder_ID","OAPractice"))
df_know_att$OAPractice <- relevel(as.factor(df_know_att$OAPractice), "Open Data")

#CURRENT KNOWLEDGE.
#Does the relationship of Att and Current Knowledge differ across OAPractices?
mod1_know1_attvalence <- lmer(Att~Knowledge*OAPractice+(1|Responder_ID),
                             filter(df_know_att, Current_Desired == "Are you knowledgeable about..."))
summary(mod1_know1_attvalence)
model_parameters(mod1_know1_attvalence, standardize = "refit")
anova(mod1_know1_attvalence)
eta_sq(mod1_know1_attvalence, partial = TRUE)
#Does the relationship of Att and Current Knolwedge differ across Perspectives?
df_know_att$Level <- relevel(as.factor(df_know_att$Level), "Daily Research Life")
mod2_know1_attvalence <- lmer(Att~Knowledge*Level+(1|Responder_ID/OAPractice),
                             filter(df_know_att, Current_Desired == "Are you knowledgeable about..."))
summary(mod2_know1_attvalence)
model_parameters(mod2_know1_attvalence, standardize = "refit")
anova(mod2_know1_attvalence)
eta_sq(mod2_know1_attvalence)

#DESIRED KNOWLEDGE.
#Does the relationship of Att and Desired Knowledge differ across OAPractices?
df_know_att$OAPractice <- relevel(as.factor(df_know_att$OAPractice), "Pre-registrations")
mod1_know2_attvalence <- lmer(Att~Knowledge*OAPractice+(1|Responder_ID),
                             filter(df_know_att, Current_Desired == "Would you like to know more about..."))
summary(mod1_know2_attvalence)
model_parameters(mod1_know2_attvalence, standardize = "refit")
anova(mod1_know2_attvalence)
eta_sq(mod1_know2_attvalence, partial = TRUE)
#Does the relationship of Att and Desired Knolwedge differ across Perspectives?
df_know_att$Level <- relevel(as.factor(df_know_att$Level), "Research Field")
mod2_know2_attvalence <- lmer(Att~Knowledge*Level+(1|Responder_ID/OAPractice)
                              ,filter(df_know_att, Current_Desired == "Would you like to know more about..."))
summary(mod2_know2_attvalence)
model_parameters(mod2_know2_attvalence, standardize = "refit")
anova(mod2_know2_attvalence)
eta_sq(mod2_know2_attvalence, partial = TRUE)

#============================== #
####     Need ~ Knowledge    ####
#============================== #
df_know_need <- left_join(df_know, df_need, by = c("Responder_ID","OAPractice"))
df_know_need$Level <- relevel(as.factor(df_know_need$Level), "Daily Research Life")

#CURRENT KNOWLEDGE.
#Does the relationship of Need and Current Knolwedge differ across OAPractices?
mod1_know1_need <- lmer(Need~Knowledge*OAPractice+(1|Responder_ID),
                       filter(df_know_need,Current_Desired=="Are you knowledgeable about..."))
anova(mod1_know1_need)
eta_sq(mod1_know1_need, partial = TRUE)
#Does the relationship of Need and Current Knolwedge differ across Perspectives?
mod2_know1_need <- lmer(Need~Knowledge*Level+(1|Responder_ID/OAPractice),
                       filter(df_know_need,Current_Desired=="Are you knowledgeable about..."))
anova(mod2_know1_need)
summary(mod2_know1_need)
eta_sq(mod2_know1_need, partial = TRUE)

#DESIRED KNOWLEDGE.
#Does the relationship of Need and Desired Knolwedge differ across OAPractices?
mod1_know2_need <- lmer(Need~Knowledge*OAPractice+(1|Responder_ID),
                        filter(df_know_need,Current_Desired == "Would you like to know more about..."))
summary(mod1_know2_need)
anova(mod1_know2_need)
eta_sq(mod1_know2_need, partial = TRUE)
#Does the relationship of Need and Desired Knolwedge differ across Perspectives?
mod2_know2_need <- lmer(Need~Knowledge*Level+(1|Responder_ID/OAPractice),
                       filter(df_know_need,Current_Desired == "Would you like to know more about..."))
summary(mod2_know2_need)
anova(mod2_know2_need)
eta_sq(mod2_know2_need, partial = TRUE)

#============================== #
####    Need ~ Attitudes     ####
#============================== #
df_need_att <- left_join(df_need, df_attvalence, by = c("Responder_ID","OAPractice","Level"))

#Does the relationship between Att and Need differ across OAPractices?
mod1_need_attvalence <- lmer(Need~Att*OAPractice+(1|Responder_ID),
                             df_need_att)
anova(mod1_need_attvalence)
eta_sq(mod1_need_attvalence,partial = TRUE)
plot_model(mod1_need_attvalence,type="pred",terms=c("Att","OAPractice"))
#Does the relationship between Att and Need differ across Perspectives?
mod2_need_attvalence <- lmer(Need~Att*Level+(1|Responder_ID/OAPractice),
                            df_need_att)
anova(mod2_need_attvalence)
eta_sq(mod2_need_attvalence, partial = TRUE)
#Given that the interactions terms were close to .05, we looked at simplest model without interactions.
mod_need_attvalence <- lmer(Need~Att+(1|Responder_ID/OAPractice),
                            df_need_att)
anova(mod_need_attvalence)
eta_sq(mod_need_attvalence, partial = TRUE)

#================================== #
####       Use ~ Knowledge       ####
#================================== #
# In order to avoid introducing interaction terms in logistic regression,
#we will transform Knowledge + Current_Desired from long to wide before merging,
# so that we include both resultant variables as independent covariates, instead of an interaction term.
df_use_know <- pivot_wider(df_know,
                           id_cols = c("Responder_ID","OAPractice"),
                           names_from = "Current_Desired",
                           values_from = "Knowledge") %>%
  left_join(df_use, by = c("Responder_ID","OAPractice")) %>%
  dplyr::rename("Current_Knowledge" = `Are you knowledgeable about...`,
                "Desired_Knowledge" = `Would you like to know more about...`)

#Past Use ~ Current vs Desired Knowledge.
#Including OAPractice as random effect lead to singular fit. The Responder_ID:OAPractice random term variance is 0.
#Decision: do not take into account OAPractice differences in the random structure.
mod1_use1_know <- glmer(Use~Current_Knowledge+Desired_Knowledge+(1|Responder_ID),
                        filter(df_use_know,Past_Future=="In the past 12 months"),
                        family = binomial(link = logit))
summary(mod1_use1_know)
#ORS and CIs.
cc_mod1_use1_know <- confint(mod1_use1_know,parm="beta_",method="Wald")
ctab_mod1_use1_know <- cbind(est=fixef(mod1_use1_know),cc_mod1_use1_know)
exp(ctab_mod1_use1_know)
#Probability of Use at max score of Current vs. Desired Knowledge: 
ggeffects::ggpredict(mod1_use1_know)

#Future Use ~ Current vs Desired Knowledge.
mod1_use2_know <- glmer(Use~Current_Knowledge+Desired_Knowledge+(1|Responder_ID),
                        filter(df_use_know,Past_Future=="In the following 12 months"),
                        family = binomial(link = logit))
summary(mod1_use2_know)

#Model does not Converge for Current Knowledge. Restart from last estimates.
ss_mod1_use2_know<-getME(mod1_use2_know,c("theta","fixef"))
mod1_use2_know.re<- update(mod1_use2_know,start=ss_mod1_use2_know)
summary(mod1_use2_know.re)
#ORS and CIs.
cc_mod1_use2_know.re <- confint(mod1_use2_know.re,parm="beta_",method="Wald")
ctab_mod1_use2_know.re <- cbind(est=fixef(mod1_use2_know.re),cc_mod1_use2_know.re)
exp(ctab_mod1_use2_know.re) #ORs Current know.reledge.
#Probability of Use at max score of Current vs. Desired Knowledge: 
ggeffects::ggpredict(mod1_use2_know)
#Past Use ~ Current Knowledge*OAPractice.
#Used different optimizer algorithm "bobyqa" due to Convergence problems.
df_use_know$OAPractice <- relevel(as.factor(df_use_know$OAPractice), "OA Publications")
mod2_use1_know1 <- glmer(Use~Current_Knowledge*OAPractice+(1|Responder_ID),
                        filter(df_use_know,Past_Future=="In the past 12 months"),
                        family = binomial(link = logit),
                        control = glmerControl(optimizer="bobyqa"))
summary(mod2_use1_know1)

#ORS and CIs.
cc_mod2_use1_know1 <- confint(mod2_use1_know1,parm="beta_",method="Wald")
ctab_mod2_use1_know1 <- cbind(est=fixef(mod2_use1_know1),cc_mod2_use1_know1)
round(exp(ctab_mod2_use1_know1),2)

#Future Use ~ Current Knowledge*OAPractice.
df_use_know$OAPractice <- relevel(df_use_know$OAPractice, "OA Publications")
mod2_use2_know1 <- glmer(Use~Current_Knowledge*OAPractice+(1|Responder_ID),
                         filter(df_use_know,Past_Future=="In the following 12 months"),
                         family = binomial(link = logit),
                         control = glmerControl(optimizer="bobyqa"))
summary(mod2_use2_know1)

cc_mod2_use2_know1 <- confint(mod2_use2_know1,parm="beta_",method="Wald")
ctab_mod2_use2_know1 <- cbind(est=fixef(mod2_use2_know1),cc_mod2_use2_know1)
round(exp(ctab_mod2_use2_know1),2)

#Probability of Use at max score of Current vs. Desired Knowledge for OA Publications: 
ggeffects::ggpredict(mod2_use2_know1)
#Manual calculation of Probabilities at Knowledge = 1. 
#exp(Intercept + Coeff)/(1+exp(Intercept + Coeff))
exp(-1.531449+0.641956)/(1+exp(-1.531449+0.641956)) # 29.12% - Probability at Knowledge = 1 of OA Publications. (i.e., Intercept + Effect).
exp(-2.14844+0.61549)/(1+exp(-2.14844+0.61549)) #17.75% - Probability at Knowledge = 1 of Open Data. (i.e., Intercept + Effect).

#============================== #
####       Use ~ Need        ####
#============================== #
# In order to avoid another level of nesting (i.e., perspective within OAPractice),
# we will transform Need + Level from long to wide before merging,
# so that we include the three resultant variables as independent covariates,
# instead of a cross-level interaction term.
df_use_need <- left_join(df_use, df_need, by = c("Responder_ID","OAPractice"))
df_use_need <- pivot_wider(df_need,
                                 id_cols = c("Responder_ID","OAPractice"),
                                 names_from = "Level",
                                 values_from = "Need") %>%
  left_join(df_use, by = c("Responder_ID","OAPractice")) %>%
  dplyr::rename("Need_DailyLife" = `Daily Research Life`,
                "Need_ResearchField" = `Research Field`,
                "Need_PublicSociety" = `Public Society`)
#For studying differences across OAPractices, we analyze if we can agregate these three measures of Need.
#OA Publications = .77
alpha(as.matrix(filter(df_use_need,
                       OAPractice == "OA Publications")[,c('Need_DailyLife',
                                                           'Need_ResearchField',
                                                           'Need_PublicSociety')]))
#Open Data = .76
alpha(as.matrix(filter(df_use_need,
                       OAPractice == "Open Data")[,c('Need_DailyLife',
                                                     'Need_ResearchField',
                                                     'Need_PublicSociety')]))
#Pre-registrations = .87
alpha(as.matrix(filter(df_use_need,
                       OAPractice == "Pre-registrations")[,c('Need_DailyLife',
                                                             'Need_ResearchField',
                                                             'Need_PublicSociety')]))
#Registered Reports = .91
alpha(as.matrix(filter(df_use_need,
                       OAPractice == "Registered Reports")[,c('Need_DailyLife',
                                                              'Need_ResearchField',
                                                              'Need_PublicSociety')]))
#Replication Studies = .84
alpha(as.matrix(filter(df_use_need,
                       OAPractice == "Replication Studies")[,c('Need_DailyLife',
                                                               'Need_ResearchField',
                                                               'Need_PublicSociety')]))
#Calculate average level of Need across Perspectives.
df_use_need$Need <- rowMeans(df_use_need[,c('Need_DailyLife',
                                            'Need_ResearchField',
                                            'Need_PublicSociety')], na.rm=TRUE)
#PAST USE.
#Need across OA Practices.
df_use_need$OAPractice <- relevel(as.factor(df_use_need$OAPractice)
                                        ,"OA Publications")
mod1_use1_need <- glmer(Use~Need*OAPractice+(1|Responder_ID),
                              filter(df_use_need, Past_Future == "In the past 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))
summary(mod1_use1_need)
cc_mod1_use1_need <- confint(mod1_use1_need,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod1_use1_need <- cbind(Coeff.=fixef(mod1_use1_need),
                                   OR=exp(fixef(mod1_use1_need)),CIs=exp(cc_mod1_use1_need)) #Transform into ORs.
round(ctab_mod1_use1_need,2)
#Need from different Perspectives.
#Including OAPractice as random effect lead to singular fit. The Responder_ID:OAPractice random term variance is 0.
#Decision: do not take into account OAPractice differences in the random structure.
mod2_use1_need <- glmer(Use~Need_DailyLife+Need_ResearchField+Need_PublicSociety+(1|Responder_ID),
                              filter(df_use_need, Past_Future == "In the past 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))
summary(mod2_use1_need)
#ORs and CIs.
cc_mod2_use1_need <- confint(mod2_use1_need,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod2_use1_need <- cbind(Coeff.=fixef(mod2_use1_need),
                                   OR=exp(fixef(mod2_use1_need)),CIs=exp(cc_mod2_use1_need)) #Transform into ORs.
round(ctab_mod2_use1_need,2)

#FUTURE USE.
#Need across OA Practices.
df_use_need$OAPractice <- relevel(as.factor(df_use_need$OAPractice)
                                  ,"OA Publications")
mod1_use2_need <- glmer(Use~Need*OAPractice+(1|Responder_ID),
                        filter(df_use_need, Past_Future == "In the following 12 months"), 
                        family = binomial(link = logit),
                        control = glmerControl(optimizer="bobyqa"))
summary(mod1_use2_need)
cc_mod1_use2_need <- confint(mod1_use2_need,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod1_use2_need <- cbind(Coeff.=fixef(mod1_use2_need),
                             OR=exp(fixef(mod1_use2_need)),CIs=exp(cc_mod1_use2_need)) #Transform into ORs.
round(ctab_mod1_use2_need,2)
#Probability of Use at max score of Need: 
ggeffects::ggpredict(mod1_use2_need)
#Manual calculation: #Probability of Use at max score of need = 6.
#exp(Intercept + Coeff*6)/(1+exp(Intercept + Coeff*6))
exp(-1.58812+(0.59884*6))/(1+exp(-1.58812+(0.59884*6))) # Open Data.
exp(-0.34465+(0.46779*6))/(1+exp(-0.34465+(0.46779*6))) # OA Publications.
exp(-4.3205+(0.7793*6))/(1+exp(-4.3205+(0.7793*6))) # Pre-registrations.

#Need from different Perspectives.
#Including OAPractice as random effect led to singular fit. The Responder_ID:OAPractice random term variance is 0.
#Decision: do not take into account OAPractice differences in the random structure.
mod2_use2_need <- glmer(Use~Need_DailyLife+Need_ResearchField+Need_PublicSociety+(1|Responder_ID),
                        filter(df_use_need, Past_Future == "In the following 12 months"), 
                        family = binomial(link = logit),
                        control = glmerControl(optimizer="bobyqa"))
summary(mod2_use2_need)
cc_mod2_use2_need <- confint(mod2_use2_need,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod2_use2_need <- cbind(Coeff.=fixef(mod2_use2_need),
                             OR=exp(fixef(mod2_use2_need)),CIs=exp(cc_mod2_use2_need)) #Transform into ORs.
round(ctab_mod2_use2_need,2)


#=============================#
####    Use ~  Attitudes   ####
#=============================#
# In order to avoid another level of nesting (i.e., perspective within OAPractice),
# we will transform AttValence + Level from long to wide before merging,
# so that we include the three resultant variables as independent covariates,
# instead of a cross-level interaction term.
df_use_attvalence <- pivot_wider(df_attvalence,
                                 id_cols = c("Responder_ID","OAPractice"),
                                 names_from = "Level",
                                 values_from = "Att") %>%
  left_join(df_use, by = c("Responder_ID","OAPractice")) %>%
  dplyr::rename("Att_DailyLife" = `Daily Research Life`,
                "Att_ResearchField" = `Research Field`,
                "Att_PublicSociety" = `Public Society`)
#For studying differences across OAPractices, we analyze if we can agregate these three measures of attitudes.
#OA Publications = .74
alpha(as.matrix(filter(df_use_attvalence,
                       OAPractice == "OA Publications")[,c('Att_DailyLife',
                                                         'Att_ResearchField',
                                                         'Att_PublicSociety')]))
#Open Data = .78
alpha(as.matrix(filter(df_use_attvalence,
                       OAPractice == "Open Data")[,c('Att_DailyLife',
                                                           'Att_ResearchField',
                                                           'Att_PublicSociety')]))
#Pre-registrations = .86
alpha(as.matrix(filter(df_use_attvalence,
                       OAPractice == "Pre-registrations")[,c('Att_DailyLife',
                                                     'Att_ResearchField',
                                                     'Att_PublicSociety')]))
#Registered Reports = .89
alpha(as.matrix(filter(df_use_attvalence,
                       OAPractice == "Registered Reports")[,c('Att_DailyLife',
                                                             'Att_ResearchField',
                                                             'Att_PublicSociety')]))
#Replication Studies = .83
alpha(as.matrix(filter(df_use_attvalence,
                       OAPractice == "Replication Studies")[,c('Att_DailyLife',
                                                              'Att_ResearchField',
                                                              'Att_PublicSociety')]))
#Calculate average level of Att across Perspectives.
df_use_attvalence$Att <- rowMeans(df_use_attvalence[,c('Att_DailyLife',
                                                       'Att_ResearchField',
                                                       'Att_PublicSociety')], na.rm=TRUE)
#PAST USE.
#Att across OA Practices.
df_use_attvalence$OAPractice <- relevel(as.factor(df_use_attvalence$OAPractice)
                                        ,"OA Publications")
mod1_use1_attvalence <- glmer(Use~Att*OAPractice+(1|Responder_ID),
                              filter(df_use_attvalence, Past_Future == "In the past 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))
summary(mod1_use1_attvalence)

cc_mod1_use1_attvalence <- confint(mod1_use1_attvalence,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod1_use1_attvalence <- cbind(Coeff.=fixef(mod1_use1_attvalence),
                                   OR=exp(fixef(mod1_use1_attvalence)),CIs=exp(cc_mod1_use1_attvalence)) #Transform into ORs.
round(ctab_mod1_use1_attvalence,2)
#Att from different Perspectives.
#Including OAPractice as random effect led to singular fit. The Responder_ID:OAPractice random term variance is 0.
#Decision: do not take into account OAPractice differences in the random structure.
mod2_use1_attvalence <- glmer(Use~Att_DailyLife+Att_ResearchField+Att_PublicSociety+(1|Responder_ID),
                              filter(df_use_attvalence, Past_Future == "In the past 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))
summary(mod2_use1_attvalence)
#ORs and CIs.
cc_mod2_use1_attvalence <- confint(mod2_use1_attvalence,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod2_use1_attvalence <- cbind(Coeff.=fixef(mod2_use1_attvalence),
                                   OR=exp(fixef(mod2_use1_attvalence)),CIs=exp(cc_mod2_use1_attvalence)) #Transform into ORs.


#FUTURE USE.
#Att across OA Practices.
df_use_attvalence$OAPractice <- relevel(as.factor(df_use_attvalence$OAPractice)
                                        ,"OA Publications")
mod1_use2_attvalence <- glmer(Use~Att*OAPractice+(1|Responder_ID),
                              filter(df_use_attvalence, Past_Future == "In the following 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))
summary(mod1_use2_attvalence)

cc_mod1_use2_attvalence <- confint(mod1_use2_attvalence,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod1_use2_attvalence <- cbind(Coeff.=fixef(mod1_use2_attvalence),
                                   OR=exp(fixef(mod1_use2_attvalence)),CIs=exp(cc_mod1_use2_attvalence)) #Transform into ORs.
round(ctab_mod1_use2_attvalence,2)

#Probability of Use at max score of Attitude.
ggeffects::ggpredict(mod1_use2_need)
#Manual calculation of  Probability of Use at max score of attitude = 6: 
#exp(Intercept + Coeff*6)/(1+exp(Intercept + Coeff*6))
exp(-2.8119+(0.8372*6))/(1+exp(-2.8119+(0.8372*6))) # Open Data.
exp(-2.4123+(0.8822*6))/(1+exp(-2.4123+(0.8822*6))) # OA Publications.
exp(-5.5292+(1.0215*6))/(1+exp(-5.5292+(1.0215*6))) # Pre-registrations.

#Att from different Perspectives.
#Including OAPractice as random effect lead to singular fit. The Responder_ID:OAPractice random term variance is 0.
#Decision: do not take into account OAPractice differences in the random structure.
mod2_use2_attvalence <- glmer(Use~Att_DailyLife+Att_ResearchField+Att_PublicSociety+(1|Responder_ID),
                              filter(df_use_attvalence, Past_Future == "In the following 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))
summary(mod2_use2_attvalence)
#ORs and CIs.
cc_mod2_use2_attvalence <- confint(mod2_use2_attvalence,parm="beta_",method="Wald") #Calculate 95%CIs for fixed effects b coefficients.
ctab_mod2_use2_attvalence <- cbind(Coeff.=fixef(mod2_use2_attvalence),
                                   OR=exp(fixef(mod2_use2_attvalence)),CIs=exp(cc_mod2_use2_attvalence)) #Transform into ORs.
round(ctab_mod2_use2_attvalence,2)


