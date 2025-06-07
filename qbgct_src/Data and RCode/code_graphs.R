#========================================================#
# 
#        Survey: ECRs and Open Science Practices
#                   Graphs and Plots
# 
#========================================================#

#By Daniel Toribio-Florez, June 2020
#Codebook with variables labelling is accesible at: https://osf.io/qbgct/

#============================== #
#           PACKAGES            #
#============================== #

library(Rmisc)
library(cowplot)
library(sjPlot)
library(tidyverse)

#============================== #
####   DESCRIPTIVE GRAPHS    ####
#============================== #
#### Current vs. Desired Knowledge ####
#Average Current vs. Desired Knowledge per Section across OAPractices.
summ_know<-summarySE(df_know, "Knowledge", groupvars=c("OAPractice","Section","Current_Desired"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")

p_know <- ggplot(summ_know, aes(x = Section, y = Knowledge, fill = Section, alpha = Current_Desired))+
  facet_grid(.~OAPractice)+
  geom_bar(position=position_dodge(),colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(0.9),aes(ymin=Knowledge-ci,ymax=Knowledge+ci), width=.15)+geom_line()+
  geom_hline(yintercept = 4, linetype = "dashed")+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_text(face = "bold",size = 12),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12))+
  labs(x = "", y = "Knowledge")+
  scale_y_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7), position = "left")+
  coord_cartesian(ylim = c(1,7))+
  geom_text(aes(y = Knowledge+ci+0.2, label = paste("n=", N, sep = "")),size=3.25, position = position_dodge(0.9), show.legend = FALSE)+
  scale_alpha_discrete(name = "Knowledge", labels = c("Current Knowledge", "Desired Knowledge"),range = c(0.6, 1)) +
  scale_fill_manual(values=c("#009E73", "#E69F00", "#56B4E9"))
p_know

#### Attitudes ####
#Average Att Valence per Section across OAPractices from different Perspectives.
summ_attvalence_level <- summarySE(df_attvalence, "Att", groupvars=c("OAPractice","Level","Section"),na.rm = TRUE, conf.interval =0.95)  %>%
  filter(Section!="NA")
summ_attvalence_level$Level <- relevel(as.factor(summ_attvalence_level$Level),"Daily Research Life")
p_attvalence_level <- ggplot(summ_attvalence_level, aes(x = Section, y = Att, fill = Section))+
  facet_grid(Level~OAPractice, scales="free", space="free_x")+
  geom_bar(position=position_dodge(), colour="black",stat="identity") +
  geom_errorbar(position=position_dodge(0.9),aes(ymin=Att-ci,ymax=Att+ci), width=.15)+
  geom_hline(yintercept = 0)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12),
                   panel.spacing.y = unit(5,"mm"))+
  labs(x = "", y = "Attitude Valence")+
  geom_text(aes(y = Att+ci+0.5, label=paste("n=", N, sep = "")),size=3.5, position = position_dodge(0.9), show.legend = FALSE)+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6), position = "left")+
  coord_cartesian(ylim = c(0,6))+
  scale_fill_manual(values=c("#009E73", "#E69F00", "#56B4E9"))
p_attvalence_level

#### Perceived Need ####
#Average Need per Section across OAPractices for different Perspectives.
summ_need<-summarySE(df_need, "Need", groupvars=c("OAPractice","Section","Level"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
summ_need$Level <- relevel(as.factor(summ_need$Level),"Daily Research Life")
p_need <- ggplot(summ_need, aes(x = Section, y = Need, fill = Section))+
  facet_grid(Level~OAPractice)+
  geom_bar(position=position_dodge(),colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(0.9),aes(ymin=Need-ci,ymax=Need+ci), width=.15)+geom_line()+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_text(face = "bold",size = 12),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12),
                   panel.spacing.y = unit(5,"mm"))+
  labs(x = "", y = "Need")+
  geom_text(aes(y = Need+ci+0.5, label = paste("n=", N, sep = "")),size=3.5, position = position_dodge(0.9), show.legend = FALSE)+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6), position = "left")+
  coord_cartesian(ylim = c(0,6))+
  scale_fill_manual(values=c("#009E73", "#E69F00", "#56B4E9"))
p_need

#### Implementation ####
#Average Use of different OAPractices in the Past and Future 12 months per Section.
summ_use<-summarySE(df_use, "Use", groupvars=c("OAPractice","Section","Past_Future"),na.rm = TRUE, conf.interval =0.95)%>%
  filter(Section!="NA")
#Transform unit to Percentage.
summ_use$Use <- summ_use$Use*100
summ_use$ci <- summ_use$ci*100

p_use <- ggplot(summ_use, aes(x = Section, y = Use, fill = reorder(Past_Future, Use)))+
  facet_grid(.~OAPractice)+
  geom_bar(position=position_dodge(),colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(0.9),aes(ymin=Use-ci,ymax=Use+ci), width=.15)+geom_line()+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12))+
  labs(x = "", y = "Implementation (in %)")+
  geom_text(aes(y = Use+ci+3, label=paste("n=", N, sep = "")),size=3, position = position_dodge(0.95), show.legend = FALSE)+
  scale_y_continuous(expand = c(0, 0),breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), position = "left")+
  coord_cartesian(ylim = c(0,100))+
  scale_alpha_discrete(name = "Use", labels = c("In the following 12 months", "In the past 12 months"),range = c(0.6, 1)) +
  scale_fill_manual(values=c("#D55E00", "#0072B2"))
p_use


#-------------------------------------------------------------------------------#

#====================================================== #
####   ESTIMATED RELATIONSHIPS FROM FITTED MODELS    ####
#====================================================== #

#============================== #
#### Attitudes ~ Knowledge ####
#============================== #
#Current Knowledge.
mod2_know1_attvalence <- lmer(Att~Knowledge*Level+(1|Responder_ID/OAPractice),
                              filter(df_know_att, Current_Desired == "Are you knowledgeable about..."))
p5 <- plot_model(mod2_know1_attvalence,type="pred",terms=c("Knowledge","Level"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Current Knowledge", y = "Attitude Valence")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

mod1_know1_attvalence <- lmer(Att~Knowledge*OAPractice+(1|Responder_ID),
                              filter(df_know_att, Current_Desired == "Are you knowledgeable about..."))
p6 <- plot_model(mod1_know1_attvalence,type="pred",terms=c("Knowledge","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Current Knowledge", y = "Attitude Valence")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

#Desired Knowledge.
mod2_know2_attvalence <- lmer(Att~Knowledge*Level+(1|Responder_ID/OAPractice)
                              ,filter(df_know_att, Current_Desired == "Would you like to know more about..."))

p7 <- plot_model(mod2_know2_attvalence,type="pred",terms=c("Knowledge","Level"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Desired Knowledge", y = "Attitude Valence")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))


mod1_know2_attvalence <- lmer(Att~Knowledge*OAPractice+(1|Responder_ID),
                              filter(df_know_att, Current_Desired == "Would you like to know more about..."))
p8 <- plot_model(mod1_know2_attvalence,type="pred",terms=c("Knowledge","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Desired Knowledge", y = "Attitude Valence")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

#============================== #
####     Need ~ Knowledge    ####
#============================== #
#Current Knowledge.
mod2_know1_need <- lmer(Need~Knowledge*Level+(1|Responder_ID/OAPractice),
                        filter(df_know_need,Current_Desired=="Are you knowledgeable about..."))
p9 <- plot_model(mod2_know1_need,type="pred",terms=c('Knowledge', 'Level'))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Current Knowledge", y = "Need")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

mod1_know1_need <- lmer(Need~Knowledge*OAPractice+(1|Responder_ID),
                        filter(df_know_need,Current_Desired=="Are you knowledgeable about..."))
p10 <- plot_model(mod1_know1_need,type="pred",terms=c('Knowledge','OAPractice'))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Current Knowledge", y = "Need")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

#Desired Knowledge.
mod2_know2_need <- lmer(Need~Knowledge*Level+(1|Responder_ID/OAPractice),
                        filter(df_know_need,Current_Desired == "Would you like to know more about..."))
p11<-plot_model(mod2_know2_need,type="pred",terms=c('Knowledge','Level'))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Desired Knowledge", y = "Need")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

mod1_know2_need <- lmer(Need~Knowledge*OAPractice+(1|Responder_ID),
                        filter(df_know_need,Current_Desired == "Would you like to know more about..."))
p12<-plot_model(mod1_know2_need,type="pred",terms=c('Knowledge','OAPractice'))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Desired Knowledge", y = "Need")+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  scale_x_continuous(expand = c(0, 0),breaks=c(1,2,3,4,5,6,7))+
  coord_cartesian(ylim = c(0,6))

#=============================#
####    Need ~  Attitudes    ####
#=============================#
mod_need_attvalence <- lmer(Need~Att*Level+(1|Responder_ID), df_need_attvalence)
plot_model(mod_need_attvalence,type="pred",terms=c("Att","Level"))

mod2_need_attvalence <- lmer(Need~Att*OAPractice+(1|Responder_ID), df_need_attvalence)
plot_model(mod2_need_attvalence,type="pred",terms=c("Att","OAPractice"))

#=============================#
####    Use ~  Knowledge    ####
#=============================#
#Past Knowledge.
mod1_use1_know <- glmer(Use~Knowledge*Current_Desired+(1|Responder_ID),
                        filter(df_use_know,Past_Future=="In the past 12 months"),
                        family = binomial(link = logit))
summary(mod1_use1_know)
p17 <- plot_model(mod1_use1_know,type="pred",terms = c("Knowledge","Current_Desired"))+
theme_bw()+theme(panel.grid.minor=element_blank(),
                 plot.title = element_blank(),
                 axis.title.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(size=12,face="bold"),
                 axis.text.y=element_text(size=12),
                 axis.text.x=element_text(size=12),
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.position = "bottom",
                 strip.text.x = element_text(size = 12),
                 strip.text.y = element_text(size = 12))+
  labs(x = "Knowledge" , y = "Probability of Implementation\nin the past 12 months")+
  scale_colour_discrete(labels = c("Current Knowledge", "Desired Knowledge"))+
  scale_y_continuous(expand = c(0,0),breaks = c(0,.2,.4,.6,.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(1,7))

#Future Knowledge.
mod1_use2_know <- glmer(Use~Knowledge*Current_Desired+(1|Responder_ID),
                        filter(df_use_know,Past_Future=="In the following 12 months"),
                        family = binomial(link = logit))
#Model do not Converge. Restart from last estimates.
ss_mod1_use2_know<-getME(mod1_use2_know,c("theta","fixef"))
mod1_use2_know.re<- update(mod1_use2_know,start=ss_mod1_use2_know)
summary(mod1_use2_know.re)

p18 <- plot_model(mod1_use2_know.re,type="pred",terms = c("Knowledge","Current_Desired"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Knowledge" , y = "Probability of Implementation\nin the following 12 months")+
  scale_colour_discrete(labels = c("Current Knowledge", "Desired Knowledge"))+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(1,7))

#Past Knowledge Across OA Practices.
df_use_know$OAPractice <- relevel(as.factor(df_use_know$OAPractice), "OA Publications")
mod2_use1_know1 <- glmer(Use~Current_Knowledge*OAPractice+(1|Responder_ID),
                         filter(df_use_know,Past_Future=="In the past 12 months"),
                         family = binomial(link = logit),
                         control = glmerControl(optimizer="bobyqa"))

p19 <- plot_model(mod2_use1_know1, type="pred",terms=c("Current_Knowledge","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Current Knowledge" , y = "Probability of Implementation\nin the past 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(1,7))

#Future Knowledge Across OA Practices.
df_use_know$OAPractice <- relevel(df_use_know$OAPractice, "OA Publications")
mod2_use2_know1 <- glmer(Use~Current_Knowledge*OAPractice+(1|Responder_ID),
                         filter(df_use_know,Past_Future=="In the following 12 months"),
                         family = binomial(link = logit),
                         control = glmerControl(optimizer="bobyqa"))

p20 <- plot_model(mod2_use2_know1, type = "pred", terms = c("Current_Knowledge","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Current Knowledge" , y = "Probability of Implementation\nin the following 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(1,7))


#============================== #
####       Use ~ Need        ####
#============================== #
#Past Use.
df_use_need$OAPractice <- relevel(df_use_need$OAPractice, "OA Publications")
mod1_use1_need <- glmer(Use~Need*OAPractice+(1|Responder_ID),
                        filter(df_use_need, Past_Future == "In the past 12 months"), 
                        family = binomial(link = logit),
                        control = glmerControl(optimizer="bobyqa"))

p21 <- plot_model(mod1_use1_need , type = "pred", terms = c("Need","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Need" , y = "Probability of Implementation\nin the past 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,.2,.4,.6,.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(-6,6))

mod2_use1_need <- glmer(Use~Need*Level+(1|Responder_ID),
                        filter(df_use_need,Past_Future == "In the past 12 months"),
                        family = binomial(link = logit),
                        control = glmerControl(optimizer="bobyqa"))
summary(mod2_use1_need)

p22 <- plot_model(mod2_use1_need, type="pred",terms=c('Need','Level'))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Need" , y = "Probability of Implementation\nin the past 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.05,.1,.15,0.2,0.25,0.3))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.3),xlim = c(-6,6))

#Future Use.
mod1_use2_need <- glmer(Use~Need*OAPractice+(1|Responder_ID),
                        filter(df_use_need, Past_Future == "In the following 12 months"), 
                        family = binomial(link = logit),
                        control = glmerControl(optimizer="bobyqa"))

p23 <- plot_model(mod1_use2_need, type = "pred", terms=c("Need","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Need" , y = "Probability of Implementation\nin the following 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(-6,6))
  


mod2_use2_need <- glmer(Use~Need*Level+(1|Responder_ID),
                        filter(df_use_need,Past_Future == "In the following 12 months"),
                        family = binomial(link = logit))

p24<-plot_model(mod2_use2_need, type="pred",terms=c('Need','Level'))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Need" , y = "Probability of Implementation\nin the following 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(-6,6))
#============================== #
####     Use ~ Attitudes     ####
#============================== #
#Past Use.
df_use_attvalence$OAPractice <- relevel(as.factor(df_use_attvalence$OAPractice)
                                        ,"OA Publications")
mod1_use1_attvalence <- glmer(Use~Att*OAPractice+(1|Responder_ID),
                              filter(df_use_attvalence, Past_Future == "In the past 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))

p25 <- plot_model(mod1_use1_attvalence, type = "pred", terms=c("Att [all]","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Attitude Valence" , y = "Probability of Implementation\nin the past 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,.2,.4,.6,.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(-6,6))


mod2_use1_attvalence <- glmer(Use~Att*Level +(1|Responder_ID),
                              filter(df_use_attvalence,Past_Future == "In the past 12 months"),
                              family = binomial(link = logit))

p26 <- plot_model(mod2_use1_attvalence,type="pred",terms=c('Att','Level'))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Attitude Valence" , y = "Probability of Implementation\nin the past 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.05,.1,.15,0.2,0.25,0.3))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.3),xlim = c(-6,6))

#Future Use.
mod1_use2_attvalence <- glmer(Use~Att*OAPractice+(1|Responder_ID),
                              filter(df_use_attvalence, Past_Future == "In the following 12 months"), 
                              family = binomial(link = logit),
                              control = glmerControl(optimizer="bobyqa"))

p28 <-plot_model(mod1_use2_attvalence, type = "pred", terms=c("Att [all]","OAPractice"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Attitude Valence" , y = "Probability of Implementation\nin the following 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(-6,6))

mod2_use2_attvalence <- glmer(Use~Att*Level +(1|Responder_ID),
                              filter(df_use_attvalence,Past_Future == "In the following 12 months"),
                              family = binomial(link = logit))
p27 <- plot_model(mod2_use2_attvalence,type="pred",terms=c('Att','Level'))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   plot.title = element_blank(),
                   axis.title.y = element_text(size=12,face="bold"),
                   axis.title.x = element_text(size=12,face="bold"),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom",
                   strip.text.x = element_text(size = 12),
                   strip.text.y = element_text(size = 12))+
  labs(x = "Attitude Valence" , y = "Probability of Implementation\nin the following 12 months")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,1),xlim = c(-6,6))
