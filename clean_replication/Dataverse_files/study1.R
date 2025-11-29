library(tidyverse)
library(ggplot2)
library(ggrepel)
library(readr)
library(lme4)
library(interactions)
library(patchwork)
library(ggridges)
library(jtools)
library(modelsummary)
library(colorspace)
library(interplot)
library(ggpubr)
library(margins)
library(estimatr)


set.seed(1)

df<- read_csv("study1_data.csv")

colors<- c("#205C8A", "#d11141")

df <-df%>% 
  mutate(treat= as.factor(treatment),
         treatnum= as.numeric(treatment),
         gender= as.factor(gender),
         degree= as.factor(degree),
         nonwhite= as.factor(nonwhite),
         queer= as.factor(queer),
         relig= as.factor(relig),
         religion= as.factor(religion),
         race= as.factor(race),
         fourarm= as.factor(fourarm),
         immbelow= as.factor(immbelow),
         imm3= as.factor(imm3),
         region= as.factor(region),
         voterecall= as.factor(voterecall),
         brexit= as.factor(brexit),
         ideology= as.factor(ideology),
         agecat= as.factor(agecat))


##Figure 3##

model1<- glm (support ~ treat*imm_1, data=df, family="binomial")
summ(model1, robust=TRUE)
df$predictINT1<-predict(model1, df, type="response")

treat <- subset(df, treatnum==1)
control <- subset(df, treatnum==0)
proimm <- subset(df, immbelow==0)
noproimm <- subset(df, immbelow==1)

pred<-interact_plot(model1, pred = imm_1, modx = treat, interval = FALSE,
                     colors = colors)+
  labs(title="",
       y="Predicted support for\nLGBT+ education in schools",
       x="")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x =element_blank())+
  annotate(
    geom="text", x = 2.5, y = .65, size = 4, color = "#d11141", fontface=2,
    label = "Slope for\ntreated group")+
  annotate(
    geom = "curve", x =1.6, y = .6, xend = 1.2, yend = .44, 
    curvature = .4, arrow = arrow(length = unit(2, "mm")), colour="#d11141")+
  annotate(
    geom="text", x = 6, y = .4, size = 4, color = "#205C8A", fontface=2,
    label = "Slope for\ncontrol group")+
  annotate(
    geom = "curve", x =5, y = .4, xend = 2.52, yend =.38, 
    curvature = -.4, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")

gg_df <-
  model1 %>%
  margins(at = list(imm_1 = seq(0, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

ame<- ggplot(gg_df, aes(imm_1, AME)) +
  geom_point(colour="#d11141") +
  geom_line(colour="#d11141") +
  coord_cartesian(xlim = c(0, 10), ylim = c(-.25, .25)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#d11141") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Pre-treatment attitudes towards immigration")+
  ylab("Conditional ATE") +
  theme_minimal()


interaction_bin<- pred/ame+ 
  plot_annotation(title = 'Conditional average treatment effect: Study 1 (UK)',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("Figure3.png", dpi=1000)
ggsave("Figure3.tiff", dpi=1000)


##FIGURE 4##

modelsub1<- lm (support ~ treat, data=proimm)
summ(modelsub1, robust=TRUE)
proimm$predictedb<-predict(modelsub1, proimm)

modelsub2<- lm (support ~ treat, data=noproimm)
summ(modelsub2, robust=TRUE)
noproimm$predictedb<-predict(modelsub2, noproimm)

treatsub1 <- subset(proimm, treatnum==1)
controlsub1 <- subset(proimm, treatnum==0)
treatsub2 <- subset(noproimm, treatnum==1)
controlsub2 <- subset(noproimm, treatnum==0)


proimmplot<- effect_plot(model = modelsub1, pred = treat, robust=TRUE, 
                         cat.geom="point", cat.interval.geom="linerange",
                         colors="black", cat.pred.point.size=3, int.width = .90)+
  labs(title = "Pro-immigrant voters (N = 595)")+
  ylab("Support for LGBT+ education in schools (0-1)")+
  xlab("")+
  ylim(0,1)+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))+
  geom_jitter(data=treatsub1, aes(x=treat, y=predictedb),
              height=.25, size=4, width=.35, alpha=.35, shape=20,
              pch=21, color="#d11141")+
  geom_jitter(data=controlsub1, aes(x=treat, y=predictedb),
              height=.25, width=.35, alpha=.35, shape=20,
              pch=21, size=4, color="#205C8A")+  
  geom_bracket(xmin = c("0"), xmax = c("1"),
               y.position = c(.45), label = c("ATE=-.06*"),
               tip.length =-0.05,
               color="black")+
  theme_minimal()+
  theme(axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold"))


noproimmplot<- effect_plot(model = modelsub2, pred = treat, robust=TRUE, 
                           cat.geom="point", cat.interval.geom="linerange",
                           colors="black", cat.pred.point.size=3, int.width = .90)+
  labs(title = "Anti-immigrant voters (N = 556)")+
  ylab("Support for LGBT+ education in schools (0-1)")+
  xlab("")+
  ylim(0,1)+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))+
  geom_jitter(data=treatsub2, aes(x=treat, y=predictedb),
              height=.25, width=.35, alpha=.35, shape=20,
              pch=21, size=4, color="#d11141")+
  geom_jitter(data=controlsub2, aes(x=treat, y=predictedb),
              height=.25, width=.35, alpha=.35, shape=20,
              pch=21, size=4, color="#205C8A")+
  geom_bracket(xmin = c("0"), xmax = c("1"),
               y.position = c(.79), label = c("ATE=.10**"),
               tip.length =0.05,
               color="black")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face="bold"))


proimmplot+noproimmplot+ 
  plot_annotation(title = 'Effect of out-group treatment among:',
                  caption="Treatment group outcome statistically distinct at p<.1(*), p<0.05(**), & p<0.01(***)",
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("Figure4.png", dpi=1000)
ggsave("Figure4.tiff", dpi=1000)


###APPENDIX FILES###

##TABLE A7##
models2 <- list()
models2 [['Model 1']] <-lm (support ~ treat, data=df)
models2 [['Model 2']] <-lm (support ~ treat*imm_1, data=df)
models2 [['Model 3']] <-lm (support ~ treat, data=proimm)
models2 [['Model 4']] <-lm (support ~ treat, data=noproimm)
modelsummary(models2, star=c('*'=.1, "**"=.05, "***"=.01), output="latex")


##TABLE A8##
models2 <- list()
models2 [['Model 1']] <-lm (outcome ~ treat, data=df)
models2 [['Model 2']] <-lm (outcome ~ treat*imm_1, data=df)
models2 [['Model 3']] <-lm (outcome ~ treat, data=proimm)
models2 [['Model 4']] <-lm (outcome ~ treat, data=noproimm)
modelsummary(models2, star=c('*'=.1, "**"=.05, "***"=.01), output="latex", robust=TRUE)


##FIGURE A4##


model2<- lm (outcome ~ treat*imm_1, data=df)
summ(model2, robust=TRUE)
df$predictINT2<-predict(model2, df)

treat <- subset(df, treatnum==1)
control <- subset(df, treatnum==0)
proimm <- subset(df, immbelow==0)
noproimm <- subset(df, immbelow==1)

pred2<- ggplot(df, aes(imm_1, outcome, shape = treat, group = treat, color=treat)) +
  scale_shape_manual(values = c(20, 17))+
  scale_color_manual(values = c("#205C8A", "#d11141")) +
  scale_fill_manual(values = c("#205C8A", "#d11141")) +
  theme_minimal()+
  stat_smooth(method = "lm_robust", fullrange = TRUE, se=FALSE) +
  geom_jitter(data=treat, aes(x=imm_1, y=predictINT2, fill=treat, size=income), 
              alpha=.2, height=.2, width=.5, shape=20,
              pch=21, color="#d11141")+
  geom_jitter(data=control, aes(x=imm_1, y=predictINT2, fill=treat, size=income), 
              alpha=.2, height=.17, width=.7, shape=18,
              pch=21, color="#205C8A")+
  theme(legend.position = "none",
        axis.text.x =element_blank())+
  xlim(0, 10)+
  ylab("Predicted opposition to\nLGBT+ education in school")+
  xlab("")+
  annotate(
    geom="text", x = 2, y = 2.5, size = 4, color = "#d11141", fontface=2,
    label = "Slope for\ntreated group")+
  annotate(
    geom = "curve", x =1.3, y = 2.5, xend = 2.2, yend = 4.85, 
    curvature = -.4, arrow = arrow(length = unit(2, "mm")), colour="#d11141")+
  annotate(
    geom="text", x = 5, y = 6, size = 4, color = "#205C8A", fontface=2,
    label = "Slope for\ncontrol group")+
  annotate(
    geom = "curve", x =4.4, y = 6, xend = 2, yend =6, 
    curvature = .4, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")

gg_df2 <-
  model2 %>%
  margins(at = list(imm_1 = seq(0, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

ame2<- ggplot(gg_df2, aes(imm_1, AME)) +
  geom_point(colour="#d11141") +
  geom_line(colour="#d11141") +
  coord_cartesian(xlim = c(0, 10), ylim = c(-1.5, 1.5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#d11141") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Pre-treatment attitudes towards immigration")+
  ylab("Conditional ATE") +
  theme_minimal()+
  theme(axis.text.x =element_blank())

interaction2<- pred2/ame2+ 
  plot_annotation(title = 'Conditional average treatment effect: Study 1 (UK)',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("Figure A4.png", dpi=600)



###FIGURES A6 & A7###
lineartest<- interact_plot(model1, pred=imm_1, modx=treat, interval=TRUE,
                           linearity.check = TRUE, modx.labels = c("0"="Control", "1"="Treatment"),
                           x.label="Pre-treatment immigration attitudes", 
                           y.label="Opposition to LGBT+ education in schools (0-10)")+
  labs(title="Linearity assumption test (Hainmueller et al 2017)",
       subtitle="Continuous outcome linear regression model")
ggsave("FigureA6.png", dpi=600)


lineartest2<- interact_plot(model2, pred=imm_1, modx=treat, interval=TRUE,
                            linearity.check = TRUE, modx.labels = c("0"="Control", "1"="Treatment"),
                            x.label="Pre-treatment immigration attitudes", 
                            y.label="Support LGBT+ education in schools (0-1)")+
  labs(title="Linearity assumption test (Hainmueller et al 2017)",
       subtitle="Binary outcome logstic regression model")
ggsave("FigureA7.png", dpi=600)

###FIGURE A9###
model1<- lm (support ~ treat, data=df)
model2<- lm (support ~ treat, data=proimm)
model3<- lm (support ~ treat, data=noproimm)
library(ritest)
ritest1<- ritest(model1, 'treat', reps = 2000)
Rplot1 <- ggplot(data.frame(betas = ritest1$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="grey47") +
  geom_vline(xintercept =  model1$coefficients[2], colour="blue2") +
  theme_minimal() +
  labs(y = "Density", x = "Permutated ATE", title = "Full sample")


ritest2<- ritest(model2, 'treat', reps = 2000)
Rplot2 <- ggplot(data.frame(betas = ritest2$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.8, fill="#205C8A") +
  geom_vline(xintercept =  model2$coefficients[2], colour="blue2") +
  theme_minimal() +
  labs(y = "Density", x = "Permutated ATE", title = "Pro-immigration respondents")


ritest3<- ritest(model3, 'treat', reps = 2000)
Rplot3 <- ggplot(data.frame(betas = ritest2$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.8, fill="#d11141") +
  geom_vline(xintercept =  model3$coefficients[2], colour="blue2") +
  theme_minimal() +
  labs(y = "Density", x = "Permutated ATE", title = "Anti-immigration respondents")+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

Rplot1/(Rplot2+Rplot3)+ 
  plot_annotation(
    title = "Randomisation inference of ATE (2000 permutations") &
  theme(plot.title = element_text(face=2))

ggsave("FigureA9.png", dpi=700)

