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

load("study2_data.Rda")
spain <-spain%>% 
  mutate(treat= as.factor(treat),
         treatnum= as.numeric(treat),
         gender= as.factor(gender),
         supportcat= as.factor(support),
         agecat= as.factor(agecat),
         child= as.factor(child),
         immdum= as.factor(immdum),
         imm5= as.factor(imm5),
         imm3= as.factor(imm3),
         foreignborn= as.factor(foreignborn),
         CCAA= as.factor(CCAA),
         queer= as.factor(queer))

colors<- c("#205C8A", "#d11141")



##FIGURE 6##

modelES<- glm (support ~ treat*imm_1, data=spain, weight=nationalweight, family="binomial")
summary(modelES, robust=TRUE)
spain$predictINT2<-predict(modelES, spain, type="response")


predES<- interact_plot(modelES, pred = imm_1, modx = treat, interval = FALSE,
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
    geom = "curve", x =5, y = .4, xend = 2.6, yend =.31, 
    curvature = -.4, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")

gg_df <-
  modelES %>%
  margins(at = list(imm_1 = seq(0, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

ameES<- ggplot(gg_df, aes(imm_1, AME)) +
  geom_point(colour="#d11141") +
  geom_line(colour="#d11141") +
  coord_cartesian(xlim = c(0, 10)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#d11141") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Pre-treatment attitudes towards immigration")+
  ylab("Conditional ATE") +
  theme_minimal()

interaction<- predES/ameES+ 
  plot_annotation(title = 'Conditional average treatment effect: Study 2 (Spain)',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("Figure6.png", dpi=1000)
ggsave("Figure6.tiff", dpi=1000)


##FIGURE 7###
treatES <- subset(spain, treat==1)
controlES <- subset(spain, treat==0)
proimmES <- subset(spain, immdum==1)
noproimmES <- subset(spain, immdum==0)

modelsub1ES<- glm (support ~ treat, weight=nationalweight, data=proimmES, family="binomial")
summary(modelsub1ES)
proimmES$predictedb<-predict(modelsub1ES, proimmES, type="response")

modelsub2ES<- glm (support ~ treat, weight=nationalweight, data=noproimmES, family="binomial")
summary(modelsub2ES)
noproimmES$predictedb<-predict(modelsub2ES, noproimmES, type="response")

treatsub1ES <- subset(proimmES, treat==1)
controlsub1ES <- subset(proimmES, treat==0)
treatsub2ES <- subset(noproimmES, treat==1)
controlsub2ES <- subset(noproimmES, treat==0)

proimmplotES<- effect_plot(model = modelsub1ES, pred = treat,
                            cat.geom="point", cat.interval.geom="linerange",
                            colors="black", cat.pred.point.size=3, int.width = .90)+
  labs(title = "Pro-immigrant voters (N = 700)")+
  ylab("Support for LGBT+ education in schools (0-1)")+
  xlab("")+
  ylim(0,1)+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))+
  geom_jitter(data=treatsub1ES, aes(x=treat, y=predictedb),
              height=.25, size=4, width=.35, alpha=.35, shape=20,
              pch=21, color="#d11141")+
  geom_jitter(data=controlsub1ES, aes(x=treat, y=predictedb),
              height=.25, width=.35, alpha=.35, shape=20,
              pch=21, size=4, color="#205C8A")+  
  geom_bracket(xmin = c("0"), xmax = c("1"),
               y.position = c(.45), label = c("ATE=.11***"),
               tip.length =-0.05,
               color="black")+
  theme_minimal()+
  theme(axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold"))

noproimmplotES<- effect_plot(model = modelsub2ES, pred = treat, 
                              cat.geom="point", cat.interval.geom="linerange",
                              colors="black", cat.pred.point.size=3, int.width = .90)+
  labs(title = "Anti-immigrant voters (N = 516)")+
  ylab("Support for LGBT+ education in schools (0-1)")+
  xlab("")+
  ylim(0,1)+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))+
  geom_jitter(data=treatsub2ES, aes(x=treat, y=predictedb),
              height=.25, width=.35, alpha=.35, shape=20,
              pch=21, size=4, color="#d11141")+
  geom_jitter(data=controlsub2ES, aes(x=treat, y=predictedb),
              height=.25, width=.35, alpha=.35, shape=20,
              pch=21, size=4, color="#205C8A")+
  geom_bracket(xmin = c("0"), xmax = c("1"),
               y.position = c(.79), label = c("ATE=.10***"),
               tip.length =0.05,
               color="black")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face="bold"))

proimmplotES+noproimmplotES+ 
  plot_annotation(title = 'Effect of out-group treatment among:',
                  caption="Treatment group outcome statistically distinct at p<.1(*), p<0.05(**), & p<0.01(***)",
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("Figure7.png", dpi=1000)
ggsave("Figure7.tiff", dpi=1000)


###FIGURE 8###

pride1<- glm (proudWvalues ~ treat*imm_1, weight=nationalweight, data=spain, family="binomial")
summary(pride1)
spain$predictPR<-predict(pride1, spain)

prideplot1<- interact_plot(pride1, pred = imm_1, modx = treat, interval = FALSE,
                          modx.labels = c("0"="Control", "1"="Treatment"))+
  labs(title="",
       y="Feeling of pride in the freedoms afforded by western culture",
       x="")+
  theme_minimal()+
  scale_color_manual(values = c("#205C8A", "#d11141")) +
  scale_fill_manual(values = c("#205C8A", "#d11141")) +
  theme(legend.position = "none",
        axis.text.x =element_blank())+
  xlim(0, 10)+
  ylab("Pr(Feels pride in the freedoms\n afforded by western culture)")+
  xlab("")+
  annotate(
    geom="text", x = 3, y = .85, size = 4, color = "#d11141", fontface=2,
    label = "Slope for\ntreated group")+
  annotate(
    geom = "curve", x =2.1, y = .85, xend = 1, yend = .77, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")), colour="#d11141")+
  annotate(
    geom="text", x = 3.5, y = .62, size = 4, color = "#205C8A", fontface=2,
    label = "Slope for\ncontrol group")+
  annotate(
    geom = "curve", x =2.7, y = .56, xend = 1.65, yend =.61, 
    curvature = -.4, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")



gg_dfmech <-
  pride1 %>%
  margins(at = list(imm_1 = seq(0, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

prideplot2<- ggplot(gg_dfmech, aes(imm_1, AME)) +
  geom_point(colour="#d11141") +
  geom_line(colour="#d11141") +
  coord_cartesian(xlim = c(0, 10)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#d11141") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Pre-treatment attitudes towards immigration")+
  ylab("Conditional ATE") +
  theme_minimal()


prideplotfull<- prideplot1/prideplot2+ 
  plot_annotation(title = 'Ancillary outcome: pride in "Freedoms of western lifsetyle"',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("Figure8.png", dpi=1000)
ggsave("Figure8.tiff", dpi=1000)


###TABLE A9###
models1 <- list()
models1 [['Model 1']] <-lm (support ~ treat + imm_1, weight=nationalweight, data=spain)
models1 [['Model 2']] <-lm (support ~ treat*imm_1, weight=nationalweight, data=spain)
models1 [['Model 3']] <-lm (support ~ treat, weight=nationalweight, data=proimmES)
models1 [['Model 4']] <-lm (support ~ treat, weight=nationalweight, data=noproimmES)
modelsummary(models1, star=c('*'=.1, "**"=.05, "***"=.01), output="latex")



###TABLE A10###
models0 <- list()
models0 [['Model 1']] <-lm (lgbtED ~ treat + imm_1, weight=nationalweight, data=spain)
models0 [['Model 2']] <-lm (lgbtED ~ treat*imm_1, weight=nationalweight, data=spain)
models0 [['Model 3']] <-lm (lgbtED ~ treat, weight=nationalweight, data=proimmES)
models0 [['Model 4']] <-lm (lgbtED ~ treat, weight=nationalweight, data=noproimmES)
modelsummary(models0, star=c('*'=.1, "**"=.05, "***"=.01), output="latex")

###TABLE A11###
mech <- list()
mech [['EU norms']]<- lm (pride_valoresUE ~ treat*imm_1, weight=nationalweight, data=spain)
mech [['Western liberal values']]<- lm (pride_libertadOCC ~ treat*imm_1, weight=nationalweight, data=spain)
mech [['Green politics']]<- lm (pride_verde ~ treat*imm_1, weight=nationalweight, data=spain)
mech [['Domestic violence protections']]<- lm (pride_viomach ~ treat*imm_1, weight=nationalweight, data=spain)
mech [['Spanish flag']]<- lm (pride_bandera ~ treat*imm_1, weight=nationalweight, data=spain)
mech [['Spanish military efforts']]<- lm (pride_mili ~ treat*imm_1, weight=nationalweight, data=spain)
modelsummary(mech, star=c('*'=.1, "**"=.05, "***"=.01), output="latex", robust=TRUE)



###FIGURE A5###
mech1<- lm (pride_libertadOCC ~ treat*imm_1, weight=nationalweight, data=spain)
spain$predictPR<-predict(mech1, spain)

treatES <- subset(spain, treat==1)
controlES <- subset(spain, treat==0)

mechplot1<- interact_plot(mech1, pred = imm_1, modx = treat, interval = FALSE,
                          colors = colors)+
  labs(title="",
       y="Feeling of pride in the freedoms afforded by western culture",
       x="")+
  theme_minimal()+
  scale_color_manual(values = c("#205C8A", "#d11141")) +
  scale_fill_manual(values = c("#205C8A", "#d11141")) +
  theme(legend.position = "none",
        axis.text.x =element_blank())+
  xlim(0, 10)+
  ylab("Feeling of pride in the freedoms\n afforded by western culture")+
  xlab("")+
  annotate(
    geom="text", x = 3, y = 7.5, size = 4, color = "#d11141", fontface=2,
    label = "Slope for\ntreated group")+
  annotate(
    geom = "curve", x =2.1, y = 7.5, xend = 1, yend = 7.3, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")), colour="#d11141")+
  annotate(
    geom="text", x = 3.5, y = 6.35, size = 4, color = "#205C8A", fontface=2,
    label = "Slope for\ncontrol group")+
  annotate(
    geom = "curve", x =2.7, y = 6.35, xend = 1.65, yend =6.4, 
    curvature = -.4, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")


gg_dfmech <-
  mech1 %>%
  margins(at = list(imm_1 = seq(0, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

mechplot2<- ggplot(gg_dfmech, aes(imm_1, AME)) +
  geom_point(colour="#d11141") +
  geom_line(colour="#d11141") +
  coord_cartesian(xlim = c(0, 10)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#d11141") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Pre-treatment attitudes towards immigration")+
  ylab("Conditional ATE") +
  theme_minimal()


mechplotfull<- mechplot1/mechplot2+ 
  plot_annotation(title = 'Ancillary outcome: pride in "Freedoms of western lifsetyle"',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))
ggsave("FigureA5.png", dpi=1000)
ggsave("FigureA5.tiff", dpi=1000)
