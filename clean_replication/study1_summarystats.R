##This file produces the summary statistics for Study 1##

library(tidyverse)
library(readr)
library(modelsummary)
library(estimatr)

set.seed(123)

df<- read_csv("UKdata_analysis.csv")

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


###TABLE A1###
datasummary(outcome  + imm_1 + income ~ N + Mean + SD + Min + Max,
            data = df, output="latex")


###TABLE A2###

datasummary(treat + immbelow + imm3 + gender + queer + agecat + nonwhite + religion + ideology + voterecall + brexit + 1 ~ N + Percent(fn = function(x, y) 100 * length(x) / length(y)),
            data = df, output="latex")



###TABLE A3###
df2 = subset(df, select = c(outcome, support, treat, imm_1, immbelow, gender, queer, agecat, nonwhite,voterecall, brexit))
df2 <-df2%>% 
  mutate(queer= as.numeric(queer),
         gender= as.numeric(gender),
         immbelow= as.numeric(immbelow),
         agecat= as.numeric(agecat),
         nonwhite= as.numeric(nonwhite))
df2 <-df2%>% 
  mutate( 
    leaver = case_when(
      brexit== "Leave" ~ 1,
      brexit!= "Leave" ~ 0))
df2 <-df2%>% 
  mutate( 
    labour = case_when(
      voterecall== "Labour" ~ 1,
      voterecall!= "Labour" ~ 0))

df2 = subset(df2, select = -c(brexit,voterecall))
datasummary_balance(~treat,
                    data = df2,
                    output="latex")

