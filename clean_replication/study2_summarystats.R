##This file produces the summary statistics for Study 2##

library(tidyverse)
library(readr)
library(modelsummary)
library(estimatr)

set.seed(123)

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

###TABLE A4###
datasummary(lgbtED + imm_1 + pride_valoresUE + pride_libertadOCC + pride_verde + pride_viomach + pride_bandera + ptvPP + ptvPSOE + ptvVOX + ptvCs + ptvMP ~ N + weighted.mean * Arguments(w = nationalweight,na.rm = TRUE) + SD + Min + Max,
            data = spain, output="latex")


###TABLE A5###
wtpct <- function(x, y) sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE) * 100
datasummary(treat + immdum + imm3 + gender + queer + agecat + child + foreignborn + CCAA+ 1 ~ N + Percent(fn = function(x, y) 100 * length(x) / length(y)),
            data = spain, output="latex")
datasummary(supportcat + treat + immdum + imm3  + gender + queer + agecat + child + foreignborn + CCAA + 1 ~ N + nationalweight * Percent(fn = wtpct),
            data = spain, output="latex")

###TABLE A6###
df2 = subset(spain, select = c(treat, imm_1, immdum, gender, queer, agecat, foreignborn, weightbins))
df2 <-df2%>% 
  subset(weightbins == "high") %>%
  mutate(queer= as.numeric(queer),
         gender= as.numeric(gender),
         immdum= as.numeric(immdum),
         agecat= as.numeric(agecat),
         weights= as.numeric(nationalweight),
         foreignborn= as.numeric(foreignborn))

dfX = subset(df2, select = -c(nationalweight))
datasummary_balance(~treat,
                    data = dfX,
                    output="latex")


