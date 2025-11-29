library(tidyverse)
library(PrettyCols)
library(haven)
library(questionr)
library(survey)
library(srvyr)
library(maps)
library(readxl)
library(mapproj)
library(patchwork)
theme_nhsr_demo <- function(base_size = 12, 
                            dark_text = "#1A242F") {
  
  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  
  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = mid_text, family = "BrandonText", lineheight = 1.1),
          plot.title = element_text(colour = dark_text, family = "EnriquetaSB", size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
          axis.text.y = element_text(colour = light_text, size = rel(0.8)),
          axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
          axis.text.x = element_text(colour = mid_text, size = 12),
          axis.title.x = element_blank(),
          legend.position = "top",
          legend.justification = 1,
          panel.grid = element_line(colour = "#F3F4F5"),
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"))
}



ess_data_raw <- read_dta("ESS-Data-Wizard-subset-2023-02-08.dta") |> 
  select(idno, essround, cntry, idno, gndr, age, ends_with("ght"), lrscale,
         freehms, #free life LG // agree/disagree // 1-5
         hmsacld, #adoptions for LGB // agree strongly -disagree strongly // 1-5 
         hmsfmlsh,euftf, 
         imsmetn, #allow same race // allow // 1-4
         imdfetn, #allow diff race // allow // 1-4
         impcntr, #allow poor non-European country // allow // 1-4
         imbgeco, #bad/good econ // 0-10
         imueclt, #cultural life undermined/enriched // 0-10
         imwbcnt) #place to live // 0-10



ess_data <- ess_data_raw |> 
  mutate(gay= as.numeric(freehms),
         gay= factor(case_when(gay== 1 ~ "Agree strongly",
                               gay== 2 ~ "Agree",
                               gay== 3 ~ "Neither agree nor disagree",
                               gay== 4 ~ "Disagree",
                               gay== 5 ~ "Disagree strongly",
         ), 
         levels= c("Disagree strongly", "Disagree", 
                   "Neither agree nor disagree",
                   "Agree", "Agree strongly")),
         gay_bin= as.numeric(case_when(freehms %in% c(1, 2) ~ 1, 
                                       freehms %in% c(3,4,5) ~ 0,
                                       TRUE ~ NA_real_)),
         
         diffrace_bin= as.numeric(case_when(imdfetn %in% c(1,2) ~ 1, 
                                            imdfetn %in% c(3,4) ~ 0,
                                            TRUE ~ NA_real_)),
         poorctry_bin= as.numeric(case_when(impcntr %in% c(1,2) ~ 1, 
                                            impcntr %in% c(3,4) ~ 0,
                                            TRUE ~ NA_real_)),
         gay_diffrace = factor(case_when(gay_bin== 1 & diffrace_bin== 1 ~ "Modern inclusive",
                                         gay_bin== 1 & diffrace_bin== 0 ~ "Modern nativist",
                                         gay_bin== 0 & diffrace_bin== 1 ~ "Conservative inclusive",
                                         gay_bin== 0 & diffrace_bin== 0 ~ "Conservative nativist"),
                               levels= c("Modern inclusive","Modern nativist",
                                         "Conservative inclusive", "Conservative nativist")),
         
         gay_poorctry = factor(case_when(gay_bin== 1 & poorctry_bin== 1 ~ "Modern inclusive",
                                         gay_bin== 1 & poorctry_bin== 0 ~ "Modern nativist",
                                         gay_bin== 0 & poorctry_bin== 1 ~ "Conservative inclusive",
                                         gay_bin== 0 & poorctry_bin== 0 ~ "Conservative nativist"),
                               levels= c("Modern inclusive","Modern nativist",
                                         "Conservative inclusive", "Conservative nativist")),
         adoption_poorctry= factor(case_when(hmsacld %in% c(1,2) & poorctry_bin== 1 ~ "Modern inclusive",
                                             hmsacld %in% c(1,2) & poorctry_bin== 0 ~ "Modern nativist",
                                             hmsacld %in% c(4,5) & poorctry_bin== 1 ~ "Conservative inclusive",
                                             hmsacld %in% c(4,5) & poorctry_bin== 0 ~ "Conservative nativist",
                                             TRUE ~ NA_character_),
                                   levels= c("Modern inclusive","Modern nativist",
                                             "Conservative inclusive", "Conservative nativist"))
  ) |> 
  filter(cntry %in% c("BE", "CH", "DE", "EE", "ES", "FI", "FR", "GB", "CZ",
                      "HU","IE",  "NL", "NO", "PL", "PT", "SE", "SI", "DK")) |>
  mutate(
    cntry= case_when(cntry== "BE" ~ "Belgium",
                     cntry== "CH" ~ "Switzerland",
                     cntry== "DE" ~ "Germany",
                     cntry== "EE" ~ "Estonia",
                     cntry== "ES" ~ "Spain",
                     cntry== "FI" ~ "Finland",
                     cntry== "FR" ~ "France",
                     cntry== "GB" ~ "United Kingdom",
                     cntry== "CZ" ~ "Czech Republic",
                     cntry== "HU" ~ "Hungary",
                     cntry== "IE" ~ "Ireland",
                     cntry== "NL" ~ "Netherlands",
                     cntry== "NO" ~ "Norway",
                     cntry== "PL" ~ "Poland",
                     cntry== "PT" ~ "Portugal",
                     cntry== "SE" ~ "Sweden",
                     cntry== "SI" ~ "Slovenia",
                     cntry== "DK" ~ "Denmark"),
    essround= case_when(essround== 1 ~ "2002",
                        essround== 2 ~ "2004",
                        essround== 3 ~ "2006",
                        essround== 4 ~ "2008",
                        essround== 5 ~ "2010",
                        essround== 6 ~ "2012",
                        essround== 7 ~ "2014",
                        essround== 8 ~ "2016",
                        essround== 9 ~ "2018",
                        essround== 10 ~ "2020"),
    region= factor(case_when(cntry %in% c("Estonia", "Czech Republic", "Hungary",
                                          "Poland", "Slovenia") ~ "Eastern Europe",
                             TRUE ~ "Western Europe"), levels=c("Western Europe", "Eastern Europe"))) 



#### Weighted graphs 


#### Figure A.1: (Rising) LGB tolerance among European nativists (2002-2020) ####
nativists_pc <- ess_data |> 
  filter(poorctry_bin==0) |> 
  #filter(cntry %in% c("Belgium", "Hungary")) |> 
  drop_na(gay) |> 
  as_survey_design(1, weight = pspwght) |> 
  group_by(cntry, essround, gay) |> 
  summarise(Proportion = survey_mean()) |> 
  as_data_frame() |> 
  mutate(label = ifelse(gay == "Agree strongly",  scales::percent(round(Proportion, 3)), "")) |> 
  ggplot(aes(x = fct_rev(essround), y = Proportion, fill = gay))  + 
  geom_col()+
  theme_nhsr_demo() +
  facet_wrap(~cntry, nrow=3) +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5), labels = scales::percent_format())+
  labs(fill = "", y= "", x= "") +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values =  c( "#B2B2B2", "#DDDDDD","#F7D9D9", "#eeafaf", "#d11141")) +
  geom_text(position = position_fill(vjust = 0.5), aes(label = label), size=3,color= "white", fontface = "bold")+
  theme(panel.spacing = unit(6, "pt"),  
        text = element_text(face = "bold"),   # Set all text to bold
        strip.text = element_text(face = "bold"),   # Set facet titles to bold
        legend.text = element_text(face = "bold"),  # Set legend text to bold
        legend.title = element_text(face = "bold")  # Set legend title to bold
  )

ggsave("FigureA1.png", width = 15, height = 18, dpi=1000)



#### Figure A.2: Prevalence of sexually modern nativists across European states (2002-2020) ####
all_modernnativists <- ess_data |> 
  #filter(lrscale>5) |> 
  drop_na(gay_poorctry) |> 
  as_survey_design(1, weight = pspwght) |> 
  group_by(cntry, essround, gay_poorctry) |> 
  summarise(Proportion = survey_mean()) |> 
  as_data_frame() |> 
  mutate(label = ifelse(gay_poorctry == "Modern nativist",  scales::percent(round(Proportion, 3)), "")) |> 
  ungroup() |> 
  ggplot(aes(x = fct_rev(essround), y = Proportion, fill = gay_poorctry))  + 
  geom_col()+
  theme_nhsr_demo() +
  facet_wrap(~cntry, ncol=3) +
  scale_y_continuous(labels = scales::percent_format())+
  labs(fill = "", y= "", x= "") +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values =  c("#F7D9D9", "#d11141", "#B2B2B2", "#DDDDDD")) +
  geom_text(position = position_fill(vjust = 0.5), aes(label = label), size=3,color= "white", fontface = "bold")+
  theme(panel.spacing = unit(6, "pt"),  
        text = element_text(face = "bold"),   # Set all text to bold
        strip.text = element_text(face = "bold"),   # Set facet titles to bold
        legend.text = element_text(face = "bold"),  # Set legend text to bold
        legend.title = element_text(face = "bold"))  # Set legend title to bold

ggsave("FigureA2.png", width = 10, height = 13)





##FIGURE A3##

df_lgbt = read.csv("lgbtED.csv")

fullEU= c('UK', 'France', 'Germany', 'Italy', 'Spain',
          'Poland', 'Romania', 'Netherlands', 'Belgium',
          'Czech Republic', 'Greece', 'Portugal', 'Sweden',
          'Hungary', 'Austria', 'Switzerland', 'Cyprus',
          'Bulgaria', 'Denmark', 'Finland', 'Slovakia',
          'Ireland', 'Croatia',  'Slovenia', 'Latvia', 
          'Estonia', 'Luxembourg', 'Malta')
eu.maps <- map_data("world", region = fullEU)

merged_df = eu.maps %>% 
  left_join(df_lgbt, by = 'region')


region.gays <- merged_df %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat), lgbED=mean(lgbED),
            transED=mean(transED), diff=mean(diff))



plot1<- ggplot(merged_df, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group,  fill = lgbED))+ 
  scale_fill_gradient(low="#FFEBF7", high="#F58ABE")+  
  theme_void()+
  geom_label(data = region.gays,aes(label = round(lgbED, 1)),
             size = 3, hjust = 0.5, fontface="bold")+
  labs(title="Support (%) for LGB-inclusive education")+
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(color="black", hjust = 0.5, face=2))

plot2<- ggplot(merged_df, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group,  fill = transED))+ 
  scale_fill_gradient(low="#FFEBF7", high="#F58ABE")+  
  theme_void()+
  geom_label(data = region.gays,aes(label = round(transED, 1)),
             size = 3, hjust = 0.5, fontface="bold")+
  labs(title="Support (%) for trans-inclusive education")+
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(color="black", hjust = 0.5, face=2))

plot3<- ggplot(merged_df, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group,  fill = diff))+ 
  scale_fill_gradient(low="#FFEBF7", high="#F58ABE")+  
  theme_void()+
  geom_label(data = region.gays,aes(label = round(diff, 1)),
             size = 3, hjust = 0.5, fontface="bold")+
  labs(title="Assymetric (LGB v T) support")+
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(color="black", hjust = 0.5, face=2))

plot1+plot2+plot3+ 
  plot_annotation(
    caption = 'Source: Own elaboration based on\nEurobarometer #914 (2019) data') & 
  theme(plot.caption = element_text(color="#F58ABE", face="bold"))
ggsave("FigureA3.png", width=29, height=21, unit="cm", dpi=1000)



###FIGURE A8###

library(mgcv)
library(tidyverse)
library(InteractionPoweR)
library(tictoc)
library(pwr) 


tic()
test_power<- power_interaction(
  n.iter = 1000,            # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = seq(800,1200,by = 50),                   # sample size
  r.x1x2.y = seq(.1,.2,by=.01), # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = -.06,              # correlation between x1 and y
  r.x2.y = -.38,              # correlation between x2 and y
  r.x1.x2 = -.01)             # correlation between x1 and x2
#seed = 581827)             # seed, for reproducibility - this generally should not be set


toc()

test_power

p1<- plot_power_curve(power_data = test_power, # output from power_interaction()
                      power_target = .8,       # the power we want to achieve 
                      x = "N",                 # x-axis
                      group = "r.x1x2.y"       # grouping variable
)+
  labs(title="Power test using dichotomous moderator")
ggsave("FigureA8.png", dpi=600)
