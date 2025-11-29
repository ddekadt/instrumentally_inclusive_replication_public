

###MULTIVERSE ANALYSIS## This script will should be run subsequently from 'study1.R' and 'study2.R'

##UK MULTIVERSE##

###FIGURE A10###
library(starbility)
LOW <- subset(df, imm3==0)
MID<- subset(df, imm3==1)
HIGH <- subset(df, imm3==2)

perm_controls = c(
  'Gender' = 'gender',
  'Age' = 'agecat',
  'LGBT+' = 'queer',
  'Non-white' = 'nonwhite',
  'Religion' = 'religion'
)

perm_fe_controls = c(
  'Regional FE' = 'region'
)

p1<- stability_plot(data = df, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    coef_ylim = c(-.2,.2), # change the endpoints of the y-axis
                    control_geom = 'circle',
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel = p1[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Full sample")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel = p1[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p1X<- combine_plots(replacement_coef_panel, 
                    replacement_control_panel, 
                    rel_height = 0.6)



p2<- stability_plot(data = noproimm, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    coef_ylim = c(-.2,.2), # change the endpoints of the y-axis
                    control_geom = 'circle',
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel2 = p2[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Below average immigration views")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel2 = p2[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p2X<- combine_plots(replacement_coef_panel2, 
                    replacement_control_panel2, 
                    rel_height = 0.6)



p3<- stability_plot(data = proimm, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    coef_ylim = c(-.2,.2), # change the endpoints of the y-axis
                    control_geom = 'circle',
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel3 = p3[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Above average immigration views")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel3 = p3[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p3X<- combine_plots(replacement_coef_panel3, 
                    replacement_control_panel3, 
                    rel_height = 0.6)


p1X+p2X+p3X+ 
  plot_annotation(title = 'Multiverse analysis (UK). Full sample, below- and above-average immigration views',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))

ggsave("FigureA10.png", dpi=600, height=32, width=58, unit="cm")


###FIGURE A11###

p4<- stability_plot(data = LOW, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    coef_ylim = c(-.2,.3), # change the endpoints of the y-axis
                    control_geom = 'circle',
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel4 = p4[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Low immigration views (0-3)")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel4 = p4[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p4X<- combine_plots(replacement_coef_panel4, 
                    replacement_control_panel4, 
                    rel_height = 0.6)



p5<- stability_plot(data = MID, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    coef_ylim = c(-.2,.3), # change the endpoints of the y-axis
                    control_geom = 'circle',
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel5 = p5[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Mid immigration views (4-6)")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel5 = p5[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p5X<- combine_plots(replacement_coef_panel5, 
                    replacement_control_panel2, 
                    rel_height = 0.6)



p6<- stability_plot(data = HIGH, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    coef_ylim = c(-.2,.3), # change the endpoints of the y-axis
                    control_geom = 'circle',
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel6 = p6[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="High immigration views (7-10)")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel6 = p6[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p6X<- combine_plots(replacement_coef_panel6, 
                    replacement_control_panel6, 
                    rel_height = 0.6)


p4X+p5X+p6X+ 
  plot_annotation(title = 'Multiverse analysis (UK). Samples by low, mid, high immigration views',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))

ggsave("FigureA11.png", dpi=600, height=32, width=58, unit="cm")




##SPAIN MULTIVERSE##



###FIGURE A12###

library(starbility)

proimmES <- subset(spain, immdum==1)
noproimmES <- subset(spain, immdum==0)
LOWES <- subset(spain, imm3==0)
MIDES <- subset(spain, imm3==1)
HIGHES <- subset(spain, imm3==2)

perm_controls = c(
  'Gender' = 'gender',
  'Age' = 'agecat',
  'LGBT+' = 'queer',
  'Foreign-born' = 'foreignborn',
  'Has children' = 'child'
)

perm_fe_controls = c(
  'Comunidad Autónoma FE' = 'CCAA'
)

p1<- stability_plot(data = spain, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    weights = 'nationalweight',
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    control_geom = 'circle',
                    coef_ylim = c(0, .6), # change the endpoints of the y-axis
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel = p1[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Full sample")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel = p1[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p1X<- combine_plots(replacement_coef_panel, 
                    replacement_control_panel, 
                    rel_height = 0.6)



p2<- stability_plot(data = noproimmES, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    weights = 'nationalweight',
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    control_geom = 'circle',
                    coef_ylim = c(0, .6), # change the endpoints of the y-axis
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)


replacement_coef_panel2 = p2[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Below average immigration views")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel2 = p2[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p2X<- combine_plots(replacement_coef_panel2, 
                    replacement_control_panel2, 
                    rel_height = 0.6)

p3<- stability_plot(data = proimmES, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    weights = 'nationalweight',
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    control_geom = 'circle',
                    coef_ylim = c(0, .6), # change the endpoints of the y-axis
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel3 = p3[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Above average immigration views")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel3 = p3[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p3X<- combine_plots(replacement_coef_panel3, 
                    replacement_control_panel3, 
                    rel_height = 0.6)


p1X+p2X+p3X+ 
  plot_annotation(title = 'Multiverse analysis (Spain). Full sample, below- and above-average immigration views',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))

ggsave("FigureA12.png", dpi=600, height=32, width=58, unit="cm")


###FIGURE A13###

p4<- stability_plot(data = LOWES, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    weights = 'nationalweight',
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    control_geom = 'circle',
                    coef_ylim = c(-.2, .6), # change the endpoints of the y-axis
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel4 = p4[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Low immigration views (0-3)")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel4 = p4[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p4X<- combine_plots(replacement_coef_panel4, 
                    replacement_control_panel4, 
                    rel_height = 0.6)



p5<- stability_plot(data = MIDES, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    weights = 'nationalweight',
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    control_geom = 'circle',
                    coef_ylim = c(-.2, .6), # change the endpoints of the y-axis
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)


replacement_coef_panel5 = p5[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="Mid immigration views (4-6)")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel5 = p5[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p5X<- combine_plots(replacement_coef_panel5, 
                    replacement_control_panel5, 
                    rel_height = 0.6)

p6<- stability_plot(data = HIGHES, 
                    lhs = 'support', 
                    rhs = 'treatnum', 
                    weights = 'nationalweight',
                    perm = perm_controls,
                    perm_fe = perm_fe_controls,
                    sort = 'asc-by-fe',
                    control_geom = 'circle',
                    coef_ylim = c(-.2, .6), # change the endpoints of the y-axis
                    rel_height = 0.6,
                    run_to = 6,
                    point_size = 2)

replacement_coef_panel6 = p6[[1]] + 
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, linetype = 'dashed',color="#673B90")+
  labs(title="High immigration views (7-10)")+
  theme(plot.title = element_text(face="bold"))

replacement_control_panel6 = p6[[2]] + 
  scale_fill_manual(values = c("#FFFFFF", "#673B90"))

p6X<- combine_plots(replacement_coef_panel6, 
                    replacement_control_panel6, 
                    rel_height = 0.6)


p4X+p5X+p6X+ 
  plot_annotation(title = 'Multiverse analysis (Spain). Samples by low, mid, high immigration views',
                  theme = theme(plot.title = element_text(size = 14, face="bold")))

ggsave("FigureA13.png", dpi=600, height=32, width=58, unit="cm")



