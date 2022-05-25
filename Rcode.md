``` r
### 2: Import packages ###
library(groundhog)

groundhog.library("openxlsx",'2022-05-24')  # load the version of the package that was available on the publication date of Hiki & Iwasaki (2020)
groundhog.library("tidyverse",'2022-05-24')
groundhog.library("ggplot2",'2022-05-24')
groundhog.library("ggExtra",'2022-05-24')
groundhog.library("scales",'2022-05-24')
groundhog.library("EnvStats",'2022-05-24')


### 2: Read the microplastic ingestion data ###
Beads_data <-  read.xlsx("Data_for_R_analysis_revised.xlsx",sheet="food") 


#### 3: Figure 3 ####
gline_left <- linesGrob(x = c(-.015, .015),  gp = gpar(lwd = 4.1)) 
gline_right <- linesGrob(x = c(.985, 1.015),  gp = gpar(lwd = 4.1)) 


Fig3_4.1 <- Beads_data %>%
  pivot_longer(cols=3:4,names_to=c("Beads","Size"),names_pattern="(.*)_(.*)",values_to="Count") %>%
  filter(Size=="4.1") %>%
  mutate(Sediment = ifelse(Sediment=="Yes","With sediment \n (Filter-feeding mode)","Without sediment \n (Filter- & deposit-feeding mode)")) %>%
  mutate(Size = ifelse(Size=="4.1","4.1 μm","20.6 μm")) %>%
  mutate(Size = fct_relevel(Size,"4.1 μm","20.6 μm")) %>%
  mutate_at(vars(TetraMin),as.factor) %>%
  ggplot( aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment) )+
   facet_wrap(~Size,scale="free_y")+
   geom_quasirandom(size=4,alpha=0.9,stroke=1,dodge=0.6,aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment))+
   stat_summary(fun = "median", size = 0.5,width=0.7,position=position_dodge(width=0.6),
                geom = "crossbar") +
   stat_boxplot(geom = "errorbar",size = 1,width=0.5,position=position_dodge(width=0.6),
                aes(ymin = ..lower.., ymax = ..upper..)) +
   theme_prism(border=TRUE,base_size=27)+
   theme(legend.position = "none", strip.text = element_text(hjust = 0.05, size=27),
         axis.title.x=element_blank(),
         plot.margin = unit(c(0,1,0,1), "lines"),
         panel.border = element_rect(fill=NA, colour = "black",size=3.7))+
   scale_color_manual(values=c("#E69F00", "#56B4E9"))+
   scale_shape_manual(values=c(21,24))+
   labs(y="Number of beads (/amphipod)",x="TetraMin (mg/amphipod)")+
   geom_text(x=2,y=5600,label="*",size=13)

Fig3_20.6_200 <- Beads_data %>%
  pivot_longer(cols=3:4,names_to=c("Beads","Size"),names_pattern="(.*)_(.*)",values_to="Count") %>%
  filter(Size=="20.6" & Count <=150 ) %>%
  mutate(Sediment = ifelse(Sediment=="Yes","With sediment","Without sediment")) %>%
  mutate(Size = ifelse(Size=="20.6","20.6 μm",NA)) %>%
  mutate(Size = fct_relevel(Size,"20.6 μm")) %>%
  mutate_at(vars(TetraMin),as.factor) %>%
  ggplot( aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment) )+
   facet_wrap(~Size,scale="free_y")+
   geom_quasirandom(size=4,alpha=0.9,stroke=1,dodge=0.6,aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment))+
   stat_summary(fun = "median", size = 0.5,width=0.7,position=position_dodge(width=0.6),
                geom = "crossbar") +
   stat_boxplot(geom = "errorbar",size = 1,width=0.5,position=position_dodge(width=0.6),
                aes(ymin = ..lower.., ymax = ..upper..)) +
   theme_prism(border=FALSE,base_size=27)+
   theme(legend.position = "none", strip.text = element_blank(), axis.title=element_blank(),
                  plot.margin = unit(c(-0.1,1,0,1), "lines") )+
   guides(y = "prism_offset_minor")+
   coord_cartesian(clip = "off")+
   scale_color_manual(values=c("#E69F00", "#56B4E9"))+
   scale_shape_manual(values=c(21,24))+
   scale_y_continuous(limits = c(0, 150),breaks=seq(0,150,50),labels=c("0","50","100","150"))+
   labs(y="Number of beads (/amphipod)",x="TetraMin (mg/amphipod)")+
   annotate(geom = 'segment', x= Inf, xend = Inf, y = -Inf, yend = 150, size=2)+
   annotate(geom = 'segment', x= -Inf, xend =-Inf, y = -Inf, yend = 150,size=2)+
   annotation_custom(gline_left,xmin=-Inf,xmax=Inf,ymin=150,ymax=150)+
   annotation_custom(gline_right,xmin=-Inf,xmax=Inf,ymin=150,ymax=150)

Fig3_20.6_400 <- Beads_data %>%
  pivot_longer(cols=3:4,names_to=c("Beads","Size"),names_pattern="(.*)_(.*)",values_to="Count") %>%
  filter(Size=="20.6"& Count >=150 ) %>%
  mutate(Sediment = ifelse(Sediment=="Yes","With sediment","Without sediment")) %>%
  mutate(Size = ifelse(Size=="20.6","20.6 μm",NA)) %>%
  mutate(Size = fct_relevel(Size,"20.6 μm")) %>%
  mutate_at(vars(TetraMin),as.factor) %>%
  ggplot( aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment) )+
   facet_wrap(~Size,scale="free_y")+
   geom_quasirandom(size=4,alpha=0.9,stroke=1,dodge=0.6,aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment))+
   theme_prism(border=FALSE,base_size=27)+
   theme(legend.position = "none",  strip.text = element_text(hjust = 0.05, size=27),
         axis.title.y=element_blank(), axis.title = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.line.x = element_blank(),
         plot.margin = unit(c(0,1,-0.05,1), "lines"))+
   guides(y = "prism_offset_minor")+
   coord_cartesian(clip = "off")+
   scale_color_manual(values=c( "#56B4E9"))+
   scale_shape_manual(values=c(24))+
   scale_y_continuous(limits = c(400, 450),breaks=c(400,450),labels=c("400","450"))+
   annotate(geom = 'segment', x= Inf, xend = Inf, y = 400, yend = 450, size=2)+
   annotate(geom = 'segment', x= -Inf, xend =-Inf, y = 400, yend = 450,size=2)+
   annotate(geom = 'segment', x= -Inf, xend =Inf, y = 450, yend = 450,size=2)+
   annotation_custom(gline_left,xmin=-Inf,xmax=Inf,ymin=400,ymax=400)+
   annotation_custom(gline_right,xmin=-Inf,xmax=Inf,ymin=400,ymax=400)

Fig3_20.6 <- plot_grid(Fig3_20.6_400 , Fig3_20.6_200, ncol=1, rel_heights = c(.18, .42),align="v")
Fig3 <-  plot_grid(Fig3_4.1 , Fig3_20.6,rel_widths=c(.125,.1))

Fig3_legend <- Beads_data %>%
  pivot_longer(cols=3:4,names_to=c("Beads","Size"),names_pattern="(.*)_(.*)",values_to="Count") %>%
  mutate(Sediment = ifelse(Sediment=="Yes","With sediment \n (Filter-feeding mode)","Without sediment \n (Filter- & deposit-feeding modes)")) %>%
  mutate_at(vars(TetraMin,Run),as.factor) %>%
  ggplot( aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment) )+
   facet_wrap(~Size,scale="free_y")+
   geom_quasirandom(size=6,alpha=0.7,stroke=1.2,dodge=0.6,aes(x=TetraMin,y=Count,color=Sediment,shape=Sediment))+
   theme_prism(border=TRUE,base_size=25)+
   theme(legend.key.size = unit(1.5, "cm"),legend.text = element_text(margin = margin(t = 11)))+
   scale_color_manual(values=c("#E69F00", "#56B4E9"))+
   scale_shape_manual(values=c(21,24))
 
legend <- get_legend(
  Fig3_legend + theme(legend.box.margin = margin(0, 0, 2, 4))
)
x.grob <- textGrob("TetraMin (mg/amphipod)", gp=gpar(fontface="bold",fontsize=25))
Fig3_combined <- grid.arrange(arrangeGrob(Fig3, bottom = x.grob ))

Fig3 <- plot_grid(Fig3_combined,  legend, rel_widths = c(1.6, 0.95),ncol=2)

Fig3
```

![](figs/fig-3.png)

``` r
#### 4: Figure 4 ####
Fig4 <- Beads_data %>%
  mutate(Sediment = ifelse(Sediment=="Yes","With sediment","Without sediment")) %>%
  mutate(TetraMin = ifelse(TetraMin=="0","0 mg",ifelse(TetraMin=="5","5 mg","20 mg") )) %>%
  mutate_at(vars(TetraMin),as.factor) %>%
  mutate(TetraMin = fct_relevel(TetraMin,"0 mg","5 mg") ) %>%
  ggplot(aes(x=Beads_20.6,y=Beads_4.1,color=Sediment,shape=TetraMin))+
   geom_point(size=3.5,stroke=1.2)+
   theme_prism(border=TRUE,base_size=27)+
   theme( )+
   labs(y="Number of 4.1 μm beads",x="Number of 20.6 μm beads",shape="TetraMin")+
   guides(shape=guide_legend(title="TetraMin")) +
   coord_cartesian(clip = "off")+
   scale_color_manual(values=c("#E69F00", "#56B4E9"))+
   scale_shape_manual(name="TetraMin",values=c(3,24,25)) +
   geom_segment(aes(x=0,y=0,yend=12000,xend=12000*(4.1^3/20.6^3) ),lty="dotted",size=1.25,color="black")+
   geom_segment(aes(x=0,y=0,yend=2250,xend=2250*(25.24*4.1^3/20.6^3) ),size=1.25,color="black")+
   annotate (geom="text",y=2600,x=340,label="italic(y) == 5.0 *italic(x)",
             size=6.5, parse=TRUE )+
   annotate (geom="text",y=9600,x=185,label="italic(y) == 130 *italic(x)",
             size=6.5, parse=TRUE )+
   annotate(geom="text",y=4400, x=340, label="Theoretical ratio \n on bottom",size=6.5)+
   annotate(geom="text",y=11000, x=185, label="Theoretical ratio \n in water",size=6.5)

Fig4
```
![](figs/fig-4.png)
