##### this script is based on one created by Todd Bond - todd.bond@uwa.edu.au

detach("package:dplyr", unload=TRUE)#will error - no worries

remove.packages("dplyr")
install.packages("dplyr")

library(tidyverse)
options(dplyr.width = Inf) #enables head() to display all coloums
library(lubridate)
library(timechange)
library(gridExtra)
library(readxl)
library(galah)
library("RColorBrewer")





rm(list = ls())

data.dir=("C:/Users/elron/UWA/EXT-MUDSRC - Documents/PROJECTS/2023_Elrond Garcia/Data analysis/CTD data")
#data.dir=("C:/Users/Todd/OneDrive - The University of Western Australia/Projects/MUDSC/Cruises/2021_05_WZFZ+WZFZ+PC/Data")
setwd("C:/Users/elron/UWA/EXT-MUDSRC - Documents/PROJECTS/2023_Elrond Garcia/Data analysis/CTD data")


study = "TJ_LF"

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-1.5, 1.5))


#############################################################################


#########------TRIPLE-JUNCTION-9100M----##############
ctd.LF2 <- read.table("BosoTTT_CTD_bottom.csv", header = TRUE, sep = ",")

View(ctd.LF2)

ctd1.plot.depth.LF2 <- ggplot(data = filter(ctd.LF2,depth > 9000), aes(x = timeLocal, y = depth))+
  geom_point()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))+
  scale_y_reverse()


ctd1.plot.depth.LF2


sub.step.LF2 <- ctd.LF2%>%
  mutate(Step = 1:n(), Depth.change = depth - lag(depth, default = depth[1]))
sub.step.LF2 <- ctd.LF2%>%
  mutate(Step = 1:n(), Depth.change = depth - lag(depth, default = depth[1]))

sub.bottom.time.LF2 <- sub.step.LF2%>%
  filter(Depth.change < 0.15 &#### I worked through different numbers here and this worked best for me.
           depth >9000)%>%
  slice(1:200)%>%  ##### can change this number a bit to fit best. Think about increasing it ... you will see I have 3000 in one instance below.
  filter(depth == max(depth))%>%
  summarise(min(Step))

sub.bottom.LF2 = sub.bottom.time.LF2[1,1]

sub.step.LF2.1 <- sub.step.LF2%>%
  mutate(bottom.sec.sub = sub.bottom.LF2)%>%
  mutate(time.since.bottom = Step/60 - bottom.sec.sub/60)%>%
  #filter(time.since.bottom >-1)%>%
  filter(depth >2000)

sub.max.LF2 = sub.step.LF2.1%>%
  summarise(max(depth))%>%
  .[1,1]

ctd1.plot.depth.LF2 <- ggplot(sub.step.LF2.1, aes(x = Step,y = depth))+
  geom_point()+
  geom_line(linewidth=0.001)+
  scale_y_reverse(limits = c(sub.max.LF2+5, sub.max.LF2-150))+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))


ctd1.plot.depth.LF2

tsb.plot.LF2 <- ggplot(sub.step.LF2.1, aes(x = time.since.bottom,y = depth))+
  geom_point(aes(colour = Depth.change))+
  sc+
  scale_y_reverse(limits = c(sub.max.LF2+50, sub.max.LF2-250))+
  theme_bw()+
  geom_vline(xintercept = 0)+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))

tsb.plot.LF2


#########------IZU-BONIN-9800M----##############
ctd.LF2.IO9800 <- read.table("IOLF9800Depth.csv", header = TRUE, sep = ",")

View(ctd.LF2.IO9800)

ctd1.plot.depth.LF2.IO9800 <- ggplot(data = filter(ctd.LF2.IO9800,depth > 9000), aes(x = Time, y = depth))+
  geom_point()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))+
  scale_y_reverse()


ctd1.plot.depth.LF2.IO9800


sub.step.LF2.IO9800 <- ctd.LF2.IO9800%>%
  mutate(Step = 1:n(), Depth.change = depth - lag(depth, default = depth[1]))
sub.step.LF2.IO9800 <- ctd.LF2.IO9800%>%
  mutate(Step = 1:n(), Depth.change = depth - lag(depth, default = depth[1]))

sub.bottom.time.LF2.IO9800 <- sub.step.LF2.IO9800%>%
  filter(Depth.change < 0.15 &#### I worked through different numbers here and this worked best for me.
           depth >9970)%>%
  slice(1:200)%>%  ##### can change this number a bit to fit best. Think about increasing it ... you will see I have 3000 in one instance below.
  filter(depth == max(depth))%>%
  summarise(min(Step))

sub.bottom.LF2.IO9800 = sub.bottom.time.LF2.IO9800[1,1]

sub.step.LF2.1.IO9800 <- sub.step.LF2.IO9800%>%
  mutate(bottom.sec.sub = sub.bottom.LF2.IO9800)%>%
  mutate(time.since.bottom = Step/60 - bottom.sec.sub/60)%>%
  #filter(time.since.bottom >-1)%>%
  filter(depth >9700)

sub.max.LF2.IO9800 = sub.step.LF2.1.IO9800%>%
  summarise(max(depth))%>%
  .[1,1]

ctd1.plot.depth.LF2.IO9800 <- ggplot(sub.step.LF2.1.IO9800, aes(x = Step,y = depth))+
  geom_point()+
  geom_line(linewidth=0.001)+
  scale_y_reverse(limits = c(sub.max.LF2+900, sub.max.LF2+600))+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))


ctd1.plot.depth.LF2.IO9800

tsb.plot.LF2.IO9800 <- ggplot(sub.step.LF2.1.IO9800, aes(x = time.since.bottom,y = depth))+
  geom_point(aes(colour = Depth.change))+
  sc+
  scale_y_reverse(limits = c(sub.max.LF2+900, sub.max.LF2+600))+
  theme_bw()+
  geom_vline(xintercept = 0)+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_line(color = "black", linewidth = 0.5, linetype = 1),
        panel.background = element_rect(colour = "black", fill = NULL))

tsb.plot.LF2.IO9800


View(sub.step.LF2.1.IO9800)
View(sub.step.LF2.1)

par(mfrow=c(2,2))

tsb.plot.LF2
tsb.plot.LF2.IO9800