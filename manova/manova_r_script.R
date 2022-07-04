library(readxl)
hamveri <- read_excel("C:/Users/tarik/Desktop/çda ödev/hamveri.xlsx")
View(hamveri)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(haven)

veri<-hamveri
veri<-as.data.frame(veri)

summary(veri)
veri$slope<-factor(veri$slope,levels = c(0,1,2),labels=c("yukarı eğimli","eğimsiz","aşağı eğimli"))
veri$kagt<-factor(veri$kagt,levels = c(0,1,2,3),labels=c("tipik angina","atipik angina","anginal olmayan","asemptomatik angina"))
summary(veri)

library(dplyr)
veri%>%
  dplyr::select(yas,drgn_kbsnc) %>%
  mshapiro_test()

library(biotools)
box_m(veri[, c("yas","drgn_kbsnc")], veri$slope)



veri %>%
  group_by(slope) %>%
  dplyr::summarise(N = n())

veri %>% 
  group_by(slope) %>%
  summarise(across(-kagt, list(mean=mean,sd=sd)))


library(gplots)
plotmeans(yas~slope,xlab="slope",ylab="yas", main="Mean Plot\nwith 95% CI",data=veri)
plotmeans(drgn_kbsnc~slope, xlab="slope",ylab="kan basıncı", main="Mean Plot\nwith 95% CI",data=veri)

plotmeans(yas~kagt,xlab="slope",ylab="yas", main="Mean Plot\nwith 95% CI",data=veri)
plotmeans(drgn_kbsnc~kagt, xlab="slope",ylab="kan basıncı", main="Mean Plot\nwith 95% CI",data=veri)

#manova
slope_kagt_manova <- manova(cbind(yas,drgn_kbsnc) ~ slope,data=veri)
summary(slope_kagt_manova, test = "Hotelling-Lawley")
summary(slope_kagt_manova, test = "Wilks")
summary(slope_kagt_manova, test = "Pillai")
summary(slope_kagt_manova, test = "Roy")

veri %>% 
  pivot_longer( c(yas,drgn_kbsnc),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ slope,center=mean)

summary.aov(slope_kagt_manova)

#çift yönlü
library(heplots)
boxM( cbind(yas,drgn_kbsnc) ~ slope*kagt, data=veri)

slope_kagt_cift <- manova(cbind(yas,drgn_kbsnc) ~ slope*kagt,data=veri)

library(car)
veri %>% 
  pivot_longer( c(yas,drgn_kbsnc),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ slope*kagt,center=mean)

summary.aov(slope_kagt_cift)

m_tukey22 <- veri %>%
  pivot_longer( c(yas,drgn_kbsnc),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ slope*kagt)
m_tukey22<-m_tukey22[,c(1,2,4,3,5:9)]
kagt_etk<-filter(m_tukey22, term=="kagt")
kagt_etk

attach(veri)
interaction.plot(kagt,slope,drgn_kbsnc, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
interaction.plot(kagt,slope,yas, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
detach(veri)

