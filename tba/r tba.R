#temel bile?enler analizi kodlar?

library(readxl)
hamveri <- read_excel("C:/Users/tarik/Desktop/ödev/hamveri.xlsx")
View(hamveri)

library(GGally)
kalp_data<-hamveri[,3:5]
View(kalp_data)
ggpairs(kalp_data)

library("Hmisc")
rcorr(as.matrix(kalp_data),type="pearson")

library(psych)
KMO(kalp_data)

cortest.bartlett(cor(kalp_data),nrow(kalp_data))

fit.pca <- prcomp(kalp_data, scale=TRUE)
fit.pca$rotation
fit.pca$x
summary(fit.pca)
(fit.pca$sdev)^2

plot(fit.pca)
plot(fit.pca,type="line")

fit.pca$rotation[,1:2]

faktor_yukleri<-t(fit.pca$rotation)*fit.pca$sdev
faktor_yukleri

