install.packages("readxl", dependencies=TRUE)
library(readxl)
ls("package:readxl")
datarls <- read_excel("data_rls_uti.xlsx",sheet=1,na="")
View(datarls)

reg <- lm(Utilidad~Ventas,datarls)
summary(reg)
str(reg)

#Centrado de Datos
meanuti <- mean(datarls[,"Utilidad"])
meanuti
meanve <- mean(datarls[,"Ventas"])
meanve

utic <- (datarls[,"Utilidad"])-meanuti
utic
ventasc <- (datarls[,"Ventas"])-meanve
ventasc

reg1 <- lm(utic~ventasc,datarls)
summary(reg1)
str(reg1)

anova <- aov(reg1)
summary(anova)

#Intervalos de Confianza

confint(reg1,level=0.95)

names(reg1)
reg1["residuals"]
str(reg1["residuals"])
str(reg1[["residuals"]])

residuo<-reg1[["residuals"]]
prediccion <- reg1[["fitted.values"]]
data2 <- data.frame(utic,ventasc,residuo,prediccion)
View(data2)

hist(residuo,15)
mean(residuo)

#normalidad
qqnorm(residuo)
qqline(residuo,col="blue")

plot(utic,ventasc)
plot(residuo,prediccion)
plot(residuo,ventasc)












