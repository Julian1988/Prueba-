#Hacer este mismo codigo pero paso por paso sin librerias 
set.seed(2000)
f1<-gl(3,24,72,labels = c("G1","G2","G3"))#FACTOR FIJO
#Se tienen 36 altitudes.  De las culaes se seleccionara una
#muestra aleatoria de 3 altitudes
f2<-gl(3,8,72,labels = c("2547","2603","2640"))
altitud<-floor(runif(36,2100,2650));altitud
sample(altitud,3,replace = F)#Tomar una muestra
rto<-sort(rnorm(72,3.5,0.5))
df<-data.frame(genotipo=f1,ambiente=f2,rto);df
medias<-tapply(df$rto, list(df$genotipo,df$ambiente), mean)
medias
library(lattice)
xyplot(df$rto~df$genotipo|df$ambiente)
#Diseño Factorial Completo en Arreglo Completamente al Azar
#Modelo de efectos fijos
#Ho(genotipo)=ug1=ug2=ug3
#Ho(ambientes)=u2547=u2603=u2640, Medias de las 3 altiudes es la misma
#Ho(genotipo*ambiente)=No existe interaccion genotipo ambiente
#Analisis estadistico, Analisis de varianza para modelo de efectos fijos
model1<-aov(df$rto~df$genotipo+df$ambiente+df$genotipo*df$ambiente)
resumen1<-summary(model1);resumen1
tabla1<-unlist(resumen1)
ifelse(tabla1[19]<0.05,"Rechazo Ho", "No rechazo Ho")
par(mar = c (5, 2, 6, 6),mgp=c (3,1, 0),las=0)
interaction.plot(df$ambiente,df$genotipo,df$rto)
####################################
#Modelo de un solo efecto aleatorio
#Ho(genotipos)=var nulla de los genotipos
stripchart(rto ~ f1, vertical = TRUE,
           pch = 19, xlab = "Genotipo", data = df)
library(lme4)
modelo2 <- lmer(rto ~ (1 | f1), data = df);modelo2
summary(modelo2)
0.289+0.097
0.097/0.386*100
##################3
#Ho(ambiente)=var nulla de los ambeintes
stripchart(rto ~ f2, vertical = TRUE,
           pch = 19, xlab = "Ambiente", data = df)
library(lme4)
modelo3 <- lmer(rto ~ (1 | f2), data = df);modelo2
summary(modelo3)
0.07+0.25
0.07/0.32*100
########################
#Modelo factorial completo de efectos aleatorios
modelo4 <- lmer(rto ~ (1 | f1) 
                + (1 | f2) + (1 | f1:f2), data = df)
summary(modelo4)
0.02430+0.07132+0.2837+0.03067
#Varianza total=0.41
#Variazna interaccion 
0.02430/0.41*100
#variazna ambeinte
0.07132/0.41*100
#variazna genotipo 
0.2837/0.41*100
#varianza residual
0.03067/0.41*100
#Variabilidad aprox 4 veces mayor por genotipo que por mabiente
69.2/17.4=4
