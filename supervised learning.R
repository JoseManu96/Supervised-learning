
######################## EJERCICIO 1:###############################
# Importamos las librerias.
library(MASS)
library(e1071)

# Cargamos la base de datos:
bd=rbind(Pima.tr,Pima.te)
 
#Veamos la estructura del dataset:
str(bd)
 
####### Análisis del discriminante lineal#######################################

# Para el análisis del discriminate lineal comenzamos revisando los 
#niveles de la variable categórica Type:
levels(bd$type)
 
# Luego graficamos las relaciones entre las variables factor:
pairs(bd[1:7],col = rainbow(2)[bd$type])
 
# Ahora realizamos varios modelos y comprobaremos 
#su exactitud a la hora de clasificar:

#### MODELO 1 #####

# Primero hagamos el modelo con todas las variables:
lda1=lda(bd$type~.,data = bd)
lda1  
  
# Veamos la estructura LDA para el modelo anterior:
str(lda1)  
  
# Lo siguiente es ver las probabilidades a priori empleadas 
#y la matriz de escalado:  
lda1$prior
lda1$scaling

# Veamos que tal las predicciones de nuestro modelo:
round(cbind(Type=bd$type,Label=predict(lda1)$class,predict(lda1)$posterior,
            predict(lda1)$x)[1:10,],3)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
t1=table(bd$type, predict(lda1)$class)
(t1=table(bd$type, predict(lda1)$class))

# Graficamos:
plot(lda1)

# Calculamos el error aparente:
er_apar=function(t1){
    e1=matrix(NA,ncol=3, nrow=1)
    colnames(e1)=c("NO","Yes","Global")
    rownames(e1)=c("Aparentes")
    (e1[1,1]=t1[1,2]/(t1[1,1]+t1[1,2]))
    (e1[1,2]=t1[2,1]/(t1[2,1]+t1[2,2]))
    (e1[1,3]=(t1[1,2]+t1[2,1])/sum(t1))
    return(e1)
}
e1=er_apar(t1)
e1

# Calculemos ahora el error de clasificación por K-fold cross validation:
lda1_loo=lda(bd$type ~ ., CV=T,data = bd) 

# La tabla de contingencia nos queda de la siguiente forma:
t2=table(bd$type, lda1_loo$class)
(t2=table(bd$type, lda1_loo$class))

# Si calculamos los errores de prediccion llegaremos a que estos serán 
#los siguientes:  
er_cv=function(t2){
    e2=matrix(NA,ncol=3, nrow=1)
    colnames(e2)=c("NO","Yes","Global")
    rownames(e2)=c("CV-LOO")
    e2[1,1]=t2[1,2]/(t2[1,1]+t2[1,2])
    e2[1,2]=t2[2,1]/(t2[2,1]+t2[2,2])
    e2[1,3]=(t2[1,2]+t2[2,1])/sum(t2)
    return(e2)
}
e2=er_cv(t2)
e2

# Fijamos nuestra semilla:
set.seed(1859)

# Definimos la matriz de los errores y hacemos un ciclo que calcula los errores:

mtxer=function(Nrep){
  materr=matrix(NA,ncol=8, nrow=Nrep)
  colnames(materr)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global","aq11","aq21")
  for(irep in 1:Nrep){
    train=createDataPartition(bd$type, p=0.7)$Resample1
    lda1_Train=lda(type ~., data=bd[train,])
    t3=table(bd[train,]$type, predict(lda1_Train, bd[train,])$class)
    materr[irep,1]=t3[1,2]/(t3[1,1]+t3[1,2]) 
    materr[irep,2]=t3[2,1]/(t3[2,1]+t3[2,2])
    materr[irep,3]=(t3[1,2]+t3[2,1])/sum(t3)
    materr[irep,7]=mean(predict(lda1_Train, bd[train,])$class==bd[train,]$type)
    t3=table(bd[-train,]$type, predict(lda1_Train, bd[-train,])$class)
    materr[irep,4]=t3[1,2]/(t3[1,1]+t3[1,2]) 
    materr[irep,5]=t3[2,1]/(t3[2,1]+t3[2,2])
    materr[irep,6]=(t3[1,2]+t3[2,1])/sum(t3)
    materr[irep,8]=mean(predict(lda1_Train, bd[-train,])$class==bd[-train,]$type)
  }
  return(materr)
}

# Calculamos nuestros errores obtenidos anteriormente:  
er_val1=function(materr){
    e3=matrix(NA,ncol=3, nrow=1)
    colnames(e3)=c("NO","Yes","Global")
    rownames(e3)=c("Train")
    aux=colMeans(materr)
    e3[1,1]=aux[1]
    e3[1,2]=aux[2]
    e3[1,3]=aux[3]
    return(e3)
}

er_val2=function(materr){
    e4=matrix(NA,ncol=3, nrow=1)
    colnames(e4)=c("NO","Yes","Global")
    rownames(e4)=c("Test")
    aux=colMeans(materr)
    e4[1,1]=aux[4]
    e4[1,2]=aux[5]
    e4[1,3]=aux[6]
    return(e4)
}

# Experimentación:
l=c(100,500,1000,2000,3000,4000,5000,6000,10000,15000)
glob=c()
for (i in l) {
  glob=c(glob,er_val2(mtxer(i))[3])
}
# Se obtuvo el siguiente vector: c(0.2231,0.2213,0.2204,0.2212,0.2218,0.2213, 0.2211,0.2207,0.2207,0.2209)
plot(l,c(0.2231,0.2213,0.2204,0.2212,0.2218,0.2213,0.2211,0.2207,0.2207,0.2209),xlab = "",ylab = "",pch=16,col=c('blue','blue','blue','blue','blue','red','blue','blue','blue','blue','blue'))
materr=mtxer(1000)
aq11=materr[,7]
aq21=materr[,8]

# Veamos los errores globales en un histograma:
hist(materr[,3], breaks = 100,main="global training errors")
hist(materr[,6], breaks = 100, main="global test errors")

e3=er_val1(materr)
e4=er_val2(materr)
e3
e4

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e1[1:3],e2[1:3],e3[1:3],e4[1:3]), pch=15, cex=1,
        col=c("black", "blue","orange","red"),
        type="b",ylab="%Errores", xlab=c("Errores"),lty = 1)
legend("topleft", legend=c("Aparente", "CV-LOO","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","orange","red"))

# Como se puede ver el error aparente para el segundo grupo ("YES") 
#es bastante alto, por tanto esto nos hace pensar que este modelo con 
#todas las variables no necesariamente debe ser el mejor. Lo siguien 
#entonces sería crear otros modelos:

############  MODELO 2: #############

# Probemos con el modelo agregar todas las interacciones entre las variables 
#en el modelo:
lda2 <- lda(type ~ .^2, data = bd)
lda2  
  
# Veamos la estructura LDA para el modelo anterior:
str(lda2)
  
# Comprobamos las probabilidades a priori y la matriz de escalado para este 
#modelo:
lda2$prior
lda2$scaling

# Veamos que tal las predicciones de nuestro modelo:
round(cbind(Type=bd$type,Label=predict(lda2)$class,predict(lda2)$posterior,
            predict(lda2)$x)[1:10,],3)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(t11=table(bd$type, predict(lda2)$class))

# Calculamos el error aparente:
e11=er_apar(t11)
e11

# Calculemos ahora el error de clasificación por K-fold cross validation:
lda2_loo=lda(bd$type ~ .^2, CV=T,data = bd) 

# La tabla de contingencia nos queda de la siguiente forma:
(t21=table(bd$type, lda2_loo$class))

# Si calculamos los errores de prediccion llegaremos a que estos serán 
#los siguientes:  
e21=er_cv(t21)
e21

# Por ultimo calculamos el error de predicción a partir del 
#acercamiento por conjunto de validación. 
#Para ello fijamos nuestra semilla y el número de repeticiones 
#(En vistas a estabilizar el error):
set.seed(1859)
Nrep=4000

# Definimos la matriz de los erroresy hacemos un ciclo que calcula los errores:
materr1=matrix(NA,ncol=6, nrow=Nrep)
colnames(materr1)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
aq12=c()
aq22=c()
for(irep in 1:Nrep){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  lda1_Train=lda(type~.^2, data=bd[train,])
  t3=table(bd[train,]$type, predict(lda1_Train, bd[train,])$class)
  materr1[irep,1]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr1[irep,2]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr1[irep,3]=(t3[1,2]+t3[2,1])/sum(t3)
  aq12=c(aq12,mean(predict(lda1_Train, bd[train,])$class==bd[train,]$type))
  t3=table(bd[-train,]$type, predict(lda1_Train, bd[-train,])$class)
  materr1[irep,4]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr1[irep,5]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr1[irep,6]=(t3[1,2]+t3[2,1])/sum(t3)
  aq22=c(aq22,mean(predict(lda1_Train, bd[-train,])$class==bd[-train,]$type))
}

# Veamos los errores en un histograma:
hist(materr1[,3], breaks = 100,main="global training errors")
hist(materr1[,6], breaks = 100, main="global test errors")

# Calculamos nuestros errores obtenidos anteriormente:  
e31=er_val1(materr1)
e41=er_val2(materr1)
e31
e41

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e11[1:3],e21[1:3],e31[1:3],e41[1:3]), pch=15, cex=1,
        col=c("black", "blue","orange","red"),
        type="b",ylab="Error", xlab=c("Clases:N0;   Yes;   Global"),lty = 1)
legend("topleft", legend=c("Aparente", "CV-LOO","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","orange","red"))


########## MODELO 3: ###########

# Calculamos las correlaciones:
cor(bd[1:6])

# Basado en los resultados anteriores agregamos las siguientes interacciones:
lda3=lda(type ~ bmi+glu+npreg+ped, data = bd)
lda3

# Veamos la estructura LDA para el modelo anterior:
str(lda3)

# Comprobamos las probabilidades a priori y la matriz de escalado para este 
#modelo:
lda3$prior
lda3$scaling

# Veamos que tal las predicciones de nuestro modelo:
round(cbind(Type=bd$type,Label=predict(lda3)$class,predict(lda3)$posterior,
            predict(lda3)$x)[1:10,],3)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(t12=table(bd$type, predict(lda3)$class))

# Calculamos el error aparente:
e12=er_apar(t12)
e12

# Calculemos ahora el error de clasificación por K-fold cross validation:
lda3_loo=lda(bd$type ~  bmi+glu+npreg+ped, CV=T,data = bd) 

# La tabla de contingencia nos queda de la siguiente forma:
(t22=table(bd$type, lda3_loo$class))

# Si calculamos los errores de prediccion llegaremos a que estos serán 
#los siguientes:  
e22=er_cv(t22)
e22

# Por ultimo calculamos el error de predicción a partir del 
#acercamiento por conjunto de validación. 
#Para ello fijamos nuestra semilla y el número de repeticiones 
#(En vistas a estabilizar el error):
set.seed(1859)
Nrep=4000

# Definimos la matriz de los erroresy hacemos un ciclo que calcula los errores:
materr2=matrix(NA,ncol=6, nrow=Nrep)
colnames(materr2)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
aq13=c()
aq23=c()
for(irep in 1:Nrep){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  lda1_Train=lda(type~ bmi+glu+npreg+ped, data=bd[train,])
  t3=table(bd[train,]$type, predict(lda1_Train, bd[train,])$class)
  materr2[irep,1]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr2[irep,2]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr2[irep,3]=(t3[1,2]+t3[2,1])/sum(t3)
  aq13=c(aq13,mean(predict(lda1_Train, bd[train,])$class==bd[train,]$type))
  t3=table(bd[-train,]$type, predict(lda1_Train, bd[-train,])$class)
  materr2[irep,4]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr2[irep,5]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr2[irep,6]=(t3[1,2]+t3[2,1])/sum(t3)
  aq23=c(aq23,mean(predict(lda1_Train, bd[-train,])$class==bd[-train,]$type))
}

# Veamos los errores en un histograma:
hist(materr2[,3], breaks = 100,main="global training errors")
hist(materr2[,6], breaks = 100, main="global test errors")

# Calculamos nuestros errores obtenidos anteriormente:  
e32=er_val1(materr2)
e42=er_val2(materr2)
e32
e42

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e12[1:3],e32[1:3],e42[1:3]), pch=15, cex=1,
        col=c("black", "blue","orange","red"),
        type="b",ylab="Error", xlab=c("Clases: No;   Yes;  Global"),lty = 1)
legend("topleft", legend=c("Aparente","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","orange","red"))


######### MODELO 4: ###########

# Basado en los resultados anteriores agregamos las siguientes interacciones:
lda4=lda(type ~ bmi+glu+npreg+ped+bmi:glu+npreg:ped, data = bd)
lda4

# Veamos la estructura LDA para el modelo anterior:
str(lda4)

# Comprobamos las probabilidades a priori y la matriz de escalado para este 
#modelo:
lda4$prior
lda4$scaling

# Veamos que tal las predicciones de nuestro modelo:
round(cbind(Type=bd$type,Label=predict(lda4)$class,predict(lda4)$posterior,
            predict(lda4)$x)[1:10,],3)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(t13=table(bd$type, predict(lda4)$class))

# Calculamos el error aparente:
e13=er_apar(t13)
e13

# Calculemos ahora el error de clasificación por K-fold cross validation:
lda4_loo=lda(bd$type ~ bmi+glu+npreg+ped+bmi:glu+npreg:ped, CV=T,data = bd) 

# La tabla de contingencia nos queda de la siguiente forma:
(t23=table(bd$type, lda4_loo$class))

# Si calculamos los errores de prediccion llegaremos a que estos serán 
#los siguientes:  
e23=er_cv(t23)
e23

# Por ultimo calculamos el error de predicción a partir del 
#acercamiento por conjunto de validación. 
#Para ello fijamos nuestra semilla y el número de repeticiones 
#(En vistas a estabilizar el error):
set.seed(1859)
Nrep=4000

# Definimos la matriz de los erroresy hacemos un ciclo que calcula los errores:
materr3=matrix(NA,ncol=6, nrow=Nrep)
colnames(materr3)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
aq14=c()
aq24=c()
for(irep in 1:Nrep){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  lda1_Train=lda(type~ bmi+skin+glu+glu:skin+npreg*bp+ped, data=bd[train,])
  t3=table(bd[train,]$type, predict(lda1_Train, bd[train,])$class)
  materr3[irep,1]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr3[irep,2]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr3[irep,3]=(t3[1,2]+t3[2,1])/sum(t3)
  aq14=c(aq14,mean(predict(lda1_Train, bd[train,])$class==bd[train,]$type))
  t3=table(bd[-train,]$type, predict(lda1_Train, bd[-train,])$class)
  materr3[irep,4]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr3[irep,5]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr3[irep,6]=(t3[1,2]+t3[2,1])/sum(t3)
  aq24=c(aq24,mean(predict(lda1_Train, bd[-train,])$class==bd[-train,]$type))
}

# Veamos los errores en un histograma:
hist(materr3[,3], breaks = 100,main="global training errors")
hist(materr3[,6], breaks = 100, main="global test errors")

# Calculamos nuestros errores obtenidos anteriormente:  
e33=er_val1(materr3)
e43=er_val2(materr3)
e33
e43

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e13[1:3],e23[1:3],e33[1:3],e43[1:3]), pch=15, cex=1,
        col=c("black", "blue","orange","red"),
        type="b",ylab="%Errores", xlab=c("Errores"),lty = 1)
legend("topleft", legend=c("Aparente", "CV-LOO","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","orange","red"))


########## MODELO 5: ###########

# Basado en los resultados anteriores agregamos las siguientes interacciones:
lda5=lda(type ~ bmi^2+glu^2+npreg^2+ped^2, data = bd)
lda5

# Veamos la estructura LDA para el modelo anterior:
str(lda5)

# Comprobamos las probabilidades a priori y la matriz de escalado para este 
#modelo:
lda5$prior
lda5$scaling

# Veamos que tal las predicciones de nuestro modelo:
round(cbind(Type=bd$type,Label=predict(lda5)$class,predict(lda5)$posterior,
            predict(lda5)$x)[1:10,],3)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(t14=table(bd$type, predict(lda5)$class))

# Calculamos el error aparente:
e14=er_apar(t14)
e14

# Calculemos ahora el error de clasificación por K-fold cross validation:
lda4_loo=lda(bd$type ~  bmi^2+glu^2+npreg^2+ped^2, CV=T,data = bd) 

# La tabla de contingencia nos queda de la siguiente forma:
(t24=table(bd$type, lda4_loo$class))

# Si calculamos los errores de prediccion llegaremos a que estos serán 
#los siguientes:  
e24=er_cv(t24)
e24

# Por ultimo calculamos el error de predicción a partir del 
#acercamiento por conjunto de validación. 
#Para ello fijamos nuestra semilla y el número de repeticiones 
#(En vistas a estabilizar el error):
set.seed(1859)
Nrep=4000

# Definimos la matriz de los erroresy hacemos un ciclo que calcula los errores:
materr2=matrix(NA,ncol=6, nrow=Nrep)
colnames(materr2)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
aq15=c()
aq25=c()
for(irep in 1:Nrep){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  lda1_Train=lda(type~ bmi^2+glu^2+npreg^2+ped^2, data=bd[train,])
  t3=table(bd[train,]$type, predict(lda1_Train, bd[train,])$class)
  materr2[irep,1]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr2[irep,2]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr2[irep,3]=(t3[1,2]+t3[2,1])/sum(t3)
  aq15=c(aq15,mean(predict(lda1_Train, bd[train,])$class==bd[train,]$type))
  t3=table(bd[-train,]$type, predict(lda1_Train, bd[-train,])$class)
  materr2[irep,4]=t3[1,2]/(t3[1,1]+t3[1,2]) 
  materr2[irep,5]=t3[2,1]/(t3[2,1]+t3[2,2])
  materr2[irep,6]=(t3[1,2]+t3[2,1])/sum(t3)
  aq25=c(aq25,mean(predict(lda1_Train, bd[-train,])$class==bd[-train,]$type))
}

# Veamos los errores en un histograma:
hist(materr2[,3], breaks = 100,main="global training errors")
hist(materr2[,6], breaks = 100, main="global test errors")

# Calculamos nuestros errores obtenidos anteriormente:  
e34=er_val1(materr2)
e44=er_val2(materr2)
e34
e44

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e14[1:3],e24[1:3],e34[1:3],e44[1:3]), pch=15, cex=1,
        col=c("black", "blue","orange","red"),
        type="b",ylab="%Errores", xlab=c("Errores"),lty = 1)
legend("topleft", legend=c("Aparente", "CV-LOO","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","orange","red"))

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla11=matrix(c(e1[1],e3[1],e4[1],e11[1],e31[1],e41[1],e12[1],e32[1]
               ,e42[1],e13[1],e33[1],e43[1],e14[1],e34[1],e44[1]),ncol = 3,nrow = 5,byrow = T) 
colnames(Tabla11)=c("E.Ap","E.Train","E.Test")
rownames(Tabla11)=c("Model1","Model2","Model3","Model4","Model5")
Tabla11

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla21=matrix(c(e1[2],e3[2],e4[2],e11[2],e31[2],e41[2],e12[2],e32[2]
               ,e42[2],e13[2],e33[2],e43[2],e14[2],e34[2],e44[2]),ncol = 3,nrow = 5,byrow = T) 
colnames(Tabla21)=c("E.Ap","E.Train","E.Test")
rownames(Tabla21)=c("Model1","Model2","Model3","Model4","Model5")
Tabla21

# Hacemos una tabla con los errores globales de los modelos:
Tabla31=matrix(c(e1[3],e3[3],e4[3],e11[3],e31[3],e41[3],e12[3],e32[3]
               ,e42[3],e13[3],e33[3],e43[3],e14[3],e34[3],e44[3]),ncol = 3,nrow = 5,byrow = T) 
colnames(Tabla31)=c("E.Ap","E.Train","E.Test") 
rownames(Tabla31)=c("Model1","Model2","Model3","Model4","Model5")
Tabla31

# Hacemos una tabla con la precision del resultado del test de los modelos:
Tabla_A1=matrix(c(mean(aq21),mean(aq22),mean(aq23),mean(aq24),mean(aq25)),ncol = 1,nrow = 5,byrow = T) 
colnames(Tabla_A1)=c("Accuracy") 
rownames(Tabla_A1)=c("Model1","Model2","Model3","Model4","Model5") 
Tabla_A1

###################### NAIVE BAYES #############################################
  
# Comenzamos con el modelo que involucra todas las variables:

# Cargamos las librerías que usaremos:
library(caret)
library(e1071)
library(rfUtilities)

# Fijamos la semilla y las repeticiones:
set.seed(1859)
Nrep=4000

########## MODELO 1: ###########

# Utilizando el paquete e1071 hacemos el clasificador:
NB_model1=naiveBayes(type~., data=bd)
print(NB_model1)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(tt=table(bd$type, predict(NB_model1, newdata = bd, type = "class")))

# Calculamos el error aparente:
e_10=er_apar(tt)
e_10

# Realizamos la siguiente función la cual calcula los errores:

# Definimos la matriz que guardará nuestros errores:
me_NB=matrix(NA,ncol=6, nrow=Nrep)
colnames(me_NB)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
a11=c()
a21=c()

# Definimos el ciclo donde se rellena la matriz en Nrep:
for(irep in 1:Nrep){
  trainIndex=createDataPartition(bd$type, p=0.7)$Resample1
  train=bd[trainIndex, ]
  test=bd[-trainIndex, ]
  NB_Train=naiveBayes(type ~npreg+glu+bp+age+ped, data=bd, newdata = train, type = "class")
  trainPred=predict(NB_Train, newdata = train, type = "class")
  tt=table(train$type, trainPred)
  me_NB[irep,1]=tt[1,2]/(tt[1,1]+tt[1,2]) 
  me_NB[irep,2]=tt[2,1]/(tt[2,1]+tt[2,2])
  me_NB[irep,3]=(tt[1,2]+tt[2,1])/sum(tt)
  a11=c(a11,mean(predict(NB_Train, train,type = "class")==train$type))
  testPred=predict(NB_Train, newdata=test, type="class")
  tt=table(test$type, testPred)
  me_NB[irep,4]=tt[1,2]/(tt[1,1]+tt[1,2]) 
  me_NB[irep,5]=tt[2,1]/(tt[2,1]+tt[2,2])
  me_NB[irep,6]=(tt[1,2]+tt[2,1])/sum(tt)
  a21=c(a21,mean(predict(NB_Train, test,type = "class")==test$type))
}    

# Veamos los errores en un histograma:
data <- data.frame(
  type = c( rep("(Train, Nsim=100)", 2000), rep("(Test, Nsim=100)", 2000)),
  value = c(me_NB[,3],me_NB[,6])
)
ggplot( data=data,aes(x=value, fill=type),main="Errores Globales") +
  geom_histogram( aes(y=..density..),color="#e9ecef", alpha=0.5, position = 'identity',bins=30) +
  scale_fill_manual(values=c("#E3C393","#E193E3")) +
  theme(text=element_text(size=10,  family="serif"))+labs(fill="",x="",y="")

# Calculamos nuestros errores obtenidos anteriormente:  
e_20=er_val1(me_NB)
e_30=er_val2(me_NB)
e_20
e_30

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e_10[1:3],e_20[1:3],e_30[1:3]), pch=15, cex=1,
        col=c("black", "blue","red"),
        type="b",ylab="%Errores", xlab=c("Errores"),lty = 1)
legend("topleft", legend=c("Aparente","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","red"))

########## Modelo 2: ##################

# Utilizando el paquete e1071 hacemos el clasificador:
NB_model2=naiveBayes(type~ npreg+glu+bp+age+ped+skin, data=bd)
print(NB_model2)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(tt=table(bd$type, predict(NB_model2, newdata = bd, type = "class")))

# Calculamos el error aparente:
e_11=er_apar(tt)
e_11

# Realizamos la siguiente función la cual calcula los errores:

# Definimos la matriz que guardará nuestros errores:
me_NB=matrix(NA,ncol=6, nrow=Nrep)
colnames(me_NB)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
a12=c()
a22=c()

# Definimos el ciclo donde se rellena la matriz en Nrep:
for(irep in 1:Nrep){
  trainIndex=createDataPartition(bd$type, p=0.7)$Resample1
  train=bd[trainIndex, ]
  test=bd[-trainIndex, ]
  NB_Train=naiveBayes(type ~npreg+glu+bp+age+ped, data=bd, newdata = train, type = "class")
  trainPred=predict(NB_Train, newdata = train, type = "class")
  tt=table(train$type, trainPred)
  me_NB[irep,1]=tt[1,2]/(tt[1,1]+tt[1,2]) 
  me_NB[irep,2]=tt[2,1]/(tt[2,1]+tt[2,2])
  me_NB[irep,3]=(tt[1,2]+tt[2,1])/sum(tt)
  a12=c(a12,mean(predict(NB_Train, train,type = "class")==train$type))
  testPred=predict(NB_Train, newdata=test, type="class")
  tt=table(test$type, testPred)
  me_NB[irep,4]=tt[1,2]/(tt[1,1]+tt[1,2]) 
  me_NB[irep,5]=tt[2,1]/(tt[2,1]+tt[2,2])
  me_NB[irep,6]=(tt[1,2]+tt[2,1])/sum(tt)
  a22=c(a22,mean(predict(NB_Train, test,type = "class")==test$type))
}    

# Veamos los errores en un histograma:
hist(me_NB[,3], breaks = 100,main="global training errors")
hist(me_NB[,6], breaks = 100, main="global test errors")

# Calculamos nuestros errores obtenidos anteriormente:  
e_21=er_val1(me_NB)
e_31=er_val2(me_NB)
e_21
e_31

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e_11[1:3],e_21[1:3],e_31[1:3]), pch=15, cex=1,
        col=c("black", "blue","red"),
        type="b",ylab="Error", xlab=c("Clases:No;  Yes;   Global"),lty = 1)
legend("topleft", legend=c("Aparente","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","red"))

########## Modelo 3: ##################

# Utilizando el paquete e1071 hacemos el clasificador:
NB_model3=naiveBayes(type~npreg+glu+bp+age+ped, data=bd)
print(NB_model3)

# Construimos ahora la tabla de contingencia entrenando y probando con 
#la muestra entera: 
(tt=table(bd$type, predict(NB_model3, newdata = bd, type = "class")))

# Calculamos el error aparente:
e_12=er_apar(tt)
e_12

# Realizamos la siguiente función la cual calcula los errores:

# Definimos la matriz que guardará nuestros errores:
me_NB=matrix(NA,ncol=6, nrow=Nrep)
colnames(me_NB)=c("trg_No","trg_Yes","trg_Global", "tst_NO","tst_YES","tst_Global")
a13=c()
a23=c()

# Definimos el ciclo donde se rellena la matriz en Nrep:
for(irep in 1:Nrep){
  trainIndex=createDataPartition(bd$type, p=0.7)$Resample1
  train=bd[trainIndex, ]
  test=bd[-trainIndex, ]
  NB_Train=naiveBayes(type ~npreg+glu+bp+age+ped, data=bd, newdata = train, type = "class")
  trainPred=predict(NB_Train, newdata = train, type = "class")
  tt=table(train$type, trainPred)
  me_NB[irep,1]=tt[1,2]/(tt[1,1]+tt[1,2]) 
  me_NB[irep,2]=tt[2,1]/(tt[2,1]+tt[2,2])
  me_NB[irep,3]=(tt[1,2]+tt[2,1])/sum(tt)
  a13=c(a13,mean(predict(NB_Train, train,type = "class")==train$type))
  testPred=predict(NB_Train, newdata=test, type="class")
  tt=table(test$type, testPred)
  me_NB[irep,4]=tt[1,2]/(tt[1,1]+tt[1,2]) 
  me_NB[irep,5]=tt[2,1]/(tt[2,1]+tt[2,2])
  me_NB[irep,6]=(tt[1,2]+tt[2,1])/sum(tt)
  a23=c(a23,mean(predict(NB_Train, test,type = "class")==test$type))
}    

# Veamos los errores en un histograma:
hist(me_NB[,3], breaks = 100,main="global training errors")
hist(me_NB[,6], breaks = 100, main="global test errors")

# Calculamos nuestros errores obtenidos anteriormente:  
e_22=er_val1(me_NB)
e_32=er_val2(me_NB)
e_22
e_32

# Por último graficamos para comprobar como se ven los errores:
matplot(1:3,cbind(e_12[1:3],e_22[1:3],e_32[1:3]), pch=15, cex=1,
        col=c("black", "blue","red"),
        type="b",ylab="%Errores", xlab=c("Errores"),lty = 1)
legend("topleft", legend=c("Aparente","Training", "Test"),
       pch=15, cex=.6, col=c("black", "blue","red"))

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla12=matrix(c(e_10[1],e_20[1],e_30[1],e_11[1],e_21[1],e_31[1],e_12[1],e_22[1]
                ,e_32[1]),ncol = 3,nrow = 3,byrow = T) 
colnames(Tabla12)=c("E.Ap","E.Train","E.Test")
rownames(Tabla12)=c("Model1","Model2","Model3")
Tabla12

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla22=matrix(c(e_10[2],e_20[2],e_30[2],e_11[2],e_21[2],e_31[2],e_12[2],e_22[2]
                ,e_32[2]),ncol = 3,nrow = 3,byrow = T) 
colnames(Tabla22)=c("E.Ap","E.Train","E.Test")
rownames(Tabla22)=c("Model1","Model2","Model3")
Tabla22

# Hacemos una tabla con los errores globales de los modelos:
Tabla32=matrix(c(e_10[3],e_20[3],e_30[3],e_11[3],e_21[3],e_31[3],e_12[3],e_22[3]
                ,e_32[3]),ncol = 3,nrow = 3,byrow = T) 
colnames(Tabla32)=c("E.Ap","E.Train","E.Test")
rownames(Tabla32)=c("Model1","Model2","Model3")
Tabla32

# Hacemos una tabla con la precision del resultado del test de los modelos:
Tabla_A2=matrix(c(mean(a21),mean(a22),mean(a23)),ncol = 1,nrow = 3,byrow = T) 
colnames(Tabla_A2)=c("Accuracy") 
rownames(Tabla_A2)=c("Model1","Model2","Model3") 
Tabla_A2

###################### Logistic Regression #####################################

# Cargamos los siguientes paquetes:
library(class)
library(Hmisc)
library(pROC)

###### Modelo 1: ##############

# Hacemos el modelo con todas las variables expl.:
LR_model1=glm(type ~ ., family=binomial(link=logit),data=bd) 

# Hacemos el resumen:
summary(LR_model1)

# Hacemos predicciones con nuestro modelo:
probabilities <- LR_model1 %>% predict(bd, type = "response")

# Asignamos las etiquetas según las probabilidades a priori:
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")

# Computamos la matriz de confusión:
(ct=table(bd$type,predicted.classes))

# Exactitud del modelo:
mean(predicted.classes==bd$type)

# Gráficas de diagnóstico: 
par(mfrow=c(2,2))
plot(LR_model1) 

#Veamos la distribuón del predictor lineal en los dos grupos:
par(mfrow=c(1,2))
plot(LR_model1$linear.predictors, col=bd$type,pch=19,cex=.5)
plot(bd$type, LR_model1$linear.predictors) # Hay solapamiento.

par(mfrow=c(1,1))
# Probabilidades a posteriori estimadas:
plot(LR_model1$fitted.values,col=bd$type,pch=16,cex=.5) # Ojo con las obs. 204.
hist(LR_model1$fitted.values,breaks=100)

# Obtenemos Dxy: 
roc.area=somers2(LR_model1$fitted.values,as.numeric(bd$type)-1)
roc.area

# Graficamos la curva de ROC:
pROC_obj <- roc(bd$type,LR_model1$linear.predictors,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Intervalo de confianza de la sensibilidad en los puntos especificados:
sens.ci <- ci.se(pROC_obj)

# Graficamos:
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

# Errores aparentes (training set = test set= full set):
errores1=er_apar(ct)

set.seed(1258)
B=4000
aux2=matrix(NA,ncol = 6, nrow = B)
ac11=c()
ac21=c()

t=proc.time()
for(irep in 1:B){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  LR_Train=glm(type ~., family=binomial(link=logit),
           data=bd[train,])
  LR_Train_Pred=predict(LR_Train,newdata=bd[train,],type="response")
  LR_Train_Clss=ifelse(LR_Train_Pred>.5, "Yes", "No")
  taux=table(bd$type[train],LR_Train_Clss)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
  ac11=c(ac11,mean(LR_Train_Clss==bd[train,]$type))
  test=bd[-train, ]
  LR_Test_Pred=predict(LR_Train,newdata=test,type="response")
  LR_Test_Clss=ifelse(LR_Test_Pred>.5, "Yes", "No")
  taux=table(test$type,LR_Test_Clss)
  aux2[irep,4]=taux[1,2]/sum(taux[1,])
  aux2[irep,5]=taux[2,1]/sum(taux[2,])
  aux2[irep,6]=1-sum(diag(taux))/sum(taux)
  ac21=c(ac21,mean(LR_Test_Clss==test$type))
}
proc.time()-t

dev.off()
par(mfrow=c(1,3))
# Graficamos los histogramas:
hist(aux2[,1], breaks=50) # Clase "No"
hist(aux2[,2], breaks=50) # Clase "Yes"
hist(aux2[,3], breaks=50) # Global

errores2=er_val1(aux2)
errores3=er_val2(aux2)

par(mfrow=c(1,1))
matplot(1:3,cbind(errores1[1:3],errores2[1:3],errores3[1:3]),pch=16, cex=1,
        col=c("black","blue","orange"),
        type="b",ylab="%Errores", xlab=" Errores")

legend("topleft", legend=c("Aparente","Train","Test"),
       pch=19, cex=.8, col=c("black","blue", "orange"))


##### Modelo 2: #######

# Realizamos el algoritmo de step con dirección both:
LR_model2 <-LR_model1 %>%
  stepAIC(trace = T,direction = "both")
LR_model2

# Hacemos predicciones con nuestro modelo:
probabilities <- LR_model2 %>% predict(bd, type = "response")

# Asignamos las etiquetas según las probabilidades a priori:
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")

# Computamos la matriz de confusión:
(ct=table(bd$type,predicted.classes))

# Exactitud del modelo:
mean(predicted.classes==bd$type)

# Gráficas de diagnóstico: 
par(mfrow=c(2,2))
plot(LR_model2) 

#Veamos la distribuón del predictor lineal en los dos grupos:
par(mfrow=c(1,2))
plot(LR_model2$linear.predictors, col=bd$type,pch=19,cex=.5)
plot(bd$type, LR_model2$linear.predictors) # Hay solapamiento.

par(mfrow=c(1,1))
# Probabilidades a posteriori estimadas:
plot(LR_model2$fitted.values,col=bd$type,pch=16,cex=.5) # Ojo con las obs. 204.
hist(LR_model2$fitted.values,breaks=100)

# Obtenemos Dxy: 
roc.area=somers2(LR_model2$fitted.values,as.numeric(bd$type)-1)
roc.area

# Graficamos la curva de ROC:
pROC_obj <- roc(bd$type,LR_model2$linear.predictors,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Intervalo de confianza de la sensibilidad en los puntos especificados:
sens.ci <- ci.se(pROC_obj)

# Graficamos:
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

# Errores aparentes (training set = test set= full set):
errores11=er_apar(ct)

set.seed(1258)
B=4000
aux2=matrix(NA,ncol = 6, nrow = B)
ac12=c()
ac22=c()

t=proc.time()
for(irep in 1:B){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  LR_Train=glm(type ~npreg+glu+bmi+ped+age, family=binomial(link=logit),
               data=bd[train,])
  LR_Train_Pred=predict(LR_Train,newdata=bd[train,],type="response")
  LR_Train_Clss=ifelse(LR_Train_Pred>.5, "Yes", "No")
  taux=table(bd$type[train],LR_Train_Clss)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
  ac12=c(ac12,mean(LR_Train_Clss==bd[train,]$type))
  test=bd[-train, ]
  LR_Test_Pred=predict(LR_Train,newdata=test,type="response")
  LR_Test_Clss=ifelse(LR_Test_Pred>.5, "Yes", "No")
  taux=table(test$type,LR_Test_Clss)
  aux2[irep,4]=taux[1,2]/sum(taux[1,])
  aux2[irep,5]=taux[2,1]/sum(taux[2,])
  aux2[irep,6]=1-sum(diag(taux))/sum(taux)
  ac22=c(ac22,mean(LR_Test_Clss==test$type))
}
proc.time()-t

dev.off()
par(mfrow=c(1,1))
# Graficamos los histogramas:
hist(aux2[,1], breaks=50) # Clase "No"
hist(aux2[,2], breaks=50) # Clase "Yes"
hist(aux2[,3], breaks=50) # Global

errores22=er_val1(aux2)
errores33=er_val2(aux2)

par(mfrow=c(1,1))
matplot(1:3,cbind(errores11[1:3],errores22[1:3],errores33[1:3]),pch=16, cex=1,
        col=c("black","blue","orange"),
        type="b",ylab="%Errores", xlab=" Errores")

legend("topleft", legend=c("Aparente","Train","Test"),
       pch=19, cex=.8, col=c("black","blue", "orange"))


######### Modelo 3: ############

# Agregamos interacciones al modelo anterior:
LR_model3=glm(type ~ npreg+glu+bmi+ped+age+bmi:skin+ped:npreg, family=binomial(link=logit),data=bd) 

# Hacemos predicciones con nuestro modelo:
probabilities <- LR_model3 %>% predict(bd, type = "response")

# Asignamos las etiquetas según las probabilidades a priori:
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")

# Computamos la matriz de confusión:
(ct=table(bd$type,predicted.classes))

# Exactitud del modelo:
mean(predicted.classes==bd$type)

# Gráficas de diagnóstico: 
par(mfrow=c(2,2))
plot(LR_model3) 

#Veamos la distribuón del predictor lineal en los dos grupos:
par(mfrow=c(1,2))
plot(LR_model3$linear.predictors, col=bd$type,pch=19,cex=.5)
plot(bd$type, LR_model3$linear.predictors) # Hay solapamiento.

par(mfrow=c(1,1))
# Probabilidades a posteriori estimadas:
plot(LR_model3$fitted.values,col=bd$type,pch=16,cex=.5) # Ojo con las obs. 204.
hist(LR_model3$fitted.values,breaks=100)

# Obtenemos Dxy: 
roc.area=somers2(LR_model3$fitted.values,as.numeric(bd$type)-1)
roc.area

# Graficamos la curva de ROC:
pROC_obj <- roc(bd$type,LR_model3$linear.predictors,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Intervalo de confianza de la sensibilidad en los puntos especificados:
sens.ci <- ci.se(pROC_obj)

# Graficamos:
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

# Errores aparentes (training set = test set= full set):
errores111=er_apar(ct)

set.seed(1258)
B=4000
aux2=matrix(NA,ncol = 6, nrow = B)
ac13=c()
ac23=c()

t=proc.time()
for(irep in 1:B){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  LR_Train=glm(type ~npreg+glu+bmi+ped+age+bmi:skin+ped:npreg, family=binomial(link=logit),
               data=bd[train,])
  LR_Train_Pred=predict(LR_Train,newdata=bd[train,],type="response")
  LR_Train_Clss=ifelse(LR_Train_Pred>.5, "Yes", "No")
  taux=table(bd$type[train],LR_Train_Clss)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
  ac13=c(ac13,mean(LR_Train_Clss==bd[train,]$type))
  test=bd[-train, ]
  LR_Test_Pred=predict(LR_Train,newdata=test,type="response")
  LR_Test_Clss=ifelse(LR_Test_Pred>.5, "Yes", "No")
  taux=table(test$type,LR_Test_Clss)
  aux2[irep,4]=taux[1,2]/sum(taux[1,])
  aux2[irep,5]=taux[2,1]/sum(taux[2,])
  aux2[irep,6]=1-sum(diag(taux))/sum(taux)
  ac23=c(ac23,mean(LR_Test_Clss==test$type))
}
proc.time()-t

dev.off()
par(mfrow=c(1,3))
# Graficamos los histogramas:
hist(aux2[,1], breaks=50) # Clase "No"
hist(aux2[,2], breaks=50) # Clase "Yes"
hist(aux2[,3], breaks=50) # Global

errores222=er_val1(aux2)
errores333=er_val2(aux2)

par(mfrow=c(1,1))
matplot(1:3,cbind(errores111[1:3],errores222[1:3],errores333[1:3]),pch=16, cex=1,
        col=c("black","blue","orange"),
        type="b",ylab="%Errores", xlab=" Errores")

legend("topleft", legend=c("Aparente","Train","Test"),
       pch=19, cex=.8, col=c("black","blue", "orange"))


######### Modelo 4: ############

# Empleamos la función step con el BIC:
LR_model4= step(LR_model1, k=log(532))
LR_model4


# Hacemos predicciones con nuestro modelo:
probabilities <- LR_model4 %>% predict(bd, type = "response")

# Asignamos las etiquetas según las probabilidades a priori:
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")

# Computamos la matriz de confusión:
(ct=table(bd$type,predicted.classes))

# Exactitud del modelo:
mean(predicted.classes==bd$type)

# Gráficas de diagnóstico: 
par(mfrow=c(2,2))
plot(LR_model4) 

#Veamos la distribuón del predictor lineal en los dos grupos:
par(mfrow=c(1,2))
plot(LR_model4$linear.predictors, col=bd$type,pch=19,cex=.5)
plot(bd$type, LR_model3$linear.predictors) # Hay solapamiento.

par(mfrow=c(1,1))
# Probabilidades a posteriori estimadas:
plot(LR_model4$fitted.values,col=bd$type,pch=16,cex=.5) # Ojo con las obs. 204.
hist(LR_model4$fitted.values,breaks=100)

# Obtenemos Dxy: 
roc.area=somers2(LR_model4$fitted.values,as.numeric(bd$type)-1)
roc.area

# Graficamos la curva de ROC:
pROC_obj <- roc(bd$type,LR_model4$linear.predictors,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Intervalo de confianza de la sensibilidad en los puntos especificados:
sens.ci <- ci.se(pROC_obj)

# Graficamos:
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

# Errores aparentes (training set = test set= full set):
errores1111=er_apar(ct)

set.seed(1258)
B=4000
aux2=matrix(NA,ncol = 6, nrow = B)
ac14=c()
ac24=c()

t=proc.time()
for(irep in 1:B){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  LR_Train=glm(type ~npreg+glu+bmi+ped, family=binomial(link=logit),
               data=bd[train,])
  LR_Train_Pred=predict(LR_Train,newdata=bd[train,],type="response")
  LR_Train_Clss=ifelse(LR_Train_Pred>.5, "Yes", "No")
  taux=table(bd$type[train],LR_Train_Clss)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
  ac14=c(ac14,mean(LR_Train_Clss==bd[train,]$type))
  test=bd[-train, ]
  LR_Test_Pred=predict(LR_Train,newdata=test,type="response")
  LR_Test_Clss=ifelse(LR_Test_Pred>.5, "Yes", "No")
  taux=table(test$type,LR_Test_Clss)
  aux2[irep,4]=taux[1,2]/sum(taux[1,])
  aux2[irep,5]=taux[2,1]/sum(taux[2,])
  aux2[irep,6]=1-sum(diag(taux))/sum(taux)
  ac24=c(ac24,mean(LR_Test_Clss==test$type))
}
proc.time()-t

dev.off()
par(mfrow=c(1,3))
# Graficamos los histogramas:
hist(aux2[,1], breaks=50) # Clase "No"
hist(aux2[,2], breaks=50) # Clase "Yes"
hist(aux2[,3], breaks=50) # Global

errores2222=er_val1(aux2)
errores3333=er_val2(aux2)

par(mfrow=c(1,1))
matplot(1:3,cbind(errores1111[1:3],errores2222[1:3],errores3333[1:3]),pch=16, cex=1,
        col=c("black","blue","orange"),
        type="b",ylab="Error", xlab=" Clases: No;   Yes;   Global")

legend("topleft", legend=c("Aparente","Train","Test"),
       pch=19, cex=.8, col=c("black","blue", "orange"))

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla13=matrix(c(errores1[1],errores2[1],errores3[1],errores11[1],errores22[1],errores33[1],errores111[1],errores222[1]
                 ,errores333[1],errores1111[1],errores2222[1]
                 ,errores3333[1]),ncol = 3,nrow = 4,byrow = T) 
colnames(Tabla13)=c("E.Ap","E.Train","E.Test")
rownames(Tabla13)=c("Model1","Model2","Model3","Model4")
Tabla13

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla23=matrix(c(errores1[2],errores2[2],errores3[2],errores11[2],errores22[2],errores33[2],errores111[2],errores222[2]
                 ,errores333[2],errores1111[2],errores2222[2]
                 ,errores3333[2]),ncol = 3,nrow = 4,byrow = T)  
colnames(Tabla23)=c("E.Ap","E.Train","E.Test")
rownames(Tabla23)=c("Model1","Model2","Model3","Model4")
Tabla23

# Hacemos una tabla con los errores globales de los modelos:
Tabla33=matrix(c(errores1[3],errores2[3],errores3[3],errores11[3],errores22[3],errores33[3],errores111[3],errores222[3]
                 ,errores333[3],errores1111[3],errores2222[3]
                 ,errores3333[3]),ncol = 3,nrow = 4,byrow = T) 
colnames(Tabla33)=c("E.Ap","E.Train","E.Test")
rownames(Tabla33)=c("Model1","Model2","Model3","Model4")
Tabla33

# Hacemos una tabla con la precision del resultado del test de los modelos:
Tabla_A3=matrix(c(mean(ac21),mean(ac22),mean(ac23),mean(ac24)),ncol = 1,nrow = 4,byrow = T) 
colnames(Tabla_A3)=c("Accuracy") 
rownames(Tabla_A3)=c("Model1","Model2","Model3","Model4") 
Tabla_A3

#################### SVM #######################################################

set.seed(1258)

####### Modelos :##################################
# Comprobamos los mejores parámetros para el modelo
# con 10-fold cv:
rfeCNTL <- rfeControl(functions = lrFuncs, 
                      method = "cv",
                      number = 10)


svm.features<- rfe(bd[, 1:7], bd [, 8],
                     sizes = c(1:7),
                     rfeControl = rfeCNTL,
                     method = "svmLinear")

svm.features

# Probamos afinar con distintos kernels:
tune_Model_rad=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="radial",
                  ranges=list(cost=c(0.001,0.01,.1,1,5,10,100),gamma=1))
tune_Model_lnl=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="linear",
                  ranges=list(cost=c(0.001,0.01,.1,1,5,10,100)))
tune_Model_sig=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="sigmoid",
                  ranges=list(cost=c(0.001,0.01,.1,1,5,10,100)))
tune_Model_pol=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="polynomial",
                  ranges=list(cost=c(0.001,0.01,.1,1,5,10,100)))


summary(tune_Model_rad)# c=1
summary(tune_Model_lnl)# c=0.01
summary(tune_Model_sig)# c=0.1
summary(tune_Model_pol)# c=100

# Afinamos de nuevo con en base a los costos anteriores:
tune_Model_rad=tune(svm,type~npreg+glu+bmi+ped+age+age,data=bd, kernel="radial",
                    ranges=list(cost=c(0.4,1,1.1,1.2,1.3,1.4,1.45),gamma=c(.1,.5,1,5,10)))
tune_Model_lnl=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="linear",
                    ranges=list(cost=c(0.006,0.008,0.009,0.0095,0.01,0.014),0.012))
tune_Model_sig=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="sigmoid",
                    ranges=list(cost=c(0.06,0.07,0.08,0.09,0.1,0.11,0.12)))
tune_Model_pol=tune(svm,type~npreg+glu+bmi+ped+age,data=bd, kernel="polynomial",
                    ranges=list(cost=c(85,90,100,110,120,130,135)))

tune_Model_rad$best.parameters# c=1.2
tune_Model_lnl$best.parameters# c=0.01
tune_Model_sig$best.parameters# c=0.09
tune_Model_pol$best.parameters# c=110


# Hacemos predicciones:
best.sigmoid <- tune_Model_sig$best.model
sigmoid.test <- predict(best.sigmoid)
t1=table(bd$type,sigmoid.test )

best.linear <- tune_Model_lnl$best.model
linear.test <- predict(best.linear)
t2=table( bd$type,linear.test)

best.poly <- tune_Model_pol$best.model
poly.test <- predict(best.poly,bd)
t3=table(bd$type,poly.test )

best.radial <- tune_Model_rad$best.model
radial.test <- predict(best.radial,test)
t4=table(bd$type,radial.test)

# Errores aparentes:
errore14=er_apar(t1)
errore13=er_apar(t2)
errore12=er_apar(t3)
errore11=er_apar(t4)

# Errores Training/Test:
set.seed(1258)
B=4000
t=proc.time()

aux2=matrix(NA,ncol = 6, nrow = B)
acu11=c()
acu21=c()

for(irep in 1:B){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  tune_Model_rad=tune(svm,type~npreg+glu+bmi+ped+age,data=bd[train,], kernel="radial",
                      ranges=list(cost=c(0.4,1,1.1,1.2,1.3,1.4),gamma=c(.1,.5,1,5,10)))
  SVM_Train=best.radial <- tune_Model_rad$best.model
  radial.train = predict(best.radial,newdata = bd[train,])
  taux=table(bd$type[train],radial.train)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
  acu11=c(acu11,mean(radial.train==bd[train,]$type))
  test=bd[-train, ]
  SVM_Train=best.radial <- tune_Model_rad$best.model
  radial.test = predict(best.radial,newdata = test)
  taux=table(test$type,radial.test)
  aux2[irep,4]=taux[1,2]/sum(taux[1,])
  aux2[irep,5]=taux[2,1]/sum(taux[2,])
  aux2[irep,6]=1-sum(diag(taux))/sum(taux)
  acu21=c(acu21,mean(radial.test==test$type))
}
proc.time()-t

errore21=er_val1(aux2)
errore31=er_val2(aux2)
 
B=4000
t=proc.time()

aux2=matrix(NA,ncol = 6, nrow = B)
acu12=c()
acu22=c()

for(irep in 1:B){
  train=createDataPartition(bd$type, p=0.7)$Resample1
  tune_Model_lnl=tune(svm,type~npreg+glu+bmi+ped+age,data=bd[train,], kernel="linear",
                      ranges=list(cost=c(0.4,1,1.1,1.2,1.3,1.4)))
  SVM_Train=best.linear <- tune_Model_lnl$best.model
  linear.train = predict(best.linear,newdata = bd[train,])
  taux=table(bd$type[train],linear.train)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
  acu12=c(acu12,mean(linear.train==bd[train,]$type))
  test=bd[-train, ]
  SVM_Train=best.linear <- tune_Model_lnl$best.model
  linear.test = predict(best.linear,newdata = test)
  taux=table(test$type,linear.test)
  aux2[irep,4]=taux[1,2]/sum(taux[1,])
  aux2[irep,5]=taux[2,1]/sum(taux[2,])
  aux2[irep,6]=1-sum(diag(taux))/sum(taux)
  acu22=c(acu22,mean(linear.test==test$type))
}
proc.time()-t

errore22=er_val1(aux2)
errore32=er_val2(aux2)

# Graficamos:
par(mfrow=c(1,1))
matplot(1:3,cbind(errore12[1:3],errore22[1:3],errore32[1:3]),pch=16, cex=1,
        col=c("black","blue","orange"),
        type="b",ylab="Error", xlab=" Clases: No;   Yes;   Global")

legend("topleft", legend=c("Aparente","Train","Test"),
       pch=19, cex=.8, col=c("black","blue", "orange"))

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla14=matrix(c(errore11[1],errore21[1],errore31[1]
                 ,errore12[1],errore22[1],errore32[1]),ncol = 3,nrow = 2,byrow = T) 
colnames(Tabla14)=c("E.Ap","E.Train","E.Test")
rownames(Tabla14)=c("Model1","Model2")
Tabla14

# Hacemos una tabla con los errores para el primer grupo de los modelos:
Tabla24=matrix(c(errore11[2],errore21[2],errore31[2]
                 ,errore12[2],errore22[2],errore32[2]),ncol = 3,nrow = 2,byrow = T)  
colnames(Tabla24)=c("E.Ap","E.Train","E.Test")
rownames(Tabla24)=c("Model1","Model2")
Tabla24

# Hacemos una tabla con los errores globales de los modelos:
Tabla34=matrix(c(errore11[3],errore21[3],errore31[3]
                 ,errore12[3],errore22[3],errore32[3]),ncol = 3,nrow = 2,byrow = T) 
colnames(Tabla34)=c("E.Ap","E.Train","E.Test")
rownames(Tabla34)=c("Model1","Model2")
Tabla34

# Hacemos una tabla con la precision del resultado del test de los modelos:
Tabla_A4=matrix(c(mean(acu21),mean(acu22)),ncol = 1,nrow = 2,byrow = T) 
colnames(Tabla_A4)=c("Accuracy") 
rownames(Tabla_A4)=c("Model1","Model2") 
Tabla_A4

ft=matrix(c(e12[1],e12[2],e12[3],e32[1],e32[2],e32[3],e42[1],e42[2],e42[3]
            ,e_11[1],e_11[2],e_11[3],e_21[1],e_21[2],e_21[3],e_31[1],e_31[2],e_31[3],
          errores1111[1],errores1111[2],errores1111[3],errores2222[1],errores2222[2],errores2222[3],
          errores3333[1],errores3333[2],errores3333[3],errore12[1],errore12[2],errore12[3],
          errore22[1],errore22[2],errore22[3],errore32[1],errore32[2],errore32[3]),ncol = 3,nrow = 12,byrow = T) 
colnames(ft)=c("No","Yes","Global")
rownames(ft)=c("lda_Apa.","lda_Train","lda_Test","NB_Apa.","NB_Train","NB_Test",
               "LR_Apa.","LR_Train","LR_Test","SVM_Apa.","SVM_Train","SVM_Test") 
ft

# Graficamos:
par(mfrow=c(1,1))
matplot(1:3,cbind(e12[1:3],e42[1:3]
                  ,e_11[1:3],e_21[1:3],
                  errores1111[1:3],
                  errores3333[1:3],errore13[1:3],
                  errore32[1:3]),pch=17, cex=1,
        col=c("black","blue","red","green","grey","orange","brown","purple"),
        type="b",ylab="Error", xlab=" Clases: No;   Yes;   Global")

legend("topleft", legend=c("LDA_Apa.","LDA_Test","NB_Apa.","NB_Test",
                           "LR_Apa.","LR_Test","SVM_Apa.","SVM_Test") ,
       pch=17, cex=.7, col=c("black","blue","red","green","grey","orange","brown","purple"))

################### EJERCICIO 2:#############################

library(dplyr)
library(MASS)
library(readr)
library(ggplot2)
library(gridExtra)
library(nnet)
library(caret)
library(h2o)
library(stats)
library(base)
library(lmtest)# paquete para hacer test Likelihood ratio

fp<-file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 4/Niyereth sol/ejercicio2/Glucose1.txt")

Glucose <- Glucose <- read_csv(fp)

View(Glucose) # 145 observaciones, 7 variables
# Variables: Peso, Fglucosa, Glucoselnt, Respuesta a la insulina, resistencia a la insulina.

summary(Glucose)# No hay observaciones faltantes. 3 Clases a clasificar.
str(Glucosa)

# Convertimos la variable class en categorica y quitamos la varible paciente
Glucosa<- Glucose[,-1]
Glucosa$Class<- factor(Glucosa$Class)
summary(Glucosa)
clas_real=table(Glucosa$Class)
clas_real

# Hacemos unos boxplot exploratorios para las categorias 
plot1 <- ggplot(Glucosa, aes(x=Class, y=Weight, fill=Class)) + geom_boxplot()  
plot2 <- ggplot(Glucosa, aes(x=Class, y=Fglucose, fill=Class)) + geom_boxplot()
plot3 <- ggplot(Glucosa, aes(x=Class, y=GlucoseInt, fill=Class)) + geom_boxplot()
plot4 <- ggplot(Glucosa, aes(x=Class, y=InsulinResp, fill=Class)) + geom_boxplot()
plot5 <- ggplot(Glucosa, aes(x=Class, y=InsulineResist, fill=Class)) + geom_boxplot()


grid.arrange(plot1, plot2, plot3, plot4,plot5, ncol=3)

# Investigando un poco con un experto en medicina, tenemos que los valores normales
# para una persona en estos mediciones hechas en la sangre son aproximadamente las representadas por la class 3
# Las clases 1 y 2 representan pacientes enfermos de algun tipo de diabetes

cor(Glucose1)


# Se hace un modelo multinomial logistico con todas las variables
modelo_completo <- multinom(Class ~ ., Glucosa,maxit=10000)
summary(modelo_completo) # Devianza=1.77 AIC=25.77

modelo_completo$fitted.values[1:10,] #Probabilidades a posteriori
?confusionMatrix
#confusion matrix modelocompleto.
con_matriz=confusionMatrix(predict(modelo_completo, Glucosa, type = "class"),Glucosa$Class) # Clasifica todos en su lugar
tab=con_matriz$table
tab
#Errores modelo_completo
#errores aparentes
for(i in 1:3){
  print(1- tab[i,i]/sum(tab[i,])) 
}  #clasifica muy bien 

# Errores por trainig-test validation Para modelo completo 
set.seed(1224)
B=3000
aux2=matrix(NA,ncol = 4, nrow = B)

for(irep in 1:B){
  train=createDataPartition(Glucosa$Class, p=.7, list=FALSE)
  model=multinom(Class ~ .,data=Glucosa[train,],maxit=5000)
  taux=table(Glucosa$Class[-train],predict(model, Glucosa[-train,], type = "class"))
  for(i in 1:3) aux2[irep,i]=1- taux[i,i]/sum(taux[i,])
  aux2[irep,4]=1- sum(diag(taux))/sum(taux)
}


for (i in 1:4){
  print(mean(aux2[,i]))
} # Errores de prediccion por grupo y global 

#-----------Modelos adicionales.
#-------Revision de colinealidad.
datcol<-select(Glucosa,-Class)

cor(datcol)

Glucosa2<- Glucosa[ ,-3]#sacamos GlucoseInt variable Glucoseint que tiene correlacion de .9 con Fglucosa
# Modelo 2
modelo_2 <- multinom(Class ~ ., Glucosa2)
summary(modelo_2) #devianza=81.23 AIC=101.23
# El siguiente sera el test para comparar los modelos, es el likelihoodratio test 
lrtest(modelo_completo, modelo_2)# La diferencia es estadisticamente significativa por lo que nos quedamos con el completo
modelo_2$fitted.values[1:10,] # Probabilidades a posteriori

# Confusion matrix 
con_matriz2=confusionMatrix(predict(modelo_2, Glucosa2, type = "class"),Glucosa2$Class) # Clasifica todos en su lugar

tab2=con_matriz2$table
tab2 # No hace un buen ajuste.


# Errores modelo_2
# Errores aparentes
for(i in 1:3){
  print(1- tab2[i,i]/sum(tab2[i,])) 
}  

# Errores por trainig-test validation modelo_2
set.seed(1224)
B=4000
aux2_2=matrix(NA,ncol = 4, nrow = B)

for(irep in 1:B){
  train=createDataPartition(Glucosa2$Class, p=.7, list=FALSE)
  model=multinom(Class ~ .,data=Glucosa2[train,])
  taux=table(Glucosa2$Class[-train],predict(model, Glucosa2[-train,], type = "class"))
  for(i in 1:3) aux2_2[irep,i]=1- taux[i,i]/sum(taux[i,])
  aux2_2[irep,4]=1- sum(diag(taux))/sum(taux)
}

for (i in 1:4){
  print(mean(aux2_2[,i]))
} #errores de prediccion por grupo para modelo 2

#---Con ayuda de la funcion step se  hace seleccion de variables y hacer otro modelo.
stepAIC(modelo_completo) #Sugiere eliminar la variable Insuline resp.
Glucosa4<- Glucosa[,-4]
modelo_4<-multinom(Class ~ ., data = Glucosa4,maxit=10000) # Modelo sin la variable Insuline resp.
modelo_4
lrtest(modelo_completo, modelo_4)#comparacion de modelos
#confusion matrix 
con_matriz4=confusionMatrix(predict(modelo_4, Glucosa4, type = "class"),Glucosa4$Class) # Clasifica todos en su lugar
con_matriz4
?confusionMatrix

tab4=con_matriz4$table
tab4 #Clasifica a todos correctamente

# Errores modelo_completo
# Errores aparentes

for(i in 1:3){
  print(1- tab4[i,i]/sum(tab4[i,])) 
}  #clasifica muy bien 

# Errores por trainig-test validation modelo 4
set.seed(2224)
B=3000
aux2_4=matrix(NA,ncol = 4, nrow = B)

for(irep in 1:B){
  train=createDataPartition(Glucosa4$Class, p=.7, list=FALSE)
  model=multinom(Class ~ .,data=Glucosa4[train,],maxit=3000)
  taux=table(Glucosa4$Class[-train],predict(model, Glucosa4[-train,], type = "class"))
  for(i in 1:3) aux2_4[irep,i]=1- taux[i,i]/sum(taux[i,])
  aux2_4[irep,4]=1- sum(diag(taux))/sum(taux)
}

for (i in 1:4){
  print(mean(aux2_4[,i]))
} # Errores de prediccion por grupo

set.seed(1324)
B=3000
aux2_4=matrix(NA,ncol = 4, nrow = B)

for(irep in 1:B){
  train=createDataPartition(Glucosa4$Class, p=.7, list=FALSE)
  model=multinom(Class ~ .,data=Glucosa4[train,],maxit=3000)
  taux=table(Glucosa4$Class[-train],predict(model, Glucosa4[-train,], type = "class"))
  for(i in 1:3) aux2_4[irep,i]=1- taux[i,i]/sum(taux[i,])
  aux2_4[irep,4]=1- sum(diag(taux))/sum(taux)
}

for (i in 1:4){
  print(mean(aux2_4[,i]))
} # Errores de prediccion por grupo

## Modelos con interacciones:

modelo_5<-multinom(Class ~ .+InsulineResist:Weight, data = Glucosa4,maxit=10000)
modelo_5
lrtest(modelo_4, modelo_5) # No es estadisticamente significativo

modelo_6 = multinom(Class ~ .+InsulineResist:Fglucose, data = Glucosa4,maxit=10000)
lrtest(modelo_4, modelo_6)# No es estadisticamente Significativo

modelo_7 = multinom(Class ~ .+InsulineResist:GlucoseInt, data = Glucosa4,maxit=10000)
lrtest(modelo_4, modelo_7)# No es estadisticamente Significativo

modelo_8 = multinom(Class ~ .+Fglucose:Weight, data = Glucosa4,maxit=10000)
lrtest(modelo_4, modelo_8)# No es estadisticamente Significativo

modelo_9 = multinom(Class ~ .+GlucoseInt:Weight, data = Glucosa4,maxit=10000)
lrtest(modelo_4, modelo_9)# No es estadisticamente Significativo

modelo_10 = multinom(Class ~ .+Fglucose:GlucoseInt, data = Glucosa4,maxit=10000)
modelo_10
lrtest(modelo_4, modelo_10) # La diferencia no es estadisticamente significativa sin embargo el p valor es el mas bajo con respecto a las anteriores 
#asi que revisamos sus errores predictivos

# Confusion matrix 
con_matriz10=confusionMatrix(predict(modelo_10, Glucosa4, type = "class"),Glucosa4$Class) # Clasifica todos en su lugar
con_matriz10

# Training/test para modelo 10.
set.seed(1234)
B=3000
aux2_10=matrix(NA,ncol = 4, nrow = B)

for(irep in 1:B){
  train=createDataPartition(Glucosa4$Class, p=.7, list=FALSE)
  model=multinom(Class ~ .+Fglucose:GlucoseInt,data=Glucosa4[train,],maxit=3000)
  taux=table(Glucosa4$Class[-train],predict(model, Glucosa4[-train,], type = "class"))
  for(i in 1:3) aux2_10[irep,i]=1- taux[i,i]/sum(taux[i,])
  aux2_10[irep,4]=1- sum(diag(taux))/sum(taux)
}

for (i in 1:4){
  print(mean(aux2_10[,i]))
}
# Modelos de prueba con todas las interacciones.
modelo_11 = multinom(Class ~ .+Fglucose:GlucoseInt+GlucoseInt:Weight+InsulineResist:GlucoseInt, data = Glucosa4,maxit=10000)
lrtest(modelo_4, modelo_11) #la diferencia no es estadisticamente significativa sin embargo el p valor es el mas bajo con respecto a las anteriores 
#asi que revisamos sus errores predictivos

# Confusion matrix 
con_matriz11=confusionMatrix(predict(modelo_11, Glucosa4, type = "class"),Glucosa4$Class) # Clasifica todos en su lugar
con_matriz11

####### Gráficas: #####
#training/test para modelo 10.
set.seed(2234)
B=3000
aux2_11=matrix(NA,ncol = 4, nrow = B)
vect=rep(0,10)
class1=rep(0,8)
class2=rep(0,8)
class3=rep(0,8)
globalerr=rep(0,8)
for (B in c(10,20,100,500,1000,2000,5000,10000)) {
  aux2_11=matrix(NA,ncol = 4, nrow = B)
  for(irep in 1:B){
    train=createDataPartition(Glucosa4$Class, p=.7, list=FALSE)
    model=multinom(Class ~ .+Fglucose:GlucoseInt,data=Glucosa4[train,],maxit=3000)
    taux=table(Glucosa4$Class[-train],predict(model, Glucosa4[-train,], type = "class"))
    for(i in 1:3) aux2_11[irep,i]=1- taux[i,i]/sum(taux[i,])
    aux2_11[irep,4]=1- sum(diag(taux))/sum(taux)
  }
  if(B==10){
    class1[1]=mean(aux2_11[,1])
    class2[1]=mean(aux2_11[,2])
    class3[1]=mean(aux2_11[,3])
    globalerr[1]=mean(aux2_11[,4])
  }
  if(B==20){
    class1[2]=mean(aux2_11[,1])
    class2[2]=mean(aux2_11[,2])
    class3[2]=mean(aux2_11[,3])
    globalerr[2]=mean(aux2_11[,4])
  }
  if(B==100){
    class1[3]=mean(aux2_11[,1])
    class2[3]=mean(aux2_11[,2])
    class3[3]=mean(aux2_11[,3])
    globalerr[3]=mean(aux2_11[,4])
  }
  if(B==500){
    class1[4]=mean(aux2_11[,1])
    class2[4]=mean(aux2_11[,2])
    class3[4]=mean(aux2_11[,3])
    globalerr[4]=mean(aux2_11[,4])
  }
  if(B==1000){
    class1[5]=mean(aux2_11[,1])
    class2[5]=mean(aux2_11[,2])
    class3[5]=mean(aux2_11[,3])
    globalerr[5]=mean(aux2_11[,4])
  }
  if(B==2000){
    class1[6]=mean(aux2_11[,1])
    class2[6]=mean(aux2_11[,2])
    class3[6]=mean(aux2_11[,3])
    globalerr[6]=mean(aux2_11[,4])
  }
  if(B==5000){
    class1[7]=mean(aux2_11[,1])
    class2[7]=mean(aux2_11[,2])
    class3[7]=mean(aux2_11[,3])
    globalerr[7]=mean(aux2_11[,4])
  }
  
  if(B==10000){
    class1[8]=mean(aux2_11[,1])
    class2[8]=mean(aux2_11[,2])
    class3[8]=mean(aux2_11[,3])
    globalerr[8]=mean(aux2_11[,4])
  }
}

K= c(10,20,100,500,1000,2000,5000,10000)
plot(K, class1, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "B", ylab = "Error",lty = 2, ylim = c(0,0.075))
# Add a second line
lines(K, class2, pch = 18, col = "blue", type = "b", lty = 2)
lines(K, class3, pch = 18, col = "purple", type = "b", lty = 2)
lines(K, globalerr, pch = 18, col = "orange", type = "b", lty = 2)
abline(h=0, col="green")

# Agregamos la leyenda:
legend(7000,0.054, legend=c("Error Class1", "Error Class2", "Error Class3","Error Global", "ErrAparente"),
       col=c("red", "blue", "purple","orange","green"), lty = 2, cex=0.8)

set.seed(2234)
vect=rep(0,10)
class1=rep(0,8)
class2=rep(0,8)
class3=rep(0,8)
globalerr=rep(0,8)
for (B in c(10,20,100,500,1000,2000,5000,10000)) {
  aux2_4=matrix(NA,ncol = 4, nrow = B)
  for(irep in 1:B){
    train=createDataPartition(Glucosa$Class, p=.7, list=FALSE)
    model=multinom(Class ~ .,data=Glucosa[train,],maxit=3000)
    taux=table(Glucosa$Class[-train],predict(model, Glucosa[-train,], type = "class"))
    for(i in 1:3) aux2_4[irep,i]=1- taux[i,i]/sum(taux[i,])
    aux2_4[irep,4]=1- sum(diag(taux))/sum(taux)
  }
  if(B==10){
    class1[1]=mean(aux2_4[,1])
    class2[1]=mean(aux2_4[,2])
    class3[1]=mean(aux2_4[,3])
    globalerr[1]=mean(aux2_4[,4])
  }
  if(B==20){
    class1[2]=mean(aux2_4[,1])
    class2[2]=mean(aux2_4[,2])
    class3[2]=mean(aux2_4[,3])
    globalerr[2]=mean(aux2_4[,4])
  }
  if(B==100){
    class1[3]=mean(aux2_4[,1])
    class2[3]=mean(aux2_4[,2])
    class3[3]=mean(aux2_4[,3])
    globalerr[3]=mean(aux2_4[,4])
  }
  if(B==500){
    class1[4]=mean(aux2_4[,1])
    class2[4]=mean(aux2_4[,2])
    class3[4]=mean(aux2_4[,3])
    globalerr[4]=mean(aux2_4[,4])
  }
  if(B==1000){
    class1[5]=mean(aux2_4[,1])
    class2[5]=mean(aux2_4[,2])
    class3[5]=mean(aux2_4[,3])
    globalerr[5]=mean(aux2_4[,4])
  }
  if(B==2000){
    class1[6]=mean(aux2_4[,1])
    class2[6]=mean(aux2_4[,2])
    class3[6]=mean(aux2_4[,3])
    globalerr[6]=mean(aux2_4[,4])
  }
  if(B==5000){
    class1[7]=mean(aux2_4[,1])
    class2[7]=mean(aux2_4[,2])
    class3[7]=mean(aux2_4[,3])
    globalerr[7]=mean(aux2_4[,4])
  }
  
  if(B==10000){
    class1[8]=mean(aux2_4[,1])
    class2[8]=mean(aux2_4[,2])
    class3[8]=mean(aux2_4[,3])
    globalerr[8]=mean(aux2_4[,4])
  }
}

K= c(10,20,100,500,1000,2000,5000,10000)
plot(K, class1, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "B", ylab = "Error",lty = 2, ylim = c(0,0.09))
# Add a second line
lines(K, class2, pch = 18, col = "blue", type = "b", lty = 2)
lines(K, class3, pch = 18, col = "purple", type = "b", lty = 2)
lines(K, globalerr, pch = 18, col = "orange", type = "b", lty = 2)
abline(h=0, col="green")

# Agregamos la leyenda:
legend(7000,0.03, legend=c("Error Class1", "Error Class2", "Error Class3","Error Global", "ErrAparente"),
       col=c("red", "blue", "purple","orange","green"), lty = 2, cex=0.8)

#--------------Modelo (Ejercicio2 Tarea 1 )----------

crashdata <- read_csv("/Users/Octavalo/Library/Mobile Documents/com~apple~CloudDocs/Niyereth/June_13_data.csv")
crashdata$Month = as.factor(crashdata$Month) 
crashdata$Time_of_Day<-as.factor(crashdata$Time_of_Day) 
crashdata$year<-as.factor(crashdata$year)
crashdata$Rd_Feature<-as.factor(crashdata$Rd_Feature) 
crashdata$Rd_Character<-as.factor(crashdata$Rd_Character) 
crashdata$Rd_Class <-as.factor(crashdata$Rd_Class)
crashdata$Rd_Configuration<-as.factor(crashdata$Rd_Configuration) 
crashdata$Rd_Surface<-as.factor(crashdata$Rd_Surface) 
crashdata$Rd_Conditions<-as.factor(crashdata$Rd_Conditions)
crashdata$Light<-as.factor(crashdata$Light) 
crashdata$Weather<-as.factor(crashdata$Weather) 
crashdata$Traffic_Control <-as.factor(crashdata$Traffic_Control)
crashdata$Work_Area<-as.factor(crashdata$Work_Area)

# Rd_Feature : None
#contrasts(crashdata$Rd_Feature)
crashdata$Rd_Feature=relevel(crashdata$Rd_Feature, ref=3) #cambio
contrasts(crashdata$Rd_Feature)
#RD_Class: State HWY
contrasts(crashdata$Rd_Class)
crashdata$Rd_Class=relevel(crashdata$Rd_Class, ref=2) #cambio3
contrasts(crashdata$Rd_Class)
#Rd_Character: Straightlevel 
contrasts(crashdata$Rd_Character)
crashdata$Rd_Character=relevel(crashdata$Rd_Character, ref=6) #cambio
contrasts(crashdata$Rd_Character)

#Rd_Surface: Smoothasfalt
contrasts(crashdata$Rd_Surface)
crashdata$Rd_Surface=relevel(crashdata$Rd_Surface, ref=5) #cambio
contrasts(crashdata$Rd_Surface)
#Light: day light
contrasts(crashdata$Light)
crashdata$Light=relevel(crashdata$Light, ref=4) #cambio
contrasts(crashdata$Light)
# Weather: clear
contrasts(crashdata$Weather)
# Traffic control : none
contrasts(crashdata$Traffic_Control)


modelcrash= lm(formula = Crash_Score ~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control, 
               data = crashdata)
summary(modelcrash)

# Reducimos time of the day categoris 3y4
levels(crashdata$Time_of_Day)[c(3,4,6)]="346"
modelc4<- lm(Crash_Score ~Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc4)
anova(modelc3,modelc4)
# Reducimos time of the day categoris 1y2
levels(crashdata$Time_of_Day)[1:2]="12"
#contrasts(crashdata$Time_of_Day)
modelc5<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc5)
anova(modelc4,modelc5) # cComo la comparacion no es estadisticamente significativa nos quedamos con modelc5 el reducido.

# Intentamos ahora para la variable Rd Feature, con las categorias Driveway-intersection
levels(crashdata$Rd_Feature)[2:3]="dry_inter"
modelc6<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc6)
anova(modelc5,modelc6) # Como la comparacion no es estadisticamente significativa nos quedamos con modelc66, el reducido.

#Intentamos ahora para la variable Rd character, con las categorias straight level y straight grade
levels(crashdata$Rd_Character)[c(1,4,6)]="A-straightLyG"
modelc7<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc7)
anova(modelc6,modelc7) # Como la comparacion no es estadisticamente significativa nos quedamos con modelc7, el reducido.

# 2,3,4 caracterisi
#Intentamos ahora para la variable Rd character, con las categorias straight level y straight grade
levels(crashdata$Rd_Character)[c(2,3,4)]="curve-other"
modelc8<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc8)


anova(modelc7,modelc8) # Como la comparacion no es estadisticamente significativa nos quedamos con modelc8, el reducido.


#Intentamos ahora para la variable Rd surface, con las categorias concrete y grooved concrete 
levels(crashdata$Rd_Surface)[c(4,5)]="Grooved_other"
modelc9<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc9)
anova(modelc8,modelc9)  # Como la comparacion no es estadisticamente significativa nos quedamos con modelc8, el reducido.

#Intentamos ahora para la variable Light, con las categorias down dusk y clear

levels(crashdata$Light)[c(1,4,5)]="AClear-downdusk"
modelc10<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc10)
anova(modelc9,modelc10) # Como la comparacion no es estadisticamente significativa nos quedamos con modelc10, el reducido.

#Intentamos ahora para la variable Traffic control, con las categorias none and other.

levels(crashdata$Traffic_Control)[c(1,2)]="None-other"
modelc11<- lm(Crash_Score~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata )
summary(modelc11)
anova(modelc10,modelc11) # Como la comparacion no es estadisticamente significativa nos quedamos con modelc11, el reducido.

#------Ejercicio 3:-----

# Modelo con el que se trabaja:

Modelfinal= glm(log(Crash_Score)~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata)
summary(Modelfinal)
#Modelfinal=modeloGaussiano
R1=mean(Modelfinal$residuals**2) #suma de cuadrados de residuos
Modelfinal$deviance # Devianza
#Modelo Gamma

modelgamma=glm(Crash_Score ~  Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = crashdata, 
               family = Gamma(link = "log"))
summary(modelgamma)
R2=mean(modelgamma$residuals**2)# Suma de cuadrados residuos:
modelgamma$deviance
# Diferencia en los coeficientes:
coefdif=modelgamma$coefficients-Modelfinal$coefficients
coefdif



# Media de los cuadrados de las diferencias en valores ajustados en los modelos
VA=mean((Modelfinal$fitted.values-modelgamma$fitted.values)**2)
sqrt(VA)

# Graficamos estos valores ajustados y los reales.
Vareales= data.frame(crashdata$Crash_Score) # Valores ajustados Gaussiana
colnames(Vareales)[1] <- "ajustados"
Vareales$dis="Real"
vajustados=data.frame(exp(Modelfinal$fitted.values)) # Valores ajustados Gaussiana
colnames(vajustados)[1] <- "ajustados"
vajustados
vajustados_gamma=data.frame(modelgamma$fitted.values)# Valores ajustados Gamma
colnames(vajustados_gamma)[1] <- "ajustados"
View(vajustados)
vajustados$dis <- 'Gaussiana'  
vajustados_gamma$dis <- 'Gamma'

# Comparamos primero los modelos entre ellos.
ajustados_ambos= rbind( vajustados, vajustados_gamma)
ggplot(ajustados_ambos, aes(ajustados, fill = dis)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

# Comparamos los modelos con los valores reales
ajustados_todos= rbind(Vareales, vajustados, vajustados_gamma)
ggplot(ajustados_todos, aes(ajustados, fill = dis)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

# Otra grafica para ver un poco mejor diferencias entre los valores ajustados y predichos estan los modelos de los valores reales

cerc_valores= exp(Modelfinal$fitted.values)/modelgamma$fitted.values #razon predicciones gaussian/gamma
cerc_valoresl= exp(Modelfinal$fitted.values)/crashdata$Crash_Score #razon predicciones gaussian/reales
cerc_valoresg= modelgamma$fitted.values/crashdata$Crash_Score #razon predicciones gamma/reales
c=rep(1:23137)

par(mfrow=c(1,2))
plot(exp(Modelfinal$fitted.values),modelgamma$fitted.values,pch = 20,xlab = "Predicciones Gaussian",ylab = "Predicciones Gamma")
lines(c(0,23137), c(0,23137), pch = 20, col = "blue", type = "b", lty = 2)
legend(5.5,5, legend=c("Y=x"),col=c("blue"),lty = 2, cex=0.8)
plot(c,cerc_valores, ylim = c(0.6,1.5), type = "p", frame = FALSE, pch = 20,xlab = "Observaciones",ylab = "Cociente entre valores predichos")
abline(h=1, col="blue")


plot(exp(Modelfinal$fitted.values),crashdata$Crash_Score,pch = 20,xlab = "Predicciones Gaussian",ylab = "Predicciones Reales")
lines(c(0,23137), c(0,23137), pch = 20, col = "blue", type = "b", lty = 2)
legend(3.5,50, legend=c("Y=x"),col=c("blue"),lty = 2, cex=0.8)
plot(c,cerc_valoresl, ylim = c(-1,70), type = "p", frame = FALSE, pch = 20,xlab = "Observaciones",ylab = "Cociente, valores predichos/valores reales")
abline(h=1, col="blue")


plot(modelgamma$fitted.values,crashdata$Crash_Score,pch = 20,xlab = "Predicciones Gamma",ylab = "Predicciones Reales")
lines(c(0,23137), c(0,23137), pch = 20, col = "blue", type = "b", lty = 2)
legend(3.5,50, legend=c("Y=x"),col=c("blue"),lty = 2, cex=0.8)
plot(c,cerc_valoresg, ylim = c(-.1,70), type = "p", frame = FALSE, pch = 20,xlab = "Observaciones",ylab = "Cociente, valores predichos/valores reales")
abline(h=1, col="blue")

########b ################
set.seed(1234)
error_test=rep(0,8)
error_train=rep(0,8)

for (B in c(10,20,100,500,1000,2000,5000,10000)) {
  err_test=rep(0,10)
  err_train=rep(0,10)
  for (k in 1:100) {
    dt = sort(sample(nrow(crashdata), nrow(crashdata)*.7))
    traincrash<-crashdata[dt,]
    testcrash <-crashdata[-dt,]
    modelgausstest=glm(log(Crash_Score)~ Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = traincrash)
    
    
    pred=predict(modelgausstest, newdata = testcrash, se.fit = FALSE,type = c( "response"))
    err_test[k]=mean((testcrash$Crash_Score-exp(pred))**2)
    err_train[k]=mean((exp(modelgausstest$fitted.values)-traincrash$Crash_Score)**2)
  }
  if(B==10){
    error_train[1]=mean(err_train)
    error_test[1]=mean(err_test)
  }
  if(B==20){
    error_train[2]=mean(err_train)
    error_test[2]=mean(err_test)
  }
  if(B==100){
    error_train[3]=mean(err_train)
    error_test[3]=mean(err_test)
  }
  if(B==500){
    error_train[4]=mean(err_train)
    error_test[4]=mean(err_test)
  }
  if(B==1000){
    error_train[5]=mean(err_train)
    error_test[5]=mean(err_test)
  }
  if(B==2000){
    error_train[6]=mean(err_train)
    error_test[6]=mean(err_test)
  }
  if(B==5000){
    error_train[7]=mean(err_train)
    error_test[7]=mean(err_test)
  }
  
  if(B==10000){
    error_train[8]=mean(err_train)
    error_test[8]=mean(err_test)
  }
}  

err_train
err_test


#----Gamma
set.seed(1234)
error_test2=rep(0,8)
error_train2=rep(0,8)

for (B in c(10,20,100,500,1000,2000,5000,10000)) {
  err_test2=rep(0,10)
  err_train2=rep(0,10)
  
  for (k in 1:B) {
    dt = sort(sample(nrow(crashdata), nrow(crashdata)*.7))
    traincrash<-crashdata[dt,]
    testcrash <-crashdata[-dt,]
    modelgammatest=glm(Crash_Score ~  Time_of_Day + Rd_Feature + Rd_Class + Rd_Character+ Rd_Surface + Light + Traffic_Control,  data = traincrash, 
                       family = Gamma(link = "log"))
    pred=predict(modelgammatest, newdata = testcrash, se.fit = FALSE,type = c( "response"))
    err_test2[k]=mean((testcrash$Crash_Score-pred)**2)
    err_train2[k]=mean((modelgammatest$fitted.values-traincrash$Crash_Score)**2)
  }
  if(B==10){
    error_train2[1]=mean(err_train2)
    error_test2[1]=mean(err_test2)
  }
  if(B==20){
    error_train2[2]=mean(err_train2)
    error_test2[2]=mean(err_test2)
  }
  if(B==100){
    error_train2[3]=mean(err_train2)
    error_test2[3]=mean(err_test2)
  }
  if(B==500){
    error_train2[4]=mean(err_train2)
    error_test2[4]=mean(err_test2)
  }
  if(B==1000){
    error_train2[5]=mean(err_train2)
    error_test2[5]=mean(err_test2)
  }
  if(B==2000){
    error_train2[6]=mean(err_train2)
    error_test2[6]=mean(err_test2)
  }
  if(B==5000){
    error_train2[7]=mean(err_train2)
    error_test2[7]=mean(err_test2)
  }
  
  if(B==10000){
    error_train2[8]=mean(err_train2)
    error_test2[8]=mean(err_test2)
  }
  
}  












