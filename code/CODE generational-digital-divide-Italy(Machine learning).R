install.packages("randomForest")
library(randomForest)
d21 = read.table(file.choose(), header = TRUE, sep = "\t", fill = TRUE, na.strings = c("", " "))
d22 = read.table(file.choose(), header = TRUE, sep = "\t", fill = TRUE, na.strings = c("", " "))

#rinomino la variabile con nome diverso
library(dplyr)
d21s=subset(subset(d21, select=c(CITTMi,CONDMi,REGMf,INTTEMPO,ETAMi,ISTRMi,SESSO,ANNO,INTATT14,INTSAL3,INTFASC,INTALTSAL,INTATT7BN,INTATT30A,INTATT30B,INTATT31,INTATT28B,INTATT11,COSINT9a,COSINT9b,COSINT9c,INCOMU6,INCOMU7,PWEB,INTATT8,INTATT16)))
d22s=subset(subset(d22, select=c(CITTMi,CONDMi,REGMf,INTTEMPO,ETAMi,ISTRMi,SESSO,ANNO,INTATT14,INTSAL3,INTFASC,INTALTSAL,INTATT7BN,INTATT30A,INTATT30B,INTATT31,INTATT28B,INTATT11,COSINT9a,COSINT9b,COSINT9c,INCOMU6,INCOMU7,PWEB,INTATT8,INTATT16)))
dataset=list(d21s,d22s)


# Unione dei dataset usando le chiavi comuni
key= Reduce(intersect, lapply(dataset, names))
data= Reduce(function(x, y) merge(x, y, by = key, all = TRUE), dataset)
data=na.omit(data)
data=data[data$ISTRMi!=99,]
data=data[data$CONDMi!=9,]
data=data[data$CITTMi!=9,]
data=data[data$INTTEMPO==1,]
summary(data)


# Creiamo le variabili per zona di Italia, quindi
# Nord=1 , Centro=2, Sud e Isole=3
data$GEO[data$REGMf %in% c(10, 20, 30, 40, 50, 60, 70, 80, 444, 555)]=1
data$GEO[data$REGMf %in% c(90, 100, 110, 120, 666)]=2
data$GEO[data$REGMf %in% c(130, 140, 150, 160, 170, 180, 190, 200, 777, 888)]=3
summary(data$GEO)
data$GEO


#creiamo i coefficienti dei pesi della variabile SANITARIA
data$INTFASC[data$INTFASC==5]=0
data$INTFASC[data$INTFASC==6]=1
data$INTATT14[data$INTATT14==1]=0
data$INTATT14[data$INTATT14==2]=1
data$INTSAL3[data$INTSAL3==3]=0
data$INTSAL3[data$INTSAL3==4]=1
data$INTALTSAL[data$INTALTSAL==7]=0
data$INTALTSAL[data$INTALTSAL==8]=1
mdata=data[,c('INTFASC','INTATT14','INTSAL3','INTALTSAL')]
mdata
library(psych)
fa1 <- fa(mdata, nfactors = 1, rotate ='varimax',cor='poly')  # 1 fattore principale, nessuna rotazione
print(fa1$weights)

#Ecco i pesi ottenuti dall'analisi fattoriale
#             MR1
# INTFASC   0.3199099
# INTATT14  0.2936928
# INTSAL3   0.4456105
# INTALTSAL 0.3903852

# Creazione della variabile aggregata con i pesi normalizzati
wsal=as.matrix(fa1$weights/sum(fa1$weights))
wsal

# pesi ottenuti per la creazione di idxsal
#             MR1
# INTFASC   0.2206886
# INTATT14  0.2026029
# INTSAL3   0.3074027
# INTALTSAL 0.2693058

data$idxsal=as.matrix(mdata)%*%wsal


#ora studiamo il lato di intrattenimento
data$INTATT7BN[data$INTATT7BN==7|data$INTATT7BN==5]=0
data$INTATT7BN[data$INTATT7BN==8|data$INTATT7BN==6]=1
data$INTATT30A[data$INTATT30A==1|data$INTATT30A==7]=0
data$INTATT30A[data$INTATT30A==2|data$INTATT30A==8]=1
data$INTATT30B[data$INTATT30B==1|data$INTATT30B==3]=0
data$INTATT30B[data$INTATT30B==2|data$INTATT30B==4]=1
data$INTATT31[data$INTATT31==3]=0
data$INTATT31[data$INTATT31==4]=1
data$INTATT28B[data$INTATT28B==5|data$INTATT28B==3]=0
data$INTATT28B[data$INTATT28B==6|data$INTATT28B==4]=1

mvdata=data[,c('INTATT7BN','INTATT30A','INTATT30B','INTATT31','INTATT28B')]
mvdata
fa2=fa(mvdata,nfactors=1,cor='poly')  # 1 fattore principale, nessuna rotazione
print(fa2$weights)

# risultati FA intrattenimento
#              MR1
# INTATT7BN 0.3423757
# INTATT30A 0.4700454
# INTATT30B 0.3437956
# INTATT31  0.2385438
# INTATT28B 0.1853647


# Creazione della variabile aggregata con i pesi normalizzati
wint=as.matrix(fa2$weights/sum(fa2$weights))
wint

# pesi per la creazione di idxint
#             MR1
# INTATT7BN 0.2166763
# INTATT30A 0.2974735
# INTATT30B 0.2175749
# INTATT31  0.1509651
# INTATT28B 0.1173101

data$idxint=as.matrix(data[,c('INTATT7BN','INTATT30A','INTATT30B','INTATT31','INTATT28B')])%*%c(wint)

# ora occupiamoci delle variabili finanziarie
data$INTATT11[data$INTATT11==1|data$INTATT11==3]=0
data$INTATT11[data$INTATT11==2|data$INTATT11==4]=1
data$COSINT9a[data$COSINT9a==1]=0
data$COSINT9a[data$COSINT9a==2]=1
data$COSINT9b[data$COSINT9b==3]=0
data$COSINT9b[data$COSINT9b==4]=1
data$COSINT9c[data$COSINT9c==5]=0
data$COSINT9c[data$COSINT9c==6]=1

mfdata=data[,c('INTATT11',"COSINT9a","COSINT9b","COSINT9c")]
summary(mfdata)
fa3=fa(mfdata, cor='poly')  # 1 fattore principale, nessuna rotazione
print(fa3$weights)

#risultati FA finanziaria
#             MR1
# INTATT11 0.4657489
# COSINT9a 0.4528887
# COSINT9b 0.4918191
# COSINT9c 0.4166225

# Creazione della variabile aggregata con i pesi normalizzati
wfin=as.matrix(fa3$weights/sum(fa3$weights))
wfin

#             MR1
# INTATT11 0.2549145
# COSINT9a 0.2478758
# COSINT9b 0.2691833
# COSINT9c 0.2280265

data$idxfin=as.matrix(mfdata)%*%c(wfin)

#Ora la variabile informazione
data$INTATT8[data$INTATT8==1]=0
data$INTATT8[data$INTATT8==2]=1
data$PWEB[data$PWEB==1]=0
data$PWEB[data$PWEB==2]=1
data$INTATT16[data$INTATT16==7]=0
data$INTATT16[data$INTATT16==8]=1

midata=data[,c("INTATT8",'PWEB',"INTATT16")]
fa4=fa(midata,nfactors=1, cor = "poly")
fa4$weights

# Risultati FA4: Informazione
#           MR1
# PWEB     0.4521007
# INTATT8  0.5416543
# INTATT16 0.2967846

# Creazione della variabile aggregata con i pesi normalizzati
winf=as.matrix(fa4$weights/sum(fa4$weights))
winf

#         MR1
# PWEB     0.3503191
# INTATT8  0.4197115
# INTATT16 0.2299694

data$idxinf=as.matrix(midata)%*%c(winf)
data$idxinf

#Creazione variabili Social
data$INCOMU6[data$INCOMU6==7]=0
data$INCOMU6[data$INCOMU6==8]=1
data$INCOMU7[data$INCOMU7==1]=0
data$INCOMU7[data$INCOMU7==2]=1

msdata=data[,c("INCOMU6","INCOMU7")]
summary(msdata)
fa5=fa(msdata,nfactors=1, cor = "poly")
fa5$weights

# Risultati FA5: Social
#              MR1
# INCOMU6 0.5974568
# INCOMU7 0.5974568

# Creazione dell'indicatore aggregato con i pesi standardizzati
wsoc=as.matrix(fa5$weights/sum(fa5$weights))
wsoc

#                  MR1
# INCOMU6    0.5
# INCOMU7    0.5

data$idxsoc=as.matrix(msdata)%*%c(wsoc)
data$idxsoc
#creazione dell'indice generale
mtdata=data[,c('idxsal','idxint','idxfin','idxinf','idxsoc')]

library(cluster)
p1=pca(mtdata,nfactors=1)
p2=pca(mtdata,nfactors=2)
p3=pca(mtdata,nfactors=3)
p4=pca(mtdata,nfactors=4)
p5=pca(mtdata,nfactors=5)
y=c(1-p5$Vaccounted[5,])
plot(y)
p1$Vaccounted
fat1
y1=(1-p1$Vaccounted[2])
y2=(1-p2$Vaccounted[3,2])
y3=(1-p3$Vaccounted[3,3])
y4=(1-p4$Vaccounted[3,4])
y5=(1-p5$Vaccounted[3,5])
y=c(y1,y2,y3,y4,y5)
y
#dato che i risultati della pca non sono molto chiari proviamo una factor analysis 
#con 2 e 3 fattori
fa1=fa(mtdata,nfactors=1,rotate='varimax')
fat2=fa(mtdata,nfactors=2,rotate='varimax')
fat3=fa(mtdata,nfactors=3,rotate='varimax')
fat4=fa(mtdata,nfactors=4,rotate='varimax')
y1=fa1$Vaccounted[2]
y2=fat2$Vaccounted[3,2]
y3=fat3$Vaccounted[3,3]
y4=fat4$Vaccounted[3,4]
y=c(1,1-y1,1-y2,1-y3,1-y4)*100
## y è già il vettore della varianza residua (lunghezza = 4)
x= 0:4              # ascissa da 0 a 4
x
plot(x, y, type = "b", pch = 19,
     xlab = "Numero di fattori",
     ylab = "Varianza residua (%)",
     xaxt = "n", ylim = c(0.5, 1.05))
axis(1, at = x)
text(x+0.07, y, sprintf("%.1f%%", y * 100),
     pos = 3, adj = 0,
     cex = 0.8, xpd = NA)

# risultati factor analysis a 2 fattori
#                       MR1  MR2
# SS loadings           1.00 0.97
# Proportion Var        0.20 0.19
# Cumulative Var        0.20 0.39
# Proportion Explained  0.51 0.49
# Cumulative Proportion 0.51 1.00

fat3
# Risultati FA a 3 fattori
#                       MR1  MR2  MR3
# SS loadings           1.01 0.85 0.16
# Proportion Var        0.20 0.17 0.03
# Cumulative Var        0.20 0.37 0.41
# Proportion Explained  0.50 0.42 0.08
# Cumulative Proportion 0.50 0.92 1.00
# in base ai risultati ottenuti sembra che 2 fattori siano sufficienti, dato
# che a partire dal terzo fattore i loading sono molto bassi, quidi teniamo solo 2 fattori
fat2$weights
# dai risultati notiamo che il primo fattore dipende prevalentemente dalle variabili di intrattenimento e le variabili
# di utilizzo social in piccola parte, quindi probabilmente un utilizzo più di svago, mentre
# il secondo fattore sia un tipo di utilizzo abbastanza serio, poichè
# quindi dipende principalmente dai fattori di utilizzo per motivi di salute,
# seguono i motivi finanziari ed infine i utilizzo a scopo informativ
# Ora costruiamo 2 indicatori basati sui 2 fattori ed infine l'indicatore generale che racchiude
# i risultati di questi 2 indicatori. Il primo lo chiamiamo idxFUN, il secondo,
# idxSERIO
fat2$weights
wfun=fat2$weights[,2]
wserio=fat2$weights[,1]
wfun[wfun<0.1]=0
wfun=wfun/sum(wfun)
wfun

#risultati dei pesi per la creazione dell'indice idxFUN
#    idxsal    idxint    idxfin    idxinf    idxsoc 
# 0.0000000 0.7460662 0.0000000 0.0000000 0.2539338 

wserio[wserio<0.1]=0
wserio=wserio/sum(wserio)
wserio

# risultati dei pesi per indice idxSERIO
# idxsal    idxint    idxfin    idxinf    idxsoc 
# 0.3996557 0.0000000 0.3465639 0.2537804 0.0000000  


data$idxFUN=as.matrix(data[,c('idxsal','idxint','idxfin','idxinf','idxsoc')])%*%wfun
data$idxSERIO=as.matrix(data[,c('idxsal','idxint','idxfin','idxinf','idxsoc')])%*%wserio

# ora otteniamo il totale come somma pesata per la proporzione di varianza spiegata
# dal singolo fattore
fat2$Vaccounted
fat2
wtot=as.matrix(fat2$Vaccounted[4,])
wtot

#pesi dei fattori
#         [,1]
# MR1 0.5202369
# MR2 0.4797631
data$idxTOT=as.matrix(data[,c('idxSERIO','idxFUN')])%*%wtot

summary_stats <- function(x) {
  n   <- length(x)
  mu  <- mean(x)
  sd  <- sd(x)
  se  <- sd / sqrt(n)
  ci  <- qt(0.975, df = n - 1) * se
  tibble(
    media = mu,
    sd    = sd,
    min   = min(x, na.rm = TRUE),
    max   = max(x, na.rm = TRUE),
    IC95L = mu - ci,
    IC95U = mu + ci
  )
}

# applica la funzione alle tre variabili d'interesse
tab <- bind_rows(
  idxFUN   = summary_stats(data$idxFUN),
  idxSERIO = summary_stats(data$idxSERIO),
  idxTOT   = summary_stats(data$idxTOT),
  .id = "indice"
)

print(tab)

#facciamo alcune analisi sui valori
cdata=data[,c('CITTMi','CONDMi','GEO','ETAMi','ISTRMi','SESSO' ,'idxFUN','idxSERIO')]
cdata
Rtot=cor(cdata,method='spearman')
Rtot
# ecco i risultati della matrice di correlazione
#
#                CITTMi      CONDMi          GEO       ETAMi      ISTRMi         SESSO       idxFUN
# CITTMi    1.000000000 -0.02324164 -0.072817518 -0.06016354  0.01980675  0.0437037348 -0.009238906
# CONDMi   -0.023241640  1.00000000  0.059139835  0.19660594  0.26560805  0.1328343137 -0.119535623
# GEO      -0.072817518  0.05913983  1.000000000 -0.04081170 -0.01407776 -0.0226616487 -0.005204668
# ETAMi    -0.060163542  0.19660594 -0.040811703  1.00000000  0.11871209 -0.0127347693 -0.466460941
# ISTRMi    0.019806751  0.26560805 -0.014077761  0.11871209  1.00000000 -0.0798210323 -0.195023318
# SESSO     0.043703735  0.13283431 -0.022661649 -0.01273477 -0.07982103  1.0000000000 -0.042010069
# idxFUN   -0.009238906 -0.11953562 -0.005204668 -0.46646094 -0.19502332 -0.0420100687  1.000000000
# idxSERIO -0.020605622 -0.22941616 -0.172637404 -0.05621618 -0.34856376  0.0006948555  0.408980052

#               idxSERIO
# CITTMi   -0.0206056223
# CONDMi   -0.2294161642
# GEO      -0.1726374044
# ETAMi    -0.0562161784
# ISTRMi   -0.3485637595
# SESSO     0.0006948555
# idxFUN    0.4089800518
# idxSERIO  1.0000000000

#creiamo dei grafici di confronto per verdere il tipo di utilizzo per età

# Calcolo dei valori medi per ciascuna classe ETAMi
# Calcolo dei valori medi per ciascuna classe di età
# Calcolo dei valori medi per ciascuna classe (classi 1-15)
# Calcolo delle medie


mSERIO=numeric(15)
mFUN=numeric(15)
mTOT=numeric(15)

for(i in 1:15){
  mSERIO[i]=mean(data$idxSERIO[data$ETAMi==i],na.rm=TRUE)
  mFUN[i]=mean(data$idxFUN[data$ETAMi==i],na.rm=TRUE)
  mTOT[i]=mean(data$idxTOT[data$ETAMi==i],na.rm=TRUE)
}

# Etichette delle classi dalla 5a alla 15a
ages=c("14-15","16-17","18-19","20-24","25-34",
       "35-44","45-54","55-59","60-64","65-74","75+")

# Dati in matrice per le classi dalla 5 in poi
mat=rbind(mSERIO[5:15],mFUN[5:15],mTOT[5:15])

# Imposta margini per far spazio alla legenda
par(mar=c(6,5,4,10))

# Crea il barplot
barplot(mat,
        beside=TRUE,
        names.arg=ages,
        col=c("steelblue","forestgreen","darkorange"),
        xlab="Classe di età",
        ylab="Valore medio dell’indice",
        main="Utilizzo medio per indice e classe di età",
        cex.names=0.8,
        las=2)

legend("topright",
       inset=c(-0.25,0.05),
       xpd=TRUE,
       legend=c("SERIO","FUN","TOT"),
       fill=c("steelblue","forestgreen","darkorange"),
       bty="n",
       cex=0.5,
       text.width=0.01)

#siccome la variabile idxSERIO per i rispondenti più giovani è composta solamente da 
#verifichiamo le medie di questo indice per età

midxinf=tapply(data$idxinf,data$ETAMi,mean)
print(midxinf)
plot(midxinf)
plot(midxinf,
     type="b",
     pch=19,
     col="steelblue",
     xlab="Classe di età",
     ylab="Utilizzo di Internet per ottenere informazioni",
     main="Media di idxinf per classe di età",
     xaxt="n")
axis(1,at=seq_along(ages),labels=ages,las=2)
# interessante notare che l'utilizzo di internet in ambito informativo è paragonabile
# tra gli individui delle 2 classi più giovani(14-15 e 16-17 contro 65-74 e 75+)

# proviamo a fare una factor analysis con tutte le variabili e confrontare
# il risultato col numero di fattori e pesi scelti
fdata=data[,c('INTATT14','INTSAL3','INTFASC','INTALTSAL','INTATT7BN','INTATT30A','INTATT30B','INTATT31','INTATT28B','INTATT11','COSINT9a','COSINT9b','COSINT9c','INCOMU4AGG','INCOMU6','INCOMU7','PWEB','INTATT8','INTATT16')]
fatot1=fa(fdata, nfactors= 1, rotate='varimax',cor='poly')
fatot2=fa(fdata, nfactors= 2, rotate='varimax',cor='poly')
fatot3=fa(fdata, nfactors= 3, rotate='varimax',cor='poly')
fatot4=fa(fdata, nfactors= 4, rotate='varimax',cor='poly')
fatot5=fa(fdata, nfactors= 5, rotate='varimax',cor='poly')
ytot=c(1-fatot1$Vaccounted[2], 1-fatot2$Vaccounted[3,2],1-fatot3$Vaccounted[3,3],1-fatot4$Vaccounted[3,4],1-fatot5$Vaccounted[3,5])
plot(ytot,ylab='Prop.Varianza residua', xlab='N° di fattori',y)
fatot2$Vaccounted
fatot3$Vaccounted

fatot2$weights
# Ecco la tabella con i pesi con una FA a 2 fattori:
#               MR1          MR2
# INTATT14    0.211947915 -0.035356996
# INTSAL3     0.257991074 -0.104822117
# INTFASC     0.220370327 -0.066717192
# INTALTSAL   0.240232148 -0.080330948
# INTATT7BN  -0.002468672  0.180021910
# INTATT30A  -0.032642888  0.236689634
# INTATT30B  -0.047050968  0.304841978
# INTATT31    0.092112378  0.158091153
# INTATT28B  -0.066325017  0.182294450
# INTATT11    0.247987654 -0.029803899
# COSINT9a    0.254319339  0.040999508
# COSINT9b    0.228306342  0.003763124
# COSINT9c    0.234259384  0.076022556
# INCOMU4AGG  0.047293334  0.306893879
# INCOMU6    -0.115121731  0.301968669
# INCOMU7    -0.028021020  0.207174914
# PWEB        0.053329710  0.077203328
# INTATT8     0.129468233  0.058996700
# INTATT16    0.148126677  0.070837745
# sembra che le variabili si distribuiscano allo stesso mano, eccezzion fatta per le variabili
# della categoria INFORMAZIONE, che risultano comunque incluse nella parte dell'idxSERIO ma non
# nettamente e comunque non sembrano avere pesi molto alti.
wght=cbind(wsal,wfin,wint,winf,wsoc)

##############################################################################################################
# Creazione di vari grafici che mettono a confronto gli stessi indici del grafico precedente
# Ma differenziano per sesso o per livello di istruzione, il sesso(SESSO) è una variabile dicotomica
# pari a 1 se l'individuo è maschio e pari a 2 se l'individuo è femmina,l'istuzione invece
#è 01	laurea e post-laurea	 	
#  07	diploma	 	
#  09	licenza di scuola media	 	
#  10	licenza di scuola elementare, nessun titolo di studio
#############################################################################################################
library(ggplot2)
islab=c("1"="Laurea","7"="Diploma","9"="Media","10"="Elementare/Nessuno")
data$islab=factor(as.character(data$ISTRMi),levels=names(islab),labels=islab)
idx=c("idxSERIO","idxFUN","idxTOT")  # ordine come nella legenda

for (istr in levels(data$islab)) {
  subdata=data[data$islab==istr,]
  
  # Calcolo media per età e indice
  mat=sapply(idx,function(var)tapply(subdata[[var]],subdata$ETAMi,mean,na.rm=TRUE))
  mat=t(mat)
  
  ages=colnames(mat)
  
  barplot(mat,
          beside=TRUE,
          names.arg=ages,
          col=c("steelblue", "forestgreen", "darkorange"),
          xlab="Classe di età",
          ylab="Valore medio dell’indice",
          main=paste("Utilizzo medio per indice e classe di età\nIstruzione:", istr),
          cex.names=0.8,
          las=2)
  
  legend("topright",
         inset=c(-0.25, 0.05),
         xpd=TRUE,
         legend=c("SERIO", "FUN", "TOT"),
         fill=c("steelblue", "forestgreen", "darkorange"),
         bty="n",
         cex=0.5,
         text.width=0.01)
}




##############################################################################################################
# Calcolo di random forest usando Eta(ETAMi), sesso(SESSO) e istruzione(ISTRMi) come variabili indipendenti e 
# idxTOT come variabile dipendente
#############################################################################################################

# Caricamento libreria
library(randomForest)
rftdata=na.omit(data[,c("ETAMi","SESSO","ISTRMi","idxTOT")])

#Fit del modello Random Forest
rftot=randomForest(idxTOT~ETAMi+SESSO+ISTRMi,
                         data=rftdata,
                         importance=TRUE,
                         ntree=1000)

#Ora testiamo i risultati usanto il baggin
rfbtot=randomForest(idxTOT~ETAMi+SESSO+ISTRMi,
                           data=rftdata,
                           mtry=3,          
                           ntree=1000,
                           importance=TRUE)


print(rftot)
print(rfbtot)
importance(rftot)
importance(rfbtot)
varImpPlot(rftot, main = "Importanza delle variabili predittive")
varImpPlot(rfbtot, main = "Importanza delle variabili predittive")

# Proviamo ora per le variabili specifiche idxSERIO ed ixdFUN
# Partiamo da idxSERIO
rfsdata=na.omit(data[,c("ETAMi","SESSO","ISTRMi","idxSERIO")])

#Fit del modello Random Forest
rfser=randomForest(idxSERIO~ETAMi+SESSO+ISTRMi,
                   data=rfsdata,
                   importance=TRUE,
                   ntree=1000)

#Ora testiamo i risultati usanto il baggin
rfbser=randomForest(idxSERIO~ETAMi+SESSO+ISTRMi,
                    data=rfsdata,
                    mtry=3,          
                    ntree=1000,
                    importance=TRUE)

print(rfser)
print(rfbser)
importance(rfser)
importance(rfbser)
varImpPlot(rfser, main = "Importanza delle variabili predittive")
varImpPlot(rfbser, main = "Importanza delle variabili predittive")

#i risultati non sono pienamente soddisfacenti, proviamo ad attuare una cluster analysis
cdata=data[,c("idxTOT", "idxSERIO", "idxFUN")]
set.seed(123)

# Cluster su idxTOT
ctot <- kmeans(cdata$idxTOT, centers = 3)
table(ctot$cluster)

# Cluster su idxSERIO
cserio <- kmeans(cdata$idxSERIO, centers = 3)
table(cserio$cluster)

# Cluster su idxFUN
cfun <- kmeans(cdata$idxFUN, centers = 3)
table(cfun$cluster)
library(ggplot2)
cfunserio <- kmeans(cdata[, c("idxFUN", "idxSERIO")], centers = 3)
table(cfunserio$cluster)
pdata <- cdata
pdata$cluster <- as.factor(cfunserio$cluster)

ggplot(pdata, aes(x = idxFUN, y = idxSERIO, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Cluster su idxFUN e idxSERIO", x = "idxFUN", y = "idxSERIO") +
  theme_minimal()


#metodo del gomito per valutare il numero di cluster necessari
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(cdata[, c("idxFUN", "idxSERIO")], centers = k)$withinss)
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Numero di cluster K",
     ylab = "Somma dei quadrati intra-cluster",
     main = "Metodo del gomito")

#sembra che bastino tra i 2 e i 4 cluster ora proviamo con la silhouette
# per avere un risultato numerico più chiaro

library(cluster)
diss <- dist(cdata[, c("idxFUN", "idxSERIO")])
sil <- numeric(9)

for (k in 2:10) {
  km <- kmeans(cdata[, c("idxFUN", "idxSERIO")], centers = k)
  sil[k] <- mean(silhouette(km$cluster, diss)[, 3])
}

plot(2:10, sil[2:10], type = "b", pch = 19,
     xlab = "Numero di cluster K",
     ylab = "Valore medio silhouette",
     main = "Analisi silhouette")


########################################################################################
######################################################################################################################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

#DA QUI IN POI SONO SCARTI DI ALTRI CALCOLI E SIMULAZIONI


#data_cluster <- data %>%
select(age, gender, internet_music, internet_tv, internet_health, internet_finance, internet_politics)

# Trasformazione delle variabili in fattori se necessario
data_cluster$gender <- as.factor(data_cluster$gender)

# Normalizzazione delle variabili di utilizzo Internet
data_cluster_scaled <- data_cluster %>%
  mutate_if(is.numeric, scale)

# K-means clustering: scegli un numero di cluster (es. 3 o 4)
set.seed(123) # Per riproducibilità
k <- 4 # Numero di cluster, da adattare secondo i dati
kmeans_result <- kmeans(data_cluster_scaled[, -c(1,2)], centers = k)

# Aggiungi i cluster ai dati originali
data_cluster$cluster <- as.factor(kmeans_result$cluster)

# Visualizzazione dei cluster
fviz_cluster(kmeans_result, data = data_cluster_scaled[, -c(1,2)],
             geom = "point", ellipse.type = "convex", main = "Cluster sull'uso di Internet")

# Analisi dei cluster
# Calcola le medie delle variabili per ciascun cluster
cluster_summary <- data_cluster %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

print(cluster_summary)

# Visualizza una tabella di contingenza per esaminare la distribuzione di genere nei cluster
table(data_cluster$gender, data_cluster$cluster)

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

hist(d22$ETAMi)

m=numeric(14)
for (i in 1:14) {
  m[i] <- mean(d22$FREQIN12[d22$ETAMi == i], na.rm = TRUE)  # Calcola la media
}
plot(m)
use=subset(d22, select=c(ETAMi,FREQIN12))
use$FREQIN12[is.na(use$FREQIN12)]=0
use$FREQIN12[use$FREQIN12==1]=30
use$FREQIN12[use$FREQIN12==2]=12
use$FREQIN12[use$FREQIN12==3]=4
use$FREQIN12[use$FREQIN12==4]=2
use$FREQIN12[use$FREQIN12==5]=0.5

m1=numeric(15)
for (i in 1:15) {
  m1[i] <- mean(use$FREQIN12[use$ETAMi == i], na.rm = TRUE)  # Calcola la media
}
m1
plot(m1)


#ora vediamo per altri anni
d13 <- read.table(file.choose(), header = TRUE, sep = "\t", fill = TRUE, na.strings = c("", " "))

use1=subset(d13, select=c(eta,freqin12))
use1$freqin12[is.na(use1$freqin12)]=0
use1$freqin12[use1$freqin12==1]=30
use1$freqin12[use1$freqin12==2]=12
use1$freqin12[use1$freqin12==3]=4
use1$freqin12[use1$freqin12==4]=2
use1$freqin12[use1$freqin12==5]=0.5

m2=numeric(18)
for (i in 1:18) {
  m2[i] <- mean(use1$freqin12[use1$eta == i])  # Calcola la media
}
plot(m2)


m3=numeric(15)
for (i in 1:15) {
  m3[i] <- mean(sdata$idx[sdata$ETAMi == i], na.rm = TRUE)  # Calcola la media
}
m3
plot(m3)

mean(sdata$ETAMi[sdata$INTATT14]==1)
sdata$ETAMi[sdata$INTATT14]==1

sdata1=sdata[sdata$idx!=0,]

m4=numeric(14)
for (i in 1:14) {
  m4[i] <- mean(sdata1$idx[sdata1$ETAMi == i], na.rm = TRUE)  # Calcola la media
}
m4
plot(m4)

#sembra che non ci siano differenze con una semplice media
idata$idx <- rowSums(idata[, c("INTVID", "INTATT28B", "INTATT32", "INTATT31")] * niwght)

im=numeric(15)
for (i in 1:15) {
  im[i] <- mean(idata$idx[idata$ETAMi == i], na.rm = TRUE)  # Calcola la media
}
im
plot(im)
idata$idx[idata$ETAMi==1]

widata=idata[idata$SESSO==1,]
fim=numeric(15)
for (i in 1:15) {
  fim[i] <- mean(widata$idx[widata$ETAMi == i], na.rm = TRUE)  # Calcola la media
}
plot(fim)