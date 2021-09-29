library(dplyr)
library(survival)
library(survminer)
library(eha)
data=read.csv(file.choose(),header=T)
head(data)

#pembersihan kolom
data1<-data[ -c(1:2,6,10,15:1570) ]
dim(data1)

#pendefinisian variabel
ti<-data1$survival
di<-data1$eventdeath
X1<-data1$age
X2<-data1$chemo
X3<-data1$hormonal
X4<-data1$amputation
X5<-data1$diam
X6<-data1$grade
X7<-data1$angioinv
#membulatkan variable survival
round(ti)

View(data1)
dim(data1)
head(data1)

#missing value
sum(is.na(data1))

#outlier
summary(data1)
boxplot(X1, ylab="umur")
outliers <- boxplot(X1, plot=FALSE)$out
outliers
data1<-data1[-which(X1 %in% outliers),]
X1<-data1$age
boxplot(X5, ylab="diameter")
boxplot(X6, ylab="positive lymph node")
outliers <- boxplot(X6, plot=FALSE)$out
outliers

#umur dan kematian
boxplot(umur~di, data=data1, main="Hubungan Umur dan Kematian",
        xlab="Kematian",
        ylab="umur")
#umur dan stadium
boxplot(umur~X7, data=data1, main="Hubungan Umur dan Stadium",
        xlab="Stadium",
        ylab="umur")
#survival dan stadium
boxplot(survival~X7, data=data1, main="Hubungan Waktu Survival dan Stadium",
        xlab="Stadium",
        ylab="Waktu Hingga Meninggal (dalam Tahun)")
#diameter dan kematian
boxplot(X5~di, data=data1, main="Hubungan Diameter Kanker dan Kematian",
        xlab="Kematian",
        ylab="Diameter Kanker (mm)")

#fungsi survival tanpa pengaruh apapun
St1<-survfit(Surv(ti, di)~1,data=data1)
summary(St1)
plot(St1, main="Fungsi Survival tanpa pengaruh apapun", 
     col=c("red","blue"), 
     xlab="Waktu Survival (T)", 
     ylab="Fungsi Survival, S(t)", lty=c(1,2))
summary(St1)
#chemo
St2<-survfit(Surv(ti, di)~X2)
summary(St2)
plot(St2, main="Fungsi Survival berdasarkan Efek Kemoterapi", 
     col=c("red","blue"), 
     xlab="Waktu Survival (T)", 
     ylab="Fungsi Survival, S(t)", lty=c(1,2))
legend("topright", c("tidak", "iya"), 
       lty=1:2, col=c("red", "blue"), bty="n")
St2[["cumhaz"]]
ggsurvplot(St2, data = data1, fun = "cumhaz")

#hormonal
St3<-survfit(Surv(ti, di)~X3)
summary(St3)
plot(St3, main="Fungsi Survival berdasarkan Efek Hormonal", 
     col=c("red","blue"), 
     xlab="Waktu Survival (T)", 
     ylab="Fungsi Survival, S(t)", lty=c(1,2))
legend("topright", c("tidak", "iya"), 
       lty=1:2, col=c("red", "blue"), bty="n")
St3[["cumhaz"]]
ggsurvplot(St3, data = data1, fun = "cumhaz")

#amputation
St4<-survfit(Surv(ti, di)~X4)
summary(St4)
plot(St4, main="Fungsi Survival berdasarkan Efek Amputasi", 
     col=c("red","blue"), 
     xlab="Waktu Survival (T)", 
     ylab="Fungsi Survival, S(t)", lty=c(1,2))
legend("topright", c("tidak", "iya"), 
       lty=1:2, col=c("red", "blue"), bty="n")
St4[["cumhaz"]]
ggsurvplot(St4, data = data1, fun = "cumhaz")

#Stadium
St7<-survfit(Surv(ti, di)~X7)
summary(St7)
plot(St7, main="Fungsi Survival berdasarkan Stadium", 
     col=c("red","blue","green"), 
     xlab="Waktu Survival (T)", 
     ylab="Fungsi Survival, S(t)", lty=c(1))
legend("bottomright", c("Stage 1", "Stage 2", "Stage 3"), 
       lty=1:1, col=c("red", "blue", "green"), bty="n")
St7[["cumhaz"]]
ggsurvplot(St7, data = data1, fun = "cumhaz")

#Penyebaran
St8<-survfit(Surv(ti, di)~X8)
summary(St8)
plot(St8, main="Fungsi Survival berdasarkan Penyebaran Kanker ke Pembuluh Darah", 
     col=c("red","blue","green"), 
     xlab="Waktu Survival (T)", 
     ylab="Fungsi Survival, S(t)", lty=c(1))
legend("bottomright", c("Rendah", "Sedang", "Besar"), 
       lty=1:1, col=c("red", "blue", "green"), bty="n")
summary(St8)
St8[["cumhaz"]]
ggsurvplot(St7, data = data1, fun = "cumhaz")

#Kemoterapi
sfit <- survfit(Surv(ti, di)~X2, data=data1)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Tidak Kemoterapi", "Kemoterapi"), legend.title="Kemoterapi",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Breast Cancer Survival", 
           risk.table.height=.3)
#terapi hormonal
sfit2 <- survfit(Surv(ti, di)~X3, data=data1)
sfit2
summary(sfit2)
plot(sfit2)
ggsurvplot(sfit2)

ggsurvplot(sfit2, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Tidak Terapi Hormonal", "Terapi Hormonal"), legend.title="Terapi Hormonal",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Breast Cancer Survival", 
           risk.table.height=.3)
#amputasi
sfit3 <- survfit(Surv(ti, di)~X4, data=data1)
sfit3
summary(sfit3)
plot(sfit3)
ggsurvplot(sfit3)

ggsurvplot(sfit3, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Tidak Amputasi", "Amputasi"), legend.title="Amputasi",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Breast Cancer Survival", 
           risk.table.height=.3)
#stadium
sfit4 <- survfit(Surv(ti, di)~X7, data=data1)
sfit4
summary(sfit4)
plot(sfit4)
ggsurvplot(sfit4)

ggsurvplot(sfit4, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Stage 1", "Stage 2", "Stage 3"), legend.title="Stadium",  
           palette=c("dodgerblue2", "orchid2","red"), 
           main="Kaplan-Meier Curve for Breast Cancer Survival", 
           risk.table.height=.3)
#penyebaran
sfit5 <- survfit(Surv(ti, di)~X8, data=data1)
sfit5
summary(sfit5)
plot(sfit5)
ggsurvplot(sfit5)

ggsurvplot(sfit5, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Kecil", "Sedang", "Besar"), legend.title="Penyebaran",  
           palette=c("dodgerblue2", "orchid2","red"), 
           main="Kaplan-Meier Curve for Breast Cancer Survival", 
           risk.table.height=.3)

attach(data1)
surv.time<-Surv(ti,di)
model.1<-coxph(surv.time~X1+as.factor(X2)+as.factor(X3)+as.factor(X4)+X5+as.factor(X6)+as.factor(X7),method = 'breslow')
summary(model.1)

#uji log rank
survdiff(Surv(ti,di)~X1, data=data1) #umur
survdiff(Surv(ti,di)~X2, data=data1) 
survdiff(Surv(ti,di)~X3, data=data1) 
survdiff(Surv(ti,di)~X4, data=data1) 
survdiff(Surv(ti,di)~X5, data=data1) #diameter kanker
survdiff(Surv(ti,di)~X6, data=data1) #posnodes
survdiff(Surv(ti,di)~X7, data=data1) #stadium
survdiff(Surv(ti,di)~X8, data=data1) #penyebaran ke pembuluh

plot(survfit(Surv(ti,di)~X7,data=data1),
     fun="cloglog",lty=1:1,mark.time=FALSE,
     xlab="Waktu Survival T",ylab="log(H(t))",
     col=c("red","blue","green"))
legend("topleft",col=c("red", "blue", "green"),lty = 1:1, legend=c("Stage 3","Stage 2","Stage 1"), bty="n")
# Asumsi COX PH
res.cox1 <- coxph(SurvObj ~ age + sex + ph.karno + wt.loss, data =  lung)
cph <- coxph(Surv(ti, di) ~ X1+X2+X3+X4+X5+X6+X7+X8 , data = data1)
cox.zph(cph)
