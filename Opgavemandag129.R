#Åben forbrug
library(readxl)
Forbrug <- read_excel("~/Desktop/R projekter/Forbrug.xlsx")

#Fjern kolonnerne fra 1990 1. kvartal til 1998 4. kvartal
Forbrug = Forbrug[, -c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)]

#Vend tabellen om
Forbrug <- t(Forbrug)
View(Forbrug)


#Åben forbrugertillid
library(readxl)
Forbrugertillid <- read_excel("~/Desktop/R projekter/Forbrugertillid12.9/Forbrugertillid.xlsx")
Raw=Forbrugertillid

#For at lave rownames skal den først være en matrix, se længere nede for numeric formel
Forbrugertillid<- as.matrix(Forbrugertillid)

#Lav en kolonne til colname
colnames(Forbrugertillid) <- Forbrugertillid[1,]
#Fjern derefter kolonnen hvor colnames er taget fra
Forbrugertillid<-Forbrugertillid[-c(1),]
#For at lave rownames fra en kolonne
rownames(Forbrugertillid) <- Forbrugertillid[,1]
#Fjern kolonnen hvor rownames er taget fra
Forbrugertillid<-Forbrugertillid[,-c(1)]
#rotér tabellen
Forbrugertillid<- t(Forbrugertillid)
View(Forbrugertillid)

#Nedenfor er koden til at lave al data i matrixen fra karakterstrenge til numeric. 
Forbrugertillid1<- as.data.frame(Forbrugertillid, stringsAsFactors = FALSE)
Forbrugertillid1<- data.frame(lapply(Forbrugertillid1, function(x) as.numeric(as.character(x))),
                              check.names=F, row.names = rownames(Forbrugertillid1))

#Årlig realvækst:
aarlreal<-(diff(log(as.numeric(Forbrug)),lag=4))*100
aarlreal<-(exp(diff(log(as.numeric(Forbrug)),lag=4))-1)*100

#FTillid omregnet til kvartal:
ftillid<-ts(Forbrugertillid1, start= c(2000,1), frequency = 12)
ftillid1<-aggregate(ftillid, nfrequency=4)/3

#plot
plot(aarlreal, type="h", col="dark red")
plot(ftillid1[ ,1], type="l", col="dark blue")

#Korrelation
Korrelation<-cor(aarlreal, ftillid1[ ,1])

#Multiple regression
lm.forbrug.ftillid <- lm(aarlreal ~ ftillid1[ , 2]+ftillid1[ ,3]+ftillid1[ , 4]+ftillid1[ ,5]+ftillid1[ ,6])
summary(lm.forbrug.ftillid)
#Koefficienter
coef(lm.forbrug.ftillid)
#signifikansniveau overfor konfidensinterval
confint(lm.forbrug.ftillid)
