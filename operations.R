setwd("C:/Users/vic8t/Desktop/Cours EFREI/L2/Semestre 2/Data analysis/TAI/PCA-analysis/data")
elec2012<-read.table(file="Elections 2012/Elections 2012 par departement.csv", sep=";", header=T, row.name=1)
elec2017<-read.table(file="Elections 2017/Elections 2017.csv", sep=";", header=T, row.name=1)
socioeco<-read.table(file="Etude socio-economique/Etude socio-economique des departements de France metropolitaine.csv", sep=";", header=T, row.name=1)
View(elec2012)
View(elec2017)
View(socioeco)
summary(elec2012)
summary(elec2017)
summary(socioeco)
elec2012_cor<-cor(elec2012)
elec2017_cor<-cor(elec2017)
socioeco_cor<-cor(socioeco)
View(elec2012_cor)
highCor<-function(table,threshold){
    for(i in seq(1,length(table[,1]))){
        for(j in seq(1,length(table[1,]))){
            if(table[i,j]>-threshold && table[i,j]<threshold || i==j){
                table[i,j]<-NA
            }
        }
    }
    print(table,digits=2,na.print=".")
}
highCor(elec2012_cor,0.5)
highCor(elec2017_cor,0.5)
highCor(socioeco_cor,0.6)
elec2012_pca<-princomp(elec2012,cor=T,scores=T)
elec2017_pca<-princomp(elec2017,cor=T,scores=T)
socioeco_pca<-princomp(socioeco,cor=T,scores=T)
View(elec2012_pca)
View(elec2017_pca)
View(socioeco_pca)
cat("\nInformation comp 1 and 2:", (elec2012_pca$sdev[1]^2+elec2012_pca$sdev[2]^2)/length(elec2012_pca$sdev)*100, "%\n")
cat("Information comp 1, 2 and 3:", (elec2012_pca$sdev[1]^2+elec2012_pca$sdev[2]^2+elec2012_pca$sdev[3]^2)/length(elec2012_pca$sdev)*100, "%\n")
cat("\nInformation comp 1 and 2:", (elec2017_pca$sdev[1]^2+elec2017_pca$sdev[2]^2)/length(elec2017_pca$sdev)*100, "%\n")
cat("Information comp 1, 2 and 3:", (elec2017_pca$sdev[1]^2+elec2017_pca$sdev[2]^2+elec2017_pca$sdev[3]^2)/length(elec2017_pca$sdev)*100, "%\n")
cat("\nInformation comp 1 and 2:", (socioeco_pca$sdev[1]^2+socioeco_pca$sdev[2]^2)/length(socioeco_pca$sdev)*100, "%\n")
cat("Information comp 1, 2 and 3:", (socioeco_pca$sdev[1]^2+socioeco_pca$sdev[2]^2+socioeco_pca$sdev[3]^2)/length(socioeco_pca$sdev)*100, "%\n")
elec2012_pca$loadings
elec2017_pca$loadings
socioeco_pca$loadings
M1<-as.matrix(sort(elec2012_pca$loadings[,1], decreasing = TRUE))
print(M1)
cat("\nComp 2:\n")
M2<-as.matrix(sort(elec2012_pca$loadings[,2], decreasing = TRUE))
print(M2)
cat("\nComp 3:\n")
M3<-as.matrix(sort(elec2012_pca$loadings[,3], decreasing = TRUE))
print(M3)
M1<-as.matrix(sort(elec2017_pca$loadings[,1], decreasing = TRUE))
print(M1)
cat("\nComp 2:\n")
M2<-as.matrix(sort(elec2017_pca$loadings[,2], decreasing = TRUE))
print(M2)
cat("\nComp 3:\n")
M3<-as.matrix(sort(elec2017_pca$loadings[,3], decreasing = TRUE))
print(M3)
M1<-as.matrix(sort(socioeco_pca$loadings[,1], decreasing = TRUE))
print(M1)
cat("\nComp 2:\n")
M2<-as.matrix(sort(socioeco_pca$loadings[,2], decreasing = TRUE))
print(M2)
cat("\nComp 3:\n")
M3<-as.matrix(sort(socioeco_pca$loadings[,3], decreasing = TRUE))
print(M3)
var<-elec2012_pca$sdev^2
plot(1:length(elec2012[1,]),var,type="b")
var<-elec2017_pca$sdev^2
plot(1:length(elec2017[1,]),var,type="b")
var<-socioeco_pca$sdev^2
plot(1:length(socioeco[1,]),var,type="b")
elec2012_pca
elec2017_pca
socioeco_pca
comp1<-elec2012_pca$loadings[,1]*elec2012_pca$sdev[1]
comp2<-elec2012_pca$loadings[,2]*elec2012_pca$sdev[2]
plot(comp1, comp2,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp2,labels=colnames(elec2012))
abline(h=0,v=0)
comp3<-elec2012_pca$loadings[,3]*elec2012_pca$sdev[3]
plot(comp1, comp3,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp3,labels=colnames(elec2012))
abline(h=0,v=0)
comp1<-elec2017_pca$loadings[,1]*elec2017_pca$sdev[1]
comp2<-elec2017_pca$loadings[,2]*elec2017_pca$sdev[2]
comp3<-elec2017_pca$loadings[,3]*elec2017_pca$sdev[3]
plot(comp1, comp2,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp2,labels=colnames(elec2017))
abline(h=0,v=0)
plot(comp1, comp3,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp3,labels=colnames(elec2017))
abline(h=0,v=0)
comp1<-socioeco_pca$loadings[,1]*socioeco_pca$sdev[1]
comp2<-socioeco_pca$loadings[,2]*socioeco_pca$sdev[2]
comp3<-socioeco_pca$loadings[,3]*socioeco_pca$sdev[3]
plot(comp1, comp2,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp2,labels=colnames(socioeco_pca))
abline(h=0,v=0)
plot(comp1, comp3,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp3,labels=colnames(socioeco_pca))
abline(h=0,v=0)
comp1<-socioeco_pca$loadings[,1]*socioeco_pca$sdev[1]
comp2<-socioeco_pca$loadings[,2]*socioeco_pca$sdev[2]
comp3<-socioeco_pca$loadings[,3]*socioeco_pca$sdev[3]
plot(comp1, comp2,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp2,labels=colnames(socioeco))
abline(h=0,v=0)
plot(comp1, comp3,xlim=c(-1,+1),ylim=c(-1,+1), type="n") # correlation circle
text(comp1, comp3,labels=colnames(socioeco))
abline(h=0,v=0)
plot(elec2012_pca$scores[,1],elec2012_pca$scores[,2],col="white",main="Elections 2012",xlab=paste("POUTOU, BN, ARTHAUD","<->","SARKOZY, ABS"),ylab=paste("MELENCHON, HOLLANDE","<->","DUPONT AIGNAN, SARKOZY, LE PEN"),col.main="red",col.lab="blue") # 2D graph for analysis
text(elec2012_pca$scores[,1],elec2012_pca$scores[,2],labels=rownames(elec2012))
abline(h=0,v=0)
plot(elec2012_pca$scores[,1],elec2012_pca$scores[,3],col="white",main="Elections 2012",xlab=paste("POUTOU, BN, ARTHAUD","<->","SARKOZY, ABS"),ylab=paste("BAYROU, JOLY","<->","LE PEN"),col.main="red",col.lab="blue") # 2D graph for analysis
text(elec2012_pca$scores[,1],elec2012_pca$scores[,3],labels=rownames(elec2012))
abline(h=0,v=0)
plot(elec2012_pca$scores[,2],elec2012_pca$scores[,3],col="white",main="Elections 2012",xlab=paste("MELENCHON, HOLLANDE","<->","DUPONT AIGNAN, SARKOZY, LE PEN"),ylab=paste("BAYROU, JOLY","<->","LE PEN"),col.main="red",col.lab="blue") # 2D graph for analysis
text(elec2012_pca$scores[,2],elec2012_pca$scores[,3],labels=rownames(elec2012))
abline(h=0,v=0)
plot(elec2017_pca$scores[,1],elec2017_pca$scores[,2],col="white",main="Elections 2017",xlab=paste("HAMON, MACRON","<->","LE PEN, DUPONT AIGNAN, ARTHAUD"),ylab=paste("POUTOU, BN, ARTHAUD","<->","FILLON, ASSELINEAU"),col.main="red",col.lab="blue") # 2D graph for analysis
text(elec2017_pca$scores[,1],elec2017_pca$scores[,2],labels=rownames(elec2017))
abline(h=0,v=0)
plot(elec2017_pca$scores[,1],elec2017_pca$scores[,3],col="white",main="Elections 2017",xlab=paste("HAMON, MACRON","<->","LE PEN, DUPONT AIGNAN, ARTHAUD"),ylab=paste("FILLON","<->","MELENCHON, ABS"),col.main="red",col.lab="blue") # 2D graph for analysis
text(elec2017_pca$scores[,1],elec2017_pca$scores[,3],labels=rownames(elec2017))
abline(h=0,v=0)
plot(elec2017_pca$scores[,2],elec2017_pca$scores[,3],col="white",main="Elections 2017",xlab=paste("POUTOU, BN, ARTHAUD","<->","FILLON, ASSELINEAU"),ylab=paste("FILLON","<->","MELENCHON, ABS"),col.main="red",col.lab="blue") # 2D graph for analysis
text(elec2017_pca$scores[,2],elec2017_pca$scores[,3],labels=rownames(elec2017))
abline(h=0,v=0)
plot(socioeco_pca$scores[,1],socioeco_pca$scores[,2],col="white",main="Etude socio-économique",xlab=paste("VIEU, AGRI","<->","PINT, JEUN"),ylab=paste("CPIS","<->","OUVR"),col.main="red",col.lab="blue") # 2D graph for analysis
text(socioeco_pca$scores[,1],socioeco_pca$scores[,2],labels=rownames(socioeco))
abline(h=0,v=0)
eigen(elec2012_cor)
eigen(elec2017_cor)
eigen(socioeco_cor)
View(elec2012_pca)
elec2012_pca$scores
View(elec2012_pca$scores)
View(elec2017_pca$scores)
View(socioeco_pca$scores)
