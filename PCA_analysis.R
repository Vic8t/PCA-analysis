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

PCA<-function(path,file){
    setwd(dir=path)
    table<-read.table(file=file,sep=";",header=T,row.name=1) # import data
    summary(table) # basic analysis of variables (mean, median, quartile)
    table_cor<-cor(table) # correlation table
    highCor(table_cor,0.6) # display high values

    table_pca<-princomp(table,cor=T,scores=T) # principal components analysis
    table_pca # display standard deviation, number of variables and observations
    table_pca$loadings # eigen vectors
    summary(table_pca) # importance of components

    var<-table_pca$sdev^2 # calculation of variance
    png(filename="Elbow curve.png") # create a png for the following plot
    plot(1:length(table[1,]),var,type="b") # elbow curve
    dev.off() # close png
    comp1<-table_pca$loadings[,1]*table_pca$sdev[1]
    comp2<-table_pca$loadings[,2]*table_pca$sdev[2]
    png(filename="Correlation circle.png")
    plot(comp1, comp2,xlim=c(-1,+1),ylim=c(-1,+1)) # correlation circle
    text(comp1, comp2,labels=colnames(table))
    abline(h=0,v=0)
    dev.off()
}

print("Path: ")
path<-readLines(n = 1)
print("File: ")
file<-readLines(n = 1)
PCA(path,file)