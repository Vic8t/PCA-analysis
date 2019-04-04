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
    table<-read.table(file=file, sep=";", header=T, row.name=1) # import data
    cat("\nSummary:\n")
    print(summary(table)) # basic analysis of variables (mean, median, quartile)
    table_cor<-cor(table) # correlation table
    cat("\nCorrelation threshold: ")
    threshold<-as.numeric(readLines(file("stdin"), n = 1))
    cat("\nCorrelation table:\n")
    highCor(table_cor,threshold) # display high values

    table_pca<-princomp(table,cor=T,scores=T) # principal components analysis
    table_pca # display standard deviation, number of variables and observations
    table_pca$loadings # eigen vectors
    summary(table_pca) # importance of components
    cat("\nComp 1:\n")
    print(as.matrix(sort(table_pca$loadings[,1])))
    cat("\nComp 2:\n")
    print(as.matrix(sort(table_pca$loadings[,2])))

    var<-table_pca$sdev^2 # calculation of variance (eigen values)
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

    cat("\nGraph title: ")
    title<-readLines(file("stdin"), n = 1)
    cat("X axis (low): ")
    xl<-readLines(file("stdin"), n = 1)
    cat("X axis (high): ")
    xh<-readLines(file("stdin"), n = 1)
    cat("Y axis (low): ")
    yl<-readLines(file("stdin"), n = 1)
    cat("X axis (high): ")
    yh<-readLines(file("stdin"), n = 1)
    png(filename="Graph.png", width = 1500, height = 1000, res = 100)
    plot(table_pca$scores[,1],table_pca$scores[,2],col="white",main=title,xlab=paste(xl,"<->",xh),ylab=paste(yl,"<->",yh),col.main="red",col.lab="blue") # 2D graph for analysis
    text(table_pca$scores[,1],table_pca$scores[,2],labels=rownames(table))
    abline(h=0,v=0)
    dev.off()
}

cat("Path: ")
path<-readLines(file("stdin"), n = 1)
cat("File: ")
file<-readLines(file("stdin"), n = 1)
PCA(path,file)
