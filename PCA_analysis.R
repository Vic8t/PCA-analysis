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
    # importance of components
    cat("\nInformation comp 1 and 2:", (table_pca$sdev[1]^2+table_pca$sdev[2]^2)/length(table_pca$sdev)*100, "%\n")
    cat("Information comp 1, 2 and 3:", (table_pca$sdev[1]^2+table_pca$sdev[2]^2+table_pca$sdev[3]^2)/length(table_pca$sdev)*100, "%\n")

    cat("\nComp 1:\n")
    M1<-as.matrix(sort(table_pca$loadings[,1], decreasing = TRUE)) # find important variables for comp 1
    print(M1)
    xl<-paste(rownames(M1)[length(M1)], "-", rownames(M1)[length(M1)-1])
    xh<-paste(rownames(M1)[1], "-", rownames(M1)[2])

    cat("\nComp 2:\n")
    M2<-as.matrix(sort(table_pca$loadings[,2], decreasing = TRUE))
    print(M2)
    yl<-paste(rownames(M2)[length(M2)], "-", rownames(M2)[length(M2)-1])
    yh<-paste(rownames(M2)[1], "-", rownames(M2)[2])

    cat("\nComp 3:\n")
    M3<-as.matrix(sort(table_pca$loadings[,3], decreasing = TRUE))
    print(M3)
    zl<-paste(rownames(M3)[length(M3)], "-", rownames(M3)[length(M3)-1])
    zh<-paste(rownames(M3)[1], "-", rownames(M3)[2])

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
    png(filename="Graph.png", width = 1500, height = 1000, res = 100)
    plot(table_pca$scores[,1],table_pca$scores[,2],col="white",main=title,xlab=paste(xl,"<->",xh),ylab=paste(yl,"<->",yh),col.main="red",col.lab="blue") # 2D graph for analysis
    text(table_pca$scores[,1],table_pca$scores[,2],labels=rownames(table))
    abline(h=0,v=0)
    dev.off()

    if((table_pca$sdev[1]^2+table_pca$sdev[2]^2)/length(table_pca$sdev)*100 < 70){
        # not enough information so we take comp 3
        png(filename="Graph bis.png", width = 1500, height = 1000, res = 100)
        plot(table_pca$scores[,1],table_pca$scores[,3],col="white",main=title,xlab=paste(xl,"<->",xh),ylab=paste(zl,"<->",zh),col.main="red",col.lab="blue")
        text(table_pca$scores[,1],table_pca$scores[,3],labels=rownames(table))
        abline(h=0,v=0)
        dev.off()

        png(filename="Graph ter.png", width = 1500, height = 1000, res = 100)
        plot(table_pca$scores[,2],table_pca$scores[,3],col="white",main=title,xlab=paste(yl,"<->",yh),ylab=paste(zl,"<->",zh),col.main="red",col.lab="blue")
        text(table_pca$scores[,2],table_pca$scores[,3],labels=rownames(table))
        abline(h=0,v=0)
        dev.off()
    }

    # png(filename="Biplot.png", width = 1500, height = 1000, res = 100)
    # biplot(table_pca)
    # abline(h=0,v=0)
    # dev.off()
}

cat("\nAvailable folders: ", dir(paste(getwd(), "/data/", sep = ""), full.names = FALSE), "\n", sep = "   ")
cat("Folder: ")
path<-paste(getwd(), "/data/", readLines(file("stdin"), n = 1), sep = "")
cat("\nAvailable files: ", list.files(path), "\n", sep = "   ")
cat("CSV file: ")
file<-readLines(file("stdin"), n = 1)
PCA(path,file)
