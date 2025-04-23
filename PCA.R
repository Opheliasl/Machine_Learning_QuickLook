library(rgl)
mydata <- read.table("./sampledrugs04.txt")

mydata <- mydata[,c(1,6,7)]

mydata <- scale(mydata, scale = FALSE)
# need x, y, z limits to be the same for segments to look like the orthogonal projections
# that they are
plot3d(mydata, type="s", radius=0.5, xlim=c(-15,25), ylim=c(-15,25), zlim=c(-15,25))

?prcomp
mypca <- prcomp(mydata, retx=TRUE)
mypca$rotation # rotation/loadings matrix
mypca$x  # scores

# Projection plane 
planes3d(mypca$rotation[,3], alpha=0.05)

myrecon <- mypca$x[,1:2] %*% t(mypca$rotation[,1:2])
plot3d(myrecon, col="red", add=TRUE, type="s", radius=0.5)

mylist <- list(mydata, myrecon)
segments3d(do.call(rbind,mylist)[order(sequence(sapply(mylist,nrow))),])

segments3d(25*rbind(-mypca$rotation[,1], mypca$rotation[,1]), col="blue", lwd=2)
segments3d(25*rbind(-mypca$rotation[,2], mypca$rotation[,2]), col="green", lwd=2)
segments3d(25*rbind(-mypca$rotation[,3], mypca$rotation[,3]), col="cyan", lwd=2)


# just data and PCs
plot3d(mydata, type="s", radius=0.1, xlim=c(-15,25), ylim=c(-15,25), zlim=c(-15,25))
segments3d(25*rbind(-mypca$rotation[,1], mypca$rotation[,1]), col="blue", lwd=2)
segments3d(25*rbind(-mypca$rotation[,2], mypca$rotation[,2]), col="green", lwd=2)
segments3d(25*rbind(-mypca$rotation[,3], mypca$rotation[,3]), col="cyan", lwd=2)


# Determine the number of PCs
summary(mypca)
mypca$sdev
myvar <- mypca$sdev^2 # variance explained by each PC
myvar/sum(myvar) # % proportion of variance explained by each PC
barplot(myvar)

plot(mypca) # scree plot

# projections onto the plane in 2D
plot(mypca$x[,1:2], pch=16, xlab="PC1", ylab="PC2")
lines(c(-20,30), c(0,0), col="blue", lwd=2)
lines(c(0,0), c(-30,30), col="green",lwd=2)
