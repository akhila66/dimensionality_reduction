# akhila podupuganti
# --------------------------------- LIBRARYS / PACKAGES  ---------------------------------
install.packages('mlbench')
install_github("vqv/ggbiplot", force = TRUE)
install.packages('MASS')
install.packages('Rtsne')
install.packages('caret')
library(mlbench)
library(devtools)
library(ggbiplot)
library(MASS)
library(Rtsne)
library(caret)
library(tidyverse) 

# ---------------------------- GLASS DATA 1---------------------------------
data(Glass)
Glass[duplicated(Glass),] # DUPLICATED ROW
Glass <- Glass[!duplicated(Glass),] # removed duplicated row

# ---------------------------- glass data (a) Mathematics of PCA -------------------------
# ----------- a1
#excluding type which is catagorical
corMat <- cor(Glass[,-c(10)])

# ----------- a2
ev <- eigen(corMat)
ev$values # VALUES
ev$vectors # VECTORS

# ----------- a3
#excluding type which is catagorical
Glass.pca<-prcomp(Glass[,-c(10)],scale=T)
Glass.pca
summary(Glass.pca)
# this explains pc1 has 27.8% varience almost 1/4th of imformation is encapsulated in pc1 and
#similarly 22.9 varience % in pc2 which is almost 1/4th of information

plot(Glass.pca)

# ----------- a4
#they are same vector matrixs
#the sign of the PC is irrelevant - the central operation of PCA squares the data, so all signs become arbitrary.
#we are free to multiply the offensive PCs by -1. The signs of the PCs are arbitrary.
#You could also apply a factor rotation, an orthogonal transformation of your PCA factors, to yield more intuitive scores
#While the eigendecomposition of the covariance or correlation matrix may be more intuitiuve, 
#most PCA implementations perform a Singular Value Decomposition (SVD) to improve the computational efficiency. 
#So, they both are indeed same

# sd's (or eigen values) are diff bcz we didn't do the data centered in pca. but the vectors will be same

# ----------- a5
PC_vectors <- Glass.pca$rotation
PC1 <- PC_vectors[,1]
PC2 <- PC_vectors[,2]
ggplot(Glass, aes(x=Ca, y=Ba)) + theme_bw() + 
  geom_segment(aes(x = -PC1[1], y = -PC1[2], xend = PC1[1], yend = PC1[2]), color = "red", size = 2) +
  geom_segment(aes(x = -PC2[1], y = -PC2[2], xend = PC2[1], yend = PC2[2]), color = "red", size = 2) +
  geom_point()
crossprod(PC1, PC2)

PC1 %*% PC2
# not 0 so cannot say those are orthogonal, may be we can neglact since the value is very close to 0

# ---------------------------- glass data (b) Application of PCA-------------------------


# ----------- b1
ggbiplot(Glass.pca ,obs.scale = 2, var.scale = 1,  circle = TRUE,varname.size = 5)
ggbiplot(Glass.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=Glass$Type)

# just checking with other pc's as well
ggbiplot(Glass.pca ,obs.scale = 2, var.scale = 1,  circle = TRUE,varname.size = 5, choices = c(2,3), group=Glass$Type, ellipse=TRUE)
ggbiplot(Glass.pca ,obs.scale = 2, var.scale = 1,  circle = TRUE,varname.size = 5, choices = c(3,4), group=Glass$Type, ellipse=TRUE)

# ----------- b2
#will make a biplot, which includes both the position of each sample in terms of PC1 and PC2 and also will show you how the initial variables map onto this
#A biplot is a type of plot that will allow you to visualize how the samples relate to one another in our PCA (which samples are similar and which are different) and will simultaneously reveal how each variable contributes to each principal component.
# around 50% of data only encapsulated with this pc1 and pc2 and we can aslo see the axis are having less variance . 


# The axes are seen as arrows originating from the center point. 
# Here, you see that the variable Mg contribute to PC2. the remaining variables are in between which have equal contribution in both PC's
# Here we can also find which variables are correlected +ly or -ly 
# like example - Ca and RI is having very less angle distance, which are +ly corelated (near to 0 degrees), 
# similarly Ba, Na, AI are also +ly correlated, 
# K and Si are +ly correlated
# Ca and RI to K and Si are -ly correlated
# if the angle b/w variable is 90 degrees they are not correlated, if 180 -ive correlated (like Ca and K/Si), if 0 degrees +ive correlated


# Understanding the group which are type of glass 

#1 building windows float processed
#2 building windows non-float processed
#3 vehicle windows float processed
#5 containers
#6 tableware
#7 headlamps

# from plot
# type 7 (headlamps) are made of Ba, Na, AI materials 
# most of type 1 (building windows float processed) and type 3 (vehicle windows float processed)  are made of Fe and Mg

# ----------- b3


plot(Glass.pca)
# we cannot just decide till what degree,  we can consider depending on application

#Since we standardized our data and we now have the corresponding eigenvalues of each PC we can actually use these to draw a boundary for us. Since an eigenvalues <1 would mean that the component actually explains less than a single explanatory variable we would like to discard those. If our data is well suited for PCA we should be able to discard these components while retaining at least 70–80% of cumulative variance. Lets plot and see:
screeplot(Glass.pca, type = "l", npcs = 15, main = "Screeplot of the PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
# Cumulative variance plot with set plot pc 5
cumpro <- cumsum(Glass.pca$sdev^2 / sum(Glass.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="blue", lty=5)
abline(h = 0.9, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.6)
# We notice is that the first 5 components has an Eigenvalue >1 and explains almost 90% of variance, this is great! We can effectively reduce dimensionality from 9 to 5 while only “loosing” about 10% of variance!


# ---------------------------- glass data (c) Application of LDA-------------------------
#using MASS lda
#Glass.lda = lda(Glass[,c(-10)],grouping=(Glass$Type))
par(mar=c(1,1,1,1)) # for plot margins
# 1st let pre process the data

preproc.param <- Glass %>% preProcess(method = c("center", "scale")) 

# Transform the data using the estimated parameters 
transformed <- preproc.param %>% predict(Glass)

Glass.lda = lda(Type ~ ., data = transformed)
Predictions=predict(Glass.lda,transformed)
summary(Predictions)
table(Predictions$class, Glass$Type)
# there are many points are not linearly spearated. classification is of each type is difficult.

ldahist(data = Predictions$x[,1], g=Glass$Type)
# here we can see little bit the data is spreaded and can see the difference. But still there are no perfect clusters
ldahist(data = Predictions$x[,2], g=Glass$Type)
# here we can see that the data is not spreaded and cannot see the differences
plot(Predictions$x[,1], Predictions$x[,2])
text(Predictions$x[,1], Predictions$x[,2], Glass$Type, cex = 0.7, pos = 4, col = "red")

#convert to data frame 
newdata <- data.frame(type = Glass[,10], lda = Predictions$x)
library(ggplot2)
ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

#-----------------------------------------------------------------------------------
# ---------------------------- FACE BOOK PROBLEM-------------------------

# ---------------------------- PCA-------------------------

fb <- read.csv(file="FB-metrics.csv", header=TRUE, sep=",")
fb[duplicated(fb),] # checking DUPLICATED ROWs
fbcorMat <- cor(fb[,c(8:18)])
fbev <- eigen(fbcorMat)
fbev$values # VALUES
fbev$vectors # VECTORS


fb.pca<-prcomp(fb[,c(8:18)],scale=T)
fb.pca
summary(fb.pca)
# this explains pc1 has 53.6% varience almost 1/2th of imformation is encapsulated in pc1 and
#similarly 15.5% varience % in pc2 so total 69% in 1st two componets
# we can also consider pc3 which is 14.6%. and neglect remaining
plot(fb.pca)


# to check orthogonal are not - 
fbPC_vectors <- fb.pca$rotation
fbPC1 <- fbPC_vectors[,1]
fbPC2 <- fbPC_vectors[,2]
crossprod(fbPC1, fbPC2)


ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1,  circle = TRUE,varname.size = 5)
# cannot say anything except the correlation b/w the variable at this point with pc1 and pc2 encapsulated data
# comments, share, likes are +ly strongly correlated and with 0 degrees
# similarly enagaed users and people who liked pages and engaged with posts are also +ly strongly corrrested with 0 degrees
# combindedly impressions and consumers/consumptions are not at all correlated with each other with 90 degrees

ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=fb$Type)
# grouped by type. all are overlaped
# the status are distributed along the post consumers and post consumptions
# photos and videos are spreaded all way with vectors.
# links are subset of photos

ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=as.factor(fb$Category))


ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=as.factor(fb$Paid))


ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=as.factor(fb$Post.Weekday))

ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=as.factor(fb$Post.Month))

ggbiplot(fb.pca ,obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE,varname.size = 5, group=as.factor(fb$Post.Hour))


# checking how much to reduce
screeplot(fb.pca, type = "l", npcs = 11, main = "Screeplot of the PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
# Cumulative variance plot with set plot pc 3
cumpro <- cumsum(Glass.pca$sdev^2 / sum(Glass.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="green", lty=5)
abline(v = 3, col="blue", lty=5)
abline(h = 0.9, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)
# if we consider 5 pc's only we can get the cum varience of 90%

# ---------------------------- T SNE------------------------
# using Rtsne
# no duplicates

# without scaling
fb_matrix <- as.matrix(fb[,c(8:18)])
set.seed(42) # Set a seed if you want reproducible results
tsne <- Rtsne(fb_matrix) # Run TSNE
#tsne <- Rtsne(fb[,c(8:18)], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
# Show the objects in the 2D tsne representation
qplot(tsne$Y[,1], tsne$Y[,2], colour = as.factor(fb$Type))
qplot(tsne$Y[,1], tsne$Y[,2], colour = as.factor(fb$Category))
qplot(tsne$Y[,1], tsne$Y[,2], colour = as.factor(fb$Paid))
qplot(tsne$Y[,1], tsne$Y[,2], colour = as.factor(fb$Post.Month))
qplot(tsne$Y[,1], tsne$Y[,2], colour = as.factor(fb$Post.Hour))
qplot(tsne$Y[,1], tsne$Y[,2], colour = as.factor(fb$Post.Weekday))

# by centering and scaling the day. 
preproc.paramfb <- fb %>% preProcess(method = c("center", "scale")) 
transformedfb <- preproc.paramfb %>% predict(fb)
tsne_out <- Rtsne(transformedfb[,c(8:18)],pca=FALSE,perplexity=10, max_iter = 1000, num_threads=6) # Run TSNE
qplot(tsne_out$Y[,1], tsne_out$Y[,2], colour = as.factor(fb$Type))
qplot(tsne_out$Y[,1], tsne_out$Y[,2], colour = as.factor(fb$Post.Weekday))
qplot(tsne_out$Y[,1], tsne_out$Y[,2], colour = as.factor(fb$Post.Month))
qplot(tsne_out$Y[,1], tsne_out$Y[,2], colour = as.factor(fb$Post.Hour))
qplot(tsne_out$Y[,1], tsne_out$Y[,2], colour = as.factor(fb$Paid))
qplot(tsne_out$Y[,1], tsne_out$Y[,2], colour = as.factor(fb$Category))



## ---------------------------- references-------------------------

#https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/ (read and understand)
#https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff
#http://rstudio-pubs-static.s3.amazonaws.com/374116_4d85d22a4e3742ca91a9daaf63a548da.html
#https://www.datacamp.com/community/tutorials/pca-analysis-r references to understand better
#https://www.r-bloggers.com/discriminant-analysis-statistics-all-the-way/
#https://rpubs.com/ifn1411/LDA
#
