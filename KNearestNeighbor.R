

#K Nearest Neighbor Algorithm

#Install packages to run kNN

library(class)

#Creating a dataset
new <- c(0.05,0.25)
A <- c(0.0467,0.2471)
B <- c(0.0533,0.1912)
C <- c(0.0917,0.2794)
data <-rbind(A,B,C)
dimnames(data) <- list(c("Dark","Medium","Medium"),c("Age (MMN)", "Na/K (MMN)"))
#Declare true classification of A, B and C.
trueclass <-c("Dark","Medium","Medium")

#Run kNN
knn(data,new,cl=trueclass,k=3,prob = TRUE)

#Calculate the Euclidean Distance
install.packages("fields")
library(fields)
together <- rbind(new,data)
#The top row of the rdist are the distance values from the New rdist(together)
rdist(together)


#Stretch the axes
ds_newA <- sqrt((new[1]-A[1])^2 +(3*(new[2]-A[2]))^2)
ds_newB <- sqrt((new[1]-B[1])^2 +(3*(new[2]-B[2]))^2)
ds_newC <- sqrt((new[1]-C[1])^2 +(3*(new[2]-C[2]))^2)


distance<-c(ds_newA,ds_newB,ds_newC)
BP<-c(120,122,130)
data<-cbind(BP,data,distance)
data

#Weights-inverse of distance
weights<-(1/(distance)^2)
sum_wi<-sum(weights)
sum_wiyi<-sum(weights*data[,1])
yhat_new<-sum_wiyi/sum_wi
yhat_new
