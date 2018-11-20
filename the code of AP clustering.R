#file-change work path-select the current folder

# download packages that we need
library(apcluster)	

# import input measure matrix for each type of lung cancer, ADsimilarity represents the measure matrix of AD class
ADsimilarity=read.csv("ADsimilarity.csv",header=F)  
dim(ADsimilarity)
ADsimilarity<-as.matrix(ADsimilarity)   

# run affinity propagation
apclusterK(ADsimilarity, K=2, maxits=1000, convits=100, lam=0.9, verbose=TRUE)
# apclusterK can show the number of clusters, and we can get the appropriate number of clusters by setting the P value
apres1<-apcluster(ADsimilarity, p= -1.686357) 

# show results
apres1

#plot the three performance measures that AP uses internally for each iteration
apres1b<- apcluster(ADsimilarity, p= -1.681747, details=TRUE)
plot(apres1b)

# plot heatmap
heatmap(apres1b,ADsimilarity)

# save clustering results
a<-labels(apres1, type="enum")
a<-as.matrix(a)
datout2=data.frame(a)
write.table(datout2, file="ADfenqun.csv", sep=",", row.names=F)
