#file-change work path-select the current folder
# download R packages that we need
library(SGL)                      
#Import data, lungcancerdata4x represents the expanded input matrix, newy1, newy2, newy3, newy4 represents new lable vector,groupindex represents the grouping order
x<-read.csv('lungcancerdata4x.csv',header=F)
y1<-read.csv('newy1.csv',header=F)
y2<-read.csv('newy2.csv',header=F)
y3<-read.csv('newy3.csv',header=F)
y4<-read.csv('newy4.csv',header=F)
b<-read.csv('groupindex.csv',header=F)
x<-as.matrix(x)
y1<- as.matrix(y1)
y2<- as.matrix(y2)
y3<- as.matrix(y3)
y4<- as.matrix(y4)
b<-as.matrix(b)
b<-c(b)

#we randomly divide the data set into two parts: training set and test set according to the strategic dividing method. The following is one of random dividing methods. 
train<-x[c(1:93,140:150,157:170,178:190),]
test<-x[-c(1:93,140:150,157:170,178:190),]

# training data 
X<-train
data1<-list(x = X, y = y1)
data2<-list(x = X, y = y2)
data3<-list(x = X, y = y3)
data4<-list(x = X, y = y4)

####solveSGL1
cv1= cvSGL(data1, b, type = "linear", maxit = 500, thresh = 0.001, min.frac = 0.01, nlam = 50, gamma = 0.8, nfold =10,standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)  
plot(cv1)   
 c<-cv1$lldiff
c<-as.matrix(c)
m<-min(c)
h<- which(c==m)
Fit1<- SGL(data1, b, type = "linear", maxit = 500, thresh = 0.001, min.frac = 0.01, nlam = 50, gamma = 0.5, standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
# extract non-zero coefficients
coefSGL1 <-Fit1$beta
n1<- which(coefSGL1[,h]!= 0)   
# make prediction
y1<-predictSGL(Fit1,test,h)
y1<-sign(y1-0.5)
y1[y1==-1]<-0
# expanding y1 into matrix p1
a<-c(rep(0,66))
a[y1==0]<-1
p1<-cbind(y1,a,a,a)
p1<-as.matrix(p1)

####solveSGL2
cv2= cvSGL(data2, b, type = "linear", maxit = 500, thresh = 0.001,  min.frac = 0.01, nlam = 50, gamma = 0.8, nfold =10,standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
plot(cv2)
c<-cv2$lldiff
c<-as.matrix(c)
m<-min(c)
h<- which(c==m)
Fit2<- SGL(data2, b, type = "linear", maxit = 500, thresh = 0.001,  min.frac = 0.01, nlam = 50, gamma = 0.8, standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
# extract non-zero coefficients
coefSGL2 <-Fit2$beta
n2<- which(coefSGL2[,h]!= 0)  
# make prediction
y2<-predictSGL(Fit2,test,h)
y2<-sign(y2-0.5)
y2[y2==-1]<-0
# expanding y2 into matrix p2
a<-c(rep(0,66))
a[y2==0]<-1
p2<-cbind(a,y2,a,a)
p2<-as.matrix(p2) 

####solveSGL3
cv3= cvSGL(data3, b, type = "linear", maxit = 500, thresh = 0.001,  min.frac = 0.01, nlam = 50, gamma = 0.8, nfold =10,standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
plot(cv3)
c<-cv3$lldiff
c<-as.matrix(c)
m<-min(c)
h<- which(c==m)
Fit3<- SGL(data3, b, type = "linear", maxit = 500, thresh = 0.001,  min.frac = 0.01, nlam = 50, gamma = 0.8, standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
# extract non-zero coefficients
coefSGL3 <-Fit3$beta
n3<- which(coefSGL3[,h]!= 0)  
# make prediction
y3<-predictSGL(Fit3,test,h)
y3<-sign(y3-0.5)
y3[y3==-1]<-0
# expanding y3 into matrix p3
a<-c(rep(0,66))
a[y3==0]<-1
p3<-cbind(a,a,y3,a)
p3<-as.matrix(p3)


####solveSGL4
cv4= cvSGL(data4, b, type = "linear", maxit = 500, thresh = 0.001,  min.frac = 0.01, nlam = 50, gamma = 0.8, nfold =10,standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
plot(cv4) 
c<-cv4$lldiff
c<-as.matrix(c)
m<-min(c)
h<- which(c==m)
Fit4<- SGL(data4, b, type = "linear", maxit = 500, thresh = 0.001,  min.frac = 0.01, nlam = 50, gamma = 0.8, standardize = TRUE, verbose = FALSE, step = 1, reset = 10, alpha = 0.8, lambdas = NULL)
# extract non-zero coefficients
coefSGL4<-Fit4$beta
n4<- which(coefSGL4[,h]!= 0)  
# make prediction
y4<-predictSGL(Fit4,test,h)
y4<-sign(y4-0.5)
y4[y4==-1]<-0
# expanding y4 into matrix p4
a<-c(rep(0,66))
a[y4==0]<-1
p4<-cbind(a,a,a,y4)
p4<-as.matrix(p4)

####obtain voting matrix 
p<-p1+p2+p3+p4
dimnames(p)<-list(1:66,1:4)
#voting
l<-apply(p, 1, function(t) colnames(p)[which.max(t)])
l<-as.numeric(l)
l<-as.matrix(l)   
#save results
write.table(l, file="first8.csv", sep=",", row.names=F)
