
data(iris)

a<- hist(iris$Sepal.Length,breaks=15)
a$breaks
a<-c(4.2,5.2,5.8,6.4,8.0) # corresponds to minima of Sepal Lengths

test<-hist(iris$Sepal.Width,breaks=20)
test$breaks
b<-c(2.0,2.3,2.8,3.2,3.9)

########### Create Nominal variables  ############
testdf<-iris
g = rep(0,nrow(testdf))

for (k in 1:4){
        
        idx<-which(testdf$Sepal.Length>=a[k]&testdf$Sepal.Length<a[k+1])
        g[idx]=k
        
} # created nominal g on Sepal Length

h = rep(0,nrow(testdf))
for (k in 1:4){
        
        idx<-which(testdf$Sepal.Width>=b[k]&testdf$Sepal.Width<b[k+1])
        h[idx]=k
        
} # created nominal h on Sepal Width

t <- rep(0,nrow(testdf))

for (k in 1:3){
        
        f1<-(k-1)*50
        f2<-k*50
        
        t[f1:f2]<-k
        
} # created nominal t on species assuming they are arranged in order in df


########### Update Dataset  ############

testdf$g<-g
testdf$h<-h
testdf$t<-t

########### Create contingency tables  ############

ct<-matrix(,nrow=4, ncol=5)
rownames(ct)<-c('T1','T2','T3','Total')
colnames(ct)<-c('g1','g2','g3','g4','Total')

ct1<-matrix(,nrow=4, ncol=5)
rownames(ct1)<-c('T1','T2','T3','Total')
colnames(ct1)<-c('h1','h2','h3','h4','Total')



for(i in 1:3){
        
        for (j in 1:4){
                
                ct[i,j]<-length(which(testdf$t==i & testdf$g==j))
                
        }
        
}

# compute rowsums
for (j in 1:4){
        
        ct[4,j]=sum(ct[1:3,j])
        
}

# compute colsums

for (i in 1:4){
        
        ct[i,5]=sum(ct[i,1:4])
        
}

############

for(i in 1:3){
        
        for (j in 1:4){
                
                ct1[i,j]<-length(which(testdf$t==i & testdf$h==j))
                
        }
        
}

# compute rowsums
for (j in 1:4){
        
        ct1[4,j]=sum(ct1[1:3,j])
        
}

# compute colsums

for (i in 1:4){
        
        ct1[i,5]=sum(ct1[i,1:4])
        
}



