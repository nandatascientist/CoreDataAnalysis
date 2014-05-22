############## Read Assignment File ##############

df<-read.table("dataset_390_1.txt")

names(df)<-c('sepallength','sepalwidth','petallength','petalwidth','species')


########### Create Nominal variables  ############
pewinom = rep(0,nrow(df))

for (x in 1:nrow(df)){
        
        class<--1
        currval<-df$petalwidth[x]
        
        if(currval<=0.5){ 
                class<-1
        }else if(currval>0.5& currval<=1.5){
                class<-2
        }else if(currval>1.5& currval<=2){
                class<-3
        }else if(currval>2){
                class<-4
        }
        
        pewinom[x]=class
        
} # created pewinom on Petal Width

specnom <- rep(0,nrow(df))

specnom[which(df$species %in% "I.setosa")]<-1
specnom[which(df$species %in% "I.versicolor")]<-2
specnom[which(df$species %in% "I.virginica")]<-3

# created specnom on species

# Update Dataset

df$pwc<-pewinom
df$spec<-specnom

########### Create contingency tables  ############

ct<-matrix(,nrow=4, ncol=5)

rownames(ct)<-c('I.setosa','I.versicolor','I.virginica','Total')
colnames(ct)<-c('pwc1','pwc2','pwc3','pwc4','Total')

for(i in 1:3){
        
        for (j in 1:4){
                
                ct[i,j]<-length(which(df$spec==i & df$pwc==j))
                
        }
        
}

# compute colsums
for (j in 1:4){
        
        ct[4,j]=sum(ct[1:3,j])
        
}

# compute rowsums

for (i in 1:4){
        
        ct[i,5]=sum(ct[i,1:4])
        
}

########### Create conditional probability  ctables  ############


cpt<-matrix(,nrow=3,ncol=5)

for(i in 1:3){
        
        for (j in 1:5){
                
                cpt[i,j] = ct[i,j]/ct[4,j]
                
        }
        
}
# conditional probabilities  table

########## compute quetelet index ############

quetelet<-matrix(,nrow=3,ncol=4)

for (i in 1:3){
        
        for (j in 1:4){
                     
             quetelet[i,j] = round(
                     (cpt[i,j]-cpt[i,5])/cpt[i,5]
                     ,3) 
                     
        }
        
}

########## compute pearsons index ############ 

totalobs<-ct[4,5]
rft<-ct/totalobs

pearson<-matrix(,nrow=3,ncol=4)

# compute pearsons index
for (i in 1:3){
        
        for (j in 1:4){
                
                numerator<- rft[i,j]- (rft[i,5]*rft[4,j])
                denominator<- sqrt(rft[i,5]*rft[4,j])
                
                pearson[i,j] = round(numerator/denominator,3)
                
        }
        
}

print("Quetelet:")
print(quetelet)
print("Pearson:")
print(pearson)
