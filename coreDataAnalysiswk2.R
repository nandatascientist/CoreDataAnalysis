##############  Week #2 1D analysis

data(iris)
hist(iris[,1],30) 


##### MATHEMATICAL STATISTICS WAY OF COMPUTING MEANS WITH 95% CONFIDENCE

a_obs<-mean(iris[,1]) # 5.843333
sd_obs<-sd(iris[,1]) # 0.8280661
sd_mathstat<-sd_obs/sqrt(nrow(iris))

math_stat_mean_95p<-c(a_obs-1.96*sd_mathstat,a_obs+1.96*sd_mathstat)
# the above calculates the range of means with 95% confidence using 
# mathematical Statistics approach


##### BOOTSTRAPPING TO COMPUTE MEANS WITH 95% CONFIDENCE



irisone<-iris[,1]
sampleIndex<-matrix(0,nrow=150,ncol=5000)

for (i in 1:5000){
        
        sampleIndex[,i]<- sample(1:150,150,replace=TRUE)
        
}

sampleValues<-matrix(0,nrow=150,ncol=5000)

for (i in 1:5000){
        
        sampleValues[,i]<- irisone[sampleIndex[,i]]
        
}

sampleMeans<-colMeans(sampleValues)


### WAY #1 PIVOTAL  METHOD

avg_sampleMeans<-mean(sampleMeans)
sd_sampleMeans<-sd(sampleMeans)
bootstrap_pivotal_mean_95p<-c(avg_sampleMeans-1.96*sd_sampleMeans,avg_sampleMeans+1.96*sd_sampleMeans)

### WAY #2 NON-PIVOTAL  METHOD
sortedSampleMeans<-sort(sampleMeans)

bootstrap_nonpivotal_mean_95p<-c(sortedSampleMeans[126],sortedSampleMeans[4875])
