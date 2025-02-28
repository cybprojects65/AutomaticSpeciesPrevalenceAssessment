rm(list = ls())
index<-6
stfull<-read.csv(paste0("./centroid_classification_assignment_6_all_features.csv"),header = TRUE, sep=",")
stfull$distance<-0
cluster <-1
for(cluster in 1:index){
st<-stfull[stfull$cluster == cluster,]
st$A <-as.numeric(st$A)
meanA<-mean(st$A)
st$Aloc <- as.numeric(st$Aloc)
meanAloc<-mean(as.numeric(st$Aloc))
st$IntraDO<-as.numeric(st$IntraDO)
meanIntraDO <-mean(st$IntraDO)
st$InterDO<-as.numeric(st$InterDO)
meanInterDO <-mean(st$InterDO)
st$E <- as.numeric(st$E)
meanE <-mean(st$E)
st$TR <- as.numeric(st$TR)
meanTR <-mean(st$TR)
st$TRMO<-as.numeric(st$TRMO)
meanTRMO <-mean(st$TRMO)

minspecies<-st$specie[1]
mindistance <- (meanA-st$A[1])^2+(meanAloc-st$Aloc[1])^2+(meanIntraDO-st$IntraDO[1])^2+(meanInterDO-st$InterDO[1])^2+(meanE-st$E[1])^2+(meanTR-st$TR[1])^2+(meanTRMO-st$TRMO[1])^2
#mindistance <- (meanAloc-st$aloc[1])^2+(meanIntraDO-st$intrado[1])^2+(meanInterDO-st$interdo[1])^2+(meanE-st$e[1])^2+(meanTR-st$tr[1])^2+(meanTRMO-st$trmo[1])^2
#mindistance <- (meanA-st$A[1])^2+(meanIntraDO-st$IntraDO[1])^2+(meanInterDO-st$InterDO[1])^2+(meanE-st$E[1])^2+(meanTR-st$TR[1])^2+(meanTRMO-st$TRMO[1])^2


i<-1
for(i in 1:nrow(st)){
  distance<-(meanA-st$A[i])^2+(meanAloc-st$Aloc[i])^2+(meanIntraDO-st$IntraDO[i])^2+(meanInterDO-st$InterDO[i])^2+(meanE-st$E[i])^2+(meanTR-st$TR[i])^2+(meanTRMO-st$TRMO[i])^2
  #distance<-(meanAloc-st$aloc[i])^2+(meanIntraDO-st$intrado[i])^2+(meanInterDO-st$interdo[i])^2+(meanE-st$e[i])^2+(meanTR-st$tr[i])^2+(meanTRMO-st$trmo[i])^2
  #distance<-(meanA-st$A[i])^2+(meanIntraDO-st$IntraDO[i])^2+(meanInterDO-st$InterDO[i])^2+(meanE-st$E[i])^2+(meanTR-st$TR[i])^2+(meanTRMO-st$TRMO[i])^2
  
  stfull[stfull$specie==st$specie[i],]$distance <- distance
  
  st$distance[i]<-distance
  if(distance < mindistance){
    mindistance<-distance
    minspecies<-st$specie[i]
  }
}
cat(paste0("centroide ",cluster," (",meanA,",",meanAloc,",",meanIntraDO,",",meanInterDO,",",meanE,",",meanTR,",",meanTRMO,")\n"))
#cat(paste0("centroid(",meanAloc,",",meanIntraDO,",",meanInterDO,",",meanE,",",meanTR,",",meanTRMO,")\n"))
#cat(paste0("centroid(",meanA,",",meanIntraDO,",",meanInterDO,",",meanE,",",meanTR,",",meanTRMO,")\n"))


cat(paste0("elemento piu vicino al centroide ",minspecies,"\n"))
stfull[nrow(stfull)+1,]<-c(paste0("centroide ",cluster),meanA,meanAloc,meanIntraDO,meanInterDO,meanE,meanTR,meanTRMO,cluster,0)
#stfull[nrow(stfull)+1,]<-c(paste0("centroide ",cluster),meanAloc,meanIntraDO,meanInterDO,meanE,meanTR,meanTRMO,cluster,"centroid",0)
#stfull[nrow(stfull)+1,]<-c(paste0("centroide ",cluster),meanA,meanIntraDO,meanInterDO,meanE,meanTR,meanTRMO,cluster,"centroid",0)
}

write.csv(stfull,paste0("cluster_",index,"_all_features_enriched.csv"),row.names=FALSE)

#write.csv(st,paste0("clustering All features minus Aloc/cluster ",index," enriched.csv"),row.names=FALSE)

