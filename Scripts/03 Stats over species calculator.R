library(dplyr)

TRMOThreeshold<-5
digitsapp<-3
startobbyear<-2013
endobbyear<-2023
polygons<-read.csv("Polygon siti protetti.csv",header = TRUE, sep=",")
species<-read.csv("occurrences_gbif_lake_massaciuccoli_2013_2023_coordinates_complete_cleaned_cut_aggregated.csv",header = TRUE, sep=",")
occurrence_folder <- "Occorrenze"
stats <- data.frame(specie=character(), A=numeric(), Aloc=numeric(),IntraDO=numeric(), InterDO=numeric(), E=numeric(), TR=numeric(), TRMO=numeric())



#for any species
for(j in 1:nrow(species)){
cat(paste0("elaborationg species ",species$species[j]," number ",j,"\n"))
totalocc<-0
totalarea<-0
areaswspecie<-0
distinctspot<-0
totalobservedyears<-0
totalyearswithenoughobb<-0
avgE<-0
avgA<-0
avgTR<-0
avgTRMO<-0
avgAloc<-0
#for any polygons
for(i in 1:nrow(polygons)){
#cat(paste0(" in ",polygons$nome_gazze[i],"\n")) 
current<-read.csv(paste0("./",occurrence_folder,"/",species$species[j],"/",species$species[j],"_in_",polygons$nome_gazze[i],".csv"),header = TRUE, sep=",")

totalocc<-totalocc+nrow(current)
totalarea<-totalarea+polygons$area_mq[i]
if(nrow(current)>0) {
areaswspecie<- areaswspecie+1 
yearsonly<-current[,c("year")]
yearsonlynodup<-unique(yearsonly)
observedyears<-length(yearsonlynodup)
totalobservedyears<-totalobservedyears+observedyears
onlycoordinates<-as.data.frame(current[,c("decimalLatitude","decimalLongitude")])
onlycoordinates$decimalLatitude<-round(onlycoordinates$decimalLatitude, digits = digitsapp)
onlycoordinates$decimalLongitude<-round(onlycoordinates$decimalLongitude, digits = digitsapp)
onlycoordinates2 <- onlycoordinates %>% distinct()
distinctspot<- distinctspot + nrow(onlycoordinates2)
yearswithenoughobb<-0
for(year in startobbyear:endobbyear){
 if(length(yearsonly[yearsonly == year])>TRMOThreeshold){
   yearswithenoughobb<-yearswithenoughobb+1
 }
}
totalyearswithenoughobb<-totalyearswithenoughobb+yearswithenoughobb
avgE<-avgE+(nrow(onlycoordinates2)*10000/polygons$area_mq[i])
avgTR<-avgTR+(observedyears/(endobbyear-startobbyear ))
avgTRMO<-avgTRMO+(yearswithenoughobb/(endobbyear-startobbyear ))
avgAloc<-avgAloc+nrow(current)/polygons$area_mq[i]
}
avgA<-avgA+nrow(current)/polygons$area_mq[i]



  } #end for over polygons
Aloc<-avgAloc/areaswspecie
A<-avgA/nrow(polygons)
IntraDO<-totalocc/nrow(polygons)
InterDO<-areaswspecie/nrow(polygons)
ncells<-totalarea/10000
E<- avgE/areaswspecie
TR<-avgTR/areaswspecie 
TRMO<-avgTRMO/areaswspecie 
stats[nrow(stats) + 1,] <- c(species$species[j], A, Aloc, IntraDO,InterDO,E,TR,TRMO)

} #end for over species
write.csv(stats,"Stat species.csv",row.names=FALSE)
