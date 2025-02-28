library("sf")
polygons<-read.csv("Polygon siti protetti.csv",header = TRUE, sep=",")
species<-read.csv("occurrences_gbif_lake_massaciuccoli_2013_2023_coordinates_complete_cleaned_cut_aggregated.csv",header = TRUE, sep=",")
wktgeometries<-st_as_sf(polygons,wkt="WKT")
input_folder<-"Occorrenze"


i<-1
j<-1

for(i in 1:nrow(species)){ 
  
cat(paste0("cutting specie ",species$species[i]," number ",i,"\n"))
global<-read.csv(paste0("./",input_folder,"/",species$species[i],"_in_BB.csv"),header = TRUE, sep=",")
  
global$decimalLatitude<-round(global$decimalLatitude, digits = 5)
global$decimalLongitude<-round(global$decimalLongitude, digits = 5)

outputdir <- paste0("./",input_folder,"/",species$species[i])

if (!dir.exists(outputdir))
 {
  dir.create(outputdir)
  
}
  
global$points_wkt<-0
global$points_wkt<-paste0("POINT (",global$decimalLongitude," ",global$decimalLatitude,")")
point_geometries<-st_as_sfc(global$points_wkt)

for(j in 1:nrow(polygons)){
intersection<-c()
intersection<-st_as_text(st_intersection(st_make_valid(wktgeometries$WKT[j]),point_geometries))

extracted<- global[is.element(global$points_wkt, intersection),]
write.csv(extracted,paste0("./",input_folder,"/",species$species[i],"/",species$species[i],"_in_",wktgeometries$nome_gazze[j],".csv"),row.names=FALSE)
}}