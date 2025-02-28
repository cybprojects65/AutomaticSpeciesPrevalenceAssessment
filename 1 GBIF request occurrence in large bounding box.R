library("rgbif")
library("sf")

outputfolder <- "./Occorrenze/"

if (!dir.exists(outputfolder)) {
  dir.create(outputfolder, recursive = TRUE)
}

species<-read.csv("codici specie.csv",header = TRUE, sep=",")
year <- 2013
j<-1
i<-1


poligon <- "MULTIPOLYGON (((19.51 47.46, 6.41 47.46, 6.41 36.21, 19.51 36.21, 19.51 47.46)))"
for(j in  1:nrow(species)){
  cat(paste0("I retrieve data of species ",species$species[j]," with code ", species$code[j], "\n"))

  gbif_download <- occ_download(pred("taxonKey", species$code[j]),format = "SIMPLE_CSV",user="pasquale.bove",pwd="F5RynWe.a$vk*2S",email="pasquale.bove@cnr.it",pred_gt("year", year),pred_within(poligon))
  occ_download_wait(gbif_download)
  d <- occ_download_get(gbif_download,overwrite = TRUE) %>% occ_download_import()
  write.csv(d,paste0(outputfolder,species$species[j],"_in_BB.csv"),row.names=FALSE)
  
}

