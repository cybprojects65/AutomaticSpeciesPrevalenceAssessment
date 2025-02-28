rm(list = ls())
library(mclust)
options(warn = -1)

rm(list = ls())
data_raw <- read.csv(file="./Stat species.csv", header=TRUE, sep=",")
data_raw <- na.omit(data_raw)

data_wo_species <- data_raw[, -c(1)]

data_means <-apply(data_wo_species, 2, mean)
data_sd <-apply(data_wo_species, 2, sd)

std_med <- as.data.frame(data_wo_species)
colnames(std_med) <- colnames(data_wo_species)

for (column in 1:ncol(data_wo_species)) {
  for (riga in 1:nrow(data_wo_species)) {
    std_med[riga,column] <- (data_wo_species[riga,column] - data_means[column]) / data_sd[column]
  }
  
}

specie <- data_raw$specie


data <- data.frame(specie, std_med)

write.csv(data, "./Stat species_std.csv", row.names = F)





# Selection features

selected_features<-c("specie","A","Aloc","IntraDO","InterDO","E","TR","TRMO")
#selected_features<-c("specie","A","IntraDO","InterDO","E","TR","TRMO")

cat(paste0("***Initialization***", "\n"))

#multi_centroidi<-seq(from=2, to=12, by=1)
multi_centroidi<-  c(6)
N <- 30

bics<-c()
for (n_centroidi in multi_centroidi){
  
  selected_features_coords <- data[,selected_features]
  # Study of centroids
  cat(paste0("Study of centroids", "\n"))
  # Prepare the dataset
  v <- as.data.frame(selected_features_coords[,2:ncol(selected_features_coords)])
  
  cat("####I'm analyzing ",n_centroidi,"centroids\n")
  
  # Create an EMPTY matrix of centroids to be filled later
  centroidi <- matrix(nrow=n_centroidi, ncol=ncol(v))

  for (centroide in 1:nrow(centroidi)) {
      
    centroidi[centroide,] <- as.numeric(v[centroide,])
    
  }



# Empty vector to be filled with the assignment of the centroid for each row of v
d <- numeric(length = nrow(v))

v$distance_class <- NA
prev_centr_distr<-rep(0,n_centroidi)
for (k in 1:N) {
  cat(paste0("I am executing loop number ", k, "\n"))
  
  cat(paste0("***Assignment***", "\n"))
  
  for (punto in 1:nrow(v)) {
    
    distanze<-sapply(1:n_centroidi, function(centroide){
      
      d_vi_centroide = sqrt( sum ((v[punto,1:(ncol(v)-1)]-centroidi[centroide,])*(v[punto,1:(ncol(v)-1)]-centroidi[centroide,])) )
      return(d_vi_centroide)
      
    },simplify = T)
    
    d[punto] <- which(distanze == min(distanze))[1]  # Index of the smallest value to assign to each point
  }
  
  cat(paste0("***FILE***", "\n"))
  
  # Assign the column with the centroid value to the original dataset.
  selected_features_coords$distance_class <- d
  
  v$distance_class <- d
  
  # Empty matrix to store the averages
  v_medie <- matrix(0,nrow = nrow(centroidi), ncol = ncol(centroidi))
  centroid_distribution<-c()
  for (centroide in 1:nrow(centroidi)) {
    cat(paste0("---- I am examining the centroid ", centroide, "\n"))
    # Take the rows with the centroid of interest: rows = the row of the centroid, columns = v minus the column with the centroid number
    v_centroide <- v[v$distance_class==centroide, -which(colnames(v)=="distance_class")]
    v_centroide<-as.matrix(v_centroide)
    if (nrow(v_centroide)==0){
      cat(paste0("The points assigned to the centroid ", centroide, " are ", nrow(v_centroide), "\n"))
      v_medie[centroide,]<-centroidi[centroide,]
      centroid_distribution<-c(centroid_distribution,0)
    }else{
      # Calculate the mean on this new set of data selected for the centroid.
      cat(paste0("The points assigned to the centroid ", centroide, " are ", nrow(v_centroide), "\n"))
      medie_centroide<-apply(v_centroide,2,mean)
      v_medie[centroide,] <- medie_centroide
      centroid_distribution<-c(centroid_distribution,nrow(v_centroide))
    }
  }
  cat(paste0("***Update***", "\n"))
  centroidi <- v_medie
  
  if (length(which(centroid_distribution!=prev_centr_distr))==0){
    cat(paste0("***Convergence***", "\n"))
    break
  }else{
    prev_centr_distr<-centroid_distribution
  }
}

centroidi_df<-as.data.frame(centroidi)
names(centroidi_df) <- names(v)[1:(length(v)-1)]

ks.test(centroid_distribution, "punif")

#############


# Quantiles
v_quantili <- apply(v, 2, quantile)

# Prepare the centroids matrix with "M" for medium
centroidi_labelled <- matrix("M", nrow=nrow(centroidi), ncol=ncol(centroidi))

# Filling labeled centroids
for (centroide in 1:nrow(centroidi)) {
  for (feat in 1:(ncol(v)-1)) {
    if (centroidi[centroide,feat]<v_quantili[3,feat]){
      centroidi_labelled[centroide, feat] <- "L"
    }
    else if (centroidi[centroide,feat]>v_quantili[4,feat]) {
      centroidi_labelled[centroide, feat] <- "H"
    }
    
  }
  
}






cat("Saving the dataset\n")
nuovo_v <- cbind(selected_features_coords[,1],v)
names(nuovo_v)<-c(c(selected_features,"cluster"))
output_file<-paste0("./Clustering all features/centroid_classification_assignment_",n_centroidi,".csv")
write.csv(nuovo_v, output_file, row.names = F)

#### CALCULATING ChiSqr
if (length(which(centroid_distribution<=2))>0 || 
    ( (min(centroid_distribution)/max(centroid_distribution) ) <0.007) 
    ){
  cat("Unsuitable distribution: low uniformity:",(min(centroid_distribution)/max(centroid_distribution))," --- outliers: ",length(which(centroid_distribution<=2)),"\n")
  bic<-0
}else{
  centroid_distribution.norm<-centroid_distribution/sum(centroid_distribution)
  reference<-rep(mean(centroid_distribution),length(centroid_distribution) )
  reference.norm<-reference/sum(reference)
  chi<-chisq.test(centroid_distribution.norm, p = reference.norm)
  bic<-chi$p.value
}
  cat("ChiSqr:",bic,"\n")
  bics<-c(bics,bic)
  cat("Done\n")
}

best_clusterisation<-multi_centroidi[which(bics == max(bics))]
cat("Ks: ",multi_centroidi,"\n")
cat("ChiSQRs: ",bics,"\n")
cat("Best clustering: K=",best_clusterisation,"\n")
best_clusterisation_file = paste0("./centroid_classification_assignment_",best_clusterisation,".csv")
cat("Best clustering file to take as result:",best_clusterisation_file,"\n")
centroidi_labelled_ds<-as.data.frame(centroidi_labelled)
names(centroidi_labelled_ds)<-selected_features[-1]
centroidi_labelled_ds$cluster<-c(1:n_centroidi)
centroidi_labelled_ds <- centroidi_labelled_ds[, c(ncol(centroidi_labelled_ds), 1:(ncol(centroidi_labelled_ds) - 1))]
output_file_c<-paste0("./Clustering all features/centroid_labelled_",n_centroidi,".csv")

write.csv(centroidi_labelled_ds, output_file_c, row.names = F)