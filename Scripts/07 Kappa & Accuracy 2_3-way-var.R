library(dplyr) 
rm(list = ls())


filecluster<-"VAE_wo_e_labeled"
expert <-"Expert_ensemble_assessment"

histogram<- TRUE

# Upload both files
df2 <- read.csv(file=paste0("./",expert,".csv"), header=TRUE, sep=",") 
df1 <- read.csv(file=paste0("./VAE/",filecluster,".csv"), header=TRUE, sep=",")  


selected_values1 <- unique(df2$Specie)
df1_filtered <- df1 %>% filter(specie %in% selected_values1)
df1_ordered <- df1_filtered[order(df1_filtered$specie), ]

selected_values2 <- unique(df1_ordered$specie)
df2_filtered <- df2 %>% filter(Specie %in% selected_values2)
df2_ordered <- df2_filtered[order(df2_filtered$Specie), ]


data <- cbind(df2_ordered[, c("Specie", "commonness_from_experts")], 
              df1_ordered[, c("specie", "clusterunif")])
colnames(data)<-c("specieex","experts","specieclu","clusterunif")

comparisondf <- data

colnames(comparisondf)<-c("specieA","A","specieB","B")

#cat("Calculating the agreement on three categories: Low/Medium/High..\n")
write.csv(data, paste0("./Confronti/",expert,"_Vs_",filecluster,".csv"), row.names = F)

calcPerformance3<-function(A_2_B_2,A_0_B_0,A_1_B_2,A_0_B_2,A_2_B_1,A_1_B_1,A_0_B_1,A_2_B_0,A_1_B_0){
  accuracy<-(nrow(A_2_B_2)+nrow(A_1_B_1)+nrow(A_0_B_0))*100/nrow(comparisondf)
  
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_1_B_1),nrow(A_1_B_0)), 
                         c(nrow(A_0_B_1), nrow(A_0_B_0))))
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_0_B_0),nrow(A_0_B_1),nrow(A_0_B_2)), 
                         c(nrow(A_1_B_0),nrow(A_1_B_1),nrow(A_1_B_2)),
                         c(nrow(A_2_B_0),nrow(A_2_B_1),nrow(A_2_B_2))
  ))
  # Descriptive statistics
  diagonal.counts <- diag(xtab)
  N <- sum(xtab)
  row.marginal.props <- rowSums(xtab)/N
  col.marginal.props <- colSums(xtab)/N
  # Compute kappa (k)
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  cat(filecluster,"\n")
  cat("N=",N,"\n")
  cat("Accuracy=",accuracy,"%\n")
  cat("Kappa=",k,"\n")
}

calcPerformance2<-function(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0){
  accuracy<-(nrow(A_1_B_1)+nrow(A_0_B_0))*100/nrow(comparisondf)
  
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_1_B_1),nrow(A_1_B_0)), 
                         c(nrow(A_0_B_1), nrow(A_0_B_0))))
  # Descriptive statistics
  diagonal.counts <- diag(xtab)
  N <- sum(xtab)
  row.marginal.props <- rowSums(xtab)/N
  col.marginal.props <- colSums(xtab)/N
  # Compute kappa (k)
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  cat("Accuracy=",accuracy,"%\n")
  cat("Kappa=",k,"\n")
}

cat("\nTwo classes:\n")
A_1_B_1=rbind(
  comparisondf[which(comparisondf$A=="high" & comparisondf$B=="high"),],
  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="high"),])

A_0_B_0=rbind(
  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="low"),],
  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="medium"),],
  comparisondf[which(comparisondf$A=="low" & comparisondf$B=="low"),],
  comparisondf[which(comparisondf$A=="low" & comparisondf$B=="medium"),]
)

A_1_B_0=rbind(
  comparisondf[which(comparisondf$A=="high" & comparisondf$B=="low"),],
  comparisondf[which(comparisondf$A=="high" & comparisondf$B=="medium"),]
)

A_0_B_1=comparisondf[which(comparisondf$A=="low" & comparisondf$B=="high"),]

cat("True positives: ",nrow(A_1_B_1),"\n")
cat("True negatives: ",nrow(A_0_B_0),"\n")
cat("False positives: ",nrow(A_0_B_1),"\n")
cat("False negatives: ",nrow(A_1_B_0),"\n")
cat("Uncertain (low vs medium): ",nrow(comparisondf[which(comparisondf$A=="low" & comparisondf$B=="medium"),]),"\n")
cat("Total: ",(nrow(A_1_B_1)+nrow(A_0_B_0)+nrow(A_0_B_1)+nrow(A_1_B_0)),"\n")


calcPerformance2(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0)

cat("\nThree classes:\n")
A_2_B_2=rbind(
  comparisondf[which(comparisondf$A=="high" & comparisondf$B=="high"),],
  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="high"),])

A_0_B_2=comparisondf[which(comparisondf$A=="low" & comparisondf$B=="high"),]
A_2_B_0=rbind(
  comparisondf[which(comparisondf$A=="high" & comparisondf$B=="low"),]
)
A_2_B_1=rbind(
  comparisondf[which(comparisondf$A=="high" & comparisondf$B=="medium"),]
)

A_1_B_1=rbind(
  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="low"),],
  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="medium"),]
#  comparisondf[which(comparisondf$A=="low" & comparisondf$B=="low"),],
#  comparisondf[which(comparisondf$A=="low" & comparisondf$B=="medium"),]
)

  
A_0_B_0=rbind(
#  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="low"),],
#  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="medium"),],
#  comparisondf[which(comparisondf$A=="medium" & comparisondf$B=="high"),],
  comparisondf[which(comparisondf$A=="low" & comparisondf$B=="low"),]
#  comparisondf[which(comparisondf$A=="low" & comparisondf$B=="medium"),]
)

A_0_B_1=rbind(comparisondf[which(comparisondf$A=="low" & comparisondf$B=="medium"),])
  
A_1_B_0 = df_new <- data.frame(lapply(A_1_B_1, function(x) vector(mode = typeof(x))))
A_1_B_2 = df_new <- data.frame(lapply(A_1_B_1, function(x) vector(mode = typeof(x))))


calcPerformance3(A_2_B_2,A_0_B_0,A_1_B_2,A_0_B_2,A_2_B_1,A_1_B_1,A_0_B_1,A_2_B_0,A_1_B_0)



write.csv(data, paste0("./Clustering/",expert,"_Vs_",filecluster,".csv"), row.names = F)
if(histogram){
hist(df1$reconstruction_log_probability, 
     main = "log probability distribution", 
     xlab = "log probability values", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black")
}
