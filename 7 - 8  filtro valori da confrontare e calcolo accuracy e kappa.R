library(dplyr) 
rm(list = ls())
filecluster<- "cluster_6_all_features_enriched"
# Caricare entrambi i file
df2 <- read.csv(file="./Prevalence assessment across species High medium low experts.csv", header=TRUE, sep=",") # Dataset principale
df1 <- read.csv(file=paste0("./",filecluster,".csv"), header=TRUE, sep=",")  # Dataset con i valori da filtrare

# Estrarre i valori unici dalla colonna di riferimento del secondo file
selected_values1 <- unique(df2$Specie)

# Filtrare il primo dataset
df1_filtered <- df1 %>% filter(specie %in% selected_values1)

df1_ordered <- df1_filtered[order(df1_filtered$specie), ]

selected_values2 <- unique(df1_ordered$specie)

df2_filtered <- df2 %>% filter(Specie %in% selected_values2)
df2_ordered <- df2_filtered[order(df2_filtered$Specie), ]


# Salvare il file filtered
write.csv(df1_ordered, paste0("./",filecluster,"_solo_specie_valutate.csv"), row.names = F)

##### calcolo accuracy e Kappa

data <- cbind(df2_ordered[, c("Specie", "commonness_from_experts")], df1_ordered[, c("specie", "clusterunif")])
colnames(data)<-c("specieex","experts","specieclu","clusterunif")
a <- 0
b <- 0
c <- 0
d <-0
for(i in 1:nrow(data))
{
  if(data$specieex[i] != data$specieclu[i]){
    i<- nrow(data)+1
    cat("errore di allineamento \n")
  } else{
    if(data$clusterunif[i]=="high" && data$experts[i]=="high") a <- a + 1  
    if(data$clusterunif[i]=="high" && data$experts[i]!="high") b <- b + 1    
    if(data$clusterunif[i]!="high" && data$experts[i]=="high") c <- c + 1    
    if(data$clusterunif[i]!="high" && data$experts[i]!="high") d <- d + 1 
  }
}
N <- a+b+c+d
accuracy <- (( a + d)/N)*100
cat(paste0("accuracy = ",accuracy,"% \n"))

Po <- (a+d)/N
Pe <-((a + b) * (a + c) + (c + d) * (b + d)) / N^2
kappa <- (Po - Pe) / (1 - Pe)
cat(paste0("kappa = ",kappa,"\n"))
write.csv(data, paste0("./expert_Vs_workflow.csv"), row.names = F)