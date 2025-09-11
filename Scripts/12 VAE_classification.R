VAE_data<-read.csv("./Eval/VAE_second_eval_Xmeans.csv")
log_prob<-VAE_data$reconstruction_log_probability
inv_log_prob<--1*log_prob

lowbound<-as.numeric(quantile(inv_log_prob)[3])
uppbound<-as.numeric(quantile(inv_log_prob)[4])

VAE_data$clusterunif<-"medium"

VAE_data$clusterunif[which(inv_log_prob<lowbound)]<-"low"

VAE_data$clusterunif[which(inv_log_prob>uppbound)]<-"high"

write.csv(x=VAE_data,row.names = F,file="./Eval/VAE_t.csv")