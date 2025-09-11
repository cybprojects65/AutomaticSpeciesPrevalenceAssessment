################################################################################
#####                        VAE with Java call                            #####
################################################################################

################################################################################
#####                        Training                                      #####
################################################################################

tag <-"all_features"

input_file_path <-paste0("Xmean_all_features_7.csv")
variable_names<- "a,aloc,intrado,interdo,e,tr,trmo"
#variable_names<- "a,aloc,intrado,interdo,e,tr"
valutazione <- TRUE

number_of_hidden_nodes <- 5
number_of_epochs <- 1000
output_folder <- paste0("./out_test/")
model_folder <- paste0("./out_test/")
number_of_reconstruction_samples <- 16
trained_model_file<-paste0(model_folder,"model_norm_348X6_c19a22917732839124215b25d28f20330430618e24018523#6.bin")
training_mode_active<-"false"


if(training_mode_active=="true"){


command_training<-paste0("java -cp vae.jar it.cnr.anomaly.JavaVAE -i\"./",input_file_path,"\" -v\"",variable_names,"\" -o\"",output_folder,"\" -h",number_of_hidden_nodes," -e",number_of_epochs," -r",number_of_reconstruction_samples," -t",training_mode_active)


VAU_execution_train<-system(command_training, intern = TRUE,
                               ignore.stdout = FALSE, ignore.stderr = FALSE,
                               wait = TRUE, input = NULL, show.output.on.console = TRUE,
                               minimized = FALSE, invisible = TRUE)


execution_train_success<-(length(which(grepl(pattern="OK VAU Training",x=VAU_execution_train)))>0)
log_file <- paste0(output_folder,"log_file_training.txt")
writeLines(VAU_execution_train, log_file)
}else{

################################################################################
#####                           Test                                       #####
################################################################################



command_test <- paste0("java -cp vae.jar it.cnr.anomaly.JavaVAE -i\"./",input_file_path,"\" -v\"",variable_names,"\" -o\"",output_folder,"\" -r",number_of_reconstruction_samples," -t",training_mode_active," -m\"",trained_model_file,"\"")
                       

VAU_execution_test<-system(command_test, intern = T,
                            ignore.stdout = FALSE, ignore.stderr = FALSE,
                            wait = TRUE, input = NULL, show.output.on.console = TRUE,
                            minimized = FALSE, invisible = TRUE)

execution_train_success<-(length(which(grepl(pattern="OK VAU Test",x=VAU_execution_test)))>0)
log_file <- paste0(output_folder,"log_file_test.txt")
writeLines(VAU_execution_test, log_file)

################################################################################
#####                           assessment                                 #####
################################################################################


file_pattern <- "classification_test_"
files <- list.files(path = output_folder, pattern = paste0("^", file_pattern))
if (length(files) == 1) {
  file_path <- file.path(output_folder, files[1])
  data_projected <- read.csv(file_path,header = TRUE)
} else {
  cat("file not found")
}
namelist<- unlist(strsplit(variable_names, split = ","))
data_projected_rdx <- data_projected[,namelist]

data_input<-read.csv(input_file_path,header = TRUE)
data_input <- data_input[,namelist]

difference_vector <- data_projected_rdx - data_input
difference_vector_vector <- unlist(difference_vector)
difference_vector_numeric <- as.numeric(difference_vector_vector)
error <- mean((as.numeric(difference_vector_numeric))^2)


rec_prob_avg<-mean(data_projected$reconstruction_log_probability)
cat(paste0("error =",error,", average probability recostruction =",rec_prob_avg),"\n")
}