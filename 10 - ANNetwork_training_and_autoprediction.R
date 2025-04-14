rm(list=ls(all=TRUE))
library(neuralnet)
input_file<-"Xmean_all_features_7"

data = read.csv(paste0("./Eval/",input_file,".csv"), header=TRUE, sep=",")

tag <-"all_feature"
#hard learning example
training_features<-c("a","aloc","intrado","interdo","e","tr","trmo")
#training_features<-c("aloc","intrado","interdo","e","tr")

target_features<-c("prevalence")
#hidden_neurons_per_layer<-c(2)


#rule of thumb: start from 2*n+1
#hidden_neurons_per_layer<-c(20,2) # CRE 27.34
hidden_neurons_per_layer<-c(16,8)

cat("Training with hidden neurons =",paste0(hidden_neurons_per_layer,collapse = ","),"\n")

data_training_features<-(subset(data, select = c(training_features,target_features) ))

# Function to scale a vector between 0 and 1
scale_0_1 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply the scaling function to each column and overwrite the original data frame
data_training_features <- as.data.frame(lapply(data_training_features, scale_0_1))


dependency_target_vs_training<-paste(paste0(target_features,collapse = "+"), "~", paste(training_features, collapse = " + ") )
data_training_features2 <- data_training_features
data_training_features2$specie <- data$specie
cat("Simulation setup =",dependency_target_vs_training,"\n")

f <- as.formula(dependency_target_vs_training)

### Artificial Neural Network Parameters ###
# rp - number of repetitions for the training
rp=10
# thld - threshold for minimum decrease in overall error, default 0.01 = 1%
thld=0.01
# stp - the maximum steps for the training of the neural network, default 1e+05
stp=1e+05
# alg - possible: backprop, rprop+, rprop-, sag, or slr
alg ="backprop"
#alg ="sag"
# act.fct - possible: "logistic" (=sigmoid) or "tanh"; linear.output must be 
act.fct ="logistic"
#learning rate
learningrate=0.01
#number of cross-validation folders
nfold<-10


#train the ANN with the hidden neurons
nn <- neuralnet(f,
                #data = data_training_features,
                data = data_training_features2,
                hidden = hidden_neurons_per_layer, 
                threshold = thld, 
                stepmax = stp, 
                rep = rp,
                learningrate = learningrate,
                act.fct = act.fct, 
                linear.output = FALSE, #activation function will be present also on the output nodes
                lifesign = "minimal", 
                algorithm = alg)


# Compute predictions on the training data
data_selftest_features<-(subset(data_training_features, select = c(training_features) ))



prediction_self<- compute(nn, data_selftest_features)

data_selftest_output<-cbind(data_selftest_features,prediction_self$net.result)
names(data_selftest_output)<-c(training_features,target_features)


#####Similarity calculation for one-output ANN

optimal_similarity<-0

differences<-data_training_features[target_features]-data_selftest_output[target_features]

mse<-sum(differences*differences)/dim(data_training_features[target_features])[1]
smse<-sqrt(mse)
me<-sum(abs(differences))/dim(data_training_features[target_features])[1]

absdiff<-abs( differences )
#delete infinites
absdiff <- absdiff[!apply(absdiff, 1, function(row) any(is.infinite(row))), ]

#mean absolute error
mae<-mean(absdiff)

#return the mean relative error
mre<-mae/mean(unlist(data_training_features[target_features]),na.rm = T)

cat("\nMean Squared Error=",mse,", Mean Error=",me,"\n")
cat("Mean Relative Error=",100*mre,"%\n")

#if nfold>0 do cross validation with 95%-5% approach
if (nfold>0){
  cat("\nCross-validating..\n")
  proportion <- 0.80 # Set to 0.995 for LOOCV
  mses <- NULL
  mes<- NULL
  mres<-NULL
  #for each fold, select a random (95%) subset for training and another (5%) for testing
  for(i in 1:nfold) {
    cat(i," ")
    #random selection of training and testing rows
    index    <- sample(1:nrow(data_training_features), round(proportion*nrow(data_training_features)))
    train_cv <- data_training_features[index, ]
    test_cv  <- data_training_features[-index, ]
    
    if (dim(test_cv)[1]==0)
       test_cv=train_cv
    
    #ANN training
    nn_cv    <- neuralnet(f,data = train_cv,hidden = hidden_neurons_per_layer,threshold = thld,
                          stepmax = stp,rep = rp,act.fct = act.fct,learningrate = learningrate,
                          linear.output = FALSE,
                          algorithm = alg)
    
    test_cv.1<-(subset(test_cv, select = c(training_features) ))
    prediction_test_cv<- compute(nn_cv, test_cv.1)
    prediction_test_cv_output<-cbind(test_cv.1,prediction_test_cv$net.result)
    names(prediction_test_cv_output)<-c(training_features,target_features)
    
    differences_cv<-test_cv[target_features]-prediction_test_cv_output[target_features]
    mse_cv<-sum(differences_cv*differences_cv)/dim(test_cv[target_features])[1]
    me_cv<-sum(abs(differences_cv))/dim(test_cv[target_features])[1]
    
    absdiff_cv<-abs( differences_cv )
    absdiff_cv <- absdiff_cv[!apply(absdiff_cv, 1, function(row) any(is.infinite(row))), ]
    mae_cv<-mean(absdiff_cv)
    mre_cv<-mae_cv/mean(unlist(test_cv[target_features]))
    
    mses[i]  <- mse_cv
    mes[i]   <- me_cv
    mres[i]  <- mre_cv
  }#end loop on folds  
  
  mean_mse_cv <- mean(mses)
  mean_me_cv <- mean(mes)
  mean_mres_cv <- mean(mres)
  cat("\nCrossvalidation MSE =",mean_mse_cv,"(min",min(mses),", max",max(mses),")")
  cat("\nCrossvalidation ME =",mean_me_cv,"(min",min(mes),", max",max(mes),")")
  cat("\nCrossvalidation Relative Error =",100*mean_mres_cv,"%(min",100*min(mres),", max",100*max(mres),")\n\n")
  
}#end nfold>0

write.csv(data_selftest_output, paste0("./Eval/Ann_selftest_on_",input_file,"_with_",tag,".csv"), row.names = F)
write.csv(data_training_features2, paste0("./Eval/Ann_training_features_on_",input_file,"_with_",tag,".csv"), row.names = F)
