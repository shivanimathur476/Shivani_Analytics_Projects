rm(list = ls())
# Reading thetraining dataset
TrainData1=read.csv("C:/Users/shiva/Documents/Spring Sem/Advanced Data Mining/Project 1/train_v3/train_v3.csv")
# Data Cleaning
#Looking at NA values

colMeans(is.na(TrainData1))
rowMeans(is.na(trainTransformedLoan))
# Removing NA values
sum(is.na(TrainData1))
library(caret)
Imputed_LoanData=preProcess(TrainData1,method = c("medianImpute"))
Loan_Data=predict(Imputed_LoanData,TrainData1)
#Removing duplicate columns
Filtered_Loan_Data<-Loan_Data[!duplicated(lapply(Loan_Data,summary))]
#Removing the loss column from the data
LossValues=Filtered_Loan_Data$loss
IdValues=Filtered_Loan_Data$id
Filtered_Loan_Data$loss=NULL
Filtered_Loan_Data$id=NULL
# Normalizing the data
Normalized_Loan_Data1=data.frame(lapply(Filtered_Loan_Data,scale))
#Adding the loss column in the data
Normalized_Loan_Data1$loss=LossValues
Normalized_Loan_Data1$id=IdValues
#Remove ZV
zv <- apply(Normalized_Loan_Data1, 2, function(x) length(unique(x)) == 1)
dfr <- Normalized_Loan_Data1[, !zv]
n=length(colnames(dfr))
# see highly correlated variables
library(caret)
correlationMatrix <- cor(dfr[,1:n],use="complete.obs")
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=(0.90),verbose = FALSE)
print(highlyCorrelated)
important_var=colnames(dfr[,-highlyCorrelated])
important_var1=dfr[,-highlyCorrelated]
# Adding default column in dataset
important_var1$default<- ifelse(important_var1$loss>0, 1, 0)
str(important_var1)
#change into numeric from char
#important_var1$default=as.number(important_var1$default)
# Create training and test set 
index =sample(nrow(important_var1),round(nrow(important_var1)*.7))
datatrain = important_var1[ index, ]
datatest = important_var1[ -index, ]

#New Code
# Running Lasso model for feature extraction
library(glmnet)
cvfit_lasso = cv.glmnet(default~.-loss,datatrain, alpha = 1, nlambda =100)
plot(cvfit_lasso)
cvfit_lasso$lambda.min
coef(cvfit_lasso, s = "lambda.min")

predicts_glm_lasso<-predict(cvfit_lasso, newdata = datatest, s = "lambda.min")
MSE_glm_lasso=mean(sqrt((predicts_glm_lasso-datatest$default)^2))
print(MSE_glm_lasso)
MAE_lasso=sqrt((sum((datatest$loss-predicts_glm_lasso)^2))/nrow(datatest))
print(MAE_lasso)

# Running Logistic regression for the extracted model
Model_lasso=cv.glmnet(default~f3+f13+f25+f26+f44+f54+f57+f61+f67+f70+f71+f73+f76+f81+f83+f84+f91+f94+f99+f103+f129+f131+f140+f143+f146+f150+f153+f170+f172 +f198+f202+f203+f209+f217+f218+f229+f222+f233++f252+f262+f270+f277+f279+f285+f290++f293+f298+f301+f306+f309+f321+f333+f340+f341+f347+f350+f358+f366+f367+f383+f384+f385+f398+f403+f411+f419+f433+f451+f461+f468+f471+f479+f514+f518+f525+f526+f533+f536+f546+f587+f598+f601+f612+f617+f619+f628+f637+f638+f647+f649+f651+f650+f654+f664+f672+f673+f674+f677+f680+f725+f734+f755+f756+f760+f763+f765+f768+f775, datatrain, alpha = 1, nlambda =100)
Modelglm=glm(default~f3+f13+f25+f26+f44+f54+f57+f61+f67+f70+f71+f73+f76+f81+f83+f84+f91+f94+f99+f103+f129+f131+f140+f143+f146+f150+f153+f170+f172 +f198+f202+f203+f209+f217+f218+f229+f222+f233++f252+f262+f270+f277+f279+f285+f290++f293+f298+f301+f306+f309+f321+f333+f340+f341+f347+f350+f358+f366+f367+f383+f384+f385+f398+f403+f411+f419+f433+f451+f461+f468+f471+f479+f514+f518+f525+f526+f533+f536+f546+f587+f598+f601+f612+f617+f619+f628+f637+f638+f647+f649+f651+f650+f654+f664+f672+f673+f674+f677+f680+f725+f734+f755+f756+f760+f763+f765+f768+f775, datatrain,family ="binomial")
summary(Modelglm)
Predict_default=predict(Modelglm,datatest,type='response')
Predict_default
TestData2=datatest
TestData2$DefaultProb=Predict_default
X=TestData2[,c("default","DefaultProb")] #To see predicted results vs actual results
library(pROC)
roc(TestData2$default, Predict_default)
#Predictions for train_data
Pred_default_Train=predict(ModelFinalLasso,datatrain,type='response')
TrainData2=datatrain
TrainData2$Default_TrainProb=Pred_default_Train
Y=TrainData2[,c("default","Default_TrainProb")] #To see predicted results vs actual results
library(pROC)
roc(TrainData2$default, Pred_default_Train)



#################Regression Model######################

library(glmnet)
library(glmnetUtils)
#Filtering loss >0 for regression model
NewDataLoss= important_var1 %>% filter(loss >0)
# Dividing into training and test
index1 =sample(nrow(NewDataLoss),round(nrow(NewDataLoss)*.7))
TrainLossData = NewDataLoss[ index, ]
TestLossData = NewDataLoss[ -index, ]
#Trying neuralnet
# Lasso for feature selection of Loss
Lasso_lossPred = cv.glmnet(loss~.-default,TrainLossData, alpha = 1, nlambda =100)
plot(Lasso_lossPred)
Lasso_lossPred$lambda.min
coef(Lasso_lossPred, s = "lambda.min")
#Trying neural network
#library(nnet)
#Model_nnnet1 = neuralnet(loss ~ f4+f5+f13+f32+f44+f61+f67+f70+f73+f76+f81+f83+f84+f91+f93+f110+f129+f130+f133+f140+f143+f144+f189+f198+f199+f212+f218+f229+f242+f270+f289+f291+f297+f298+f316+f329+f330+f340+f341+f402+f413+f431+f448+f451+f509+f514+f523+f526+f546+f556+f591+f598+f604+f614+f617+f619+f627+f637+f648+f649+f654+f659+f661+f664+f669+f675+f725+f734+f740+f746+f765+f768+f774-default ,TrainLossData,  hidden=0,threshold = 0.01)
#Model_nnnet <- nnet(loss ~ . -default,TrainLossData, size = 5,linout=TRUE)
#Lasso Regression for 
LassoModelLoss=cv.glmnet(loss~ f4+f5+f13+f32+f44+f61+f67+f70+f73+f76+f81+f83+f84+f91+f93+f110+f129+f130+f133+f140+f143+f144+f189+f198+f199+f212+f218+f229+f242+f270+f289+f291+f297+f298+f316+f329+f330+f340+f341+f402+f413+f431+f448+f451+f509+f514+f523+f526+f546+f556+f591+f598+f604+f614+f617+f619+f627+f637+f648+f649+f654+f659+f661+f664+f669+f675+f725+f734+f740+f746+f765+f768+f774-default ,TrainLossData,alpha = 1, nlambda =100)
plot(Lasso_lossPred)
Lasso_lossPred$lambda.min
coef(Lasso_lossPred, s = "lambda.min")
#Prediction on test set
LassoLossPrediction<-predict(LassoModelLoss, newdata = TestLossData, s = "lambda.min")
MAE_lasso=sqrt((sum((TestLossData$loss-LassoLossPrediction)^2))/nrow(TestLossData))
###Removing Loss colum from train data
LossRemoved=TrainLossData$loss
TrainLossData$loss=NULL
# Predicting Loss on Train data
Pred_loss_Train=predict(LassoModelLoss,TrainLossData,type='response')
TrainLossData$Loss_Prediction=Pred_loss_Train
#Adding back the loss column
TrainLossData$loss=LossRemoved
#Actual vs predicted loss
ActualVsPredLoss=TrainLossData[,c("id","loss","Loss_Prediction")] 
library(pROC)
roc(TrainLossData$loss, Loss_Prediction)

