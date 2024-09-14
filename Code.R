###################################################
##################Libraries########################
###################################################
library(randomForest)
library(caret)
library(class)
library(MASS)
library(e1071)
library(neuralnet)
library(ggplot2)
library(tidyverse)
###################################################
##############Data Read and Clean##################
###################################################
#Read data
data = read.csv("alzheimers_disease_data.csv")
dataEDA = data
dataEDA$Diagnosis <- factor(dataEDA$Diagnosis, levels = c(0,1), labels = c("Negative", "Positive"))
######Create dummy variables
data$Ethnicity = as.factor(data$Ethnicity)
data$EducationLevel = as.factor(data$EducationLevel)
dummies1 = model.matrix(~ Ethnicity - 1, data = data)
dummies2 = model.matrix(~ EducationLevel - 1, data = data)
data$Ethnicity = NULL
data$EducationLevel = NULL
data$DoctorInCharge = NULL
data$PatientID = NULL
data = cbind(data, dummies1, dummies2)
#Scale data
data = as.data.frame(scale(data))
#Separate Train and test sets
indices = seq(1,2149,1)
set.seed(22)
train.ind = sample(indices, nrow(data)*.8)
train = as.data.frame(data[train.ind,])
test = as.data.frame(data[-train.ind,])
train$Diagnosis = as.factor(train$Diagnosis)
test$Diagnosis = as.factor(test$Diagnosis)
#####################################################
#############Exploratory Data Analysis###############
#####################################################
ggplot(dataEDA, aes(x = Diagnosis, y = Age , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = BMI , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = AlcoholConsumption , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = PhysicalActivity , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = DietQuality , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = SleepQuality , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = SystolicBP , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = DiastolicBP , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = CholesterolTotal , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = CholesterolLDL , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = CholesterolHDL , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = CholesterolTriglycerides , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = MMSE , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = FunctionalAssessment , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
ggplot(dataEDA, aes(x = Diagnosis, y = ADL , fill = Diagnosis)) +
  geom_boxplot() +
  labs(x = "Diagnosis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Gender, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Gender", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Gender, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Ethnicity, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Ethnicity", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Ethnicity, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$EducationLevel, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("EducationLevel", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = EducationLevel, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Smoking, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Smoking", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Smoking, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$FamilyHistoryAlzheimers, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("FamilyHistoryAlzheimers", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = FamilyHistoryAlzheimers, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$CardiovascularDisease, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("CardiovascularDisease", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = CardiovascularDisease, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Diabetes, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Diabetes", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Diabetes, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Depression, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Depression", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Depression, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$HeadInjury, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("HeadInjury", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = HeadInjury, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Hypertension, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Hypertension", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Hypertension, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$MemoryComplaints, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("MemoryComplaints", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = MemoryComplaints, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$BehavioralProblems, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("BehavioralProblems", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = BehavioralProblems, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Confusion, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Confusion", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Confusion, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Disorientation, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Disorientation", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Disorientation, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$PersonalityChanges, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("PersonalityChanges", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = PersonalityChanges, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$DifficultyCompletingTasks, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("DifficultyCompletingTasks", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = DifficultyCompletingTasks, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()

prop_df <- as.data.frame(prop.table(table(dataEDA$Forgetfulness, dataEDA$Diagnosis), margin = 1))
colnames(prop_df) <- c("Forgetfulness", "Diagnosis", "Proportion")
ggplot(prop_df, aes(x = Forgetfulness, y = Proportion, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Proportion",
       fill = "Diagnosis") +
  theme_minimal()
#################################################################
####################Model Building###############################
#################################################################
#Train Random Forest model
rf_model <- randomForest(Diagnosis ~ ., data = train)

#Predictions on test data
new_predictions <- predict(rf_model, newdata = test)

rf_cm = confusionMatrix(new_predictions, test$Diagnosis)
rf_cm2 = rf_cm$byClass
rf_F1 = rf_cm2["F1"]




#Train KNN model
trainknn = train
train_knn_diag = train$Diagnosis
trainknn$Diagnosis = NULL
testknn = test
test_knn_diag = test$Diagnosis
testknn$Diagnosis = NULL

class.rate<-numeric(25)
for(k in 1:25) {
  
  knn_predictions <- knn(train = trainknn, test = testknn, cl = train_knn_diag, k = k)
  x = confusionMatrix(knn_predictions, test_knn_diag)
  y = x$byClass
  class.rate[k] = y["F1"]
  
}

plot(c(1:25), class.rate, type="b", main="Correct classification rates on the validation data for a range of k", xlab="k",ylab="F1 Score",cex.main=0.7)

knn_final_pred <- knn(train = trainknn, test = testknn, cl = train_knn_diag, k = 17)
knn_cm = confusionMatrix(knn_final_pred, test_knn_diag)
knn_cm2 = knn_cm$byClass
knn_F1 = knn_cm2["F1"]


#Train SVM model
pred.error<-function(pred,truth){ 
  mean(pred!=truth) 
} 
C.val<-c(0.001,0.01,0.1,1,2,5,10) 
C.error<-numeric(length(C.val))

for (i in 1:length(C.val)) { 
  model <- svm(Diagnosis ~.,data=train, type="C-classification", 
               kernel="linear", cost=C.val[i]) 
  pred.model <- predict(model, test) 
  C.error[i] <- pred.error(pred.model, test$Diagnosis)
}

C.sel <- C.val[min(which.min(C.error))] 

plot(C.val, C.error, type="b") 
abline(v=C.sel,lty=2)

final.svm<-svm(Diagnosis ~.,data=train,
               kernel="linear",cost=C.sel,type="C-classification") 
final.pred <- predict(final.svm, test)
svm_cm = confusionMatrix(final.pred, test$Diagnosis)

svm_cm2 = svm_cm$byClass
svm_F1 = svm_cm2["F1"]



#Train Logistic Model
logistic.mod <- glm(Diagnosis ~ ., family = binomial, data = train)
log.pred.prob <- predict(logistic.mod, test, type = "response")
log.pred <- ifelse(log.pred.prob > 0.5, 1, 0)
log.pred <- factor(log.pred, levels = c(0, 1))
levels(test$Diagnosis) = c(0,1)
glm_cm = confusionMatrix(log.pred, test$Diagnosis)
glm_cm2 = glm_cm$byClass
glm_F1 = glm_cm2["F1"]




#Train Neural Network
nn.model <- neuralnet(Diagnosis ~ . , data = train, hidden = c(6, 4, 4, 2), linear.output = FALSE)

nn.pred.prob <- predict(nn.model, test)

nn.pred <- ifelse(nn.pred.prob[,2] > 0.5, 1, 0)
nn.pred <- factor(nn.pred, levels = c(0, 1))
levels(test$Diagnosis) = c(0,1)
nn_cm = confusionMatrix(nn.pred, test$Diagnosis)
nn_cm2 = nn_cm$byClass
nn_F1 = nn_cm2["F1"]




###############################################################
#####################Review Results############################
###############################################################
#Create data frame of F1 Scores
scores = data.frame("Random Forest" = rf_F1,
                    KNN = knn_F1,
                    SVM = svm_F1,
                    Logistic = glm_F1,
                    "Neural Network" = nn_F1)

scores = as.data.frame(t(scores))
scores$Model <- rownames(scores)
rownames(scores) <- NULL
scores <- scores[order(scores$F1), ]
names(scores)[1] <- "F1.Score"


#Bar Plot of F! scores
ggplot(scores, aes(y = reorder(Model, F1.Score), x = F1.Score, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7) + 
  coord_flip() +                            
  labs(title = "F1 Score Comparison by Model",
       y = "Model",
       x = "F1 Score") +                   
  theme_minimal() +                         
  theme(legend.position = "none") +         
  geom_text(aes(label = round(F1.Score, 2)), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white")    

