library("fastDummies")
library("dplyr")
library(nnet)
library(tidyverse)
library(caret)
#data<-read.csv("D:\\SPRING 23\\Adv2\\project\\Train.csv")
data<-read.csv("C:/Users/anuhi/OneDrive/Desktop/DS/stats-2/Train.csv")
dim(data)

####DATA CLEANING
#----outliers-------
dim(data)
ggplot(data, aes(x=Segmentation,y=Age)) +
  geom_boxplot(fill='steelblue')
data<-data %>% filter(!Age>=60)


ggplot(data, aes(x=Segmentation,y=Family_Size)) +
  geom_boxplot(fill='steelblue')
data<-data %>% filter(!Family_Size>=5)

#----Replacing null values------
#null values
colSums(is.na(data))
unique(data$Work_Experience)
data<-data %>% replace_na(list(Work_Experience = median(data$Work_Experience, na.rm=TRUE)))
unique(data$Family_Size)
data<-data %>% replace_na(list(Family_Size = median(data$Family_Size, na.rm=TRUE)))
unique(data$Ever_Married)
data['Ever_Married'][data['Ever_Married'] == ""] <- 'No'
unique(data$Graduated)
data['Graduated'][data['Graduated'] == ""] <- 'No'
unique(data$Profession)
data<-data %>% filter(!Profession=="")
unique(data$Var_1)
data<-data %>% filter(!Var_1=="")



####DATA VISUALIZATION
#------categorical features------
library(ggplot2)
ggplot(data, aes(x=Gender)) +
  geom_bar()
library(ggplot2)
ggplot(data, aes(x=Ever_Married)) +
  geom_bar()
library(ggplot2)
ggplot(data, aes(x=Graduated)) +
  geom_bar()
library(ggplot2)
ggplot(data, aes(x=Spending_Score)) +
  geom_bar()
library(ggplot2)
ggplot(data, aes(x=Profession)) +
  geom_bar()

#---feature vs target-----
#Gender vs Segmentation
ggplot(data, 
       aes(x = Gender, 
           fill = Segmentation)) + 
  geom_bar(position = "dodge")
#Graduated vs Segementation
ggplot(data, 
       aes(x = Graduated, 
           fill = Segmentation)) + 
  geom_bar(position = "dodge")
#Ever_married vs segmetation
ggplot(data, 
       aes(x = Ever_Married, 
           fill = Segmentation)) + 
  geom_bar(position = "dodge")
#profession vs Segementation
ggplot(data, 
       aes(x = Segmentation, 
           fill = Profession)) + 
  geom_bar(position = "dodge")
#VAR_1 vs Segementation
ggplot(data, 
       aes(x = Var_1, 
           fill = Segmentation)) + 
  geom_bar(position = "dodge")
#age vs segmentation
ggplot(data, 
       aes(x = Age, 
           fill = Segmentation)) +
  geom_density(alpha = 0.4) +
  labs(title = "Age distribution by customer segments")
#work experience vs segmentation
ggplot(data, 
       aes(x = Work_Experience, 
           fill = Segmentation)) +
  geom_density(alpha = 0.4)
#work experience vs segmentation
ggplot(data, 
       aes(x = Family_Size, 
           fill = Segmentation)) +
  geom_density(alpha = 0.4)


#creating dummies
head(data)
unique(data$Ever_Married)
data['Ever_Married'][data['Ever_Married'] == "Yes"] <- 1
data['Ever_Married'][data['Ever_Married'] == "No"] <- 0
data$Ever_Married<-as.numeric(data$Ever_Married)
unique(data$Gender)
data['Gender'][data['Gender'] == "Male"] <- 1
data['Gender'][data['Gender'] == "Female"] <- 0
data$Gender<-as.numeric(data$Gender)
unique(data$Graduated)
data['Graduated'][data['Graduated'] == "Yes"] <- 1
data['Graduated'][data['Graduated'] == "No"] <- 0
data$Graduated<-as.numeric(data$Graduated)
unique(data$Spending_Score)
data['Spending_Score'][data['Spending_Score'] == "Low"] <- 0
data['Spending_Score'][data['Spending_Score'] == "Average"] <- 1
data['Spending_Score'][data['Spending_Score'] == "High"] <- 2
data$Spending_Score<-as.numeric(data$Spending_Score)
unique(data$Var_1)
data['Var_1'][data['Var_1'] == "Cat_1"] <- 0
data['Var_1'][data['Var_1'] == "Cat_2"] <- 1
data['Var_1'][data['Var_1'] == "Cat_3"] <- 2
data['Var_1'][data['Var_1'] == "Cat_4"] <- 3
data['Var_1'][data['Var_1'] == "Cat_5"] <- 4
data['Var_1'][data['Var_1'] == "Cat_6"] <- 5
data['Var_1'][data['Var_1'] == "Cat_7"] <- 6
data$Var_1<-as.numeric(data$Var_1)
final_data<-dummy_cols(data,select_columns='Profession',remove_selected_columns=TRUE,remove_most_frequent_dummy = TRUE)
head(final_data)
final_data<-subset(final_data,select=-c(ID))


###Splitting data to testing and train
library(caTools)
set.seed(1)
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample.split(final_data$Segmentation, SplitRatio = 0.7)
train  <- subset(final_data, sample == TRUE)
test   <- subset(final_data, sample == FALSE)



#### Classification Models
#Logistic regression
train$Segmentation<-as.factor(train$Segmentation)
test$Segmentation<-as.factor(test$Segmentation)
log_reg <- multinom(Segmentation~., data = train,type='response')
summary(log_reg)
predict_lg<-predict(log_reg,test,type='class')
head(predict_lg)
conf_lg<-table(predict_lg,test$Segmentation)
conf_lg
#Accuracy of LG model
acc_lg<-sum(diag(conf_lg))/sum(conf_lg)
acc_lg

#decision tree
library(tree)
set.seed(1)
model_tree <- tree(Segmentation ~ ., data=train)
summary(model_tree)
predict_dt<-predict(model_tree, test,type='class')
conf_dt<-table(predict_dt,test$Segmentation)
conf_dt
acc_dt<-sum(diag(conf_dt))/sum(conf_dt)
acc_dt
###
set.seed(7)
cv.model_tree <- cv.tree(model_tree, FUN = prune.misclass)
names(cv.model_tree)
cv.model_tree
###
par(mfrow = c(1, 2))
plot(cv.model_tree$size, cv.model_tree$dev, type = "b")
plot(cv.model_tree$k, cv.model_tree$dev, type = "b")
#--
prune.model_tree <- prune.misclass(model_tree, best = 3)
plot(prune.model_tree)
text(prune.model_tree, pretty = 0)
###
tree.pred <- predict(prune.model_tree, test,
                     type = "class")
conf_dt2<-table(tree.pred, test$Segmentation)
conf_dt2
acc_dt2<-sum(diag(conf_dt2))/sum(conf_dt2)
acc_dt2


#Support vector Machines
#svm
library(e1071)
set.seed(1)
tune.out <- tune(svm, Segmentation ~ ., data = train, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
predict_svm <- predict(bestmod, test)
conf_svm<-table(predict = predict_svm, truth = test$Segmentation)
acc_svm<-sum(diag(conf_svm))/sum(conf_svm)
acc_svm
#tuning with gamma and cost function
tune.out2 <- tune(svm, Segmentation ~ ., data = train, 
                 kernel = "radial", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)
                 ))
summary(tune.out2)
bestmod2 <- tune.out2$best.model
summary(bestmod2)
predict_svm2 <- predict(bestmod2, test)
conf_svm2<-table(predict = predict_svm2, truth = test$Segmentation)
acc_svm2<-sum(diag(conf_svm2))/sum(conf_svm2)
acc_svm2


#Random Forest

#Random Forest
library(randomForest)
model_rf<-randomForest(Segmentation~., train)
predict_rf<-predict(model_rf, test)
conf_rf<-table(predict_rf,test$Segmentation)
conf_rf
acc_rf<-sum(diag(conf_rf))/sum(conf_rf)
acc_rf

#Accuracies of all models
results<-data.frame(Model=c('MLR','DT','DT_prune','SVM_linear','SVM_radial','RF'),
Accuraies=c(acc_lg,acc_dt,acc_dt2,acc_svm,acc_svm2,acc_rf))

results

###AUC
library(pROC)
#multinomial
p<-predict(log_reg,test,type='prob')
multiclass.roc(test$Segmentation,p,percent=T)
#decision tree
p2<-predict(prune.model_tree,test)
multiclass.roc(test$Segmentation,p2,percent=T)
#SVM
p3<-predict(bestmod,test,type='prob')
multiclass.roc(test$Segmentation,p3,percent=T)
#random forest
p4<-predict(model_rf,test,type='prob')
multiclass.roc(test$Segmentation,p4,percent=T)


