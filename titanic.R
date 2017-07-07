library('dplyr')
library('ggplot2')
library('randomForest') 
library('caret')

test <- read.csv("test.csv")      #Test Data
train <- read.csv("train.csv")    #Train Data  
full <- bind_rows(train,test)

str(full)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
full$Title <- as.factor(full$Title)
table(full$Sex, full$Title)
table(full$Pclass, full$Title)

full <- mutate(full, FamilySize = SibSp + Parch + 1)
ggplot(full[1:891,], aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(stat='Count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')
  
summary(full)

full[(which(is.na(full$Fare))),1]
full[1044, ]

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
  colour='red', linetype='dashed', lwd=1)

full$Embarked[full$Embarked == ""] <- NA
full[(which(is.na(full$Embarked))), 1]

full[c(62, 830), c(1,3,10)]

full %>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(mfare = median(Fare),
            n = n())
			
full$Embarked[c(62, 830)] <- 'C'

sum(is.na(full$Age))

predicted_age <- train(
  Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize ,
  tuneGrid = data.frame(mtry = c(2, 3, 7)),
  data = full[!is.na(full$Age), ],
  method = "ranger",
  trControl = trainControl(
      method = "cv", number = 10,
      repeats = 10, verboseIter = TRUE),
  importance = 'impurity'
  )

full$Age[is.na(full$Age)] <- predict(predicted_age, full[is.na(full$Age),])

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
         'Title')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

train <- full[1:891,]
test <- full[892:1309,]

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + 
                                            FamilySize,
                                            data = train)

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'rfSolution.csv', row.names = F)