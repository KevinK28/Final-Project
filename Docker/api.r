#api.r
library(tidyverse)
library(caret)

#Intro Message
#* @get /readme
function() {
  "Welcome to my API for the ST-558 Final Project"
}

diabetes <- as_tibble(read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")) |>
  mutate(Diabetes_binary = as.factor(Diabetes_binary), HighBP = as.factor(HighBP),
         HighChol = as.factor(HighChol), CholCheck = as.factor(CholCheck),
         Smoker = as.factor(Smoker), Stroke = as.factor(Stroke),
         HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
         PhysActivity = as.factor(PhysActivity), HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
         GenHlth = as.factor(GenHlth), PhysHlth = as.factor(PhysHlth),
         DiffWalk = as.factor(DiffWalk), Sex = as.factor(Sex),
         Age = as.factor(Age)) |>
  select(Diabetes_binary, BMI, HighBP, HighChol, CholCheck, Smoker, Stroke, HeartDiseaseorAttack,
         PhysActivity, HvyAlcoholConsump, GenHlth, PhysHlth, DiffWalk, Sex, Age)

levels(diabetes$Diabetes_binary)      <- c("No", "Yes")
levels(diabetes$HighBP)               <- c("No", "Yes")
levels(diabetes$HighChol)             <- c("No", "Yes")
levels(diabetes$CholCheck)            <- c("No", "Yes")
levels(diabetes$Smoker)               <- c("No", "Yes")
levels(diabetes$Stroke)               <- c("No", "Yes")
levels(diabetes$HeartDiseaseorAttack) <- c("No", "Yes")
levels(diabetes$PhysActivity)         <- c("No", "Yes")
levels(diabetes$HvyAlcoholConsump)    <- c("No", "Yes")
levels(diabetes$GenHlth)              <- c("Excellent", "Very Good", "Good", "Fair", "Poor")
levels(diabetes$DiffWalk)             <- c("No", "Yes")
levels(diabetes$Sex)                  <- c("Female", "Male")

set.seed(123)
trctrl <- trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss,
                       classProbs = TRUE)
bestModel <- train(Diabetes_binary ~ ., data = diabetes, method = "glm", family = "binomial",
                trControl = trctrl, tuneLength = 10, metric = "logLoss")


defaultVals <- diabetes |>
  summarise(BMI = mean(BMI),
            HighBP = names(sort(table(HighBP), decreasing = TRUE))[1],
            HighChol = names(sort(table(HighChol), decreasing = TRUE))[1],
            CholCheck = names(sort(table(CholCheck), decreasing = TRUE))[1],
            Smoker = names(sort(table(Smoker), decreasing = TRUE))[1],
            Stroke = names(sort(table(Stroke), decreasing = TRUE))[1],
            HeartDiseaseorAttack = names(sort(table(HeartDiseaseorAttack), decreasing = TRUE))[1],
            PhysActivity = names(sort(table(PhysActivity), decreasing = TRUE))[1],
            HvyAlcoholConsump = names(sort(table(HvyAlcoholConsump), decreasing = TRUE))[1],
            GenHlth = names(sort(table(GenHlth), decreasing = TRUE))[1],
            PhysHlth = names(sort(table(PhysHlth), decreasing = TRUE))[1],
            DiffWalk = names(sort(table(DiffWalk), decreasing = TRUE))[1],
            Sex = names(sort(table(Sex), decreasing = TRUE))[1],
            Age = names(sort(table(Age), decreasing = TRUE))[1]) |>
  as.list()

#* Take in desired parameters from best model
#* @param HighBP Yes or No
#* @param HighChol Yes or No
#* @param CholCheck Yes or No
#* @param BMI numeric value
#* @param Smoker Yes or No
#* @param Stroke Yes or No
#* @param HeartDiseaseorAttack Yes or No
#* @param PhysActivity Yes or No
#* @param HvyAlcoholComsump Yes or NO
#* @param GenHlth Excellent, Very Good, Good, Fair, or Poor
#* @param PhysHlth 1-30 integer value
#* @param DiffWalk Yes or No
#* @param Sex Male or Female
#* @param Age 1-9 integer
#* @get /pred
function(HighBP = defaultVals$HighBP,
         HighChol = defaultVals$HighChol,
         CholCheck = defaultVals$CholCheck,
         BMI = defaultVals$BMI,
         Smoker = defaultVals$Smoker,
         Stroke = defaultVals$Stroke,
         HeartDiseaseorAttack = defaultVals$HeartDiseaseorAttack,
         PhysActivity = defaultVals$PhysActivity,
         HvyAlcoholConsump = defaultVals$HvyAlcoholConsump,
         GenHlth = defaultVals$GenHlth,
         PhysHlth = defaultVals$PhysHlth,
         DiffWalk = defaultVals$DiffWalk,
         Sex = defaultVals$Sex,
         Age = defaultVals$Age) {
  
  inputData <- data.frame(HighBP = HighBP, HighChol = HighChol, CholCheck = CholCheck,
                          BMI = as.numeric(BMI), Smoker = Smoker, Stroke = Stroke, 
                          HeartDiseaseorAttack = HeartDiseaseorAttack,
                          PhysActivity = PhysActivity,
                          HvyAlcoholConsump = HvyAlcoholConsump,
                          GenHlth = GenHlth, PhysHlth = PhysHlth, DiffWalk = DiffWalk,
                          Sex = Sex, Age = Age)
  
  prediction <- predict(bestModel, inputData, type = "prob")
  
  return(prediction)
}

#* Provide Info
#* @get /info
function() {
  list(name = "Kevin Krupa (ST-558)",
       gitHubSite = "https://kevink28.github.io/Final-Project/")
}

#Function Calls

#pred
#http://127.0.0.1:8000/pred?HighBP=Yes
#http://127.0.0.1:8000/pred?HighChol=Yes&HvyAlcoholConsump=Yes&HighBP=Yes
#http://127.0.0.1:8000/pred?DiffWalk=Yes&Sex=Male

#info
#http://127.0.0.1:8000/info 