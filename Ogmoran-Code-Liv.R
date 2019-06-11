## Octavio Morán
## HarvardX: PH125.9x - Capstone Project
## Indian Liver Disease 
## https://github.com/ogmoran13/Ogmoran_Liv.git

##########################################
# Indian Liver Disease Code
##########################################

###################################
# Create edx set and validation
###################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(pscl)) install.packages("pscl", repos = "http://cran.us.r-project.org")

liver_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv", 
                       header = FALSE)
colnames(liver_data) <- c("Age", "Sex", "Tot_Bil", "Dir_Bil", "Alkphos", "Alamine", 
                          "Aspartate", "Tot_Prot", "Albumin", "A_G_Ratio", "Disease")
liver_data$Sex <- (ifelse(liver_data$Sex == "Male", "M", "F"))  #made shorter
liver_data$Disease <- as.numeric(ifelse(liver_data$Disease == 2, 0, 1))  #converted to zeros and ones

# Show dataset

head(liver_data) %>%
  print.data.frame()

# Summary of data set
summary(liver_data)

# Age graph and data

liver_data %>%
  ggplot(aes(Age)) +
  geom_histogram(bins = 6, color = "#00AFBB") +
  xlab("Age") + 
  ylab("Frequency") +
  ggtitle("Age of patientes")

liver_data %>%
  group_by(Sex,Disease)%>%
  summarise (n = n())

# Total Bil Graph
liver_data %>%
  ggplot(aes(log10(Tot_Bil))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Total Bilirubin") +
  ylab("Frequency") +
  ggtitle("Distribution of Total Bilirubin (ln)")

# Direct Bil Graph
liver_data %>%
  ggplot(aes(log10(Dir_Bil))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Bilirubin") +
  ylab("Frequency") +
  ggtitle("Distribution of Direct Bilirubin (ln)")

# ALK Graph
liver_data %>%
  ggplot(aes(log10(Alkphos))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Alkaline Phosphotase") +
  ylab("Frequency") +
  ggtitle("Distribution of Alkaline Phosphotase (ln)")

# ALM Graph
liver_data %>%
  ggplot(aes(log10(Alamine))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Alamine Aminotransferase") +
  ylab("Frequency") +
  ggtitle("Distribution of Alamine Aminotransferase (ln)")

# Aspar Graph
liver_data %>%
  ggplot(aes(log10(Aspartate))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Aspartate Aminotransferase") +
  ylab("Frequency") +
  ggtitle("Distribution of Aspartate Aminotransferase (ln)")

# TOT PROT Graph
liver_data %>%
  ggplot(aes(log10(Tot_Prot))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Total Proteins") +
  ylab("Frequency") +
  ggtitle("Distribution of Total Proteins (ln)")

# Albumin Graph
liver_data %>%
  ggplot(aes((Albumin))) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Albumin") +
  ylab("Frequency") +
  ggtitle("Distribution of Albumin")

# A_G Ratio Graph
liver_data %>%
  ggplot(aes(A_G_Ratio)) +
  geom_histogram(bins = 26, color = "#00AFBB") +
  xlab("Albumin_G") +
  ylab("Frequency") +
  ggtitle("Distribution of Albumin-Globulin Ratio")

#train and test data
set.seed(455)  
liver_data$Splits <- sample.split(liver_data, SplitRatio = 0.7)  #Index
liver_data <- liver_data %>% mutate_each(funs(log), -Age, -Sex, -Albumin, -A_G_Ratio, 
                                         -Disease, -Splits)
train <- liver_data[liver_data$Splits == TRUE, ]  #training index
test <- liver_data[liver_data$Splits == FALSE, ]  #test indexes

#Summary of training data
summary(train)

#LM
fit <- glm(Disease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
             Tot_Prot + Albumin + A_G_Ratio, data = train, family = binomial(link = "logit"))
#Coefficients LM
summary(fit)

#Pseudo R-Square and Log-Likelihoods
pR2(fit)

# CD Model
Test_Predictions <- data.frame(Probability = predict(fit, test, type = "response"))
Test_Predictions$Prediction <- ifelse(Test_Predictions > 0.5, 1, 0)
Test_Predictions$Disease <- test$Disease
accuracy <- mean(Test_Predictions$Disease == Test_Predictions$Prediction, na.rm = TRUE)
disease <- Test_Predictions$Probability[which(Test_Predictions$Disease == 1)]
non <- Test_Predictions$Probability[which(Test_Predictions$Disease == 0)]
Coef_Desc <- mean(disease, na.rm = TRUE) - mean(non, na.rm = TRUE)

#Coefficient
print(Coef_Desc)

#accuracy of the model
print(accuracy)