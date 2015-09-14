Claims <- read.csv("ClaimsData.csv")
str(Claims)
table(Claims$bucket2009)/nrow(Claims)
# Split data into Training and Test Set
library(caTools)
set.seed(88)
split <- sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain <- subset(Claims, split == TRUE)
ClaimsTest <- subset(Claims, split == FALSE)
str(ClaimsTrain)
#Average age of patients in training set
mean(ClaimsTrain$age)
#Proportion of patients in training set that had Diagnosis code for diabetes
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain)

# Baseline Method <- Cost bucket for 2009 is same as that if 2008
#Classification Matrix to check the accuracy of Baseline Method

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
#Accuracy is total of the diagonals in the above table/ nrow(ClaimsTest) which is 0.68

# Create a Penalty Matrix
PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
PenaltyMatrix
# Multiply classification table after converting to matrix by Penalty Matrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
# Penalty error is sum of the panalty (above matrix) divided by nrow(ClaimsTest)
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

# If Baseline is assumed to be the most freq bucket which is 1 i.e. all predicted outcomes are 1
table(ClaimsTest$bucket2009)

# accuracy of this is 122798/nrow(ClaimsTest)

# Penalty Error
sum(as.matrix(table(ClaimsTest$bucket2009))*PenaltyMatrix[,1])/nrow(ClaimsTest)

# Build a CART model
library(rpart)
library(rpart.plot)
ClaimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005)
prp(ClaimsTree)

# Predict for test data using the above CART model
PredictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
# Classification matrix to compute the accuracy
table(ClaimsTest$bucket2009, PredictTest)
#as.matrix(table(ClaimsTest$bucket2009, PredictTest))[2,2]
# Penalty Error
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# Since the cart model uses the penalty error weightage to be same for all predictions, penalty error is similar to baseline
# use parms = list(loss=PenaltyMatrix) to include the penalty matrix in CART model
ClaimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005, parms = list(loss=PenaltyMatrix))
prp(ClaimsTree)
# Predict for test data using the above CART model
PredictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
# Classification matrix to compute the accuracy
table(ClaimsTest$bucket2009, PredictTest)
# Penalty Error
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
