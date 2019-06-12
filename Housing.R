house.train<-read.csv(file = "trainw.csv" , stringsAsFactors = FALSE , header = TRUE)
house.test<-read.csv(file = "testw.csv" , stringsAsFactors = FALSE , header = TRUE)


str(house.test)
str(house.train)   


#median()


#ncol

names(house.test)
.rs.r.keywords















names(house.train)

house.test$SalePrice<-NA

#bind
house.train$IsTrain<- TRUE
house.test$IsTrain<- FALSE

house.full<-rbind(house.test,house.train)


house.full$CentralAir


table(is.na(house.full$LotFrontage))
LotFrontage.median<- median(house.full$LotFrontage,na.rm = TRUE)
house.full[is.na(house.full$LotFrontage),"LotFrontage"] <- LotFrontage.median


table(is.na(house.full$Utilities))
Utilities.median<- median(house.full$Utilities,na.rm = TRUE)
house.full[is.na(house.full$Exterior1st),"Exterior1st"] <- "VinylSd"


table(is.na(house.full$Exterior1st))
sort(table(house.full$Exterior1st))



table(is.na(house.full$Exterior1st))

colSums(is.na(house.full))
#house.test<-house.test(-Alley , -PoolQc , -Fence , -MiscFeature)


house.full$FireplaceQu = NULL


MS.median<- median(house.full$MSZoning,na.rm = TRUE)
house.full[is.na(house.full$MSZoning),"MSZoning"] <- MS.median


#M.median<- median(house.full$Exterior2nd,na.rm = TRUE)
#house.full[is.na(house.full$MSZoning),"MSZoning"] <- MS.median

sort(table(house.full$Exterior2nd))
house.full[is.na(house.full$Exterior2nd),"Exterior2nd"] <- "VinylSd"


sort(table(house.full$MasVnrType))
house.full[is.na(house.full$MasVnrType),"MasVnrType"] <- "None"


Mse.median<- median(house.full$MasVnrArea,na.rm = TRUE)
house.full[is.na(house.full$MasVnrArea),"MasVnrArea"] <- Mse.median


A.median<- median(house.full$BsmtFinSF1,na.rm = TRUE)
house.full[is.na(house.full$BsmtFinSF1),"BsmtFinSF1"] <- A.median


colSums(is.na(house.full))


As.median<- median(house.full$BsmtFinSF2,na.rm = TRUE)
house.full[is.na(house.full$BsmtFinSF2),"BsmtFinSF2"] <- As.median


Ad.median<- median(house.full$BsmtUnfSF,na.rm = TRUE)
house.full[is.na(house.full$BsmtUnfSF),"BsmtUnfSF"] <- Ad.median


A.median<- median(house.full$TotalBsmtSF,na.rm = TRUE)
house.full[is.na(house.full$TotalBsmtSF),"TotalBsmtSF"] <- A.median


A.median<- median(house.full$BsmtFullBath,na.rm = TRUE)
house.full[is.na(house.full$BsmtFullBath),"BsmtFullBath"] <- A.median

A.median<- median(house.full$GarageCars,na.rm = TRUE)
house.full[is.na(house.full$GarageCars),"GarageCars"] <- A.median


A.median<- median(house.full$Functional,na.rm = TRUE)
house.full[is.na(house.full$Functional),"Functional"] <- A.median


A.median<- median(house.full$GarageArea,na.rm = TRUE)
house.full[is.na(house.full$GarageArea),"GarageArea"] <- A.median


sort(table(house.full$Electrical))
house.full[is.na(house.full$Electrical),"Electrical"] <- "SBrkr"


sort(table(house.full$SaleType))
house.full[is.na(house.full$SaleType),"SaleType"] <- "WD"


sort(table(house.full$KitchenQual))
house.full[is.na(house.full$KitchenQual),"KitchenQual"] <- "TA"


sort(table(house.full$BsmtQual))
house.full[is.na(house.full$BsmtQual),"BsmtQual"] <- "None"


colSums(is.na(house.full))


sort(table(house.full$GarageCond))
house.full[is.na(house.full$GarageCond),"GarageCond"] <- "TA"


sort(table(house.full$GarageFinish))
house.full[is.na(house.full$GarageFinish),"GarageFinish"] <- "Unf"


A.median<- median(house.full$GarageYrBlt,na.rm = TRUE)
house.full[is.na(house.full$GarageYrBlt),"GarageYrBlt"] <- A.median

#cleaning done


#split


house.train<-house.full[house.full$IsTrain==TRUE,]
house.test<-house.full[house.full$IsTtain==FALSE,]

house.full$IsTrain<-as.factor(house.full$IsTrain)

#install.packages('gbm')
#library(gbm)
#gbm_model <- gbm(SalePrice ~., data =house.train,
              #   shrinkage = 0.05,
               #  interaction.depth = 5,
              #   bag.fraction = 0.66,
               ##  n.minobsinnode = 1,
#cv.folds = 100,
               ##  keep.data = F,
               #  verbose = F,
                # n.trees = 300)

#gbm.perf(gbm_model)
#predict <- predict(gbm_model, house.test, n.trees=300)


# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
#solution <- data.frame(Id = house.test$Id, SalePrice = predict)

# Write the solution to file
#write.csv(solution, file = 'housesubmission.csv', row.names = FALSE)


 S.equation<-"SalePrice ~ MSSubClass  +  MSZoning +  LotFrontage  +  LotArea  +  Street + LotShape + LandContour  +  Utilities  + LotConfig  +  LandSlope +  Neighborhood + Condition1  +  Condition2  + BldgType  +  HouseStyle  + OverallQual  + OverallCond  + YearBuilt + YearRemodAdd +   RoofStyle +  RoofMatl +  Exterior1st +  Exterior2nd + MasVnrType +  MasVnrArea  +   ExterQual  +   ExterCond  +  Foundation  +  BsmtQual + BsmtCond +  BsmtExposure  + BsmtFinType1  +  BsmtFinSF1  + BsmtFinType2  +  BsmtFinSF2 + BsmtUnfSF +  TotalBsmtSF +  Heating   +  HeatingQC +  CentralAir +  Electrical + X1stFlrSF +  X2ndFlrSF +  LowQualFinSF   +  GrLivArea +  BsmtFullBath  + BsmtHalfBath + FullBath  +    HalfBath  + BedroomAbvGr  + KitchenAbvGr + KitchenQual +  TotRmsAbvGrd + Functional  +  Fireplaces  +  GarageType  +  GarageYrBlt +  GarageFinish + GarageCars + GarageArea  + GarageQual  + GarageCond  + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold  +    SaleType + SaleCondition"     
S.formula<-as.formula(S.equation)
install.packages("randomForest")

library(randomForest)

house.model <- randomForest(formula = S.formula , data = house.train , ntry = 500 , mtry = 3 ,nodesize = 0.01 * nrow(house.test))

features.equation <- "MSSubClass  +  MSZoning +  LotFrontage  +  LotArea  +  Street + LotShape + LandContour  +  Utilities  + LotConfig  +  LandSlope +  Neighborhood + Condition1  +  Condition2  + BldgType  +  HouseStyle  + OverallQual  + OverallCond  + YearBuilt + YearRemodAdd +   RoofStyle +  RoofMatl +  Exterior1st +  Exterior2nd + MasVnrType +  MasVnrArea  +   ExterQual  +   ExterCond  +  Foundation  +  BsmtQual + BsmtCond +  BsmtExposure  + BsmtFinType1  +  BsmtFinSF1  + BsmtFinType2  +  BsmtFinSF2 + BsmtUnfSF +  TotalBsmtSF +  Heating   +  HeatingQC +  CentralAir +  Electrical + X1stFlrSF +  X2ndFlrSF +  LowQualFinSF   +  GrLivArea +  BsmtFullBath  + BsmtHalfBath + FullBath  +    HalfBath  + BedroomAbvGr  + KitchenAbvGr + KitchenQual +  TotRmsAbvGrd + Functional  +  Fireplaces  +  GarageType  +  GarageYrBlt +  GarageFinish + GarageCars + GarageArea  + GarageQual  + GarageCond  + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold  +    SaleType + SaleCondition"
SalePrice <-predict(house.model , newdata = house.test)

Id<-house.test$Id
output.df<-as.data.frame(Id)
output.df$SalePrice<- SalePrice

write.csv(output.df , file = "House.csv", row.names = FALSE)










SalePrice <-predict(gbm_model , newdata = house.test)
Id<-house.test$Id
output.df<-as.data.frame(Id)
output.df$SalePrice<- SalePrice

write.csv(output.df , file = "Kagglenew.csv", row.names = FALSE)
