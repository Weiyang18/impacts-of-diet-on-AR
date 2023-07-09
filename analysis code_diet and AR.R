library(readxl)
k <- read_xlsx("Race_1.xlsx")
k1<-subset(k,k$`SPT (≥3+)`!="999"&k$`SPT (≥3+)`!="SPT not done")
find<-which(k1$Meat==9&k1$Seafood==9&k1$Fruits==9)
k1<-k1[-find,]
k1$SPT<-ifelse(k1$`SPT (≥3+)`=="Yes","positive","negative")
k1$SPT<-as.factor(k1$SPT)
summary(k1$SPT)
symptom = k1[,c('Section341a','Section342a','Section343a','Section344a')]
k1$count.1<-apply(symptom,1,function(x)length(which(x=="1")))
k1$phenotype <-ifelse(k1$count.1>=2, "with","without")
k1$phenotype<-as.factor(k1$phenotype)
k1$Age<-as.numeric(k1$Age)
k1$Sex<-as.factor(k1$Sex)
k1$Age[which(k1$Age == "999" )] <- NA
k1$Sex[which(k1$Sex == "9" )] <- NA
k1$family<-ifelse(k1$`Mother AR`==1|k1$`Father AR`==1,"yes","no")
k1$family<-as.factor(k1$family)
k1$family<-relevel(k1$family,ref="no")
k1$family<-as.numeric(k1$family)
k1$`Weight (kg)`<-as.numeric(k1$`Weight (kg)`)
k1$`Height (cm)`<-as.numeric(k1$`Height (cm)`)
k1$`Weight (kg)`[which(k1$`Weight (kg)`==999)]<-NA
k1$`Height (cm)`[which(k1$`Height (cm)`==999)]<-NA
k1$BMI<-k1$`Weight (kg)`/(k1$`Height (cm)`*k1$`Height (cm)`*0.0001)
k1$case3<-ifelse(k1$SPT=="positive"&k1$phenotype=="with"&k1$Section32==1,"case",
                 ifelse(k1$SPT=="negative"&k1$phenotype=="without"&k1$Section32!=1,"nonatopic_control",NA))
k1$case3<-as.factor(k1$case3)
k1$case3<-relevel(k1$case3,ref = "nonatopic_control")

summary(k1$case3)
k1$RC<-ifelse(k1$SPT=="positive"&k1$phenotype=="with"&k1$Section32==1 & k1$Section33==1,"RC",
              ifelse(k1$SPT=="negative"&k1$phenotype=="without"&k1$Section32!=1,"nonatopic_control",NA))
k1$RC<-as.factor(k1$RC)
k1$RC<-relevel(k1$RC,ref = "nonatopic_control")
summary(k1$RC)
k1$atref<-ifelse(k1$SPT=="positive"&k1$phenotype=="with"&k1$Section32==1,"case",
                 ifelse(k1$SPT=="positive"&k1$phenotype=="without"&k1$Section32!=1,"atopic",NA))
k1$atref<-as.factor(k1$atref)
k1$atref<-relevel(k1$atref,ref = "atopic")
summary(k1$atref)
k1$at<-ifelse(k1$SPT=="positive"&k1$phenotype=="without"&k1$Section32!=1,"atopic",
              ifelse(k1$SPT=="negative"&k1$phenotype=="without"&k1$Section32!=1,"nonatopic_control",NA))
k1$at<-as.factor(k1$at)
k1$at<-relevel(k1$at,ref = "nonatopic_control")
summary(k1$at)
k1$Meat[which(k1$Meat == "9" )] <- NA
k1$Seafood[which(k1$Seafood == "9" )] <- NA
k1$Fruits[which(k1$Fruits == "9" )] <- NA
k1$Vegetables[which(k1$Vegetables == "9" )] <- NA
k1$Pulses[which(k1$Pulses == "9" )] <- NA
k1$Cereals[which(k1$Cereals == "9" )] <- NA
k1$Pasta[which(k1$Pasta == "9" )] <- NA
k1$Rice[which(k1$Rice == "9" )] <- NA
k1$Butter[which(k1$Butter == "9" )] <- NA
k1$Margarine[which(k1$Margarine == "9" )] <- NA
k1$Nuts[which(k1$Nuts == "9" )] <- NA
k1$Potatoes[which(k1$Potatoes == "9" )] <- NA
k1$Milk[which(k1$Milk == "9" )] <- NA
k1$Eggs[which(k1$Eggs == "9" )] <- NA
k1$Burgers_fast_food[which(k1$Burgers_fast_food == "9" )] <- NA
k1$Yakult_Vitagen[which(k1$Yakult_Vitagen == "9" )] <- NA
k1$Seafood[which(k1$Seafood == "1,3" )] <- NA
k1$Seafood[which(k1$Seafood == "2,3" )] <- NA
k1$Fruits[which(k1$Fruits == "2/3" )] <- NA
k1$Fruits[which(k1$Fruits == "2,3" )] <- NA
k1$Pulses[which(k1$Pulses == "1,3" )] <- NA
k1$Pulses[which(k1$Pulses == "2,3" )] <- NA
k1$Cereals[which(k1$Cereals == "1,2" )] <- NA
k1$Cereals[which(k1$Cereals == "2 and 3" )] <- NA
k1$Age[which(k1$Age == "999" )] <- NA
k1$Sex[which(k1$Sex == "9" )] <- NA

m <- glm(case3~Yakult_Vitagen, data=k1, family=binomial(link="logit"))
summary(m)
exp(m$coefficients) 
exp(confint(m))

k1$Margarine[is.na(k1$Margarine)] <- 0
k1$Butter[is.na(k1$Butter)] <- 0
k1$Seafood[is.na(k1$Seafood)] <- 0
k1$Burgers_fast_food[is.na(k1$Burgers_fast_food)] <- 0
k1$Meat[is.na(k1$Meat)] <- 0
k1$Eggs[is.na(k1$Eggs)] <- 0
k1$Milk[is.na(k1$Milk)] <- 0
k1$Fruits[is.na(k1$Fruits)] <- 0
k1$Vegetables[is.na(k1$Vegetables)] <- 0
k1$Pulses[is.na(k1$Pulses)] <- 0
k1$Cereals[is.na(k1$Cereals)] <- 0
k1$Potatoes[is.na(k1$Potatoes)] <- 0
k1$Rice[is.na(k1$Rice)] <- 0
k1$Nuts[is.na(k1$Nuts)] <- 0
k1$Pasta[is.na(k1$Pasta)] <- 0
k1$Yakult_Vitagen[is.na(k1$Yakult_Vitagen)] <- 0

k1$Meat2<-ifelse(k1$Meat=="2",2,
                 ifelse(k1$Meat=="3",7,0))
k1$Seafood2<-ifelse(k1$Seafood=="2",2,
                    ifelse(k1$Seafood=="3",7,0))
k1$Fruits2<-ifelse(k1$Fruits=="2",2,
                   ifelse(k1$Fruits=="3",7,0))
k1$Vegetables2<-ifelse(k1$Vegetables=="2",2,
                       ifelse(k1$Vegetables=="3",7,0))
k1$Pulses2<-ifelse(k1$Pulses=="2",2,
                   ifelse(k1$Pulses=="3",7,0))
k1$Cereals2<-ifelse(k1$Cereals=="2",2,
                    ifelse(k1$Cereals=="3",7,0))
k1$Pasta2<-ifelse(k1$Pasta=="2",2,
                  ifelse(k1$Pasta=="3",7,0))
k1$Rice2<-ifelse(k1$Rice=="2",2,
                 ifelse(k1$Rice=="3",7,0))
k1$Butter2<-ifelse(k1$Butter=="2",2,
                   ifelse(k1$Butter=="3",7,0))
k1$Margarine2<-ifelse(k1$Margarine=="2",2,
                      ifelse(k1$Margarine=="3",7,0))
k1$Nuts2<-ifelse(k1$Nuts=="2",2,
                 ifelse(k1$Nuts=="3",7,0))
k1$Potatoes2<-ifelse(k1$Potatoes=="2",2,
                     ifelse(k1$Potatoes=="3",7,0))
k1$Milk2<-ifelse(k1$Milk=="2",2,
                 ifelse(k1$Milk=="3",7,0))
k1$Eggs2<-ifelse(k1$Eggs=="2",2,
                 ifelse(k1$Eggs=="3",7,0))
k1$Burgers_fast_food2<-ifelse(k1$Burgers_fast_food=="2",2,
                              ifelse(k1$Burgers_fast_food=="3",7,0))
k1$Yakult_Vitagen2<-ifelse(k1$Yakult_Vitagen=="2",2,
                           ifelse(k1$Yakult_Vitagen=="3",7,0))
k1$fats<- k1$Meat2*12.888+k1$Butter2*82+k1$Margarine2*63.875+k1$Nuts2*50.498+k1$Burgers_fast_food2*14.677+k1$Eggs2*13.2333333333333+k1$Pasta2*5.546+k1$Seafood2*3.773+k1$Milk2*3.31666666666667+k1$Cereals2*2.2+k1$Rice2*0.5+k1$Pulses2*0.2+k1$Potatoes2*0.2+k1$Vegetables2*0.18+k1$Fruits2*0.16
k1$protein<-k1$Meat2*25.06+k1$Butter2*0.65+k1$Margarine2*0.35+k1$Nuts2*21.4+k1$Burgers_fast_food2*10.5+k1$Eggs2*12.2+k1$Pasta2*6.6+k1$Seafood2*22.73+k1$Milk2*3.97+k1$Cereals2*9.68+k1$Rice2*2.56+k1$Pulses2*2.48+k1$Potatoes2*1.98+k1$Vegetables2*2.15+k1$Fruits2*0.65+k1$Yakult_Vitagen2*1.2
k1$carbohydrates<-k1$Butter2*0.7+k1$Margarine2*0.5+k1$Nuts2*16.282+k1$Burgers_fast_food2*24.699+k1$Eggs2*2.26666666666667+k1$Pasta2*26.5444444444444+k1$Seafood2*3.1075+k1$Milk2*6.93333333333333+k1$Cereals2*33.8325+k1$Rice2*51.4+k1$Pulses2*9.55+k1$Potatoes2*37.59+k1$Vegetables2*6.29333333333333+k1$Fruits2*11.01+k1$Yakult_Vitagen2*10.1
k1$Sodium<-k1$Meat2*137.40+k1$Butter2*310.00+k1$Margarine2*523.50+k1$Nuts2*5.60+k1$Burgers_fast_food2*440.80+k1$Eggs2*106.20+k1$Pasta2*215.10+k1$Seafood2*464.80+k1$Milk2*44.17+k1$Cereals2*515.00+k1$Rice2*4.33+k1$Pulses2*5.00+k1$Potatoes2*10.50+k1$Vegetables2*53.62+k1$Fruits2*2.83+k1$Yakult_Vitagen2*41.00
k1$Fibers<-k1$Nuts2*9.20+k1$Burgers_fast_food2*2.21+k1$Pasta2*1.92+k1$Seafood2*0.05+k1$Milk2*0.18+k1$Cereals2*3.98+k1$Rice2*0.97+k1$Pulses2*2.37+k1$Potatoes2*2.65+k1$Vegetables2*2.23+k1$Fruits2*2.07+k1$Yakult_Vitagen2*0.70
k1$Cholesterol<-k1$Meat2*73.80+k1$Butter2*240.00+k1$Burgers_fast_food2*19.78+k1$Eggs2*551.33+k1$Pasta2*14.24+k1$Seafood2*115.60+k1$Milk2*10.07
k1$Sugar<-k1$Butter2*0.70+k1$Margarine2*0.50+k1$Nuts2*4.97+k1$Burgers_fast_food2*4.80+k1$Eggs2*0.24+k1$Pasta2*1.37+k1$Milk2*7.28+k1$Cereals2*6.90+k1$Rice2*0.05+k1$Potatoes2*7.32+k1$Vegetables2*6.93+k1$Fruits2*8.68+k1$Yakult_Vitagen2*10.10
k1$vitaminC<-k1$Meat2*0.2+k1$Nuts2*14.26+k1$Burgers_fast_food2*0.2+k1$Seafood2*2.96+k1$Milk2*1.53+k1$Cereals2*1.86+k1$Pulses2*18.40+k1$Potatoes2*15.05+k1$Vegetables2*24.59+k1$Fruits2*22.70+k1$Yakult_Vitagen2*0.4
k1$calcium<-k1$Meat2*10.80+k1$Butter2*17.50+k1$Margarine2*1.5+k1$Nuts2*115.20+k1$Burgers_fast_food2*50.13+k1$Eggs2*51.00+k1$Pasta2*29.81+k1$Seafood2*107.60+k1$Milk2*117.67+k1$Cereals2*136.75+k1$Rice2*3.00+k1$Pulses2*48.00+k1$Potatoes2*15.50+k1$Vegetables2*46.17+k1$Fruits2*12.67+k1$Yakult_Vitagen2*79.00
k1$Iron<-k1$Meat2*1.91+k1$Nuts2*4.87+k1$Burgers_fast_food2*1.25+k1$Eggs2*3.13+k1$Pasta2*1.00+k1$Seafood2*2.73+k1$Milk2*0.17+k1$Cereals2*8.25+k1$Rice2*0.26+k1$Pulses2*0.99+k1$Potatoes2*0.98+k1$Vegetables2*1.23+k1$Fruits2*0.27+k1$Yakult_Vitagen2*1.30
k1$vitaminA<-k1$Meat2*10.20+k1$Butter2*1082.50+k1$Margarine2*1106.00+k1$Nuts2*16.00+k1$Burgers_fast_food2*16.64+k1$Eggs2*267.67+k1$Pasta2*35.96+k1$Seafood2*67.00+k1$Milk2*95.50+k1$Cereals2*22.75+k1$Pulses2*47.67+k1$Vegetables2*480.85+k1$Fruits2*41.83+k1$Yakult_Vitagen2*39.00
k1$vitaminD<-k1$Meat2*8.60+k1$Butter2*44.00+k1$Margarine2*347.00+k1$Seafood2*30.34+k1$Milk2*26.36+k1$Cereals2*1.50+k1$Potatoes2*0.98

k1$fats<-as.numeric(k1$fats)
summary(k1$fats)
quantile(k1$fats,na.rm=TRUE)
quantile(k1$fats,probs=c(.68,.95),na.rm=TRUE)
sd(k1$fats)
k1$protein<-as.numeric(k1$protein)
summary(k1$protein)
quantile(k1$protein,na.rm=TRUE)
k1$carbohydrates<-as.numeric(k1$carbohydrates)
summary(k1$carbohydrates)
quantile(k1$carbohydrates,na.rm=TRUE)
k1$Fibers<-as.numeric(k1$Fibers)
summary(k1$Fibers)
quantile(k1$Fibers,na.rm=TRUE)
k1$Sodium<-as.numeric(k1$Sodium)
summary(k1$Sodium)
quantile(k1$Sodium,na.rm=TRUE)
k1$Cholesterol<-as.numeric(k1$Cholesterol)
summary(k1$Cholesterol)
quantile(k1$Cholesterol,na.rm=TRUE)
k1$Sugar<-as.numeric(k1$Sugar)
summary(k1$Sugar)
quantile(k1$Sugar,na.rm=TRUE)
k1$vitaminC<-as.numeric(k1$vitaminC)
summary(k1$vitaminC)
quantile(k1$vitaminC,na.rm=TRUE)
k1$calcium<-as.numeric(k1$calcium)
summary(k1$calcium)
quantile(k1$calcium,na.rm=TRUE)
k1$Iron<-as.numeric(k1$Iron)
summary(k1$Iron)
quantile(k1$Iron,na.rm=TRUE)
k1$vitaminA<-as.numeric(k1$vitaminA)
summary(k1$vitaminA)
quantile(k1$vitaminA,na.rm=TRUE)
k1$vitaminD<-as.numeric(k1$vitaminD)
summary(k1$vitaminD)
quantile(k1$vitaminD,na.rm=TRUE)
k1$vitaminA_level<-ifelse(k1$vitaminA<5544.29,"low",
                          ifelse(k1$vitaminA>10674.82,"high","moderate"))
k1$vitaminA_level<-as.factor(k1$vitaminA_level)
k1$vitaminA_level<-relevel(k1$vitaminA_level,ref = "low")
summary(k1$vitaminA_level)
k1$vitaminD_level<-ifelse(k1$vitaminD<272.22,"low",
                          ifelse(k1$vitaminD>1099.86,"high","moderate"))
k1$vitaminD_level<-as.factor(k1$vitaminD_level)
k1$vitaminD_level<-relevel(k1$vitaminD_level,ref = "low")
summary(k1$vitaminD_level)
k1$fats_level<-ifelse(k1$fats<253.646,"low",
                      ifelse(k1$fats>641.301,"high","moderate"))
k1$fats_level<-as.factor(k1$fats_level)
k1$fats_level<-relevel(k1$fats_level,ref = "low")
summary(k1$fats_level)
k1$protein_level<-ifelse(k1$protein<374.450,"low",
                         ifelse(k1$protein>556.930,"high","moderate"))
k1$protein_level<-as.factor(k1$protein_level)
k1$protein_level<-relevel(k1$protein_level,ref = "low")
summary(k1$protein_level)
k1$carbohydrates_level<-ifelse(k1$carbohydrates<698.287,"low",
                               ifelse(k1$carbohydrates>995.619,"high","moderate"))
k1$carbohydrates_level<-as.factor(k1$carbohydrates_level)
k1$carbohydrates_level<-relevel(k1$carbohydrates_level,ref = "low")
summary(k1$carbohydrates_level)
k1$Fibers_level<-ifelse(k1$Fibers<59.770,"low",
                        ifelse(k1$Fibers>99.47,"high","moderate"))
k1$Fibers_level<-as.factor(k1$Fibers_level)
k1$Fibers_level<-relevel(k1$Fibers_level,ref = "low")
summary(k1$Fibers_level)
k1$Sodium_level<-ifelse(k1$Sodium<5904.703,"low",
                        ifelse(k1$Sodium>10274.612,"high","moderate"))
k1$Sodium_level<-as.factor(k1$Sodium_level)
k1$Sodium_level<-relevel(k1$Sodium_level,ref = "low")
summary(k1$Sodium_level)
k1$Cholesterol_level<-ifelse(k1$Cholesterol<2390.16,"low",
                             ifelse(k1$Cholesterol>5274.19,"high","moderate"))
k1$Cholesterol_level<-as.factor(k1$Cholesterol_level)
k1$Cholesterol_level<-relevel(k1$Cholesterol_level,ref = "low")
summary(k1$Cholesterol_level)
k1$Sugar_level<-ifelse(k1$Sugar<144.980,"low",
                       ifelse(k1$Sugar>237.340,"high","moderate"))
k1$Sugar_level<-as.factor(k1$Sugar_level)
k1$Sugar_level<-relevel(k1$Sugar_level,ref = "low")
summary(k1$Sugar_level)
k1$vitaminC_level<-ifelse(k1$vitaminC<304.77,"low",
                          ifelse(k1$vitaminC>465.85,"high","moderate"))
k1$vitaminC_level<-as.factor(k1$vitaminC_level)
k1$vitaminC_level<-relevel(k1$vitaminC_level,ref = "low")
summary(k1$vitaminC_level)
k1$calcium_level<-ifelse(k1$calcium<1971.482,"low",
                         ifelse(k1$calcium>3363.710,"high","moderate"))
k1$calcium_level<-as.factor(k1$calcium_level)
k1$calcium_level<-relevel(k1$calcium_level,ref = "low")
summary(k1$calcium_level)
k1$Iron_level<-ifelse(k1$Iron<73.9175,"low",
                      ifelse(k1$Iron>129.2800,"high","moderate"))
k1$Iron_level<-as.factor(k1$Iron_level)
k1$Iron_level<-relevel(k1$Iron_level,ref = "low")
summary(k1$Iron_level)

m <- glm(case3~Iron_level, data=k1, family=binomial(link="logit"))
summary(m)
exp(m$coefficients) 
exp(confint(m))
k1$fat_vitaminC<-ifelse(k1$Cholesterol_level=="low" & k1$vitaminC_level=="low","low",
                        ifelse(k1$Cholesterol_level=="low" & k1$vitaminC_level=="moderate","lowF_highP",
                               ifelse(k1$Cholesterol_level=="low" & k1$vitaminC_level=="high","lowF_highP",
                                      ifelse(k1$Cholesterol_level=="moderate" & k1$vitaminC_level=="low","moderateF_lowP",
                                             ifelse(k1$Cholesterol_level=="moderate" & k1$vitaminC_level=="moderate","moderate",
                                                    ifelse(k1$Cholesterol_level=="moderate" & k1$vitaminC_level=="high","moderateF_highP",
                                                           ifelse(k1$Cholesterol_level=="high" & k1$vitaminC_level=="low","highF_lowP",
                                                                  ifelse(k1$Cholesterol_level=="high" & k1$vitaminC_level=="moderate","highF_lowP",
                                                                         ifelse(k1$Cholesterol_level=="high" & k1$vitaminC_level=="high","high",NA)))))))))
k1$fat_vitaminC<-as.factor(k1$fat_vitaminC)
k1$fat_vitaminC<-relevel(k1$fat_vitaminC,ref="highF_lowP")
summary(k1$fat_vitaminC)
addmargins(table(k1$fat_vitaminC,k1$case3,exclude=NULL))

k1$fat_fibers<-ifelse(k1$vitaminD_level=="low" & k1$fats_level=="low","low",
                      ifelse(k1$vitaminD_level=="low" & k1$fats_level=="moderate","lowF_highP",
                             ifelse(k1$vitaminD_level=="low" & k1$fats_level=="high","lowF_highP",
                                    ifelse(k1$vitaminD_level=="moderate" & k1$fats_level=="low","moderateF_lowP",
                                           ifelse(k1$vitaminD_level=="moderate" & k1$fats_level=="moderate","moderate",
                                                  ifelse(k1$vitaminD_level=="moderate" & k1$fats_level=="high","moderateF_highP",
                                                         ifelse(k1$vitaminD_level=="high" & k1$fats_level=="low","highF_lowP",
                                                                ifelse(k1$vitaminD_level=="high" & k1$fats_level=="moderate","highF_lowP",
                                                                       ifelse(k1$vitaminD_level=="high" & k1$fats_level=="high","high",NA)))))))))
k1$fat_fibers<-as.factor(k1$fat_fibers)
k1$fat_fibers<-relevel(k1$fat_fibers,ref="low")
summary(k1$fat_fibers)
m <- glm(at~fat_fibers+Age+Sex+Income, data=k1, family=binomial(link="logit"))
summary(m)
exp(m$coefficients)
exp(confint(m))

k1$fat_vitaminC<-ifelse(k1$vitaminA_level=="low" & k1$vitaminC_level=="low","low",
                        ifelse(k1$vitaminA_level=="low" & k1$vitaminC_level=="moderate","lowF_highP",
                               ifelse(k1$vitaminA_level=="low" & k1$vitaminC_level=="high","lowF_highP",
                                      ifelse(k1$vitaminA_level=="moderate" & k1$vitaminC_level=="low","moderateF_lowP",
                                             ifelse(k1$vitaminA_level=="moderate" & k1$vitaminC_level=="moderate","moderate",
                                                    ifelse(k1$vitaminA_level=="moderate" & k1$vitaminC_level=="high","moderateF_highP",
                                                           ifelse(k1$vitaminA_level=="high" & k1$vitaminC_level=="low","highF_lowP",
                                                                  ifelse(k1$vitaminA_level=="high" & k1$vitaminC_level=="moderate","highF_lowP",
                                                                         ifelse(k1$vitaminA_level=="high" & k1$vitaminC_level=="high","high",NA)))))))))
k1$fat_vitaminC<-as.factor(k1$fat_vitaminC)
k1$fat_vitaminC<-relevel(k1$fat_vitaminC,ref="highF_lowP")
summary(k1$fat_vitaminC)
summary(k1$DFS.plant)
k1$fat_vitaminC<-ifelse(k1$DFS.animal=="Low" & k1$DFS.plant=="Low","a", #low#
                        ifelse(k1$DFS.animal=="Low" & k1$DFS.plant=="Moderate","b", #lowA_highP#
                               ifelse(k1$DFS.animal=="Low" & k1$DFS.plant=="High","b", #lowA_highP#
                                      ifelse(k1$DFS.animal=="Moderate" & k1$DFS.plant=="Low","c", #moderateA_lowP#
                                             ifelse(k1$DFS.animal=="Moderate" & k1$DFS.plant=="Moderate","d", #moderate#
                                                    ifelse(k1$DFS.animal=="Moderate" & k1$DFS.plant=="High","e", #moderateA_highP#
                                                           ifelse(k1$DFS.animal=="High" & k1$DFS.plant=="Low","f", #highA_lowP#
                                                                  ifelse(k1$DFS.animal=="High" & k1$DFS.plant=="Moderate","f", #highA_lowP#
                                                                         ifelse(k1$DFS.animal=="High" & k1$DFS.plant=="High","g",NA))))))))) #High#
k1$fat_vitaminC<-as.factor(k1$fat_vitaminC)
k1$fat_vitaminC<-relevel(k1$fat_vitaminC,ref="b")
summary(k1$fat_vitaminC)
addmargins(table(k1$fat_vitaminC,k1$case3,exclude=NULL))
m <- glm(case3~fat_vitaminC+Age+Sex+family+BMI, data=k1, family=binomial(link="logit"))
summary(m)
exp(m$coefficients)
exp(confint(m))
#----------Linoleic acid---------------------18:2 PUFA
k1$LA <- (k1$Meat2*1.25 + k1$Seafood2*0.736 + k1$Fruits2*0.0478 + k1$Vegetables2*0.0457  + k1$Pulses2*0.105 + 
            k1$Cereals2*1.25  + k1$Pasta2*0.420 + k1$Rice2*0.132 + k1$Butter2*2.73 + k1$Margarine2*17.9 + k1$Nuts2*10.6 + 
            k1$Potatoes2*0.0520 + k1$Milk2*0.280 + k1$Eggs2*1.06 + k1$Burgers_fast_food2*4.63 + k1$Yakult_Vitagen2*0.00)
quantile(k1$LA, probs =c(.33,.66))
summary(k1$LA)
sd(k1$LA)

k1$LA2 <- ifelse(k1$LA >= 88.70617  ,"High intake",
                 ifelse(k1$LA <= 44.16651   ,"Low intake", "Moderate intake"))
k1$LA2 <- factor(k1$LA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(k1$LA2)

#-----------Alpha-linoleic acid--------------18:3 PUFA
k1$ALA <- (k1$Meat2*0.120 + k1$Seafood2*0.0477 + k1$Fruits2*0.0167 + k1$Vegetables2*0.0310  + k1$Pulses2*0.0975 + 
             k1$Cereals2*0.120  + k1$Pasta2*0.0340 + k1$Rice2*0.00870 + k1$Butter2*0.315 + k1$Margarine2*2.67 + k1$Nuts2*0.131 + 
             k1$Potatoes2*0.00650 + k1$Milk2*0.0620 + k1$Eggs2*0.180 + k1$Burgers_fast_food2*0.410 + k1$Yakult_Vitagen2*0.00)
quantile(k1$ALA, probs =c(.33,.66))
summary(k1$ALA)
sd(k1$ALA)

k1$ALA2 <- ifelse(k1$ALA >= 9.51955    ,"High intake",
                  ifelse(k1$ALA <= 4.06070   ,"Low intake", "Moderate intake"))
k1$ALA2 <- factor(k1$ALA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(k1$ALA2)

#----------Trans fatty acid----------------
k1$TFA <- (k1$Meat2*0.514 + k1$Seafood2*0.00350 + k1$Fruits2*0.00 + k1$Vegetables2*0.00  + k1$Pulses2*0.00 + 
             k1$Cereals2*0.0210  + k1$Pasta2*0.0404 + k1$Rice2*0.00 + k1$Butter2*3.28 + k1$Margarine2*6.86 + k1$Nuts2*0.0350 + 
             k1$Potatoes2*0.00 + k1$Milk2*0.185 + k1$Eggs2*0.0380 + k1$Burgers_fast_food2*0.180 + k1$Yakult_Vitagen2*0.00)
quantile(k1$TFA, probs =c(.33,.66))
summary(k1$TFA)
sd(k1$TFA)

k1$TFA2 <- ifelse(k1$TFA >= 24.8188    ,"High intake",
                  ifelse(k1$TFA <= 5.3305   ,"Low intake", "Moderate intake"))
k1$TFA2 <- factor(k1$TFA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(k1$TFA2)

#---------Saturated fatty acids-----------
k1$SFA <- (k1$Meat2*6.43 + k1$Seafood2*0.717 + k1$Fruits2*0.0540 + k1$Vegetables2*0.0359  + k1$Pulses2*0.0742 + 
             k1$Cereals2*0.113  + k1$Pasta2*0.380 + k1$Rice2*0.0870 + k1$Butter2*51.0 + k1$Margarine2*12.7 + k1$Nuts2*7.04 + 
             k1$Potatoes2*0.0325 + k1$Milk2*1.50 + k1$Eggs2*3.40 + k1$Burgers_fast_food2*3.93 + k1$Yakult_Vitagen2*0.00)
quantile(k1$SFA, probs =c(.33,.66))
summary(k1$SFA)
sd(k1$SFA)

k1$SFA2 <- ifelse(k1$SFA >= 208.0798  ,"High intake",
                  ifelse(k1$SFA <= 90.7717  ,"Low intake", "Moderate intake"))
k1$SFA2 <- factor(k1$SFA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(k1$SFA2)

#---------Mono-unsaturated fatty acids------
k1$MUFA <- (k1$Meat2*7.64 + k1$Seafood2*1.07 + k1$Fruits2*0.0480 + k1$Vegetables2*0.0190  + k1$Pulses2*0.0520 + 
              k1$Cereals2*0.580  + k1$Pasta2*0.409 + k1$Rice2*0.145 + k1$Butter2*21.7 + k1$Margarine2*27.7 + k1$Nuts2*34.7 + 
              k1$Potatoes2*0.00150 + k1$Milk2*0.830 + k1$Eggs2*4.89 + k1$Burgers_fast_food2*5.77 + k1$Yakult_Vitagen2*0.00)
quantile(k1$MUFA, probs =c(.33,.66))
summary(k1$MUFA)
sd(k1$MUFA)

k1$MUFA2 <- ifelse(k1$MUFA >= 249.309   ,"High intake",
                   ifelse(k1$MUFA <= 142.111  ,"Low intake", "Moderate intake"))
k1$MUFA2 <- factor(k1$MUFA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(k1$MUFA2)

#----------------Total Fats--------------------
k1$TotF <- (k1$Meat2*17.4 + k1$Seafood2*3.22 + k1$Fruits2*0.225 + k1$Vegetables2*0.191  + k1$Pulses2*0.435 + 
              k1$Cereals2*3.38  + k1$Pasta2*1.57 + k1$Rice2*0.410 + k1$Butter2*81.1 + k1$Margarine2*65.2 + k1$Nuts2*54.4 + 
              k1$Potatoes2*0.135 + k1$Milk2*2.84 + k1$Eggs2*11.6 + k1$Burgers_fast_food2*16.1 + k1$Yakult_Vitagen2*0.00)
quantile(k1$TotF, probs =c(.33,.66))
summary(k1$TotF)
sd(k1$TotF)

k1$TotF2 <- ifelse(k1$TotF >= 606.4170  ,"High intake",
                   ifelse(k1$TotF <= 332.4831   ,"Low intake", "Moderate intake"))
k1$TotF2 <- factor(k1$TotF2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(k1$TotF2)

#------------Concordant analysis---------------
#-----LA and Total Fats------------------------
k1$ToF1 <- ifelse(k1$LA2 == "High intake" & k1$TotF2 == "High intake", "High",
                  ifelse(k1$LA2 == "High intake" & k1$TotF2 == "Moderate intake", "HighLAlowTotF",
                         ifelse(k1$LA2 == "High intake" & k1$TotF2 == "Low intake", "HighLAlowTotF",
                                ifelse(k1$LA2 == "Moderate intake" & k1$TotF2 == "High intake", "ModLAhighTotF",
                                       ifelse(k1$LA2 == "Moderate intake" & k1$TotF2 == "Moderate intake", "Mod",
                                              ifelse(k1$LA2 == "Moderate intake" & k1$TotF2 == "Low intake", "ModLAlowTotF",
                                                     ifelse(k1$LA2 == "Low intake" & k1$TotF2 == "High intake", "LowLAhighTotF",
                                                            ifelse(k1$LA2 == "Low intake" & k1$TotF2 == "Moderate intake", "LowLAhighTotF",
                                                                   ifelse(k1$LA2 == "Low intake" & k1$TotF2 == "Low intake", "Low", NA)))))))))
k1$ToF1 <- as.factor(k1$ToF1)
k1$ToF1 <- relevel(k1$ToF1, ref = "Low")
summary(k1$ToF1)

addmargins(table(k1$ToF1, k1$case3, exclude = NULL))



#-----ALA and Total Fats---------------------------------
k1$ToF2 <- ifelse(k1$ALA2 == "High intake" & k1$TotF2 == "High intake", "High",
                  ifelse(k1$ALA2 == "High intake" & k1$TotF2 == "Moderate intake", "HighALAlowTotF",
                         ifelse(k1$ALA2 == "High intake" & k1$TotF2 == "Low intake", "HighALAlowTotF",
                                ifelse(k1$ALA2 == "Moderate intake" & k1$TotF2 == "High intake", "ModALAhighTotF",
                                       ifelse(k1$ALA2 == "Moderate intake" & k1$TotF2 == "Moderate intake", "Mod",
                                              ifelse(k1$ALA2 == "Moderate intake" & k1$TotF2 == "Low intake", "ModALAlowTotF",
                                                     ifelse(k1$ALA2 == "Low intake" & k1$TotF2 == "High intake", "LowALAhighTotF",
                                                            ifelse(k1$ALA2 == "Low intake" & k1$TotF2 == "Moderate intake", "LowALAhighTotF",
                                                                   ifelse(k1$ALA2 == "Low intake" & k1$TotF2 == "Low intake", "Low", NA)))))))))
k1$ToF2 <- as.factor(k1$ToF2)
k1$ToF2 <- relevel(k1$ToF2, ref = "Low")
summary(k1$ToF2)

addmargins(table(k1$ToF2, k1$case3, exclude = NULL))


#-----TFA and Total Fats----------------------------------
k1$ToF3 <- ifelse(k1$TFA2 == "High intake" & k1$TotF2 == "High intake", "High",
                  ifelse(k1$TFA2 == "High intake" & k1$TotF2 == "Moderate intake", "HighTFAlowTotF",
                         ifelse(k1$TFA2 == "High intake" & k1$TotF2 == "Low intake", "HighTFAlowTotF",
                                ifelse(k1$TFA2 == "Moderate intake" & k1$TotF2 == "High intake", "ModTFAhighTotF",
                                       ifelse(k1$TFA2 == "Moderate intake" & k1$TotF2 == "Moderate intake", "Mod",
                                              ifelse(k1$TFA2 == "Moderate intake" & k1$TotF2 == "Low intake", "ModTFAlowTotF",
                                                     ifelse(k1$TFA2 == "Low intake" & k1$TotF2 == "High intake", "LowTFAhighTotF",
                                                            ifelse(k1$TFA2 == "Low intake" & k1$TotF2 == "Moderate intake", "LowTFAhighTotF",
                                                                   ifelse(k1$TFA2 == "Low intake" & k1$TotF2 == "Low intake", "Low", NA)))))))))
k1$ToF3 <- as.factor(k1$ToF3)
k1$ToF3 <- relevel(k1$ToF3, ref = "Low")
summary(k1$ToF3)

addmargins(table(k1$ToF3, k1$case3, exclude = NULL))


#-----SFA and Total Fats-------------------------------
k1$ToF4 <- ifelse(k1$SFA2 == "High intake" & k1$TotF2 == "High intake", "High",
                  ifelse(k1$SFA2 == "High intake" & k1$TotF2 == "Moderate intake", "HighSFAlowTotF",
                         ifelse(k1$SFA2 == "High intake" & k1$TotF2 == "Low intake", "HighSFAlowTotF",
                                ifelse(k1$SFA2 == "Moderate intake" & k1$TotF2 == "High intake", "ModSFAhighTotF",
                                       ifelse(k1$SFA2 == "Moderate intake" & k1$TotF2 == "Moderate intake", "Mod",
                                              ifelse(k1$SFA2 == "Moderate intake" & k1$TotF2 == "Low intake", "ModSFAlowTotF",
                                                     ifelse(k1$SFA2 == "Low intake" & k1$TotF2 == "High intake", "LowSFAhighTotF",
                                                            ifelse(k1$SFA2 == "Low intake" & k1$TotF2 == "Moderate intake", "LowSFAhighTotF",
                                                                   ifelse(k1$SFA2 == "Low intake" & k1$TotF2 == "Low intake", "Low", NA)))))))))
k1$ToF4 <- as.factor(k1$ToF4)
k1$ToF4 <- relevel(k1$ToF4, ref = "Low")
summary(k1$ToF4)

addmargins(table(k1$ToF4, k1$case3, exclude = NULL))


#-----MUFA and Total Fats------------------------
k1$ToF5 <- ifelse(k1$MUFA2 == "High intake" & k1$TotF2 == "High intake", "High",
                  ifelse(k1$MUFA2 == "High intake" & k1$TotF2 == "Moderate intake", "HighMUFAlowTotF",
                         ifelse(k1$MUFA2 == "High intake" & k1$TotF2 == "Low intake", "HighMUFAlowTotF",
                                ifelse(k1$MUFA2 == "Moderate intake" & k1$TotF2 == "High intake", "ModMUFAhighTotF",
                                       ifelse(k1$MUFA2 == "Moderate intake" & k1$TotF2 == "Moderate intake", "Mod",
                                              ifelse(k1$MUFA2 == "Moderate intake" & k1$TotF2 == "Low intake", "ModMUFAlowTotF",
                                                     ifelse(k1$MUFA2 == "Low intake" & k1$TotF2 == "High intake", "LowMUFAhighTotF",
                                                            ifelse(k1$MUFA2 == "Low intake" & k1$TotF2 == "Moderate intake", "LowMUFAhighTotF",
                                                                   ifelse(k1$MUFA2 == "Low intake" & k1$TotF2 == "Low intake", "Low", NA)))))))))
k1$ToF5 <- as.factor(k1$ToF5)
k1$ToF5 <- relevel(k1$ToF5, ref = "Low")
summary(k1$ToF5)

addmargins(table(k1$ToF5, k1$case3, exclude = NULL))

#regression analysis
d <- glm(RC~LA2+Age+Sex+family+BMI, data=k1, family=binomial(link="logit"))
summary(d)
exp(d$coefficients) 
exp(confint(d))

#DFS
hi_fats<-c('Margarine', 'Butter', 'Seafood', 'Burgers_fast_food','Meat','Eggs','Milk')
lo_fats<-c('Fruits', 'Vegetables', 'Pulses','Cereals','Potatoes','Rice')
summary(k1[,c(hi_fats,lo_fats)])
for(.i in 1:nrow(k1))
{.lo1<-rowSums(k1[.i, lo_fats]==1)
.lo2<-rowSums(k1[.i, lo_fats]==2)
.lo3<-rowSums(k1[.i, lo_fats]==3)
.hi1<-rowSums(k1[.i, hi_fats]==1)
.hi2<-rowSums(k1[.i, hi_fats]==2)
.hi3<-rowSums(k1[.i, hi_fats]==3)
k1[.i, 'DFS']<-(-2*.lo2)+(-7*.lo3)+(2*.hi2)+(7*.hi3)}
quantile(k1$DFS,prob=c(.33,.66),na.rm=TRUE)
summary(k1$DFS)
k1$DFS.CAT<-with(k1, ifelse(DFS>=0, 'High',
                            ifelse(DFS>=-7 & DFS<=-1, 'Moderate',
                                   ifelse(DFS<=-8, 'Low',NA))))
k1$DFS.CAT<-as.factor(k1$DFS.CAT)
summary(k1$DFS.CAT)
k1$DFS.CAT<-relevel(k1$DFS.CAT,ref = "Low")
contrasts(k1$DFS.CAT)

hi_gi<-c('Cereals', 'Rice', 'Potatoes', 'Burgers_fast_food')
lo_gi<-c('Fruits','Vegetables','Pulses', 'Nuts', 'Milk', 'Yakult_Vitagen')
summary(k1[, c(hi_gi, lo_gi)])
for(.i in 1:nrow(k1))
{.lo1<-rowSums(k1[.i, lo_gi]==1)
.lo2<-rowSums(k1[.i, lo_gi]==2)
.lo3<-rowSums(k1[.i, lo_gi]==3)
.hi1<-rowSums(k1[.i, hi_gi]==1)
.hi2<-rowSums(k1[.i, hi_gi]==2)
.hi3<-rowSums(k1[.i, hi_gi]==3)
k1[.i, 'DFS']<-(-2*.lo2)+(-7*.lo3)+(2*.hi2)+(7*.hi3)}
quantile(k1$DFS,prob=c(.33,.66),na.rm=TRUE)
summary(k1$DFS)
k1$DFS.CAT<-with(k1, ifelse(DFS>=-2, 'High',
                            ifelse(DFS>=-8 & DFS<=-3, 'Moderate',
                                   ifelse(DFS<=-9, 'Low',NA))))
k1$DFS.CAT<-as.factor(k1$DFS.CAT)
summary(k1$DFS.CAT)
k1$DFS.CAT<-relevel(k1$DFS.CAT,ref = "Low")
contrasts(k1$DFS.CAT)

hi_proteins<-c('Meat', 'Seafood', 'Nuts', 'Eggs','Burgers_fast_food','Cereals')
lo_proteins<-c('Milk', 'Rice', 'Pulses','Vegetables','Potatoes','Fruits')
summary(k1[,c(hi_proteins,lo_proteins)])
for(.i in 1:nrow(k1))
{.lo1<-rowSums(k1[.i, lo_proteins]==1)
.lo2<-rowSums(k1[.i, lo_proteins]==2)
.lo3<-rowSums(k1[.i, lo_proteins]==3)
.hi1<-rowSums(k1[.i, hi_proteins]==1)
.hi2<-rowSums(k1[.i, hi_proteins]==2)
.hi3<-rowSums(k1[.i, hi_proteins]==3)
k1[.i, 'DFS']<-(-2*.lo2)+(-7*.lo3)+(2*.hi2)+(7*.hi3)}
quantile(k1$DFS,prob=c(.33,.66),na.rm=TRUE)
summary(k1$DFS)
k1$DFS.CAT<-with(k1, ifelse(DFS>=0, 'High',
                            ifelse(DFS>=-6 & DFS<=-1, 'Moderate',
                                   ifelse(DFS<=-7, 'Low',NA))))
k1$DFS.CAT<-as.factor(k1$DFS.CAT)
summary(k1$DFS.CAT)
k1$DFS.CAT<-relevel(k1$DFS.CAT,ref = "Low")
contrasts(k1$DFS.CAT)

animal_proteins<-c('Meat', 'Seafood', 'Milk', 'Eggs','Burgers_fast_food', 'Butter')
plant_proteins<-c('Nuts', 'Rice', 'Pulses','Vegetables','Potatoes','Fruits','Cereals','Pasta')
summary(k1[,c(animal_proteins,plant_proteins)])
for(.i in 1:nrow(k1))
{.plant1<-rowSums(k1[.i, plant_proteins]==1)
.plant2<-rowSums(k1[.i, plant_proteins]==2)
.plant3<-rowSums(k1[.i, plant_proteins]==3)
k1[.i, 'DFS']<-(2*.plant2)+(7*.plant3)}
quantile(k1$DFS,prob=c(.33,.66),na.rm=TRUE)
summary(k1$DFS)
k1$DFS.plant<-with(k1, ifelse(DFS>=34, 'High',
                              ifelse(DFS>=27 & DFS<=33, 'Moderate',
                                     ifelse(DFS<=26, 'Low',NA))))
k1$DFS.plant<-as.factor(k1$DFS.plant)
summary(k1$DFS.plant)
k1$DFS.plant<-relevel(k1$DFS.plant,ref = "Low")
contrasts(k1$DFS.CAT)
for(.i in 1:nrow(k1))
{.animal1<-rowSums(k1[.i, animal_proteins]==1)
.animal2<-rowSums(k1[.i, animal_proteins]==2)
.animal3<-rowSums(k1[.i, animal_proteins]==3)
k1[.i, 'DFS']<-(2*.animal2)+(7*.animal3)}
quantile(k1$DFS,prob=c(.33,.66),na.rm=TRUE)
summary(k1$DFS)
k1$DFS.animal<-with(k1, ifelse(DFS>=25, 'High',
                               ifelse(DFS>=19 & DFS<=24, 'Moderate',
                                      ifelse(DFS<=18, 'Low',NA))))
k1$DFS.animal<-as.factor(k1$DFS.animal)
summary(k1$DFS.animal)
k1$DFS.animal<-relevel(k1$DFS.animal,ref = "Low")
contrasts(k1$DFS.CAT)
m <- glm(case3~DFS.CAT, data=k1, family=binomial(link="logit"))
summary(m)
exp(m$coefficients)
exp(confint(m))

