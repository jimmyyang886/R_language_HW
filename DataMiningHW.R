library('dplyr') 
library('rpart')
library('caret')
library('modeldata')
library('readr')

# (1) 請讀取lvr_prices_big5.csv檔案
lvr <- read_csv("d://R/riii/hw/lvr_prices_big5.csv", col_names = TRUE, locale = locale(encoding='big5'))

# (2) 請問可使用哪個函式觀看檔案的資料結構？
str(lvr)

# (3) 請篩選出
# 1. city_land_type為住宅用
# 2. total_price > 0
# 3. building_sqmeter > 0
# 4. finish_ymd 非空值
# 的房屋資料,並存入house變數中。
house<-lvr  %>% filter(city_land_type=='住' , total_price>0, (is.na(finish_ymd)==FALSE))

# (4)請使用house資料，利用房屋價格(total_price)及房屋平方米數(building_sqmeter)兩欄位，產生一新欄位為每平方米價格(price_per_sqmeter)，並將其四捨五入到整數位。
house=house%>% mutate(price_per_sqmeter=round(total_price/building_sqmeter))
house%>%arrange(desc(price_per_sqmeter))

# (5) 請使用house資料，利用scale() 將每平方米價格(price_per_sqmeter)欄位資料標準化，並剔除掉outlier資料(z-score > 3)。
house=house %>% mutate(price_per_sqmeter_s=scale(price_per_sqmeter))
house=house %>% filter(abs(price_per_sqmeter_s)<=3)

# (6)試利用房屋完工日期(finish_ymd)產生一新變數為屋齡(building_age)加入house資料中。
# hint1: 取得當前日期的函數為 Sys.Date()
# hint2: 一年請以365天計算，四捨五入至整數位
# hint3: 將運算完的資料轉為整數型態(integer)
house=house%>%mutate(building_age=as.integer(round((Sys.Date()-house$finish_ymd)/365)))


# (7)請讀取final資料夾下的house_danger檔案，並將house資料集和house_danger資料集以left outer join方式join起來，存回house變數中
danger <- read_csv("d://R/riii/hw/house_danger.csv")

str(danger)

names(house)[2]=names(danger)[2]

house=house %>% merge(danger, by = "ID", all.x = TRUE)



# (8)請將house資料以8:2的比例分為訓練集和測試集，將訓練集資料存在trainset變數中，將測試集資料存在testset變數中。
ind<-sample(1:2, size=nrow(house), replace=T, prob=c(0.8, 0.2))
trainset=house[ind==1,]
testset=house[ind==2,]

# (9)利用rpart套件建立一預測房屋是否為危樓(danger)的決策樹模型，請利用行政區(area), 屋齡(building_age), 房屋總平方米數(building_sqmeter),
#房屋類型(building_type)及每平方米價格(price_per_sqmeter)5個變數作為解釋變數放入模型當中建模，並將模型存在house.rp變數中。

house$area<-as.factor(house$area)
house$building_type<-as.factor(house$building_type)
house$danger<-as.factor(house$danger)


variable.list = names(house) %in% c('area','building_age','building_sqmeter', 'building_type','price_per_sqmeter', 'danger')

house=house[,variable.list]
trainset=trainset[,variable.list]
testset=testset[,variable.list]
control=trainControl(method="cv", number=10,summaryFunction = multiClassSummary,classProbs=T)
#control=trainControl(method="repeatedcv", number=10,summaryFunction = multiClassSummary,classProbs=T)


house.rp =train(danger~., data=trainset, method="rpart",metric='F1', trControl=control)

predictions = predict(house.rp,testset,type='raw')
table(predictions,testset$danger)
confusionMatrix(table(predictions,testset$danger))



# (10)請利用plot()和text()畫出house.rp模型的決策樹
library('rpart.plot')
plot(house.rp, uniform=TRUE, compress=TRUE, margin=0.02)

rpart.plot(house.rp)


# (11)請問此決策數是否需要進行剪枝(prune)？如需剪枝請將修剪後的模型存回house.rp中。
s=summary(house.rp)
s$cptable




# (12)請將測試集資料(testset)放入模型中進行驗證，請問此模型的accuracy, precision, recall等績效分別為何?
 
#   (13)請繪製出此模型的ROC曲線，並計算其AUC




predictions = predict(model,testset,type='raw')
table(predictions,testset$churn)
confusionMatrix(table(predictions,testset$churn))

library(ROCR)
pred <-predict(model, testset, type="prob")
p_yes<-pred[, "yes"]
predictions<-prediction(p_yes, testset$churn)
per_auc<-performance(predictions, measure ="auc")
per_fpr_tpr<-performance(predictions, measure="tpr",x.measure = "fpr")
plot(per_fpr_tpr,main=paste("AUC:",(per_auc@y.values)))
  
  
  
  
  
  
  
  