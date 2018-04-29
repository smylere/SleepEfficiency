#관측 대상자의 actigraph 측정 데이터(조도,활동량) + 다른 장비 HRV 측정 데이터를 병합하여
#일괄적으로 dataset 구성 및 모델 테스트 수행
#
#현재 본 repo에는 HRV 처리 로직 반영 X 

library(ROCR)
library(randomForest)
library(pscl)


names(filtered_SDNN) = c("name","Date","w_SDNN","w_Mean_BPM","w_SDNN_status","w_BPM_status",
                         "rm7","s_SDNN","s_Mean_BPM","s_SDNN_status","s_BPM_status")

filtered_SDNN = filtered_SDNN[,-7]
filtered_SDNN[,2] = as.Date(gsub('_wake','',filtered_SDNN[,2]))

# head(filtered_RMSSD)
names(filtered_RMSSD) = c('Date','name','w_RMSSD','rm4','rm5','w_pNN50',
                          'rm7','s_RMSSD','rm9','rm10','s_pNN50')

filtered_RMSSD = filtered_RMSSD[,-c(4,5,7,9,10)]
filtered_RMSSD$Date = as.character(filtered_RMSSD$Date)
filtered_RMSSD[,1] = as.Date(gsub('_wake','',filtered_RMSSD[,1]))


settingDataPolar = merge(filtered_SDNN,filtered_RMSSD,key='name')

str(settingDataPolar)

settingDataPolar = settingDataPolar[,c(1,2,3,4,5,6,11,12,13,14,7,8,9,10)]

resultData = merge(settingDataPolar,dataTotal171106)

resultData$name = as.factor(resultData$name)
resultData$w_SDNN_status = as.factor(resultData$w_SDNN_status)
resultData$w_BPM_status = as.factor(resultData$w_BPM_status)
resultData$s_SDNN_status = as.factor(resultData$s_SDNN_status)
resultData$s_BPM_status = as.factor(resultData$s_BPM_status)

data = resultData[,-c(1,2,5,6,13,14)]

data$sleepActivityBefore = data$sleepActivity
data = data[,c(26,1:25)]
data[1:length(data[,1])-1,c(2:26)] = data[2:length(data[,1]),c(2:26)]
data = data[-length(data[,1]),]

resultDataBeforeAfter = data

# 18시 기준 전후에 대해 활동시간 안분 비율적용을 추가로 해야할 것인가에 대한 검토 필요

# write.csv(data,"/home/imcloud/resultData.csv")
# modified by excel (before value)
# read resultDataBeforeAfter.csv

# -------------------------------------------------------------------------
resultDataBeforeAfter = as.data.frame(resultDataBeforeAfter)
head(resultDataBeforeAfter)
names(resultDataBeforeAfter)

# [1] "sleepActivityBefore" "w_SDNN"              "w_Mean_BPM"          "w_RMSSD"             "w_pNN50"            
# [6] "s_RMSSD"             "s_pNN50"             "s_SDNN"              "s_Mean_BPM"          "NA1st"              
# [11] "LA1st"               "MA1st"               "HA1st"               "ISLL1st"             "ISHL1st"            
# [16] "OSLL1st"             "OSHL1st"             "NA2nd"               "LA2nd"               "MA2nd"              
# [21] "HA2nd"               "ISLL2nd"             "ISHL2nd"             "OSLL2nd"             "OSHL2nd"            
# [26] "sleepActivity"   


# ===============================================================================
# ===============================================================================

out = lm(sleepActivity ~.,data = resultDataBeforeAfter[,-c(6:9)])
out2 = step(out,direction = 'backward')
summary(out2)

dataTosleepActivity = dataValidation(resultDataBeforeAfter[,-c(6:9)])
dataTosleepActivity$label = as.factor(dataTosleepActivity$label)
summary(dataTosleepActivity$label)

# tmpTrain = dataTosleepActivity[,]
# tmpTest = dataTosleepActivity

out.logisticRegression = glm(label ~.,data = dataTosleepActivity[,-22],family = 'binomial')
out.logisticRegression.2 = step(out.logisticRegression,direction = "backward")
summary(out.logisticRegression.2)
pR2(out.logisticRegression.2)

tmp = predict(out.logisticRegression.2,type='response')
tmp2 = prediction(tmp,dataTosleepActivity$label)
tmp3 = performance(tmp2,measure = 'tpr',x.measure = 'fpr')
plot(tmp3)

auc = performance(tmp2,measure = 'auc')
auc = auc@y.values[[1]]
print(auc) #auc 검토 결과 80% 수준 확인

resultRandomForest_to_sleepActivity = randomForest(label ~.,
                                                   data = dataTosleepActivity[,-22],
                                                   importance=TRUE,ntree=1000,
                                                   localImp=TRUE,proximity=TRUE)
varImpPlot(resultRandomForest_to_sleepActivity)



# -------------------------------------------------------------------------
# legacy code
# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------
# out = lm(s_RMSSD ~.,data = resultDataBeforeAfter[,-c(7:9,34)])
# out2 = step(out,direction = 'backward')
# summary(out2)
# 
# dataToRMSSD = dataValidation(resultDataBeforeAfter[,-c(7:9,34)])
# dataToRMSSD$label = as.factor(dataToRMSSD$label)
# summary(dataToRMSSD$label)
# 
# resultRandomForest_to_RMSSD = randomForest(label ~.,
#                                            data = dataToRMSSD[,-30],
#                                            importance=TRUE,ntree=100,
#                                            localImp=TRUE,proximity=TRUE)
# 
# 
# out = lm(s_SDNN ~.,data = resultDataBeforeAfter[,-c(6,7,9,34)])
# out2 = step(out,direction = 'backward')
# summary(out2)
# 
# dataToSDNN = dataValidation(resultDataBeforeAfter[,-c(6,7,9,34)])
# dataToSDNN$label = as.factor(dataToSDNN$label)
# summary(dataToSDNN$label)
# 
# resultRandomForest_to_SDNN = randomForest(label ~.,
#                                            data = dataToSDNN[,-30],
#                                            importance=TRUE,ntree=100,
#                                            localImp=TRUE,proximity=TRUE)
# 
# 
# out = lm(s_pNN50 ~.,data = resultDataBeforeAfter[,-c(6,8,9,34)])
# out2 = step(out,direction = 'backward')
# summary(out2)
# 
# dataTopNN50 = dataValidation(resultDataBeforeAfter[,-c(6,8,9,34)])
# dataTopNN50$label = as.factor(dataTopNN50$label)
# summary(dataTopNN50$label)
# 
# resultRandomForest_to_pNN50 = randomForest(label ~.,
#                                           data = dataTopNN50[,-30],
#                                           importance=TRUE,ntree=100,
#                                           localImp=TRUE,proximity=TRUE)