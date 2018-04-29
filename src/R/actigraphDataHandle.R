# actigraph 장비를 통해 측정된 활동량, 조도 센서 측정값을 바탕으로
# 주간에 착용자가 활동하면서 측정된 센서데이터를 바탕으로 수면 시점에서
# 수면의 품질을 측정 및 예측하는 모델 구현
# 
# 목표 변수인 수면의 품질에 대해서는 수면중 움직임(뒤척임) 수준에 따라 
# 수면 시간중 움직임이 많을 수록 수면의 품질이 나쁘다 / 움직임 없이 잘 경우 
# 수면의 품질이 좋다로 구분하여 최초 정의
# 
# 따라서 본 모델의 구성요소는 
# 독립변수(input) : 관측 대상자가 착용한 센서 디바이스(actigraph)에서 측정된 주간 활동량, 조도 값(10초 기준)
# 종속변수(target) : 관측 대상자의 수면중 뒤척임(활동량) 측정 값 
# 모델 가정 : 주간의 관측 대상자가 특정한 패턴으로 활동할 경우 이는 수면시점에 영향을 미친다
# 따라서, 해당 패턴(주간에 측정되는 데이터의 특징)을 파악하는 것이 본 모델 구성의 목표


library(hms) #시간 타입 처리 라이브러리
library(jmotif) # paa, sax 관련 라이브러리
library(e1071) # svm 적용 라이브러리
library(FSelector) # 카이제곱검정 유도 라이브러리

#### data preprocessing ####

# input : 디바이스 csv 데이터,수면일지 기록 config값(별도 정리 및 호출) 
# 최초 csv loading 이후 수행해야 할 작업
# ex) OST_data$Date = as.Date(OST_data$Date,format="%d/%m/%Y")



# 최초 관측 데이터에 대해 라벨값을 부여
# 각 활동량과 측정된 조도값을 일정한 구간으로 구분하여
# 각 구간값에 맞춰 row당 활동량과 조도 측정값에 대한 라벨 추가
# 분석의 목적 및 편의성을 위하여 각 활동량과 조도 측정값에 대해 4개 구간으로
# 구분하여 각 관측값에 라벨 지정


addLabel = function(rawdata){
    
    data = as.data.frame(rawdata)
    
    # HR 속성 제거
    # HR 속성의 경우 RR 데이터를 다른 csv 파일로 추출하여
    # 변환 및 적용
    
    if(length(data) != 12){ data = as.data.frame(data[,-7]) }
    
    #rawdata 그대로 사용
    
    for (i in (1:length(data[,1]))) {
        
        # 최초 load된 csv 데이터에 대해 Step 속성을 대상으로 운동의 강도를 추출
        # 현재 취합 및 테스트 검증단계의 데이터의 record는 10초 기준으로 기록되어 있으므로
        # 10초당 걸음수에 따라 운동 강도를 산출 
        
        if(0 == data[i,6]){ data[i,13] = "notActivity" }
        else if(1<=data[i,6] & data[i,6]<=5){ data[i,13] = "lowActivity" }
        else if (6 <= data[i,6] & data[i,6] <= 12){ data[i,13] = "mediumActivity" }
        else if (13 <= data[i,6]){ data[i,13] = "highActivity" }
        else{print("error")}
        
    }
    
    for (i in (1:length(data[,1]))) {
        
        # https://actigraph.desk.com/customer/en/portal/articles/2515504-lux-measurements
        
        #조도의 강도를 측정하는 기준은 actigraph 관련 설명을 참조하여 진행
        # 일정 수준 이하의 조도와 실내활동, 낮은 야외조도, 높은 야외조도 수준으로
        # 각 조도 값에 대해 구간을 설정하여 추가 속성으로 라벨 부여
        
        
        if (0<=data[i,7] & data[i,7]<=50){ data[i,14] = "lowLux" }
        else if(51<=data[i,7] & data[i,7]<=500){ data[i,14] = "innerSpaceLux" }
        else if(500<=data[i,7] & data[i,7]<=1000){data[i,14] = "outerSpaceLowLux"}
        else if (1001 <= data[i,7]){ data[i,14] = "outerSpaceHighLux" }
        else{print("error")}
    }
    
    names(data)[c(13:14)] =c("activityLabel","luxLabel")
    data[,13] = as.factor(data[,13])
    data[,14] = as.factor(data[,14])
    
    return(data)
    
}

# 입력 받은 데이터를 전처리 하는 함수(dataHandle)
# 제공 받은 데이터와 착용자가 수기로 기록한 기상/취침 시간을 토대로 
# 해당 데이터를 각각 주/야간으로 dataframe 형태로 재구성
# 이때 각 관측값의 스케일이 상이하므로 해당 데이터의 스케일을 통일하기 위해
# 각각 표준정규분포 표준화 / 최대최소 표준화를 개별적으로 적용할 수 있는 로직을 
# 구현하여 데이터 처리 시점에서 interactive 하게 적용


dataHandle = function(data,
                      measurementDate,
                      todayWakeup,
                      todaySleep,
                      nextdayWakeup,
                      scale=FALSE){
    
    tmp =  as.data.frame(data)
    tmpLength = as.integer(length(tmp)-2)
    
    if (scale == "stdScore"){
        print ("standard Scoring")
        tmp[,3:tmpLength] = scale(tmp[,3:tmpLength])
    }
    
    # 170830 comment 
    # 상관계수 유도시 최대 최소 정규화(minmax)가 가장 적합하다고 추정
    
    else if (scale =="minmax") {
        print ("minmax Scoring Scale")
        minmax = function(attr) {(attr -min(attr)) / (max(attr) - min(attr))}
        for (i in 3:tmpLength){ tmp[,i] = minmax(tmp[,i]) }}
    
    else {print("base Scale")}
    
    if(as.hms("18:00:00") < as.hms(todaySleep) && as.hms(todaySleep) < as.hms("23:59:50")){
        
        idx_head_wake1 = as.integer(which(c(tmp[,1] == as.Date(measurementDate) & 
                                                as.character(tmp[,2]) == as.character(todayWakeup))))
        idx_tail_wake1 = as.integer(which(c(tmp[,1] == as.Date(measurementDate) & 
                                                as.character(tmp[,2]) == as.character(todaySleep))))
        idx_head_sleep1 = as.integer(which(c(tmp[,1] == as.Date(measurementDate) & 
                                                 as.character(tmp[,2]) == as.character(todaySleep))))
        idx_tail_sleep1 = as.integer(which(c(tmp[,1] == as.Date(measurementDate)+1 & 
                                                 as.character(tmp[,2]) == as.character(nextdayWakeup))))
        
        wakeTimeData = as.data.frame(tmp[idx_head_wake1:idx_tail_wake1,])
        sleepTimeData = as.data.frame(tmp[idx_head_sleep1:idx_tail_sleep1,])
        
    }
    
    else {
        
        idx_head_wake2 = as.integer(which(c(tmp[,1] == as.Date(measurementDate) & 
                                                as.character(tmp[,2]) == as.character(todayWakeup))))
        idx_tail_wake2 = as.integer(which(c(tmp[,1] == as.Date(measurementDate)+1 & 
                                                as.character(tmp[,2]) == as.character(todaySleep))))
        idx_head_sleep2 = as.integer(which(c(tmp[,1] == as.Date(measurementDate)+1 & 
                                                 as.character(tmp[,2]) == as.character(todaySleep))))
        idx_tail_sleep2 = as.integer(which(c(tmp[,1] == as.Date(measurementDate)+1 & 
                                                 as.character(tmp[,2]) == as.character(nextdayWakeup))))
        
        wakeTimeData = as.data.frame(tmp[idx_head_wake2:idx_tail_wake2,])
        sleepTimeData = as.data.frame(tmp[idx_head_sleep2:idx_tail_sleep2,])
        
    }
    
    # print(head(wakeTimeData))
    # print(tail(sleepTimeData))
    
    return(list(wakeTime = wakeTimeData,sleepTime = sleepTimeData))
    
}


# 전처리가 끝난 데이터와 별도 기입된 착용자의 기상/수면 시점 내용을 병합하여
# 전체 데이터 셋 정리

personalData = function(data,dataConfig,scaleOption) {
    
    data = addLabel(data)
    result = list()
    
    for (i in 1:length(dataConfig[,1])){
        
        tmp = dataHandle(data,
                         dataConfig[i,1],
                         dataConfig[i,2],
                         dataConfig[i,3],
                         dataConfig[i,4],
                         scaleOption)
        
        result = append(result,tmp)
    }
    
    return(result)
    
    # 해당 함수의 리턴은 데이터의 관측 일수 *2 만큼의 데이터프레임을 생성하며 
    # (측정일 주간 | 측정일 취침시간 )
    # 생성된 데이터 프레임은 list형태의 retrun으로 구현
    
}

# input = personalData(addLabel(DATA),DATA_CONFIG,SCALE)
# output = list(dataframe1,dataframe2)

# 전처리 및 라벨 적용이 끝난 데이터 셋에 대해 하나의 dataframe 형태로 최종 정리
# 이 시점에서 일별 측정 최초 rawdata(약 8640row)를 1 row로 변환 적용
# 일별로 측정된 주간 /취침 활동량에 대해 각각 라벨값 분포를 count하여 
# 수면시점의 활동량을 백분위로 환산 적용 

dataProcessing = function(data,rate=FALSE,scale=FALSE){
    
    dataLength = as.integer(length(data))
    
    sleepTime = vector()
    sleepQuality = vector()
    
    dayDataSpace = vector()
    dayDataResult = vector()
    dayDataOut = vector()
    
    for ( i in seq(from = 2, to = dataLength, by = 2)){
        
        lengthSleep = as.numeric(length(data[[i]][12]))
        sumSleepVectorValue = as.numeric(sum(data[[i]][12]))
        resultSleep = sumSleepVectorValue/lengthSleep
        # sleepTime = append(sleepTime,resultSleep)
        
        filterValue = min(data[[i]][12])
        checkingSleepData = as.vector(data[[i]][12])
        qualityCheck = sum(filterValue != checkingSleepData[,1]) / length(checkingSleepData[,1])
        sleepQuality = append(sleepQuality,qualityCheck)
        
        sleepTime = append(sleepTime,qualityCheck)
        # sleepTime = append(sleepTime,sum(data[[i]][12]))
    }
    
    for ( i in seq(from = 1, to = dataLength, by = 2)){
        
        waketimeLength = as.integer(length(data[[i]][,1]))
        
        tmp = which(data[[i]][,2] == as.hms("18:00:00"))
        
        dayFrontPeriod = data[[i]][1:tmp,c(6,7,13,14)]
        dayBackPeriod = data[[i]][tmp:waketimeLength,c(6,7,13,14)]
        
        dayData = list(dayFrontPeriod=dayFrontPeriod,dayBackPeriod=dayBackPeriod)

        labelActivityText = c("notActivity","lowActivity","mediumActivity","highActivity")
        labelLuxText = c("lowLux","innerSpaceLux","outerSpaceLowLux","outerSpaceHighLux")
        
        for(i in 1:as.integer(length(dayData))){
            
            result1 = vector()
            result2 = vector()
            
            for (j in 1:as.integer(length(labelActivityText))){
                
                tmpActivity = subset(dayData[[i]],dayData[[i]][3] == labelActivityText[j])
                tmpLux = subset(dayData[[i]],dayData[[i]][4] == labelLuxText[j])

                if(i == 1) {
                    
                    rateValue1 = as.numeric(length(tmpActivity[,1])/
                                                length(dayFrontPeriod[,1]))
                    rateValue2 = as.numeric(length(tmpLux[,2])/
                                                length(dayFrontPeriod[,1]))
                    }
                
                else if(i == 2){
                    
                    rateValue1 = as.numeric(length(tmpActivity[,1])/
                                                length(dayBackPeriod[,1]))
                    rateValue2 = as.numeric(length(tmpLux[,2])/
                                                length(dayBackPeriod[,1]))
                    }
                
                else{print ("error")}
                
                result1 = append(result1,rateValue1)
                result2 = append(result2,rateValue2)

                }
            
            dayDataSpace = c(result1,result2)
            dayDataResult = append(dayDataResult,dayDataSpace)
            
        }
    }
    
    dayDataOut = append(dayDataOut,dayDataResult)
    dayDataOut = matrix(dayDataOut,ncol=16, byrow=TRUE)
    
    result = as.data.frame(cbind(dayDataOut,sleepTime))
    names(result) = c("NA1st","LA1st","MA1st","HA1st","ISLL1st","ISHL1st","OSLL1st","OSHL1st",
                      "NA2nd","LA2nd","MA2nd","HA2nd","ISLL2nd","ISHL2nd","OSLL2nd","OSHL2nd",
                      "sleepActivity")
    

    # 추가 구현 필요 구조
    # if (rate == TRUE) { result = 1 }
    # if (scale == TRUE) { result = 2 }
    # if (scale == TRUE & rate == TRUE) { result = 3 }
    
    return(result)
    
}



# 기존 dataProcessing 함수 수정
dataProcessing2 = function(data,rate=FALSE,scale=FALSE){
    
    dataLength = as.integer(length(data))
    
    sleepTime = vector()
    sleepQuality = vector()
    
    dayDataSpace = vector()
    dayDataResult = vector()
    dayDataOut = vector()
    
    for ( i in seq(from = 2, to = dataLength, by = 2)){
        
        lengthSleep = as.numeric(length(data[[i]][12]))
        sumSleepVectorValue = as.numeric(sum(data[[i]][12]))
        resultSleep = sumSleepVectorValue/lengthSleep
        # sleepTime = append(sleepTime,resultSleep)
        
        filterValue = min(data[[i]][12])
        checkingSleepData = as.vector(data[[i]][12])
        qualityCheck = sum(filterValue != checkingSleepData[,1]) / length(checkingSleepData[,1])
        sleepQuality = append(sleepQuality,qualityCheck)
        
        sleepTime = append(sleepTime,qualityCheck)
        # sleepTime = append(sleepTime,sum(data[[i]][12]))
    }
    
    for ( i in seq(from = 1, to = dataLength, by = 2)){
        
        waketimeLength = as.integer(length(data[[i]][,1]))
        
        day1stSpace = data[[i]][1:(waketimeLength/3),c(6,7,13,14)]
        day2ndSpace = data[[i]][(waketimeLength/3):((waketimeLength/3)*2),c(6,7,13,14)]
        day3rdSpace = data[[i]][((waketimeLength/3)*2):waketimeLength,c(6,7,13,14)]
        
        dayData = list(day1stSpace=day1stSpace,day2ndSpace=day2ndSpace,day3rdSpace=day3rdSpace)
        
        labelActivityText = c("notActivity","lowActivity","mediumActivity","highActivity")
        labelLuxText = c("lowLux","innerSpaceLux","outerSpaceLowLux","outerSpaceHighLux")
        
        for(i in 1:as.integer(length(dayData))){
            
            result1 = vector()
            result2 = vector()
            
            for (j in 1:as.integer(length(labelActivityText))){
                
                tmpActivity = subset(dayData[[i]],dayData[[i]][3] == labelActivityText[j])
                tmpLux = subset(dayData[[i]],dayData[[i]][4] == labelLuxText[j])
                
                rateValue1 = as.numeric(length(tmpActivity[,1])/(waketimeLength/3)) # waketimeLength
                rateValue2 = as.numeric(length(tmpLux[,2])/(waketimeLength/3)) # waketimeLength
                
                result1 = append(result1,rateValue1)
                result2 = append(result2,rateValue2)
                
                # sumValue1 = as.numeric((sum(tmpActivity[,1])))
                # sumValue2 = as.numeric((sum(tmpLux[,2])))
                # result1 = append(result1,sumValue1)
                # result2 = append(result2,sumValue2)
                
            }
            
            dayDataSpace = c(result1,result2)
            dayDataResult = append(dayDataResult,dayDataSpace)
            
        }
    }
    
    dayDataOut = append(dayDataOut,dayDataResult)
    dayDataOut = matrix(dayDataOut,ncol=24, byrow=TRUE)
    
    result = as.data.frame(cbind(dayDataOut,sleepTime))
    names(result) = c("NA1st","LA1st","MA1st","HA1st","ISLL1st","ISHL1st","OSLL1st","OSHL1st",
                      "NA2nd","LA2nd","MA2nd","HA2nd","ISLL2nd","ISHL2nd","OSLL2nd","OSHL2nd",
                      "NA3rd","LA3rd","MA3rd","HA3rd","ISLL3rd","ISHL3rd","OSLL3rd","OSHL3rd",
                      "sleepActivity")
    
    # 추가 구현 필요 구조
    # if (rate == TRUE) { result = 1 }
    # if (scale == TRUE) { result = 2 }
    # if (scale == TRUE & rate == TRUE) { result = 3 }
    
    return(result)
    
}


# ##################################################################
# legacy code
# ##################################################################
# 
# 
# vectorCrR = function(data){
#     
#     data = data*1000
#     
#     # vectorCrR 함수의 역할은 input으로 받는 데이터에 대해 종속변수인 취침활동량과 
#     # 상관계수를 추출하는 역할 및 데이터 검증의 목적으로 사용
#     # 현재 논의된 가설에 따라 취침 활동량에 영향을 미치는 속성(주간활동량, 조도) 데이터에 대해
#     # dataProcessing을 통해 유도된 파생변수 24개를 통하여 음의 상관관계 수준을 확인하는 목적으로 사용
#     
#     
#     result = vector()
#     
#     checkLength = as.integer(length(data[1,]-1))
#     
#     for (i in 1:21){
#         
#         corOut = cor(as.integer(data[,i]),
#                      as.integer(data[,22]),
#                      use = "complete.obs")
#         
#         if (is.na(corOut)){corOut = 0}
#         
#         result = append(result,corOut)
#     }
#     
#     return(result)
#     
# }
# 
# #### classification function ####
# 
# dataValidation = function(data){
#     
#     tmp = as.data.frame(data)
#     
#     tmpLength = as.integer(length(tmp))
#     
#     k = 2
#     
#     while(k <= 100){
#         
#         km_out = kmeans(tmp,k,iter.max = 100000)
#         
#         if( km_out$betweenss / km_out$totss < 0.80){ k =k +1 }
#         else if (k == length(data[,1])) { break }
#         else { break }
#     }
#     
#     # 유도된 k 군집 추출
#     cluster = as.data.frame(km_out$centers)
#     
#     splitValue = median(cluster[,tmpLength])
#     print(splitValue)
#     
#     # splitValue = 0.15
#     
#     
#     # 추출된 중간값을 기준으로 전체 데이터에 대해 중간값 이상 / 이하에 각 라벨 부여
#     for (i in 1:length(tmp[,1])){ 
#         
#         # 수면 활동량 중간값 이하의 경우 good 라벨 부여
#         if(tmp[i,tmpLength] <= splitValue) { tmp[i,(tmpLength+1)] = "good" }  
#         
#         # 수면 활동량 중간값 이상인 경우 bad 라벨 부여 
#         else { tmp[i,(tmpLength+1)] = "bad" } 
#         
#     }
#     
#     lenName = length(tmp)
#     names(tmp)[length(tmp)] = c("label")
#     
#     return(tmp)
#     
#     # 최적 k값 기반으로 군집된 각 클러스터에 대해 클러스터 중 중간값 median 값에 대해 
#     # 기준을 두고 해당 기준값 이상(수면활동량이 중간값 이상 )일 경우 수면의 질이
#     # 나쁘다고 판정, 그 반대의 경우 수면의 질이 좋다고 판정을 내리는 속성 변수 
#     # good / bad 추가 부여 
#     
# }
# 
# #### 미반영 additional process ####
# classifierSVM = function(data){
#     
#     # 최종적으로 kmeans 군집화를 통해 추출된 good / bad 속성을 대상으로 
#     # svm 분류기 학습 모델 적용 
#     # input : dataValidation(testTotal) 예시
#     
#     # svm 모델 자체 테스트 모델을 위해 샘플 구간 설정 
#     length01 = as.integer(length(data[[1]][,1])) 
#     
#     # svm 모델 자체 테스트 모델을 위해 샘플 구간 설정 
#     length02 = as.integer(length(data[[2]][,1])) 
#     
#     # svm 모델 자체 테스트 모델을 위해 샘플 구간 설정 
#     sample = c(sample(1:length01,7),
#                sample((length01+1):(length01+length02),7))
#     
#     #print(sample)
#     
#     data = rbind(data[[1]],data[[2]])
#     
#     # 모델 학습 시점에서는 각 속성값 중 0으로 이루어진 속성을 제거하는 작업을 수행
#     #data = data[,-c(1,9,17,25)]
#     data = data[,-c(1,9,17)]
#     
#     
#     # svm 모델 상에서 종속변수인 label(good / bad)을 파악하기 위해 속성값을 factor로 변경
#     data$V26 = as.factor(data$V26) 
#     #data$V10 = as.factor(data$V10)
#     
#     data.train = data[-sample,] # train 분리
#     data.test = data[sample,] # test 분리
#     
#     #svm 선형 분리 기준으로 train 데이터 적용 (종속변수 : label)
#     model01 = svm(V26 ~., data=data.train,kernal="linear") 
#     
#     #svm 다항식 분리 기준으로 train 데이터 적용 (종속변수 : label)
#     model02 = svm(V26 ~., data=data.train,kernal="polynomial") 
#     
#     #svm 가우시안 분리 기준으로 train 데이터 적용 (종속변수 : label)
#     model03 = svm(V26 ~., data=data.train,kernal="radial")
#     
#     # 모델 자체 테스트 데이터 추출 
#     test.x = subset(data.test,select=-V26)
#     
#     # 테스트 데이터 label값만 추출 
#     test.y = data.test$V26  
#     
#     # model01 테스트 
#     predict01 = predict(model01,test.x) 
#     
#     # model02 테스트 
#     predict02 = predict(model02,test.x) 
#     
#     # model03 테스트 
#     predict03 = predict(model03,test.x) 
#     
#     # 각 테스트 결과 표현
#     print(table(test.y,predict01)) 
#     print(table(test.y,predict02)) 
#     print(table(test.y,predict03)) 
#     
#     return(list(model01,model02,model03))
#     
# }
# sax_process = function(dataWake,dataSleep,seq,sax_seq) {
#     
#     data_wake = as.data.frame(dataWake)
#     data_sleep = as.data.frame(dataSleep)
#     
#     if(length(data_wake)==12) {
#         
#         cleansingWakeData = dataWake[,c(6,7,12)]
#         cleansingSleepData = dataSleep[,12]
#         
#     }
#     
#     
#     else if (length(data_wake)== 13) {
#         
#         cleansingWakeData = dataWake[,c(6,8,13)]
#         #cleansingSleepData = dataSleep[,c(,13)]
#         
#         # data_wake = as.data.frame(dataWake)
#         # data_sleep = as.data.frame(dataSleep)
#         
#         paa_wake_VM = paa(data_wake[,12],seq)
#         paa_wake_Steps = paa(data_wake[,6],seq)
#         paa_wake_Lux = paa(data_wake[,7],seq)
#         
#         paa_sleep_VM = paa(data_sleep$`Vector Magnitude`,seq)
#         
#         sax_wake_vm = series_to_chars(paa_wake_VM,sax_seq)
#         sax_wake_step = series_to_chars(paa_wake_Steps,sax_seq)
#         sax_wake_lux = series_to_chars(paa_wake_Lux,sax_seq)
#         
#         sax_sleep_vm = series_to_chars(paa_sleep_VM,sax_seq)
#         
#         scaleValue = as.vector(
#             
#             c(min(paa_wake_VM),max(paa_wake_VM),
#               min(paa_wake_Steps),max(paa_wake_Steps),
#               min(paa_wake_Lux),max(paa_wake_Lux),
#               min(paa_sleep_VM),max(paa_sleep_VM))
#         )
#         
#         scaleValue = na.omit(scaleValue)
#         
#         plot(paa_wake_VM,type='l',ylim=c(min(scaleValue),max(scaleValue)))
#         lines(paa_wake_Steps,col="blue")
#         lines(paa_wake_Lux,col="red")
#         lines(paa_sleep_VM,col="green")
#         
#         return(list(paa_wake_VM = paa_wake_VM,
#                     paa_wake_Steps = paa_wake_Steps,
#                     paa_wake_Lux = paa_wake_Lux,
#                     paa_sleep_VM = paa_sleep_VM,
#                     sax_wake_vm = sax_wake_vm,
#                     sax_wake_step = sax_wake_step,
#                     sax_wake_lux = sax_wake_lux,
#                     sax_sleep_vm = sax_sleep_vm
#         ))
#         
#     }
#     
#     else if (length(data_wake)== 13) {
#         
#         paa_wake_VM = paa(data_wake[,13],seq)
#         paa_wake_Steps = paa(data_wake[,6],seq)
#         paa_wake_Lux = paa(data_wake[,8],seq)
#         
#         paa_sleep_VM = paa(data_sleep$`Vector Magnitude`,seq)
#         paa_sleep_HR = paa(data_sleep$HR,seq)
#         
#         sax_wake_vm = series_to_chars(paa_wake_VM,sax_seq)
#         sax_wake_step = series_to_chars(paa_wake_Steps,sax_seq)
#         sax_wake_lux = series_to_chars(paa_wake_Lux,sax_seq)
#         
#         sax_sleep_vm = series_to_chars(paa_sleep_VM,sax_seq)
#         #        sax_sleep_HR = series_to_chars(paa_sleep_HR,sax_seq)
#         
#         scaleValue = as.vector(
#             
#             c(min(paa_wake_VM),max(paa_wake_VM),
#               min(paa_wake_Steps),max(paa_wake_Steps),
#               min(paa_wake_Lux),max(paa_wake_Lux),
#               min(paa_sleep_VM),max(paa_sleep_VM)
#               # min(paa_sleep_HR),max(paa_sleep_HR)
#             ))
#         
#         scaleValue = na.omit(scaleValue)
#         
#         plot(paa_wake_VM,type='l',ylim=c(min(scaleValue),max(scaleValue)))
#         # plot(paa_wake_VM,type='l',ylim=c(0,1))
#         
#         lines(paa_wake_Steps,col="blue")
#         lines(paa_wake_Lux,col="red")
#         lines(paa_sleep_VM,col="green")
#         lines(paa_sleep_HR,col="pink")
#         
#         
#         return(list(paa_wake_VM = paa_wake_VM,
#                     paa_wake_Steps = paa_wake_Steps,
#                     paa_wake_Lux = paa_wake_Lux,
#                     paa_sleep_VM = paa_sleep_VM,
#                     sax_wake_vm = sax_wake_vm,
#                     sax_wake_step = sax_wake_step,
#                     sax_wake_lux = sax_wake_lux,
#                     sax_sleep_vm = sax_sleep_vm
#                     # sax_sleep_HR = sax_sleep_HR
#         ))
#     }
#     
#     else{print("not if else")}
#     
# }
# sax_handle = function(data) {
#     
#     result = list()
#     
#     for ( i in (seq(from=1,to=length(data),by=2))){
#         
#         tmp = sax_process(data[[i]],data[[i+1]],50,20)
#         
#         result =append(tmp,result)
#     }
#     return(result)
#     
# }