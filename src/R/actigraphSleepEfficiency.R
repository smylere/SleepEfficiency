#########################################################
# 일부코드 반영 안되어 있으므로 현시점에서는 코드 동작 X
#########################################################


# 최초 수면시점에 대해 수기로 기록된 부분을 적용
# 이후 센서데이터 상에서 수면시점을 측정할 수 있는 신규 구조 구현 


library(data.table)
library(lubridate) # ymd_hms  #시간 타입 쓰기위한 라이브러리
library(hms)
library(dplyr)


### config -> Timeindex로 ###
config_cleaning = function(data_config){
    
    ########################################################
    # 기존 작성된 config 파일에 대해 indent 불일치 수정 로직
    ########################################################
    
    return(temp)
}

call_sleep_points = function(data, config){

    ###################################################################
    #(원본데이터, config_cleaning(config))
    #
    #기존 최초 csv 데이터와 config_cleansing처리된 config 데이터를 통해
    #취침시점을 파악하는 신규로직 적용 데이터셋으로 재구성
    ###################################################################
    
    return(config)
}


# sleep_points 찾기 : 5분 이상 지속된 움직임이 250미만일 때 자는것으로 판
search_sleep_points = function(data, start){
    
    if (data[start]$`Vector Magnitude` == 0) {
        temp = as.integer(seq(from = start, length.out = 30, by = 1))
        
        if (sum(data[temp]$`Vector Magnitude`) < 1000) {
            return(start)
        }
        else{
            return(search_sleep_points(data, start+1))
        }
    }
    else{
        return(search_sleep_points(data, start+1))
    }
}

# 수면 품질 측정
# 수면관련 논문을 참조하여 수면 품질에 대해 good / bad의 기준값을 지정
# 기존에 actigraphDataHandle.R에서 최종적으로 출력되는 sleepActivity값 대신 
# 관측된 수면중 움직임 전체 데이터를 대상으로 수면중 뒤척임 수준에 대해 우선적으로 정의
# 수면중 일정수준 이상 움직임이 발생하지 않을 경우 움직임이 없음을 전제로 
# 수면중 뒤척임 비율에 대해 백분위 환산 이후, 수면시간 전체중 뒤척임 없이 잔 시간 비율이
# 전체 85% 이상일 경우 수면 품질을 good 그렇지 않으면 bad로 정의

call_sleep_quality = function(data, data_config){
    
    # 기존 데이터 셋, new_config
    data$Time_index = as.POSIXct(paste0(data$Date, data$Time))
    data_config = call_sleep_points(data, config_cleaning(data_config))
    print(str(data_config))
    
    Date_vector = vector()
    Sleep_vector = vector()
    result = vector()
    
    temp = data
    
    # 수면 시간동안 뒤척임 비율을 구하는 로직
    # 이때 착용자의 뒤척임이 일정수준 미만일 경우 역시 뒤척임이 없음으로 정의
    # 움직임 값에 대해 기존에 사용하던 벡터 내적값을 이용하여, 10초당 벡터 내적의 
    # 변위가 50 미만일 경우에도 똑같이 움직임이 없음을 가정
    
    for (i in 1:nrow(data_config)){
        
        #########################################################################
        # for문 이하에서 InBedTime,LatencyTime,WakeFullNess,SleepPeriod 계산 로직
        # 각 구분 계산로직이 완료되면 이하 변수에 대한 연산 가능
        #########################################################################
    }
    
    result = as.data.frame(cbind(Date_vector, Sleep_vector), stringsAsFactors = FALSE)
    result$Target = "Bad"
    
    binary_units = as.integer(which(result$SleepEfficiency >= 0.85))
    result[binary_units,7] = "Good"
    

    names(result) = c("Date", "LatencyTime","WakeFullNess","SleepPeriod","SleepEfficiency","OnlySleepPeriod","Target")
    return(result)
}
