#!/usr/bin/env python
#-*- coding: utf-8 -*-

import os
import numpy as np
import pandas as pd
import csv

import statsmodels.formula.api as sm

from sklearn.feature_selection import RFE
from sklearn.svm import SVR

# from app.common.comConfig import all_conf as conf
from app.model.sleepModel import SleepModel

#===============================================================================
# =
# =
# =
# =
# ===============================================================================

if __name__ == "__main__":

    sleepModel = SleepModel()
    
    # 기본 경로
    org_path = os.path.dirname( os.path.abspath( __file__ ) )
    print org_path

   
    # 데이터 경로
    # actigraph csv data Read
    
    each_data = [['sample_0809_0823_10sec.csv', 'sample_config.csv', 'sample', 'header2', '%d/%m/%Y']]

    
# ===============================================================================
# = 데이터 전처리 과정
# = dataProcessing : raw데이터를 대상으로 속성 라벨 부여 기준 3분할 적용 비율값 추출 및 수면 활동량 계산 data frame
# = cleansing_data : 각 raw데이터에 대한  dataProcessing 함수 적용 이후 dataframe
# = sumDataSet : 각 raw데이터에서 처리된 속성값에 대한 cleansing_data 묶음(리스트)
# ===============================================================================

    total_type_val = []
    sumDataSet = [] # dataProcessing 함수 결과물 적제 리스트

    for inc in xrange(len(each_data)):
        
        print inc, each_data[inc][0]

        # 데이터파일 경로
        data_path = os.path.join(org_path, 'data', 'actigraph', '10sec', each_data[inc][0])
        # print data_path
        
        # 설정파일 경로
        config_path = os.path.join(org_path, 'data', 'config', each_data[inc][1])
        # print config_path
        
        # 불필요한 헤더값 제거
        data = sleepModel.getData(data_path)
        # print 'data', data
        
        # 설정 정보 로딩(일지 정보)
        config = pd.read_csv(config_path, header=0)
        # print config

        # 데이터 전처리(일지 기준 데이터 추출)
        # 파라미터 : data, config, scale
        sleep_data = sleepModel.dataHandle(data, config, each_data[inc][3], each_data[inc][4], 'minmax')

        # 신규 함수 dataProcessing을 각 데이터셋에 적용
        # 조도 / 활동량 / 수면활동량 기준 속성 재정의 및 라벨값 기준 비율 추출 데이터 프레임 구현

        cleansing_data =  sleepModel.dataProcessing(sleep_data)
        sumDataSet.append(cleansing_data)
        
        # sumDataSet.append(sleepModel.dataProcessing(sleep_data))
        # tmp_labels2 = sleepModel.dataValidation2(cleansing_data)
        # print "tmp_labels2",tmp_labels2
        
# ===============================================================================        

# ===============================================================================
# = regressionDataSet : 리스트 형태의 sumDataSet에 대한 dataframe 구조 재적용
# ===============================================================================
    
    # 1차 데이터 프레임 병합
    regressionDataSet = pd.concat(sumDataSet) 
    
    # dataValidation2 함수 적용시 인덱싱이 깨지는 문제 때문에 resultData 병합시점 데이터를 
    # 따로 분리하여 index 리셋 적용
    tmpRegressionDataSet= regressionDataSet.reset_index()
    
    # regressionDataSet 1차 데이터셋 csv 출력
    pd.DataFrame.to_csv(regressionDataSet,"./output/dataframe.csv",sep=',')
    
    # 추출된 전체 데이터 셋에 대한 회귀모델 적��� 
    # 금일 기준으로 backward 속성 제거 함수가 구현필요

    targetVariable = "sleepActivity ~"
    independentVariable ="NA1st+LA1st+MA1st+HA1st+ISLL1st+ISHL1st+OSLL1st+OSHL1st+NA2nd+LA2nd+MA2nd+HA2nd+ISLL2nd+ISHL2nd+OSLL2nd+OSHL2nd+NA3rd+LA3rd+MA3rd+HA3rd+ISLL3rd+ISHL3rd+OSLL3rd+OSHL3rd"
    regResult = sm.ols(formula = targetVariable + independentVariable,data = regressionDataSet).fit()
 
    # 유도된 회귀모델 파라미터 추출
    # print regResult.params
    
    #유도된 회귀모델 전체 요약
    # print regResult.summary()

    
# ===============================================================================    

# ===============================================================================
# =
# =
# =
# =
# ===============================================================================
    
    # regressionDataSet 취합 전체 데이터 대상 라벨 적용
    tmp_labels2 = sleepModel.dataValidation2(regressionDataSet)

    # dataValidation2 함수 적용 라벨 리스트 -> 데이터 프레임으로 전환
    labelData = pd.DataFrame(np.array(tmp_labels2).reshape(len(tmp_labels2),1),columns=["label"])
    
    # 2차 데이터 프레임 병합 (라벨 추가)
    resultData = pd.concat([tmpRegressionDataSet,labelData],axis=1)
    # print resultData
    
    # regressionDataSet 2차 데이터셋 csv 출력
    pd.DataFrame.to_csv(resultData,"./output/label_dataframe.csv",sep=',')
    
    # SVM 모델 적용 테스트
    testModel = sleepModel.classifierSVM2(tmp_labels2,regressionDataSet)
    testData = regressionDataSet.iloc[0].tolist()
    testPredict = sleepModel.predictSVM(testModel,testData)
    
    print '# ==============================================================================='
    print '\n'
    # print testModel
    print '\n'
    print 'SVM testPredict is ', testPredict
    print 'dataSet label is ', resultData.iloc[0][26]
    
# =============================================================================== 