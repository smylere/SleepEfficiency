#!/usr/bin/env python
#-*- coding: utf-8 -*-

from __future__ import division

import collections
import os
import logging
import random
import numpy as np
import math
import datetime
import pandas as pd
import csv

from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import minmax_scale

from sklearn.cluster import KMeans
from sklearn.cluster import MiniBatchKMeans
from sklearn import svm


# ===============================================================================
# =
# =
# =
# =
# ===============================================================================
class SleepModel(object):
    
    def __init__(self):
        self.matrix = []
        self.vm = []
        self.header1 = ['Date','Time','Axis1','Axis2','Axis3','Steps','Lux','Inclinometer Off','Inclinometer Standing','Inclinometer Sitting','Inclinometer Lying','Vector Magnitude']
        self.header2 = ['Date','Time','Axis1','Axis2','Axis3','Steps','HR', 'Lux','Inclinometer Off','Inclinometer Standing','Inclinometer Sitting','Inclinometer Lying','Vector Magnitude']
        self.config_header = ['no','name','maesurementDate','todayWakeup','todaySleep','nextdayWakeup','qualityOfSleep','Recuperation']

    def getData(self, path, header=True):
        matrix = []
        f = open(path, 'r')
        csvReader = csv.reader(f)

        rowCnt = 0
        rowData = 0
        for row in csvReader:
            if header:
                if row[0].strip() == 'Date' and row[1].strip() == 'Time':
                    rowData = rowCnt
                    
                if rowData > 0 and rowCnt > rowData:
                    matrix.append(row)
                rowCnt += 1
            else:
                matrix.append(row)

        f.close()
        
        return matrix


    def getDataFilter(self, path, dfilter, header=True):
        matrix = []
        f = open(path, 'r')
        csvReader = csv.reader(f)

        rowCnt = 0
        rowData = 0
        for row in csvReader:
            if header:
                if row[0].strip() == 'Date' and row[1].strip() == 'Time':
                    rowData = rowCnt

                if rowData > 0 and rowCnt > rowData:
                    if row[0].strip() in dfilter:
                        matrix.append(row)
                rowCnt += 1
            else:
                if row[0].strip() in dfilter:
                    matrix.append(row)
                
        f.close()
        
        return matrix
        
       
    

    # 군집화(Kmeans)
    # 적용 X
    def dataValidation(self, pdata, p_n_clusters=2, p_max_iter=100000, p_tol=0.001):
            
        # print 'pdata : ', pdata
        
        # print kmeans
        kmeans = KMeans(n_clusters=p_n_clusters, max_iter=p_max_iter, tol=p_tol)
        kmeans.fit(pdata)
        
        print kmeans.fit(pdata)
        
        labels = kmeans.predict(pdata)
        centroids = kmeans.cluster_centers_
        inertia = kmeans.inertia_
        
        print 'labels : ' , labels
        print 'centroids : ', centroids
        print 'inertia : ', inertia
        
        tmp_data = []
        tmp_data.append(centroids[0][3])
        tmp_data.append(centroids[1][3])
        
        splitValue = np.median(tmp_data)
        print 'splitValue: ', splitValue
        
        c_labels = []
        for tinx in xrange(len(pdata)):
            if pdata[tinx][3] <= splitValue:
                c_labels.append('good')
            else:
                c_labels.append('bad')
        
        return c_labels

# ===============================================================================        

# ===============================================================================
# =
# =
# =
# =
# ===============================================================================
        
    def dataValidation2(self, pdata):

        pdata = pdata.values.tolist()
        
        k = 1
        p_max_iter=100000
        p_tol=0.001
        
        tmp_data = []
        
        while k <= 100:
            
            kmeans01 = KMeans(n_clusters=k, max_iter=p_max_iter, tol=p_tol)
            kmeans01.fit(pdata)
            tmp_inertia01 = kmeans01.inertia_

            kmeans02 = KMeans(n_clusters=k+1, max_iter=p_max_iter, tol=p_tol)
            kmeans02.fit(pdata)
            tmp_inertia02 = kmeans02.inertia_
            
            # elbow method 기준 기울기 유도 값 구조
            # 해당 기준값 수정 및 개선 필요
            # k군집 sum of square와 k+1 군집 sum of square의 기울기 차가
            # 0.85 이상일 때 (기울기 차이가 0.85 이상커질 때)
            # 군집 분할 종료 로직 구현
            
            if (tmp_inertia02 / tmp_inertia01) <= 0.85:
                
                # print (tmp_inertia02 / tmp_inertia01)
                
                k = k+1
            else :
                for i in range(0,k+1):
                    tmp_data.append(kmeans02.cluster_centers_[i][24])
                break

        # print 'tmp_data',tmp_data
        
        splitValue = np.median(tmp_data)
        print 'splitValue: ', splitValue

        c_labels = [] 
        
        for tinx in xrange(len(pdata)):
            if pdata[tinx][24] <= splitValue:
                c_labels.append('good')
            else:
                c_labels.append('bad')
        
        return c_labels
        
# ===============================================================================        

    # 분류기(SVM) 학습 
    # 적용 X
    def classifierSVM(self, plabel, pdata):
        
        data_scaling = minmax_scale(pdata, axis=0, copy=True)
        
        C = 1.0 # SVM regularization parameter
        svcModel = svm.SVC(kernel='rbf', gamma=40, C=C).fit(data_scaling, plabel)
        
        return svcModel


    # 분류기(SVM) 학습 수정로직
    # 적용 O
    def classifierSVM2(self, plabel, pdata):

        C = 1.0 # SVM regularization parameter
        svcModel = svm.SVC(kernel='rbf', gamma=40, C=C).fit(pdata, plabel)
        
        return svcModel

    # 모델 분류기 함수
    def predictSVM(self, pmodel, pdata):        
        return pmodel.predict([pdata])

# ===============================================================================
# =
# =
# =
# =
# ===============================================================================

    # 기본 데이터 처리 함수
    def dataHandle(self, pdata, pconfig, pheader, f_date, scale=False):

        if pheader == 'header1':
            df_data = pd.DataFrame(pdata, columns=self.header1)
        else:
            df_data = pd.DataFrame(pdata, columns=self.header2)
        
        # print len(df_data.iloc[0])
        # if len(df_data.iloc[0]) !=12 :
        #     df_data.drop(df_data.columns[6],axis=1)

        # 최초 데이터 속성중 Vector Magnitude 속성 정규화 바로 적용
        # 기존 vm 신규 속성 대신 바로 치환 적용 

        df_data['Vector Magnitude'] = minmax_scale(np.array(df_data['Vector Magnitude']).astype(np.float), axis=0, copy=True)

        # Steps 속성을 대상으로 활동량 라벨 값 부여 

        tmpStepData = map(int,np.array(df_data["Steps"]))
        tmpStepDataResult = []

        for i in range(0,int(len(tmpStepData))):

            if (tmpStepData[i] == 0) : 
                tmpStepDataResult.append("NA") # not Activity
            
            elif (1 <= tmpStepData[i] and tmpStepData[i] <= 5) : 
                tmpStepDataResult.append("LA") # low Activity
            
            elif (6 <= tmpStepData[i] and tmpStepData[i] <= 12) : 
                tmpStepDataResult.append("MA") # medium Activiy
            
            elif (13 <= tmpStepData[i] ) : 
                tmpStepDataResult.append("HA") # high Activiy
            
            else : print "error"
            
        df_data['activityLabel'] = tmpStepDataResult

        # lux  속성을 대상으로 조도 라벨 값 부여

        tmpLuxData = map(int,np.array(df_data["Lux"]))
        tmpLuxDataResult = []
        # https://actigraph.desk.com/customer/en/portal/articles/2515504-lux-measurements
        
        for i in range(0,int(len(tmpLuxData))):

            if (tmpLuxData[i] >=0) and (tmpLuxData[i] <= 50) : tmpLuxDataResult.append("ISLL") # inner Space low lux
            elif (tmpLuxData[i] >= 51) and (tmpLuxData[i] <= 500) : tmpLuxDataResult.append("ISHL") # inner Space high lux
            elif (tmpLuxData[i] >= 501) and (tmpLuxData[i] <= 1000) : tmpLuxDataResult.append("OSLL") # outer space low lux
            elif (tmpLuxData[i] >= 1001) : tmpLuxDataResult.append("OSHL") # outer space high lux
            else : print "error"
            
        df_data['luxLabel'] = tmpLuxDataResult


        for i in range(len(df_data)):
            
            sleep_data = []
        
        for i in xrange(len(pconfig)):
    
            wakeTimeData = None 
            sleepTimeData = None
            
            measurementDate = pconfig[self.config_header[2]][i]
            todayWakeup = pconfig[self.config_header[3]][i]
            todaySleep = pconfig[self.config_header[4]][i]
            nextdayWakeup = pconfig[self.config_header[5]][i]

            sleep_time = pd.to_datetime(todaySleep)

            # %d/%m/%Y
            # conf_date = (datetime.datetime.strptime(measurementDate, '%Y-%m-%d')).strftime('%d/%m/%Y')
            # tmp_next_day = (datetime.datetime.strptime(measurementDate, '%Y-%m-%d') + datetime.timedelta(days=1)).strftime('%d/%m/%Y')

            # '%Y-%m-%d'
            conf_date = (datetime.datetime.strptime(measurementDate, '%Y-%m-%d')).strftime(f_date)
            tmp_next_day = (datetime.datetime.strptime(measurementDate, '%Y-%m-%d') + datetime.timedelta(days=1)).strftime(f_date)
            
            
            # print sleep_time, conf_date, tmp_next_day

            if pd.to_datetime('18:00:00')  < sleep_time and sleep_time < pd.to_datetime('23:59:50'):

                ws_index = df_data[(df_data['Date'] == conf_date) & (df_data['Time'] == todayWakeup)].index
                we_index = df_data[(df_data['Date'] == conf_date) & (df_data['Time'] == todaySleep)].index

                ss_index = df_data[(df_data['Date'] == conf_date) & (df_data['Time'] == todaySleep)].index
                se_index = df_data[(df_data['Date'] == tmp_next_day) & (df_data['Time'] == nextdayWakeup)].index

                # print ws_index, we_index, ss_index, se_index
                # print ws_index[0], we_index[0], ss_index, se_index

                if len(ws_index) > 0 and len(we_index) > 0 :
                    wakeTimeData = df_data[ws_index[0]:we_index[0]+1]

                if len(ss_index) > 0 and len(se_index) > 0 :
                    sleepTimeData = df_data[ss_index[0]:se_index[0]+1]

            else:
                
                ws_index = df_data[(df_data['Date'] == conf_date) & (df_data['Time'] == todayWakeup)].index
                we_index = df_data[(df_data['Date'] == tmp_next_day) & (df_data['Time'] == todaySleep)].index

                ss_index = df_data[(df_data['Date'] == tmp_next_day) & (df_data['Time'] == todaySleep)].index
                se_index = df_data[(df_data['Date'] == tmp_next_day) & (df_data['Time'] == nextdayWakeup)].index

                # print ws_index, we_index, ss_index, se_index

                if len(ws_index) > 0 and len(we_index) > 0 :
                    wakeTimeData = df_data[ws_index[0]:we_index[0]+1]
                
                if len(ss_index) > 0 and len(se_index) > 0 :
                    sleepTimeData = df_data[ss_index[0]:se_index[0]+1]

            sleep_data.append({'wake': wakeTimeData, 'sleep': sleepTimeData})

        return sleep_data

# ===============================================================================

# ===============================================================================
# =
# =
# =
# =
# ===============================================================================

# 신규 함수 dataProcessing 추가
# 조도, 활동량 속성에 대해 3분위로 분할 이후 수정사항 1에서 부여한 라벨값을 통해 비율 계산

    def dataProcessing(self,sleep_data):
        
        tmpSleepData = []
        tmpDayData = []
        tmpResult = []
        
        # print len(sleep_data)
        # print sleep_data[0]['sleep'].iloc[0]
        
        for i in range(len(sleep_data)):
            
            # print sleep_data[i]['wake']['activityLabel'].value_counts()
            # print sleep_data[i]['wake']['luxLabel'].value_counts()

            sleepLength = int(len(sleep_data[i]['sleep']))
            tmpSleep = sleep_data[i]['sleep']['Vector Magnitude'].astype(bool).sum() / sleepLength

            wakeLength = int((len(sleep_data[i]['wake']))/3)
            tmpIndex = int(sleep_data[i]['wake'].iloc[0].name)

            activityLabelList = ['NA','LA','MA','HA']
            luxLabelList = ['ISLL','ISHL','OSLL','OSHL']

            tmpWakeData01 = sleep_data[i]['wake'][['activityLabel','luxLabel']][0:wakeLength]
            tmpWakeData02 = sleep_data[i]['wake'][['activityLabel','luxLabel']][wakeLength:(wakeLength*2)]
            tmpWakeData03 = sleep_data[i]['wake'][['activityLabel','luxLabel']][(wakeLength*2):(wakeLength*3)]
            
            # print('tmpWakeData01', tmpWakeData01)
            # print('tmpWakeData02', tmpWakeData02)
            
            tmpData = [tmpWakeData01,tmpWakeData02,tmpWakeData03]

            for x in range(0,3):

                tmpActivity = []
                tmpLux = []

                for y in range(0,4):
                    tmp01 = sum((tmpData[x]['activityLabel'] == activityLabelList[y]) / wakeLength)
                    tmp02 = sum((tmpData[x]['luxLabel']==luxLabelList[y]) / wakeLength)

                    tmpActivity.append(tmp01)
                    tmpLux.append(tmp02)

                    # print activityLabelList[y] ,tmp01
                    # print luxLabelList[y],tmp02

                # for x, name in enumerate(tmpData):
                #     for y, name2 in enumerate(activityLabelList):
                #         tmp01 = sum(name['activityLabel'] == name2) / wakeLength
                #         tmp02 = sum(name['luxLabel'] == luxLabelList[y]) / wakeLength

                tmpDayData.append(tmpActivity+tmpLux)
            
            tmpSleepData.append(tmpSleep)
            
            tmpResult = [tmpDayData,tmpSleepData]

        header = ["NA1st","LA1st","MA1st","HA1st","ISLL1st","ISHL1st","OSLL1st","OSHL1st",
                  "NA2nd","LA2nd","MA2nd","HA2nd","ISLL2nd","ISHL2nd","OSLL2nd","OSHL2nd",
                  "NA3rd","LA3rd","MA3rd","HA3rd","ISLL3rd","ISHL3rd","OSLL3rd","OSHL3rd"]
        
        recordLength = int(len(tmpDayData)/3)

        # print "record day is " ,recordLength," day(s)"
        
        wakeDataResult = pd.DataFrame(np.array(tmpDayData).reshape(recordLength,24),columns=header)
        sleepDataResult = pd.DataFrame(np.array(tmpSleepData).reshape(recordLength,1),columns=["sleepActivity"])
        
        result = pd.concat([wakeDataResult,sleepDataResult],axis=1)
        
        return result


# ===============================================================================

# 각 데이터에 대한 주간 / 야간 csv 출력 
# 구현 단계이며 중복 삭제 방지 기능 추가 구현 예정 
    # def writeCSV(self, sleep_data):

    #     for i in range(len(sleep_data)):
    #         for j in [0,1]:
    #             if j == 0 : tmp = 'wake'
    #             else : tmp = 'sleep'

    #             name = "test%s%d"  %(i,j)
    #             pd.DataFrame.to_csv(sleep_data[i][tmp],"./output/"+name+".csv",sep=',')


    def quantityScale2(self, pdata_w, pdata_s, cols):
        
        dataCheck = len(pdata_w)
        
        vm_sum = []
        
        vm_sum.append(pdata_w[0:int(dataCheck/3)].sum())
        vm_sum.append(pdata_w[int(dataCheck/3):int(dataCheck/3)*2].sum())
        vm_sum.append(pdata_w[int(dataCheck/3):].sum())
        
        vm_sum.append(pdata_s.sum())

        return vm_sum        