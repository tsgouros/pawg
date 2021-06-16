#!/usr/bin/env python3
import openpyxl
from pathlib import Path

class pensMort(object):
    def __init__(self):
        m_file = Path('..', 'mortalityTables', 'pub-2010-amount-mort-rates.xlsx')
        m_wb = openpyxl.load_workbook(m_file)
        pubG = m_wb['PubG-2010']
        self.data_f = {}
        self.data_m = {}
        
        for i, row in enumerate(pubG.iter_rows(4, pubG.max_row)):
            if i == 0:
                self.data_f[row[3]] = []
                self.data_f[row[4]] = []
                self.data_f[row[5]] = []
                self.data_f[row[6]] = []

                self.data_m[row[8]] = []
                self.data_m[row[9]] = []
                self.data_m[row[10]] = []
                self.data_m[row[11]] = []
				
            else:
                self.data_f['Employee'].append(row[3])
                self.data_f['Healthy Retiree'].append(row[4])
                self.data_f['Disabled Retiree'].append(row[5])
                self.data_f['Contingent Survivor'].append(row[6])

                self.data_m['Employee'].append(row[8])
                self.data_m['Healthy Retiree'].append(row[9])
                self.data_m['Disabled Retiree'].append(row[10])
                self.data_m['Contingent Survivor'].append(row[11])
				
    def getRate(sex, age, status):
        if sex == "F":
            table = self.data_f
        else:
            table = self.data_m
		
        if status == "active":
            rate = table['Employee'][age]
        elif status == "retired":
            rate = table['Healthy Retiree'][age]
        elif status == "deceased":
            rate = 1
			
        return rate

