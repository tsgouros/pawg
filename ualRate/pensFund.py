#!/usr/bin/env python3
import numpy as np

class pensFund(object):
	def __init__(assetTotal, currYear, pctE = 0.6, pctB = 0.3, pctO = 0.1):
		self.currentYear = currYear
		
		self.pcts = [pctE, pctB, pctO]
    
    	self.equity = self.pcts[0]*assetTotal
    	self.bonds = self.pcts[1]*assetTotal
    	self.other = self.pcts[2]*assetTotal
		
		self.ledger = {self.currentYear : [self.equity, self.bonds, self.other]}
	
	def annualReport(self, year):
		if year in self.ledger:
			report = self.ledger[year]
			return "Assets for the year " + year + ":\nEquity = $" + str(report[0]) + "\nBonds = $" + str(report[1]) + "\nOther = $" + str(report[2]) + "\nTotal: $" + str(sum(report))
		else:
			return "Assets could not be found for the year " + year + "."
		
	
	def makePayments(self, premiums, benefits):
		self.equity += self.pcts[0]*premiums
    	self.bonds += self.pcts[1]*premiums
    	self.other += self.pcts[2]*premiums
		
		self.equity -= self.pcts[0]*benefits
    	self.bonds -= self.pcts[1]*benefits
    	self.other -= self.pcts[2]*benefits
		
	def addInvestmentEarnings(self, year):
		e = np.random.normal(0.06, 0.03)
		b = np.random.normal(0.04, 0.01)
		o = np.random.normal(0.06, 0.05)
		
		self.equity *= (1+e)
		self.bonds *= (1+b)
		self.other *= (1+o)
		
		self.currentYear = year 
		self.ledger[self.currentYear] = [self.equity, self.bonds, self.other]
		
    
    
  
