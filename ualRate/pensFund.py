#!/usr/bin/env python3
import numpy as np

class pensFund(object):
	def __init__(self, assetTotal, currYear, pctE = 0.6, pctB = 0.3, pctO = 0.1):
		self.currentYear = currYear
		self.pcts = [pctE, pctB, pctO]
		
		self.equity = self.pcts[0]*assetTotal
		self.bonds = self.pcts[1]*assetTotal
		self.other = self.pcts[2]*assetTotal
		
		self.ledger = {self.currentYear : [self.equity, self.bonds, self.other]}
	
	def annualReport(self, year=0):
		if year == 0:
			year = self.currentYear
		if year in self.ledger:
			report = self.ledger[year]
			out = ("Assets for the year %.0f:\n\tEquity = $%0.f\n\tBonds = $%.0f\n\tOther = $%.0f\nTotal: $%.0f" %
                  (year, report[0], report[1], report[2], sum(report)))
			return out
		else:
			return "Assets could not be found for the year %.0f." % year
        
	def print(self):
		print(self.annualReport())
		
	
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
		
##### TESTING #####
	
def testFund():
	print("Creating a fund. assetTotal = 200, currYear = 2020.\n")
	fund = pensFund(200, 2020)
	print(fund.annualReport())
	print("\nAdvancing one year...\n")
	fund.makePayments(50,100)
	fund.addInvestmentEarnings(2021)
	print(fund.annualReport(2021))
	print(fund.annualReport(2020))
	print(fund.annualReport(2022))
	
	
if __name__ == "__main__":
	testFund()
		
		
    
    
  
