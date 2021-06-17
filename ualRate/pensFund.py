#!/usr/bin/env python3

class pensFund(object):
	def __init__(assetTotal, currYear, pctE = 0.6, pctB = 0.3, pctO = 0.1):
		self.assets = assetTotal
		self.currentYear = currYear
		self.ledger = {self.currentYear : self.assets}
    
    	self.equity = pctE
    	self.bonds = pctB
    	self.other = pctO
	
	def annualReport(self, year):
		if year in self.ledger:
			return "Assets for the year " + year + " amount to $" + self.ledger[year] + "."
		else:
			return "Asset total could not be found for the year " + year + "."
		
	
	def makePayments(self, premiums):
		self.assets += premiums
		self.ledger[self.currentYear] = self.assets
		
	def addInvestmentEarnings(self, year):
		self.currentYear = year ##maybe we could update the year somewhere else?
		self.assets *= (1 + self.equity + self.bonds + self.other)
		self.ledger[self.currentYear] = self.assets
		
    
    
  
