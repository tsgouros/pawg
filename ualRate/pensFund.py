#!/usr/bin/env python3
import numpy as np


class pensFund(object):
    def __init__(self, assetTotal, currYear, volatility, pctE=0.6, pctB=0.3, pctO=0.1):
        self.currentYear = currYear
        self.pcts = [pctE, pctB, pctO]
        self.vol = volatility

        self.equity = round(self.pcts[0] * assetTotal, 2)
        self.bonds = round(self.pcts[1] * assetTotal, 2)
        self.other = round(self.pcts[2] * assetTotal, 2)

        self.ledger = {self.currentYear: [self.equity, self.bonds, self.other]}

    def annualReport(self, year=0):
        if year == 0:
            year = self.currentYear

        if year in self.ledger:
            report = self.ledger[year]
            out = ("Assets:\n\tEquity = $%s\n\tBonds = $%s\n\tOther = $%s\nTotal: $%s" %
                   ('{:,}'.format(report[0]), '{:,}'.format(report[1]), '{:,}'.format(report[2]),
                    '{:,}'.format(round(sum(report), 2))))
            return out
        else:
            return "Assets could not be found for the year %.0f." % year

    def printReport(self):
        print(self.annualReport())

    def payPremiums(self, premiums):
        total = self.equity + self.bonds + self.other
        if total == 0:
            self.equity += self.pcts[0] * premiums
            self.bonds += self.pcts[1] * premiums
            self.other += self.pcts[2] * premiums
        else:
            self.equity += (self.equity / total) * premiums
            self.bonds += (self.bonds / total) * premiums
            self.other += (self.other / total) * premiums

    def payBenefits(self, benefits):
        total = self.equity + self.bonds + self.other
        if total == 0:
            self.equity += self.pcts[0] * benefits
            self.bonds += self.pcts[1] * benefits
            self.other += self.pcts[2] * benefits
        else:
            self.equity -= (self.equity / total) * benefits
            self.bonds -= (self.bonds / total) * benefits
            self.other -= (self.other / total) * benefits

    def addInvestmentEarnings(self, year):
        e = np.random.normal(self.vol[0], self.vol[1])
        b = np.random.normal(self.vol[2], self.vol[3])
        o = np.random.normal(self.vol[4], self.vol[5])
        #defaults: [0.06, 0.03, 0.04, 0.01, 0.06, 0.05]

        #print("Investment Earnings amount to $%s" % '{:,}'.format(round(earnings, 2)))

        self.equity *= (1 + e)
        self.bonds *= (1 + b)
        self.other *= (1 + o)

        self.equity = round(self.equity, 2)
        self.bonds = round(self.bonds, 2)
        self.other = round(self.other, 2)

        self.currentYear = year
        self.ledger[self.currentYear] = [self.equity, self.bonds, self.other]


##### TESTING #####

def testFund():
    print("Creating a fund. assetTotal = 200, currYear = 2020.\n")
    fund = pensFund(200, 2020)
    print(fund.annualReport())
    print("\nAdvancing one year...\n")
    fund.payPremiums(50)
    fund.payBenefits(100)
    fund.addInvestmentEarnings(2021)
    print(fund.annualReport(2021))
    print(fund.annualReport(2020))
    print(fund.annualReport(2022))


if __name__ == "__main__":
    testFund()
