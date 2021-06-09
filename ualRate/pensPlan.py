#!/usr/bin/env python3
from pensPop import pensPop
from pensFund import pensFund

class pensPlan(object):
    def __init__(self, N, currentYear):

        self.currentYear = currentYear

        ## Discount rate choice probably belongs at the plan level
        self.discountRate = 0.07

        self.population = pensPop()
        self.population.simulateMembers(N,
                                        ageRange=c(24, 65),
                                        serviceRange=(1,30))
        self.liability = self.population.calculateLiability(self.discountRate)
        ## Start off 3/4 funded.
        self.fund = pensFund(0.75 * self.liability)

    def annualReport(self, year):
        print(self.population.active(), self.population.retired())
        print(self.calculateLiability(self.discountRate))
        print(self.fund.annualReport(year))

    def advanceOneYear(self):
        self.currentYear = self.population.advanceOneYear()
        self.population.hireReplacements(1.0)

        ## Calculate the increment of the normal cost.
        premiumPayments = self.population.calculateLiability(self.discountRate) - self.liability
        self.fund.makePayments(premiumPayments)
        self.fund.addInvestmentEarnings(self.currentYear)


    def adjustEmployment(self, N):
        """Adjust employment up or down."""
        if N > 0:
            self.population.addNewMembers(N)
        else:
            self.population.layoffMembers(-N)

