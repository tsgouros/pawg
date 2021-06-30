#!/usr/bin/env python3
from pensPop import pensPop
from pensFund import pensFund

class pensPlan(object):
    def __init__(self, N, currentYear):

        self.currentYear = currentYear

        ## Discount rate choice probably belongs at the plan level
        ##TJ: Need to check with Emily before setting this up ^
        self.discountRate = 0.07

        self.population = pensPop()
        self.population.simulatePopulation()
        ## TJ: I'm using simulatePopulation for now, but in the end we should use simulateMembers in order to control the
        ## size of the population

        self.liability = self.population.calculateTotalLiability(self.population)

        ## Start off 3/4 funded.
        self.fund = pensFund(0.75 * self.liability, self.currentYear)
        ## TJ: Liability starts as zero, so the fund also starts with zero assets. If this
        ## is not ideal, then maybe liability should have a hard-coded start value?


    def annualReport(self, year=0):
        """" TODO: Make this work for any year"""

        #show number of active and retired members
        self.population.printReport()
        
        #Show current liability
        print( "Current Liability: $%s" % '{:,}'.format(round(self.liability, 2)))

        #Show assets 
        self.fund.printReport()

        #UAL
        assets = self.fund.ledger[self.currentYear]
        if self.liability - sum(assets) > 0:
            print("Unfunded Liability: $%s" % max('{:,}'.format(round((self.liability - sum(assets)), 2))))
        else:
            print("Unfunded Liability: $0")

    def advanceOneYear(self):
        self.currentYear += 1
        info = self.population.advanceOneYear()
        self.population.hireReplacements(info['replace'], 1.0)

        #Calculate benefits owed this year (TODO)
        print("Benefit payments amount to $%s" % '{:,}'.format(round(info['benefit'], 2)))

        ## Calculate the increment of the normal cost.
        newLiability = self.population.calculateTotalLiability(self.population)
        premiums = newLiability - self.liability
        self.liability = newLiability
        ##TJ: updated self.liability here since it didn't change otherwise
        print("Premium payments amount to $%s" % '{:,}'.format(round(premiums, 2)))

        self.fund.payPremiums(premiums)
        self.fund.payBenefits(info['benefit'])
        self.fund.addInvestmentEarnings(self.currentYear)




    def adjustEmployment(self, N):
        """Adjust employment up or down."""
        if N > 0:
            self.population.addNewMembers(N)
        else:
            self.population.layoffMembers(-N)


if __name__ == "__main__":
    plan = pensPlan(50, 2000)
    print("Plan created!")
    print("\nAnnual Report for %s:" % plan.currentYear)
    plan.annualReport()
    for i in range(50):
        print("\nAdvancing a year...")
        plan.advanceOneYear()
        print("\nAnnual Report for %s:" % plan.currentYear)
        plan.annualReport()
