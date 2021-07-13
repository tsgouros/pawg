#!/usr/bin/env python3
from pensPop import pensPop
from pensFund import pensFund

#modules for graphing data:
import plotly.express as px
import pandas as pd
import statistics

class pensPlan(object):
    def __init__(self, currentYear, volatility, employmentGrowth=1.0, contributionRate=1.0, discountRate=0.07):

        self.currentYear = currentYear
        self.employ = employmentGrowth
        self.cr = contributionRate

        self.discountRate = discountRate

        self.population = pensPop([], self.discountRate)
        self.population.simulatePopulation()

        self.liability = self.population.calculateTotalLiability()

        ## Start off 3/4 funded.
        self.fund = pensFund(0.75 * self.liability, self.currentYear, volatility)
        self.uaf = self.liability - sum(self.fund.ledger[self.currentYear])
        self.assets = sum(self.fund.ledger[self.currentYear])
        self.growthRate = 0.0


    def annualReport(self):
        #show number of active and retired members
        self.population.printReport()
        
        #Show current liability
        print( "Current Liability: $%s" % '{:,}'.format(round(self.liability, 2)))

        #Show assets 
        self.fund.printReport()

        #UAL
        if self.uaf > 0:
            print("Unfunded Liability: $%s (%s%%)" % ('{:,}'.format(round(self.uaf, 2)),
                                                      round((self.uaf / self.liability) * 100)))
        else:
            print("Unfunded Liability: $0")

    def advanceOneYear(self):
        self.currentYear += 1
        info = self.population.advanceOneYear()
        self.population.hireReplacements(info['replace'], self.employ)

        ## Calculate the increment of the normal cost.
        newLiability = self.population.calculateTotalLiability()
        premiums = (newLiability - self.liability) * self.cr
        self.liability = newLiability

        self.fund.payPremiums(premiums)
        self.fund.payBenefits(info['benefit'])
        self.fund.addInvestmentEarnings(self.currentYear)
        self.assets = sum(self.fund.ledger[self.currentYear])
        if self.uaf !=0:
            self.growthRate = ((max((self.liability - self.assets), 0) - self.uaf)/self.uaf)*100
        else:
            if self.liability - self.assets > 0:
                self.growthRate = 100
            else:
                self.growthRate = 0
        self.uaf = max((self.liability - self.assets), 0)

    def adjustEmployment(self, N):
        """Adjust employment up or down."""
        if N > 0:
            self.population.addNewMembers(N)
        else:
            self.population.layoffMembers(-N)

def runModel(volatility, employmentGrowth=1.0, contributionRate=1.0):
    """TODO: Annotate with comments for Tom"""
    #Dictionary to keep track of annual values
    d = {}
    p = pensPlan(2000, volatility, employmentGrowth, contributionRate)
    d["UAF"] = [p.uaf]
    d["Assets"] = [p.assets]
    d["Liability"] = [p.liability]
    d["Growth(%)"] = [round(p.growthRate, 2)]
    d["Active Members"] = [sum([m.status == 'active' for m in p.population.members])]
    d["Retired Members"] = [sum([m.status == 'retired' for m in p.population.members])]

    #Run model for 40 years, saving data along the way
    for year in range(40):
        p.advanceOneYear()
        d["UAF"].append(p.uaf)
        d["Assets"].append(p.assets)
        d["Liability"].append(p.liability)
        d["Growth(%)"].append(round(p.growthRate, 2))
        d["Active Members"].append(sum([m.status == 'active' for m in p.population.members]))
        d["Retired Members"].append(sum([m.status == 'retired' for m in p.population.members]))

    ##### Below should be commented out when using getModelData. Intended for isolated use.

    #Convert dictionary data into a DataFrame, used to create the graph.
    #df = pd.DataFrame(data=d, index=range(2000, 2041))

    #Print the DataFrame, visible in console as a table.
    #print(df)

    #Create the graph itself, and open the visualization in a new tab. Does not save the graph anywhere at the moment.
    #fig = px.line(df)
    #fig.show()

    return d

def getModelData(size, vol, eg, cr):
    #List to store the data from each run.
    model_data = []
    for i in range(size):
        #Run the model, then append the resulting dictionary to the list.
        model_data.append(runModel(vol, eg, cr))

        #Console counter for me to see how far the script is progressing (it takes a long time!)
        print(i, end=" ")

    print("\nFinished running models. Averaging the data...")

    ##Find the mean values across all runs and visualize them, to see overall shape of the data w/ the given parameters.
    mean_data = {"UAF": [], "Assets": [], "Liability": [], "Growth(%)": [], "Active Members" : [], "Retired Members" : []}
    for i in range(40):
        m_uaf = [run["UAF"][i] for run in model_data]
        m_assets = [run["Assets"][i] for run in model_data]
        m_liability = [run["Liability"][i] for run in model_data]
        m_growth = [run["Growth(%)"][i] for run in model_data]
        m_active = [run["Active Members"][i] for run in model_data]
        m_retired = [run["Retired Members"][i] for run in model_data]

        uaf = statistics.mean(m_uaf)
        assets = statistics.mean(m_assets)
        liability = statistics.mean(m_liability)
        growth = statistics.mean(m_growth)
        active = statistics.mean(m_active)
        retired = statistics.mean(m_retired)

        mean_data["UAF"].append(uaf)
        mean_data["Assets"].append(assets)
        mean_data["Liability"].append(liability)
        mean_data["Growth(%)"].append(round(growth, 2))
        mean_data["Active Members"].append(active)
        mean_data["Retired Members"].append(retired)

    # Convert dictionary data into a DataFrame, used to create the graph.
    df = pd.DataFrame(data=mean_data, index=range(2000, 2040))

    # Print the DataFrame, visible in console as a table.
    #print(df)

    # Create the graph itself, and open the visualization in a new tab. Does not save the graph anywhere at the moment.
    fig = px.line(df)
    fig.show()
    return [mean_data, model_data]






if __name__ == "__main__":

    ##Default
    vol = [0.04, 0.03, 0.02, 0.03, 0.04, 0.04]
    eg = 1.0
    cr = 1.0
    getModelData(50, vol, eg, cr)

    ##Test Impact of EG
    vol = [0.04, 0.03, 0.02, 0.03, 0.04, 0.04]
    eg = 1.25
    cr = 1.0
    getModelData(50, vol, eg, cr)

    vol = [0.04, 0.03, 0.02, 0.03, 0.04, 0.04]
    eg = 0.75
    cr = 1.0
    getModelData(50, vol, eg, cr)

    ##Test impact of CR
    vol = [0.04, 0.03, 0.02, 0.03, 0.04, 0.04]
    eg = 1.0
    cr = 1.25
    getModelData(50, vol, eg, cr)

    vol = [0.04, 0.03, 0.02, 0.03, 0.04, 0.04]
    eg = 1.0
    cr = 0.75
    getModelData(50, vol, eg, cr)

    #plan = pensPlan(2000)
    #print("Plan created!")
    #print("\nAnnual Report for %s:" % plan.currentYear)
    #plan.annualReport()
    #for i in range(50):
        #print("\nAdvancing a year...")
        #plan.advanceOneYear()
        #print("\nAnnual Report for %s:" % plan.currentYear)
        #plan.annualReport()
