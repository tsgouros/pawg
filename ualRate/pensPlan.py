#!/usr/bin/env python3
from pensPop import pensPop
from pensFund import pensFund
import plotly.express as px
import pandas as pd
import statistics as stats
import os


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
        self.ual = self.liability - sum(self.fund.ledger[self.currentYear])
        self.assets = sum(self.fund.ledger[self.currentYear])
        self.growthRate = 0.0

    def annualReport(self):
        # show number of active and retired members
        self.population.printReport()

        # Show current liability
        print("Current Liability: $%s" % '{:,}'.format(round(self.liability, 2)))

        # Show assets
        self.fund.printReport()

        # UAL
        if self.ual > 0:
            print("Unfunded Liability: $%s (%s%%)" % ('{:,}'.format(round(self.ual, 2)),
                                                      round((self.ual / self.liability) * 100)))
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
        if self.ual != 0:
            self.growthRate = ((max((self.liability - self.assets), 0) - self.ual) / self.ual) * 100
        else:
            if self.liability - self.assets > 0:
                self.growthRate = 100
            else:
                self.growthRate = 0
        self.ual = max((self.liability - self.assets), 0)

    def adjustEmployment(self, N):
        """Adjust employment up or down."""
        if N > 0:
            self.population.addNewMembers(N)
        else:
            self.population.layoffMembers(-N)


def runModel(volatility, employmentGrowth=1.0, contributionRate=1.0, years=40, saveFiles=False,
             filename="plotly_graph"):
    # Create Plan and dictionary to keep track of annual data
    d = {}
    p = pensPlan(2000, volatility, employmentGrowth, contributionRate)

    # Run 10 'prologue' years to try and get some retired members and liability setup
    for i in range(10):
        p.advanceOneYear()

    d["UAL"] = [p.ual]
    d["Assets"] = [p.assets]
    d["Liability"] = [p.liability]
    d["Growth(%)"] = [round(p.growthRate, 2)]
    d["Active Members"] = [sum([m.status == 'active' for m in p.population.members])]
    d["Retired Members"] = [sum([m.status == 'retired' for m in p.population.members])]
    d["Avg. Service"] = [p.population.getAvgService()]

    # Run model for several years, saving data along the way
    for year in range(years):
        p.advanceOneYear()
        d["UAL"].append(p.ual)
        d["Assets"].append(p.assets)
        d["Liability"].append(p.liability)
        d["Growth(%)"].append(round(p.growthRate, 2))
        d["Active Members"].append(sum([m.status == 'active' for m in p.population.members]))
        d["Retired Members"].append(sum([m.status == 'retired' for m in p.population.members]))
        d["Avg. Service"].append(p.population.getAvgService())

    # Stops here if you're not trying to save the data visualization.
    if not saveFiles:
        return d

    # Convert dictionary data into a DataFrame, used to create the graph.
    df = pd.DataFrame(data=d, index=range(2000, (2001 + years)))

    # Print the DataFrame, visible in console as a table.
    # print(df)

    # Create the graph itself.
    fig = px.line(df)

    # Save graph as HTML file in the appropriate folder. If no such directory exists yet, create it.
    # HTML files can be opened to view interactive plotly visualization.
    folder = "eg=%s_cr=%s" % (str(employmentGrowth), str(contributionRate))
    try:
        os.makedirs('Graphs/%s' % folder)
    except OSError as e:
        if e.errno != 17:
            raise

    count = 1
    temp = filename
    filename = "%s_%s" % (temp, str(count))
    directory = "Graphs/%s/%s.html" % (folder, filename)
    while os.path.exists(directory):
        count += 1
        filename = "%s_%s" % (temp, str(count))
        directory = "Graphs/%s/%s.html" % (folder, filename)

    fig.write_html(directory)

    return d


def getModelData(volatility, employmentGrowth, contributionRate, size=50, years=100, saveAll=False, filename="run_1"):
    # Create directory for data visualization, if necessary.
    folder = "eg=%s_cr=%s" % (str(employmentGrowth), str(contributionRate))
    if not os.path.exists('Graphs/%s' % folder):
        os.makedirs('Graphs/%s' % folder)

    # Determine appropriate filename for visualization of mean values
    if filename == "run_1":
        count = 2
        directory = "Graphs/%s/%s.html" % (folder, filename)
        while os.path.exists(directory):
            filename = "run_%s" % (str(count))
            directory = "Graphs/%s/%s.html" % (folder, filename)
            count += 1
    else:
        count = 1
        temp = filename
        filename = "%s_%s" % (temp, str(count))
        directory = "Graphs/%s/%s.html" % (folder, filename)
        while os.path.exists(directory):
            count += 1
            filename = "%s_%s" % (temp, str(count))
            directory = "Graphs/%s/%s.html" % (folder, filename)

    # If you wish to visualize the data of each individual model, in addition to the mean data...
    if saveAll:
        # Create a folder to hold individual model graphs, based on the mean data filename
        if not os.path.exists('Graphs/%s/%s' % (folder, filename)):
            os.makedirs('Graphs/%s/%s' % (folder, filename))

        # All individual model graphs will be named graph_1, graph_2, graph_3, etc. and placed within the above folder
        # (The numbers are added within the runModel() function.)
        subdir = "%s/graph" % filename

    else:
        subdir = None

    # Create list to store the data from each run.
    model_data = []

    for i in range(size):
        # Run the model, then append the resulting dictionary to the list.
        # Set saveAll to True if you want to save individual run visualizations.

        model_data.append(runModel(volatility, employmentGrowth, contributionRate, years, saveFiles=saveAll,
                                   filename=subdir))

        # Console counter for user to see how far the script is progressing (it takes a long time!)
        print(i, end=" ")

    print("\nFinished running models. Averaging the data...")

    # Find the mean values across all runs and visualize them, to see overall shape of the data w/ the given parameters.
    mean_data = {"UAL": [], "Assets": [], "Liability": [], "Growth(%)": [], "Active Members": [], "Retired Members": [],
                 "Avg. Service": []}
    for i in range(years):
        m_ual = [run["UAL"][i] for run in model_data]
        m_assets = [run["Assets"][i] for run in model_data]
        m_liability = [run["Liability"][i] for run in model_data]
        m_growth = [run["Growth(%)"][i] for run in model_data]
        m_active = [run["Active Members"][i] for run in model_data]
        m_retired = [run["Retired Members"][i] for run in model_data]
        m_service = [run["Avg. Service"][i] for run in model_data]

        ual = stats.mean(m_ual)
        assets = stats.mean(m_assets)
        liability = stats.mean(m_liability)
        growth = stats.mean(m_growth)
        active = stats.mean(m_active)
        retired = stats.mean(m_retired)
        service = stats.mean(m_service)

        mean_data["UAL"].append(ual)
        mean_data["Assets"].append(assets)
        mean_data["Liability"].append(liability)
        mean_data["Growth(%)"].append(round(growth, 2))
        mean_data["Active Members"].append(active)
        mean_data["Retired Members"].append(retired)
        mean_data["Avg. Service"].append(service)

    # Convert dictionary data into a DataFrame, used to create the graph.
    df = pd.DataFrame(data=mean_data, index=range(2000, (2000 + years)))

    # Print the DataFrame, visible in console as a table.
    # print(df)

    # Create the graph and save it as HTML file in the appropriate folder.
    fig = px.line(df)
    fig.write_html(directory)
    # HTML files can be opened to view interactive plotly visualization.

    return [mean_data, model_data]


def setupFolders():
    try:
        os.makedirs('Graphs')
    except OSError as e:
        if e.errno != 17:
            raise


if __name__ == "__main__":
    # Create directories for storing graphs, if necessary
    setupFolders()

    #### Add your desired model runs here! Some examples are included below.

    # Volatility values to be used throughout. List contains mean and std. deviation (in that order) for the three 
    # investment channels in pensFund. 
    vol = [0.04, 0.03, 0.02, 0.03, 0.04, 0.04]

    # Default
    eg = 1.0
    cr = 1.0
    getModelData(vol, eg, cr, size=50, years=100, saveAll=False)
    
    # Test Impact of EG
    eg = 1.25
    cr = 1.0
    getModelData(vol, eg, cr, size=50, years=100, saveAll=False)

    eg = 0.75
    cr = 1.0
    getModelData(vol, eg, cr, size=50, years=100, saveAll=False)

    # Test impact of CR
    eg = 1.0
    cr = 1.25
    getModelData(vol, eg, cr, size=50, years=100, saveAll=False)

    eg = 1.0
    cr = 0.75
    getModelData(vol, eg, cr, size=50, years=100, saveAll=False)
