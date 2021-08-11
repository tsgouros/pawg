#!/usr/bin/env python3
from pensPop import pensPop
from pensFund import pensFund
import plotly.express as px
import pandas as pd
import statistics as stats
import os


class pensPlan(object):
    def __init__(
        self, currentYear, volatility, employmentGrowth=1.0, discountRate=0.10
    ):

        self.currentYear = currentYear
        self.employ = employmentGrowth
        self.cr = 0
        self.payGo = 0
        self.totalPay = 0

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
        print("Current Liability: $%s" % "{:,}".format(round(self.liability, 2)))

        # Show assets
        self.fund.printReport()

        # Show total salary
        self.population.calculateTotalSalary()

        # UAL
        if self.ual > 0:
            print(
                "Unfunded Liability: $%s (%s%%)"
                % (
                    "{:,}".format(round(self.ual, 2)),
                    round((self.ual / self.liability) * 100),
                )
            )
        else:
            print("Unfunded Liability: $0")

    def advanceOneYear(self):
        self.currentYear += 1
        info = self.population.advanceOneYear()

        # variable to record growth in active population after hiring new members.
        popGrowth = sum([m.status == "active" for m in self.population.members])
        self.population.hireReplacements(info["replace"], self.employ)
        popGrowth = (
            sum([m.status == "active" for m in self.population.members]) - popGrowth
        )

        ## Calculate the increment of the normal cost.
        newLiability = self.population.calculateTotalLiability()
        normalCost = newLiability - self.liability
        self.liability = newLiability

        self.fund.addPremiums(normalCost)

        self.payGo = self.fund.payBenefits(info["benefit"])
        self.totalPay = self.population.calculateTotalSalary()

        if popGrowth != 0:
            self.cr = (normalCost + self.payGo) / (self.totalPay * popGrowth)
        else:
            self.cr = (normalCost + self.payGo) / self.totalPay

        self.fund.addInvestmentEarnings(self.currentYear)
        self.assets = sum(self.fund.ledger[self.currentYear])
        if self.ual != 0:
            self.growthRate = (
                (max((self.liability - self.assets - self.payGo), 0) - self.ual)
                / self.ual
            ) * 100
        else:
            if self.liability - self.assets > 0:
                self.growthRate = 100
            else:
                self.growthRate = 0
        self.ual = max((self.liability - self.assets - self.payGo), 0)

    def adjustEmployment(self, N):
        """Adjust employment up or down."""
        if N > 0:
            self.population.addNewMembers(N)
        else:
            self.population.layoffMembers(-N)


def runModel(
    volatility, employmentGrowth=1.0, years=40, saveFiles=False, filename="plotly_graph"
):
    # Create Plan and dictionary to keep track of annual data
    d = {}
    p = pensPlan(2000, volatility, employmentGrowth)

    d["UAL"] = [p.ual]
    d["Assets"] = [p.assets]
    d["Liability"] = [p.liability]
    d["UAL Growth(%)"] = [round(p.growthRate, 2)]
    d["Active Members"] = [sum([m.status == "active" for m in p.population.members])]
    d["Retired Members"] = [sum([m.status == "retired" for m in p.population.members])]
    d["Avg. Service"] = [p.population.getAvgService()]
    d["Contribution Rate"] = [p.cr]
    d["payGo"] = [p.payGo]
    d["Total Salary"] = [p.totalPay]

    # Run model for several years, saving data along the way
    for year in range(years):
        p.advanceOneYear()
        d["UAL"].append(p.ual)
        d["Assets"].append(p.assets)
        d["Liability"].append(p.liability)
        d["UAL Growth(%)"].append(round(p.growthRate, 2))
        d["Active Members"].append(
            sum([m.status == "active" for m in p.population.members])
        )
        d["Retired Members"].append(
            sum([m.status == "retired" for m in p.population.members])
        )
        d["Avg. Service"].append(p.population.getAvgService())
        d["Contribution Rate"].append(p.cr)
        d["payGo"].append(p.payGo)
        d["Total Salary"].append(p.totalPay)

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
    folder = "eg=%s" % (str(employmentGrowth))
    try:
        os.makedirs("Graphs/%s" % folder)
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


def getModelData(
    volatility, employmentGrowth, size=50, years=100, saveAll=False, filename="run_1"
):
    # Create directory for data visualization, if necessary.
    folder = "eg=%s" % (str(employmentGrowth))
    if not os.path.exists("Graphs/%s" % folder):
        os.makedirs("Graphs/%s" % folder)

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
        if not os.path.exists("Graphs/%s/%s" % (folder, filename)):
            os.makedirs("Graphs/%s/%s" % (folder, filename))

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

        model_data.append(
            runModel(
                volatility, employmentGrowth, years, saveFiles=saveAll, filename=subdir
            )
        )

        # Console counter for user to see how far the script is progressing (it takes a long time!)
        print(i, end=" ")

    print("\nFinished running models. Averaging the data...")

    # Find the mean values across all runs and visualize them, to see overall shape of the data w/ the given parameters.
    mean_data = {
        "UAL": [],
        "Assets": [],
        "Liability": [],
        "UAL Growth(%)": [],
        "Active Members": [],
        "Retired Members": [],
        "Avg. Service": [],
        "Contribution Rate": [],
        "payGo": [],
        "Total Salary": [],
    }
    for i in range(years):
        m_ual = [run["UAL"][i] for run in model_data]
        m_assets = [run["Assets"][i] for run in model_data]
        m_liability = [run["Liability"][i] for run in model_data]
        m_growth = [run["UAL Growth(%)"][i] for run in model_data]
        m_active = [run["Active Members"][i] for run in model_data]
        m_retired = [run["Retired Members"][i] for run in model_data]
        m_service = [run["Avg. Service"][i] for run in model_data]
        m_cr = [run["Contribution Rate"][i] for run in model_data]
        m_payGo = [run["payGo"][i] for run in model_data]
        m_salary = [run["Total Salary"][i] for run in model_data]

        ual = stats.mean(m_ual)
        assets = stats.mean(m_assets)
        liability = stats.mean(m_liability)
        growth = stats.mean(m_growth)
        active = stats.mean(m_active)
        retired = stats.mean(m_retired)
        service = stats.mean(m_service)
        cr = stats.mean(m_cr)
        payGo = stats.mean(m_payGo)
        salary = stats.mean(m_salary)

        mean_data["UAL"].append(ual)
        mean_data["Assets"].append(assets)
        mean_data["Liability"].append(liability)
        mean_data["UAL Growth(%)"].append(round(growth, 2))
        mean_data["Active Members"].append(active)
        mean_data["Retired Members"].append(retired)
        mean_data["Avg. Service"].append(service)
        mean_data["Contribution Rate"].append(cr)
        mean_data["payGo"].append(payGo)
        mean_data["Total Salary"].append(salary)

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
        os.makedirs("Graphs")
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
    eg = 1.0
    size = 25
    years = 100

    getModelData(vol, eg, size, years, saveAll=True, filename="vol_default")

    # Testing the effect of more 'generous' volatility lists
    vol = [0.08, 0.03, 0.06, 0.03, 0.08, 0.04]
    getModelData(vol, eg, size, years, saveAll=True, filename="vol_mean_plus4")

    vol = [0.12, 0.03, 0.10, 0.03, 0.12, 0.04]
    getModelData(vol, eg, size, years, saveAll=True, filename="vol_mean_plus8")

    # Testing the effect of more 'volatile' volatility lists
    vol = [0.04, 0.07, 0.02, 0.07, 0.04, 0.08]
    getModelData(vol, eg, size, years, saveAll=True, filename="vol_sd_plus4")

    vol = [0.04, 0.11, 0.02, 0.11, 0.04, 0.12]
    getModelData(vol, eg, size, years, saveAll=True, filename="vol_sd_plus8")
