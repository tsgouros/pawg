#!/usr/bin/env python3
from random import *
from collections import deque
import numpy as np
import pandas as pd
import openpyxl
from pathlib import Path

class pensMort: 
    def __init__(self, sex):
        self.sex = sex
        ## ET: import mortality table
        m_file = Path('..', 'mortalityTables', 'pub-2010-amount-mort-rates.xlsx')
        m_wb = openpyxl.load_workbook(m_file)
        pubG = m_wb['PubG-2010']

        ## ET: convert excel into a dataframe
        df_pubG = pd.DataFrame(pubG.values)

        ## ET: clean data frame (i.e. remove extra empty columns, reset index)
        df_pubG = df_pubG.drop(df_pubG.index[range(3)])
        df_pubG = df_pubG.drop(labels = [0,2], axis=1).reset_index()


        self.data = df_pubG
        self.mortTable = self.getMortalityTables()

    def getMortalityTables(self):
        ## ET: convert data frame into dictionary
        ## index: age (number)
        ## key: mortality rate of employee(0 position), healthy retiree(1), disabled retiree(2), contingent survivor(3)(list of numbers)
        ## split into two dictionaries, one for female, one for male
        temp ={}
        if self.sex == "F":
            df_pubG = self.data.drop(self.data.iloc[:, 6:20], axis=1)
            df_pubG = df_pubG.rename(columns=df_pubG.iloc[1])
            
            df_pubG[['Healthy Retiree', "Disabled Retiree", "Contingent Survivor"]] = df_pubG[['Healthy Retiree', "Disabled Retiree", "Contingent Survivor"]].fillna(value=0)
    
            for index, row in df_pubG.iterrows():
                temp[row["Age"]] = [row["Employee"], row["Healthy Retiree"], row["Disabled Retiree"], row["Contingent Survivor"]]
            return temp


        elif self.sex == "M":
            df_pubG = self.data.iloc[:, np.r_[1, 7:11]].reset_index()
            df_pubG = df_pubG.rename(columns=df_pubG.iloc[1])
            df_pubG[['Healthy Retiree', "Disabled Retiree", "Contingent Survivor"]] = df_pubG[['Healthy Retiree', "Disabled Retiree", "Contingent Survivor"]].fillna(value=0)
            for index, row in df_pubG.iterrows():
                temp[row["Age"]] = [row["Employee"], row["Healthy Retiree"], row["Disabled Retiree"], row["Contingent Survivor"]]
            return temp

temp_f =  pensMort("F").mortTable
temp_m = pensMort("M").mortTable

class pensMember(object):
    def __init__(
        self,
        age,
        sex,
        service,
        salary,
        currentYear,
        mortalityClass="General",
        tier="1",
        status="active",
        id="*",
    ):
        self.age = age
        self.sex = sex
        self.salary = salary
        self.mortalityClass = mortalityClass
        self.tier = tier
        self.service = service
        self.status = status
        self.id = id
        self.currentYear = currentYear
        self.birthYear = currentYear - age
        self.retireYear = 0
        self.hireYear = currentYear - service
        self.pension = 0
        self.cola = 1.025
        self.discountrate = 1.07
        self.mortDict = 0
        self.salaryHistory = deque([salary])
        self.simulateCareerBackward()
        self.getMortSex()

        if self.id == "*":
            self.id = "%0.6x" % randint(1, pow(16, 6))

    def simulateCareerBackward(self):
        """Takes the current salary and service and projects a salary
        history backwards using the salaryGrowth value."""

        if self.service > 1:
            for iyear in range(
                self.currentYear - 1, self.currentYear - self.service, -1
            ):
                self.salaryHistory.appendleft(
                    self.salaryHistory[0] / self.projectSalaryDelta()
                )

        ## That first year was probably not a complete year.  Roll
        ## some dice and pick a random fraction of the year.
        self.salaryHistory[0] = random() * self.salaryHistory[0]

    def projectSalaryDelta(self):
        """Uses the age, service, and tier to project the change in pay
        between one year and the next."""

        ## These are typical values scrounged from a valuation report
        ## for a fire department in Arizona.  (For which service and
        ## tier are not relevant.)
        if self.age < 25:
            out = 1.075
        elif self.age >= 25 and self.age < 30:
            out = 1.0735
        elif self.age >= 30 and self.age < 35:
            out = 1.0674
        elif self.age >= 35 and self.age < 40:
            out = 1.0556
        elif self.age >= 40 and self.age < 45:
            out = 1.0446
        elif self.age >= 45 and self.age < 50:
            out = 1.0374
        elif self.age >= 50:
            out = 1.035

        return out

    def applyPensionCOLA(self):
        self.pension *= self.cola

    def doesMemberSeparate(self):
        """TBD: Does member leave employment?"""
        if self.status != "active":
            return False
        rates = [
            0.070,
            0.045,
            0.037,
            0.030,
            0.025,
            0.017,
            0.017,
            0.017,
            0.017,
            0.015,
            0.011,
            0.007,
            0.007,
            0.007,
            0.006,
            0.005,
            0.005,
            0.004,
            0.004,
            0.004,
        ]

        service = min(self.service, 20)
        if random() < rates[service - 1]:
            return True
        else:
            return False

    def doesMemberRetire(self):
        """TBD: What it sounds like"""
        if (
            self.status == "retired"
            or self.status == "deceased"
            or self.status == "disabled"
        ):
            return False

        if self.age >= 62 and self.service >= 15:
            if (
                (self.age == 62 and random() > 0.4)
                or (self.age > 62 and self.age < 70 and random() > 0.5)
                or (self.age >= 70)
            ):
                return True
            elif self.service >= 20:
                rates = [
                    0.14,
                    0.14,
                    0.07,
                    0.07,
                    0.07,
                    0.22,
                    0.26,
                    0.19,
                    0.32,
                    0.30,
                    0.30,
                    0.30,
                    0.55,
                    0.55,
                    1.00,
                ]
                service = min(self.service, 34)
                if random() < rates[service - 20]:
                    return True
        return False
    def getMortSex(self):
        """determine with mortality dictionary to use"""
        if self.sex == "F":
            self.mortDict = temp_f
        elif self.sex == "M":
            self.mortDict = temp_m

    def doesMemberDie(self):
        """TBD: Check if member dies"""

        if self.status == "active": 
            if random() < self.mortDict[self.age][0]:
                return True

        ## ET: Assuming every retiree is healthy for now 
        elif self.status == "retired": 
            if random() < self.mortDict[self.age][1]:
                return True
        elif self.status == "deceased":
            return True




    def ageOneYear(self):
        """TBD: Age a year, get a raise, decide whether to separate or retire.
        Maybe die.  Change status and salary accordingly."""

        #Should members' salary and service increase before or after separation/retire checks?
        #If a person retires, should their service and salary not increase for that year? Or would it increase for the year and then stop?

        ## ET: Check for a change of status, then adjust the salary if
        ## it's still relevant.


        self.currentYear += 1
        if self.status != "deceased":
            self.age += 1

        if self.status == "active":
            self.service += 1
            self.salary *= self.projectSalaryDelta()
            if self.doesMemberSeparate():
                self.status = "separated"
                self.salary = 0
            elif self.doesMemberRetire():
                self.status = "retired"
                self.retireYear = self.currentYear
                self.salary = 0
                self.pension = self.salaryHistory[-1] * 0.55

        if self.doesMemberDie():
            self.status = "deceased"
            self.salary = 0


    def calculateLiability(self):
        """TBD: Estimate accrued liability for this member."""

        ## Step 1: if person is active, estimate year of retirement

        yearsUntilRetirement = 0
        if self.status == "active":
            while self.status == "active":
                self.ageOneYear()
                yearsUntilRetirement += 1

            yearOfRetirement = self.currentYear

        ## Step 2: Estimate how many years of retirement this person
        ## is likely to enjoy. (Or how many years left, for members
        ## who are already retired.)

        yearsOfRetirement = 0
        if self.status == "retired":
            while self.status != "deceased":
                yearsOfRetirement += 1
                self.ageOneYear()

        liabilityPresentValue = 0
        for i in range(yearsOfRetirement):
            ## Step 3: estimate retirement benefit earned
            liability = self.pension * self.cola ** (i - 1)

            ## Step 4: Apply the discount rate for each of the years to
            ## get the present value in the current year.
            liabilityPresentValue = liabilityPresentValue + (liability) / (
                self.discountrate ** i
            )


class pensPop(object):
    def __init__(self, members=[]):
        ## A list of member objects.
        self.members = members
        self.startingSalary = 50000
        self.avgAge = 30

    def simulateMembers(
        self,
        N,
        ageRange,
        serviceRange,
        avgSalary,
        sex="*",
        mortalityClass="General",
        tier="1",
    ):
        """Generates N member objects with randomly distributed ages and
        services.  Sex is random, too, unless it's specified."""
        out = []
        for i in range(0, N):
            if sex == "*":
                if random() > 0.5:
                    chosenSex = "M"
                else:
                    chosenSex = "F"
            else:
                chosenSex = sex
            out.append(
                pensMember(
                    randint(ageRange[0], ageRange[1]),
                    chosenSex,
                    randint(serviceRange[0], serviceRange[1]),
                    np.random.normal(avgSalary, avgSalary / 15),
                    2021,
                    mortalityClass=mortalityClass,
                    tier=tier,
                )
            )

        return out

    def simulatePopulation(self):
        """Generates a collection of plan members.  This can be taken from
        the table of population demographics in any valuation report."""

        self.members.extend(
            self.simulateMembers(
                1, ageRange=(20, 24), serviceRange=(0, 4), avgSalary=71362
            )
        )
        self.members.extend(
            self.simulateMembers(
                5, ageRange=(25, 29), serviceRange=(0, 4), avgSalary=73683
            )
        )

        self.members.extend(
            self.simulateMembers(
                1, ageRange=(25, 29), serviceRange=(5, 9), avgSalary=73683
            )
        )

        self.members.extend(
            self.simulateMembers(
                6, ageRange=(30, 34), serviceRange=(0, 4), avgSalary=80728
            )
        )
        self.members.extend(
            self.simulateMembers(
                1, ageRange=(30, 34), serviceRange=(5, 9), avgSalary=84728
            )
        )

        self.members.extend(
            self.simulateMembers(
                2, ageRange=(35, 39), serviceRange=(0, 4), avgSalary=84728
            )
        )
        self.members.extend(
            self.simulateMembers(
                1, ageRange=(35, 39), serviceRange=(5, 9), avgSalary=94728
            )
        )
        self.members.extend(
            self.simulateMembers(
                7, ageRange=(35, 39), serviceRange=(10, 14), avgSalary=115728
            )
        )

        self.members.extend(
            self.simulateMembers(
                2, ageRange=(40, 44), serviceRange=(0, 4), avgSalary=92728
            )
        )
        self.members.extend(
            self.simulateMembers(
                2, ageRange=(40, 44), serviceRange=(5, 9), avgSalary=94728
            )
        )
        self.members.extend(
            self.simulateMembers(
                10, ageRange=(40, 44), serviceRange=(10, 14), avgSalary=112728
            )
        )

        self.members.extend(
            self.simulateMembers(
                1, ageRange=(45, 49), serviceRange=(5, 9), avgSalary=94730
            )
        )
        self.members.extend(
            self.simulateMembers(
                4, ageRange=(45, 49), serviceRange=(10, 14), avgSalary=110730
            )
        )
        self.members.extend(
            self.simulateMembers(
                1, ageRange=(45, 49), serviceRange=(15, 19), avgSalary=140130
            )
        )

        self.members.extend(
            self.simulateMembers(
                2, ageRange=(45, 49), serviceRange=(10, 14), avgSalary=120730
            )
        )
        self.members.extend(
            self.simulateMembers(
                1, ageRange=(45, 49), serviceRange=(15, 19), avgSalary=124730
            )
        )

    def advanceOneYear(self):
        """TBD: Advance the population by a year -- increase everyone's age
        and service, give them raises, retire some people, others die,
        or separate. """
        for member in self.members:
            member.ageOneYear()



    def hireReplacements(self, pct=1.0):
        """TBD: Replace retired and separated workers to maintain headcount.
        If pct is less than one, only replace that proportion of the retired
        and separated."""
        counter = 0
        for member in self.members: 
            if member.status == "retired" or member.status == "separated":
                counter +=1
        if random() < pct: 
            self.members.extend(self.simulateMembers(counter, ageRange=(self.avgAge-5, self.avgAge+5), serviceRange=(0, 1), avgSalary=self.startingSalary))
        else: 
            self.members.extend(self.simulateMembers(counter * pct, ageRange=(self.avgAge-5, self.avgAge+5), serviceRange=(0, 1), avgSalary=self.startingSalary))

        


    def addNewMembers(
        self,
        N,
        ageRange,
        serviceRange,
        avgSalary,
        sex="*",
        mortalityClass="General",
        tier="1",
    ):
        """TBD: New hires who aren't replacements."""

    def layoffMembers(self, N):
        """TBD: Remove given number of members.  Favor removal of the
        younger members."""

    def calculateTotalLiability(self):
        """Calculate the present value of the liability, aka normal cost, for all the
        members."""

        sum = 0
        for m in self.members:
            sum += m.calculateLiability()
        return sum



##################### TESTING FUNCTIONS ######################



if __name__ == "__main__":
    def testdoesMemberRetire():
        counter = 0
        for i in range(100000):
            andy = pensMember(62, "M", 15, 1000, 2005)
            if andy.doesMemberRetire():
                counter += 1
        print(counter) ## ET: should be around 60000


    def testdoesMemberSeparate():
        counter = 0
        for i in range(100000):
            andy = pensMember(62, "M", 15, 1000, 2005)
            if andy.doesMemberSeparate():
                counter += 1
        print(counter) ## ET: should be around 600

    def testdoesMemberDie():
        counter = 0
        andy = pensMember(18, "M", 15, 1000, 2005)
        for i in range(100000):
            if andy.doesMemberDie():
                counter += 1 
        print(counter) ## ET: should be around 30 
    
    def testcalculateLiability():
        andy = pensMember(62, "M", 15, 1000, 2005)
        print(andy.calculateLiability())


    def testAgeOneYear():
        m1 = pensMember(20, "M", 2, 500, 2010)
        m2 = pensMember(30, "F", 2, 500, 2010)
        m3 = pensMember(50, "M", 2, 500, 2010)

        members = [m1,m2,m3]
        ages = [20,30,50]

        years = randint(5,15)

        for i in range(years):
            for m in members:
                ogAge = m.age
                ogStatus = m.status
                ogService = m.service
                m.ageOneYear()


                if ogStatus == "deceased" and m.age != ogAge:
                        print("someone aged while dead")
                if ogStatus != "active":
                    if m.service != ogService:
                        print("service increased for inactive member")
                    if m.salary != 0:
                        print("inactive member has a salary")


        for m in members:
            if m.status == "active":
                if (m.age - years) not in ages:
                    print("someone's age is wrong")
                elif m.service - years != 2:
                    print ("someone's service is wrong")
                elif m.currentYear != (2010+years):
                    print("wrong year")

    

    testdoesMemberRetire()
    testdoesMemberSeparate()
    testdoesMemberDie()
    testcalculateLiability()
    testAgeOneYear()
