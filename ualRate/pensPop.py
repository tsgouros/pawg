#!/usr/bin/env python3
from random import *
from collections import deque
import numpy as np

## TJ: Can we encapsulate the mortality tables into a class?  Read
## all the data as you have it in the __init__() method, and give us
## simple accessors that take a sex, age, and mortality class and
## return a chance of dying.  Or better yet, just tuck all this into
## the __init__() method of pensPop.
##
##


"""using openpyxl to access spreadsheets, creating dictionary of values."""
import openpyxl
from pathlib import Path
m_file = Path('..', 'mortalityTables', 'pub-2010-amount-mort-rates.xlsx')
m_wb = openpyxl.load_workbook(m_file)
pubG = m_wb['PubG-2010']
mort_data_f = {}
mort_data_m = {}
## TJ: The triple quotes thing is only for the doc strings right
## after a function declaration. Use hash marks for in-code comments.
##""" read mortality rates into male and female dictionaries"""
for i, row in enumerate(pubG.iter_rows(4, pubG.max_row)):
    if i == 0:

        mort_data_f[row[3]] = []
        mort_data_f[row[4]] = []
        mort_data_f[row[5]] = []
        mort_data_f[row[6]] = []

        mort_data_m[row[8]] = []
        mort_data_m[row[9]] = []
        mort_data_m[row[10]] = []
        mort_data_m[row[11]] = []
    else:
        mort_data_f['Employee'].append(row[3])
        mort_data_f['Healthy Retiree'].append(row[4])
        mort_data_f['Disabled Retiree'].append(row[5])
        mort_data_f['Contingent Survivor'].append(row[6])

        mort_data_m['Employee'].append(row[8])
        mort_data_m['Healthy Retiree'].append(row[9])
        mort_data_m['Disabled Retiree'].append(row[10])
        mort_data_m['Contingent Survivor'].append(row[11])


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

        self.salaryHistory = deque([salary])
        self.simulateCareerBackward()

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

    def doesMemberDie(self):
        if self.sex == "F":
            table = mort_data_f
        else:
            table = mort_data_m

        age = self.age - 18
        rate = 0

        if self.status == "active":
            rate = table['Employee'][age]
        elif self.status == "retired"
            rate = table['Healthy Retiree'][age]
        elif self.status == "deceased":
            rate = 1
        """are there more statuses to account for?"""
        ## TJ: Yes, "separated" are people who have quit or been laid off,
        ## but are still eligible for a pension.  They are equivalent
        ## in mortality risk to "active".
        ##
        ## We are ignoring "disabled" for now.

        return random.randrange(100) < (rate*100)



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




    def calculateLiability(self, discountRate):
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
            liability = self.pension * self.cola ^ (i - 1)
            ## Step 4: Apply the discount rate for each of the years to
            ## get the present value in the current year.
            liabilityPresentValue = liabilityPresentValue + (liability) / (
                self.discountrate ^ i
            )


class pensPop(object):
    def __init__(self, members=[]):
        ## A list of member objects.
        self.members = members

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

    def calculateLiability(self, discountRate):
        """Calculate the present value of the liability for all the
        members."""

        sum = 0
        for m in self.members:
            sum += m.calculateLiability(discountRate)
        return sum


##################### TESTING FUNCTIONS ######################

def testAgeOneYear():
  m1 = pensMember(20, "M", 2, 500, 2010)
  m2 = pensMember(30, "F", 2, 500, 2010)
  m3 = pensMember(50, "M", 2, 500, 2010)

  members = [m1,m2,m3]
  ages = [20,30,50]

  years = random.randint(5,15)

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



if __name__ == "__main__":
  testAgeOneYear()



  print("hello")

  counter = 0
  for i in range(100000):
      andy = pensMember(62, "M", 15, 1000, 2005)
      if andy.doesMemberRetire():
          counter += 1

  print(counter)
