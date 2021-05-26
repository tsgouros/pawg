from random import *
from collections import deque

class pensMember(object):
    def __init__(self, age, sex, service, salary, currentYear,
                 mortalityClass="General",
                 tier="1", status="active",
                 id="*"):
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

        self.salaryHistory = deque([ salary ])
        self.simulateCareerBackward()

        if self.id == "*":
            self.id = "%0.6x" % randint(1,pow(16,6))

    def simulateCareerBackward(self):
        """Takes the current salary and service and projects a salary
        history backwards using the salaryGrowth value."""

        if self.service > 1:
            for iyear in range(self.currentYear - 1,
                               self.currentYear - self.service, -1):
                self.salaryHistory.appendleft(self.salaryHistory[0]/self.projectSalaryDelta())

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



class pensPop(object):
    def __init__(self, members=[]):
        self.members = members

    def simulatePopulation(self, N):
        """Generates a collection of plan members."""
        for i in range(0, N):
            self.members.append(pensMember(randint(25,30),
                                           "m",
                                           randint(1,7),
                                           randint(40000,50000),
                                           2021))




