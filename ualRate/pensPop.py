from random import *
from collections import deque
import numpy as np

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
        ## A list of member objects.
        self.members = members

    def simulateMembers(self, N, ageRange, serviceRange, avgSalary,
                        sex="*", mortalityClass="General", tier="1"):
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
            out.append(pensMember(randint(ageRange[0], ageRange[1]),
                                  chosenSex,
                                  randint(serviceRange[0], serviceRange[1]),
                                  np.random.normal(avgSalary, avgSalary/15),
                                  2021,
                                  mortalityClass=mortalityClass,
                                  tier=tier))

        return(out)

    def simulatePopulation(self):

        """Generates a collection of plan members."""
        self.members.extend(self.simulateMembers(1, ageRange=(20,24),
                                                 serviceRange=(0,4),
                                                 avgSalary=71362))
        self.members.extend(self.simulateMembers(5, ageRange=(25,29),
                                                 serviceRange=(0,4),
                                                 avgSalary=73683))

        self.members.extend(self.simulateMembers(1, ageRange=(25,29),
                                                 serviceRange=(5,9),
                                                 avgSalary=73683))

        self.members.extend(self.simulateMembers(6, ageRange=(30,34),
                                                 serviceRange=(0,4),
                                                 avgSalary=80728))
        self.members.extend(self.simulateMembers(1, ageRange=(30,34),
                                                 serviceRange=(5,9),
                                                 avgSalary=84728))

        self.members.extend(self.simulateMembers(2, ageRange=(35,39),
                                                 serviceRange=(0,4),
                                                 avgSalary=84728))
        self.members.extend(self.simulateMembers(1, ageRange=(35,39),
                                                 serviceRange=(5,9),
                                                 avgSalary=94728))
        self.members.extend(self.simulateMembers(7, ageRange=(35,39),
                                                 serviceRange=(10,14),
                                                 avgSalary=115728))

        self.members.extend(self.simulateMembers(2, ageRange=(40,44),
                                                 serviceRange=(0,4),
                                                 avgSalary=92728))
        self.members.extend(self.simulateMembers(2, ageRange=(40,44),
                                                 serviceRange=(5,9),
                                                 avgSalary=94728))
        self.members.extend(self.simulateMembers(10, ageRange=(40,44),
                                                 serviceRange=(10,14),
                                                 avgSalary=112728))

        self.members.extend(self.simulateMembers(1, ageRange=(45,49),
                                                 serviceRange=(5,9),
                                                 avgSalary=94730))
        self.members.extend(self.simulateMembers(4, ageRange=(45,49),
                                                 serviceRange=(10,14),
                                                 avgSalary=110730))
        self.members.extend(self.simulateMembers(1, ageRange=(45,49),
                                                 serviceRange=(15,19),
                                                 avgSalary=140130))

        self.members.extend(self.simulateMembers(2, ageRange=(45,49),
                                                 serviceRange=(10,14),
                                                 avgSalary=120730))
        self.members.extend(self.simulateMembers(1, ageRange=(45,49),
                                                 serviceRange=(15,19),
                                                 avgSalary=124730))



