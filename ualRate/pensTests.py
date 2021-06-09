#!/usr/bin/env python3

from pensPop import pensMember
import random

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


