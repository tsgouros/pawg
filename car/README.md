This directory contains some R code for estimating the "Contractual
Accrual Rate" for a pension plan.  The code functions by modeling the
population of a pension plan, and simulating the complete salary and
retirement benefit history for each individual in it.  This sequence
of payments into and out of the pension system constitutes the cash
flow for each individual, and from that, a collective cash flow
estimate can be derived and the corresponding accrual rate.

Here are the pieces:

mortality.r -- Incorporates the pub2010 mortality tables into some R
functions for convenience.

newton.r -- Implements Newton's method for finding the accrual rate
necessary to make a given cash flow net out to zero.

car.r -- The classes and associated functions necessary to model the
system and make the estimate.  It contains some functions meant to be
overridden by specific functions tailored for the benefit structure
and promotion policies of a given employer.

qc.r -- An example of applying the system to a specific pension plan.
