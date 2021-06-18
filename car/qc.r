## Pull in the CAR calculation apparatus.
source("car.r")

## System-specific information.
##
## This example uses valuation data from the Queen Creek (AZ) Fire
## Department, 4/21
##
## These functions (doIseparate and doIretire) give the probability of
## separation or retirement, given the age and service years of the
## employee.
doesMemberSeparate <- function(age, service, status, tier=1) {
    ## If this is not currently an active employee, get out.
    if (status != "active") return(status);

    rates <- c(0.070, 0.045, 0.037, 0.030, 0.025,
               0.017, 0.017, 0.017, 0.017, 0.015,
               0.011, 0.007, 0.007, 0.007, 0.006,
               0.005, 0.005, 0.004, 0.004, 0.004);

    service <- min(service, 20);
    if (runif(1) < rates[service]) status <- "separated";

    return(status);
}

doesMemberRetire <- function(age, service, status, tier=1, verbose=FALSE) {
    ## If already retired, get out.
    if ((status == "retired") | (status == "deceased")) return(status);

    ## The service years on input refer to years that have begun, but
    ## not necessarily completed.  We want completed years.  This is
    ## related to the model's approximation that events happen on the
    ## transition from one year to the next, as opposed to the real
    ## world, where events happen whenever they feel like it.
    completedYears <- service - 1;
    if (completedYears < 10) return(status);

    if (verbose) cat(" --retire? age: ", age, ", service: ", service,
                     ", tier: ", tier, " begin: ", status, "...",
                     sep="");

    if (tier == 1) {

        if ((age >= 62) && (completedYears <= 20)) {
            if ( ((age == 62) && (runif(1) > 0.4)) |
                 (((age > 62) && (age < 70)) && (runif(1) > 0.5)) |
                 (age >= 70) ) {
                status <- "retired";
            }
        } else if (completedYears >= 20) {
            rates <- c(0.14, 0.14, 0.07, 0.07, 0.07,
                       0.22, 0.26, 0.19, 0.32, 0.30,
                       0.30, 0.30, 0.55, 0.55, 1.00);

            completedYears <- min(completedYears, 34);
            ## Roll the dice.
            if (runif(1) < rates[completedYears - 19]) status <- "retired";
        }
    } else if (tier == 2) {
        if ((age >= 53) && (completedYears >= 15)) {
            rates <- c(0.22, 0.26, 0.19, 0.32, 0.30,
                       0.30, 0.30, 0.55, 0.55, 0.55,
                       0.55, 1.00);
            if (runif(1) < rates[age - 52]) status <- "retired";
        }
    } else if (tier == 3) {
        if ((age >= 55) && (completedYears >= 15)) {
            rates <- c(0.19, 0.32, 0.30, 0.30, 0.30,
                       0.55, 0.55, 0.55, 0.55, 1.00);
            if (runif(1) < rates[age - 54]) status <- "retired";
        }
    } else if (tier == 0) {
        ## There are "tier 0" people in QC Fire.  They are already
        ## retired, and receiving a pension.  There are employer
        ## contributions happening on their behalf, but they are otherwise
        ## irrelevant to the investigation of the CAR.  We age them and
        ## retire them, and will try to avoid counting them later.
        if (age > 65) status <- "retired";
    }

    if (verbose) cat("result: ", status, "\n", sep="");

    return(status);
}

doesMemberBecomeDisabled <- function(age, sex, service, status,
                                     mortClass="General", tier=1) {
    ## If already retired or disabled, don't change anything and get out.
    if ((status == "retired") | (status == "deceased") |
        (status == "disabled") ) return(status);

    ## These are rates for ages 20-25, 25-30, 30-35, etc
    rates <- c(0.0003, 0.0003, 0.0004, 0.0009, 0.0017, 0.0017, 0.0043, 0.01);

    ## Select the appropriate rate.
    irate <- min(length(rates), ceiling((age - 20)/5));

    ## Roll the dice.
    if (runif(1) < rates[irate]) status <- "disabled";

    return(status);
}

## The assumed salary increment, from the table of merit increases in
## each valuation report.
projectSalaryDelta <- function(year, age, salary, service=1, tier=1) {

    if (age < 25) {
        out <- 1.075;
    } else if ((age >= 25) && (age < 30)) {
        out <- 1.0735;
    } else if ((age >= 30) && (age < 35)) {
        out <- 1.0674;
    } else if ((age >= 35) && (age < 40)) {
        out <- 1.0556;
    } else if ((age >= 40) && (age < 45)) {
        out <- 1.0446;
    } else if ((age >= 45) && (age < 50)) {
        out <- 1.0374;
    } else if (age >= 50) {
        out <- 1.035;
    }

    ## Tier 3 salaries are limited.  ??
##    if (tier == 3) {
##        iyear <- max(year, 2020);
##        limit <- 110000 * 1.02^(year - 2020);
##        out <- min(out, (out - limit) / limit);
##    }

    return(out);
}

projectPension <- function(salaryHistory, tier=1) {

    service <- sum(salaryHistory$salary > 0);

    ## Calculate the base salary from which to calculate the pension.
    if (tier == 1) {
        # Find the maximum 3-year period.
        s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);
        threeYears <- c(s, 0, 0) + c(0, s, 0) + c(0, 0, s);
        avgSalary <- max(threeYears) / 3.0;

        startingPension <- 0.5 * avgSalary;

        if ((service >= 15) && (service < 20)) {
            startingPension <- startingPension * (1 - ((20 - service) * 0.04));
        } else if (service >= 25) {
            startingPension <- startingPension * (1 + ((service - 20) * 0.025));
        } else { ## 20-25
            startingPension <- startingPension * (1 + ((service - 20) * 0.02));
        }

        startingPension <- min(0.8 * avgSalary, startingPension);
    } else if (tier == 2) {
        # Find the maximum 5-year period.
        s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);

        fiveYears <-
            c(s, 0, 0, 0, 0) +
            c(0, s, 0, 0, 0) +
            c(0, 0, s, 0, 0) +
            c(0, 0, 0, s, 0) +
            c(0, 0, 0, 0, s);
        avgSalary <- max(fiveYears) / 5.0;

        if ((service >= 15) && (service < 17)) {
            benefitMultiplier <- 0.015;
        } else if ((service >= 17) && (service < 19)) {
            benefitMultiplier <- 0.0175;
        } else if ((service >= 19) && (service < 22)) {
            benefitMultiplier <- 0.02;
        } else if ((service >= 22) && (service < 25)) {
            benefitMultiplier <- 0.0225;
        } else if (service >= 25) {
            benefitMultiplier <- 0.025;
        } else { print(as.data.frame(salaryHistory)); cat("tier:", tier, "service:", service, "\n");}

        startingPension <- avgSalary * (service * benefitMultiplier);

        startingPension <- min(0.8 * avgSalary, startingPension);
    } else if (tier == 3) {
        # Find the maximum 5-year period.
        s <- tail(salaryHistory %>%
                  mutate(X=pmin(salary, 110000 * (1.02^(pmax(0, year-2020))))) %>%
                  filter(X > 0) %>%
                  select(X) %>%
                  unlist(use.names=FALSE), 15);

        fiveYears <-
            c(s, 0, 0, 0, 0) +
            c(0, s, 0, 0, 0) +
            c(0, 0, s, 0, 0) +
            c(0, 0, 0, s, 0) +
            c(0, 0, 0, 0, s);
        avgSalary <- max(fiveYears) / 5.0;

        if ((service >= 15) && (service < 17)) {
            benefitMultiplier <- 0.015;
        } else if ((service >= 17) && (service < 19)) {
            benefitMultiplier <- 0.0175;
        } else if ((service >= 19) && (service < 22)) {
            benefitMultiplier <- 0.02;
        } else if ((service >= 22) && (service < 25)) {
            benefitMultiplier <- 0.0225;
        } else if (service >= 25) {
            benefitMultiplier <- 0.025;
        } else {
            ## This is an error condition, and a fault will follow.
            print(as.data.frame(salaryHistory));
            cat("tier:", tier, "service:", service, "\n");
        }

        startingPension <- avgSalary * (service * benefitMultiplier);

        startingPension <- min(0.8 * avgSalary, startingPension);
    } else if (tier == 0) {
        ## The tier 0 people can retire again but they don't get
        ## another pension.
        startingPension <- 0.0;
    }

    cola <- 1.0175;

    ## If this person never retired, send them away without a pension.
    if (!("retired" %in% salaryHistory$status))
        return(salaryHistory %>% mutate(pension = 0));

    retireYear <- as.numeric(salaryHistory %>%
                             filter(status=="retired") %>%
                             summarize(retireYear=min(year)));

    return(salaryHistory %>%
           mutate(pension = ifelse(status == "retired",
                                   startingPension * cola^(year - retireYear),
                                   0)));

}

## Accepts a salary history tibble and adds a column for the estimated
## premiums paid into the system for this employee for each year.
## (Combined employer and employee share.)
projectPremiums <- function(salaryHistory) {
    return(salaryHistory %>%
           mutate(premium = ifelse(premium==0, salary * .2265, premium)))
}



## Let's model the Queen Creek fire department.  This data is from the
## valuation report, the member population table.  This function
## produces a list of members, and the function itself can be fed to
## the runModel functions in car.r.
qcModel <- function() {
    qcFire <- genEmployees(1, ageRange=c(20,24), servRange=c(0,4),
                           avgSalary=71362);
    qcFire <- genEmployees(5, ageRange=c(25,29), servRange=c(0,4),
                           avgSalary=73683, members=qcFire);
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(5,9),
                           avgSalary=73683, members=qcFire);

    qcFire <- genEmployees(6, ageRange=c(30,34), servRange=c(0,4),
                           avgSalary=80728, members=qcFire);
    qcFire <- genEmployees(1, ageRange=c(30,34), servRange=c(5,9),
                           avgSalary=84728, members=qcFire);

    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(0,4),
                           avgSalary=84728, members=qcFire);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9),
                           avgSalary=94728, members=qcFire);
    qcFire <- genEmployees(7, ageRange=c(35,39), servRange=c(10,14),
                           avgSalary=115728, members=qcFire);

    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4),
                           avgSalary=92728, members=qcFire);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(5,9),
                           avgSalary=94728, members=qcFire);
    qcFire <- genEmployees(10, ageRange=c(40,44), servRange=c(10,14),
                           avgSalary=112728, members=qcFire);

    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(5,9),
                           avgSalary=94730, members=qcFire);
    qcFire <- genEmployees(4, ageRange=c(45,49), servRange=c(10,14),
                           avgSalary=110730, members=qcFire);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(15,19),
                           avgSalary=140130, members=qcFire);

    qcFire <- genEmployees(2, ageRange=c(45,49), servRange=c(10,14),
                           avgSalary=120730, members=qcFire);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(15,19),
                           avgSalary=124730, members=qcFire);

    return(qcFire);
}

#qcModelOutput <- runModel(qcModel, N=1, verbose=TRUE);
#qcModelPlot <- plotModelOut(qcModelOutput)
