library(tidyverse)

## Given a cash flow, find the interest rate that will allow the
## deposits to compound to a future value.  cashFlow is a tibble
## with a 'year' column and a 'flow' column.

source("newton.r")
source("mortality.r")

## Every year, premiums are collected for everyone.  Imagine a big
## matrix of premium payments, where each row represents a year of
## premiums, and each column is all the actives who will retire in a
## class.
##
## year
##   1   P11   P12   P13   P14   P15 ...
##   2     0   P22   P23   P24   P25 ...
##   3     0     0   P33   P34   P35 ...
##   4     0     0     0   P44   P45 ...
##   5     0     0     0     0   P55 ...
##   6     .     .     .     .    .  ...
##   7     .     .     .     .    .  ...
##   8     .     .     .     .    .  ...
##
## The "1" class (first column) retires at the end of year 1, so pays
## no premium for year 2.  The 2 class retires at the end of year 2,
## and so on.
##
##

##### SYSTEM SPECIFIC DEFINITIONS

## We define some things that are specific to the pension system under
## examination.


## These functions (doIseparate and doIretire) give the probability of
## separation or retirement, given the age and service years of the
## employee.
doesMemberSeparate <- function(age, service, status, class="NONE") {
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

doesMemberRetire <- function(age, service, status, class="NONE") {
    ## If already retired, get out.
    if ((status == "retired") | (status == "deceased")) return(status);

    if ((age >= 62) & (service >= 15)) {
        if ( ((age == 62) & (runif(1) > 0.4)) |
             (((age > 62) & (age < 70)) & (runif(1) > 0.5)) |
             (age >= 70) ) {
            status <- "retired";
        }
    } else if (service >= 20) {
            rates <- c(0.14, 0.14, 0.07, 0.07, 0.07,
                       0.22, 0.26, 0.19, 0.32, 0.30,
                       0.30, 0.30, 0.55, 0.55, 1.00);

            service <- min(service, 34);
            if (runif(1) < rates[service - 19]) status <- "retired";
    }

    return(status);
}

## Defines the function 'doesMemberDie' using the pubs 2010 mortality
## tables in the mortalityTables subdirectory.
source("mortality.r")
##doesMemberDie <- function(age, status, class="NONE") {
##    if (status == "retired") return("deceased");
##    return(status);
##}

## The assumed salary increment, from the table of merit increases in
## each valuation report.  Class refers to any kind of subdivision
## among the members.
projectSalary <- function(age, service=1, class="NONE") {

    if (age < 25) {
        out <- 1.075;
    } else if ((age >= 25) & (age < 30)) {
        out <- 1.0735;
    } else if ((age >= 30) & (age < 35)) {
        out <- 1.0674;
    } else if ((age >= 35) & (age < 40)) {
        out <- 1.0556;
    } else if ((age >= 40) & (age < 45)) {
        out <- 1.0446;
    } else if ((age >= 45) & (age < 50)) {
        out <- 1.0374;
    } else if (age >= 50) {
        out <- 1.035;
    }

    return(out);
}

projectPension <- function(salaryHistory) {

    startingPension <- max(salaryHistory$salary) * 0.55;
    cola <- 1.02;

    ## This shouldn't be true ever, but if this person never retired,
    ## send them away without a pension.
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
           mutate(premium = salary * .2265))
}



## For a given year, uses an age, years of service, and salary
## history, to project a typical career forward to separation or
## retirement, and backward to the initial hire.  Returns a tibble
## with salary figures for each working year, and a status column for
## active, separated, or retired.
projectCareer <- function(year, age, service, salary, class="NONE", verbose=FALSE) {

    salaries <- c(salary);
    ages <- c(age);
    services <- c(service);
    statuses <- c("active");
    years <- c(year);

    ## March backward to the year of initial hire.
    if (service > 1) {
        for (iyear in seq(from=year - 1, to=year - service + 1)) {
            ## cat("calculating for", iyear, "\n");
            ages <- c(ages, age - (year - iyear));
            services <- c(services, service - (year - iyear));
            salaries <- c(salaries,
                          tail(salaries, 1)/projectSalary(age - (year - iyear)));
            statuses <- c(statuses, "active");
            years <- c(years, iyear);
        }
    }

    ## Reverse the data.
    ord <- order(years);
    salaries <- salaries[ord];
    statuses <- statuses[ord];
    years <- years[ord];
    ages <- ages[ord];
    services <- services[ord];

    ## Now march forward through a simulated career.  Stop when you
    ## hit "deceased."
    currentStatus <- "active";
    currentService <- service + 1;
    for (iyear in seq(from = year + 1, to = year + (110 - age))) {

        testAge <- age - (year - iyear);

        if (verbose) cat (iyear, ": At age ", testAge,
                          ", start ", currentStatus, sep="");

        ## Test for transitions.
        currentStatus <-
            doesMemberDie(testAge, "male", currentStatus);
        currentStatus <-
            doesMemberSeparate(testAge, currentService, currentStatus);
        currentStatus <-
            doesMemberRetire(testAge, currentService, currentStatus);

        if (verbose) cat (", end ", currentStatus, ".\n", sep="");

        salaries <- c(salaries,
                      ifelse(currentStatus == "active",
                             tail(salaries, 1) * projectSalary(age-(year-iyear),
                                                               class),
                             0));
        ages <- c(ages, testAge);
        services <- c(services, currentService);
        statuses <- c(statuses, currentStatus);
        years <- c(years, iyear);

        if (currentStatus == "deceased") break;

        ## Add a service year if still active.  Note that the ending
        ## total of service years will be one year too large.  This is
        ## because we're dealing with integer years and the
        ## transitions happen *during* a year.
        if (currentStatus == "active") currentService <- currentService + 1;
    }

    return(tibble(year=years,
                  salary=salaries,
                  age=ages,
                  service=services,
                  status=factor(statuses,
                                levels=c("active", "separated",
                                         "retired", "deceased"))));
}

## Here's an object for a member, initialized for some specific
## year.  The inputs are ages and years of service because that's what
## is published in the pension report tables.
member <- function(age=0, service=0, salary=0,
                   currentYear=2018, birthYear=0,
                   hireYear=0, sepYear=0, retireYear=0,
                   status="active", verbose=FALSE) {

    if ((birthYear == 0) & (age != 0)) {
        birthYear <- currentYear - age;
    } else {
        age <- currentYear - birthYear ;
    }
    if (birthYear == currentYear)
        stop("Must specify an age or a birth year.\n");

    if (hireYear == 0) {
        hireYear <- currentYear - service;
    } else {
        service <- currentYear - hireYear;
    }

    ## Generate an entire career's worth of salary history.
    salaryHistory <- projectCareer(currentYear, age, service, salary);

    ## Add the premiums paid into the system.
    salaryHistory <- projectPremiums(salaryHistory);

    ## If this member gets to retire, estimate pension.
    if ("retired" %in% salaryHistory$status) {
        salaryHistory <- projectPension(salaryHistory);
        retireYear <- as.numeric(salaryHistory %>%
            filter(status=="retired") %>% summarize(retireYear=min(year)));
    } else {
        retireYear <- NA;
    }

    if ("separated" %in% salaryHistory$status) {
        sepYear <- as.numeric(salaryHistory %>%
            filter(status=="separated") %>% summarize(sepYear=min(year)));
    } else {
        sepYear <- NA;
    }

    ## Estimate CAR for this employee.
    if ("retired" %in% salaryHistory$status) {
        car <- findRate(salaryHistory %>% mutate(netFlow = premium - pension),
                        flowName="netFlow", verbose=verbose);
    } else {
        car <- NA;
    }

    ## Generate a random six-hex-digit id number, and return the rest
    ## in a list.
    out <- list(id=format(as.hexmode(round(runif(1) * 16777216)),width=6),
                birthYear=birthYear,
                hireYear=hireYear,
                sepYear=sepYear,
                retireYear=retireYear,
                car=car,
                salaryHistory=salaryHistory);
    attr(out, "class") <- "member";

    return(out);
}

#change this to format / print.
format.member <- function(m, ...) {
    out <- paste0("birthYear: ", m$birthYear,
                  ", hireYear: ", m$hireYear,
                  ", sepYear: ", m$sepYear,
                  ", retireYear: ", m$retireYear);

    ## The last row of the salary history is always zero, and not so
    ## interesting.
    career <- m$salaryHistory %>%
        group_by(status) %>%
        summarize(startYear=first(year), startSalary=first(salary),
                  endYear=last(year), endSalary=last(salary)) %>%
        filter(status == "active");

    out <- paste0(out, "\n",
                  "     salaryHistory: (", career$startYear[1], ", ",
                  format(career$startSalary[1], digits=5, big.mark=","), ") ",
                  " -> (", career$endYear[1], ", ",
                  format(career$endSalary[1], digits=5, big.mark=","), ")");

    out <- paste0(out, "\n",
                  "  car: ", format(m$car, digits=4));

    return(out);
}

print.member <- function(m, ...) {
    cat("id: ", m$id, ", ", format(m), "\n", sep="");
}

## Defines a class of 'memberList' for convenience.
memberList <- function(members=c()) {
    out <- list();

    if (length(members) > 0) {
        for (m in members) {
            out[[m$id]] <- m;
        }
    }

    attr(out, "class") <- "memberList";
    return(out);
}


format.memberList <- function(ml, ...) {
    out <- "";
    for (member in ml) {
        out <- paste0(out, "[[", member$id, "]]\n     ",
                      format(member), "\n");
    }
    return(substr(out, 1, nchar(out) - 1));
}

print.memberList <- function(ml, ...) {
    cat(format(ml), "\n");
}

# Then a 'snapshot' function to create an employee matrix for a given
# year and from a series of those, we can create the 'P' matrix above.



## Generate N new active employees with the given ranges, and append
## them to the input list of members.
genEmployees <- function (N=1, ageRange=c(20,25), servRange=c(0,5),
                         avgSalary=75000, members=memberList(),
                         status="active") {

    ages <- round(runif(N)*(ageRange[2] - ageRange[1])) + ageRange[1];
    servs <- round(runif(N)*(servRange[2] - servRange[1])) + servRange[1];
    salaries <- rnorm(N, mean=avgSalary, sd=5000);

    for (i in 1:N) {
        m <- member(age=ages[i], service=servs[i], salary=salaries[i]);
        members[[m$id]] <- m;
    }

    return(members);
}




## Let's model the Queen Creek fire department.
qcFireN <- 0;
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


makeTbl <- function(memberList) {

    out <- tibble();

    for (member in memberList) {
        out <- rbind(out,
                     tibble(id=c(member$id),
                            birthYear=c(member$birthYear),
                            hireYear=c(member$hireYear),
                            sepYear=c(member$sepYear),
                            retireYear=c(member$retireYear),
                            maxSalary=c(max(member$salaryHistory$salary)),
                            car=c(member$car)));
    }

    return(out);
}

qcFireTbl <- makeTbl(qcFire);

## Here's a class to create a big list of employee salary histories.
## Each entry is a tibble of years and salaries for the employee with
## a given ID.
initializeSalaryList <- function(elist, employeeTable, year=2018) {

    for (i in 1:length(employeeTable$id)) {
        elist[[ employeeTable$id[i] ]] <-
            tibble(year=c(year), history=c(employeeTable$salary[i]));
        }

    return(elist);
}





