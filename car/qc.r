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
            ## Roll the dice.
            if (runif(1) < rates[service - 19]) status <- "retired";
    }

    return(status);
}

doesMemberBecomeDisabled <- function(age, sex, service, status,
                                     mortClass="General", tier="None") {
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
projectSalaryDelta <- function(age, service=1, tier="None") {

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

    startingPension <- max(salaryHistory$salary) * 0.605;
    cola <- 1.02;

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

qcModelOutput <- runModel(qcModel, verbose=TRUE);
qcModelPlot <- plotModelOut(qcModelOutput)
