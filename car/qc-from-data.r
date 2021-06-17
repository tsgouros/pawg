library(readxl)
library(dplyr)

tmp <- read_excel("../Queen_Creek_Fire_2.xlsx", sheet="Queen_Creek_Fire");
qcMemberData <- tmp %>%
    mutate(year=as.numeric(substr(ppe_dt, 1, 4)),
           birth=as.numeric(substr(dob, 1, 4)),
           name=full_name) %>%
    group_by(full_name, year) %>%
    summarize(dob=first(birth),
              name=first(name),
              tier=first(tier_no),
              status=last(employment_status),
              ee=sum(ee_amount, na.rm=TRUE),
              er=sum(er_amount, na.rm=TRUE),
              salary=sum(pensionable_salary,na.rm=TRUE),
              .groups="drop") %>%
    group_by(name) %>%
    summarize(year=year,
              birthYear=dob,
              tier=tier,
              status=status,
              hireYear=first(year),
              service=year - hireYear + 1,
              ## We have data through 5/2021.
              ee=ifelse(year==2021,ee*(12/5), ee),
              er=ifelse(year==2021,er*(12/5), er),
              salary=ifelse(year==2021,salary*(12/5), salary),
              .groups="drop")


##
## Takes a table of member data and converts it to a bunch of member
## objects, and returns them.
##
genEmployeesFromData <- function(memberTbl, verbose=FALSE) {

    members = memberList();

    for (uname in unique(memberTbl$name)) {
        memberScalarData <- memberTbl %>%
            filter(name==uname) %>%
            group_by(name) %>%
            summarize(birthYear=first(birthYear),
                      hireYear=first(hireYear),
                      tier=first(tier),
                      status=last(status))

        if (verbose) print(memberScalarData);

        memberStatus <- memberScalarData %>% select(status);

        if (memberStatus == "Refunded") {
            ## Skip this one.
            next;
        } else if (memberStatus == "KIA") {
            memberStatus <- "deceased";
        } else if (memberStatus == "Active") {
            memberStatus <- "active";
        } else if (memberStatus == "Quit/Terminated") {
            memberStatus <- "separated";
        } else if (memberStatus == "Resume Service") {
            memberStatus <- "active";
        } else if (memberStatus == "Retired") {
            memberStatus <- "retired";
        } else if (memberStatus == "Transferred") {
            ## Skip this one, too.
            next;      ## memberStatus <- "separated";
        }

        memberSalaryHistory <- memberTbl %>%
            filter(name==uname) %>%
            mutate(premium = ee + er,
                   age = year - birthYear,
                   status = ifelse(salary > 0, "active", status)) %>%
            select(year, salary, age, service, status, premium)

        if (verbose) print(memberSalaryHistory)

        m <- member(salaryHistory=memberSalaryHistory,
                    currentYear=2021,
                    birthYear = memberScalarData %>%
                        select(birthYear) %>% as.numeric(),
                    hireYear = memberScalarData %>%
                        select(hireYear) %>% as.numeric(),
                    tier = memberScalarData %>%
                        select(tier) %>% as.numeric(),
                    mortClass="Safety",
                    note=uname,
                    sex="M",
                    status = memberStatus,
                    verbose=verbose)

        members[[m$id]] <- m;

        if (verbose) print(m);
    }

    return(members);
}

qcFireFromData <- genEmployeesFromData(qcMemberData, verbose=FALSE)

qcModelOutputFromData <-
    runModel(function() { genEmployeesFromData(qcMemberData) },
             verbose=FALSE, N=1);
qcModelPlotFromData <- plotModelOut(qcModelOutputFromData)
