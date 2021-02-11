# read in data
#outcome <-read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)

# make histogram
#outcome[, 11] <- as.numeric(outcome[,11])  # coerce to numeric
#hist(outcome[,11])


# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. 
#Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
# ## Read outcome data
# ## Check that state and outcome are valid
# ## Return hospital name in that state with lowest 30-day death
# ## rate
# }

best <- function(state, outcome) {
    # inputs:
    # state(str, length=2): state name abbreviation
    # outcome(str): name of the desired outcome, e.g. "heart attack", "heart failure", or "pneumonia"

    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv")  # , colClasses = "character")
    head(outcomeData)

    ## Check that state is valid
    stateColumn <- 7  # column number for the State field
    valid_states <- state.abb
    if (!(state %in% valid_states)) {
        stop("invalid state: ", state)
    }
    print(paste("state input: ", state))

    # column numbers for the outcome fields
    outcomeColumns <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)

    ## Check that outcome is valid
    if (!(outcome %in% names(outcomeColumns))) {
        stop("invalid outcome: ", outcome)
    }
    print(paste("outcome input: ", outcome))

    ## Return hospital name in that state with lowest 30-day death
    # stateData <- outcomeData[which(outcomeData[as.numeric(outcomeColumns[outcome])]==state),]
    stateData <- outcomeData[which(outcomeData[stateColumn]==state),]
    
    # sort state data by outcome(ASC) and then by hospital name(ASC)
    sortedData <- stateData[order(stateData[as.numeric(outcomeColumns[outcome])],
                                  stateData$Hospital.Name),]
    hospitalNameCol  <- 2  # hospital name is column 2
    bestHospitalName <- sortedData[1, hospitalNameCol]
    bestHospitalRate <- sortedData[1, as.numeric(outcomeColumns[outcome])]
    print(paste("best hospital in", state, "for", outcome, "is", bestHospitalName,
                "with a rate of", bestHospitalRate))
    return(bestHospitalName)
}