best <- function(state, outcome) {
    # function to return hospital name in specified state with lowest rate for
    # specified outcome.  Upon tie, first hospital name alphabetically sorted
    # is returned.
    #
    # inputs:
    # state(str, length=2): state name abbreviation
    # outcome(str): name of the desired outcome, e.g. "heart attack", "heart failure", or "pneumonia"
    # returns:
    #    vector: best hospital name

    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv")  # , colClasses = "character")
    head(outcomeData)

    ## Check that state is valid
    stateColumn <- 7  # column number for the State field
    states <- sort(unique(outcomeData[, stateColumn]))
    if (!(state %in% states)) {
        stop("invalid state")
    }
    # print(paste("state input: ", state))

    # column numbers for the outcome fields
    outcomeColumns <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)

    ## Check that outcome is valid
    if (!(outcome %in% names(outcomeColumns))) {
        stop("invalid outcome")
    }
    # print(paste("outcome input: ", outcome))

    ## Return hospital name in that state with lowest 30-day death
    stateData <- outcomeData[which(outcomeData[stateColumn]==state),]
    
    # sort state data by outcome(ASC) and then by hospital name(ASC)
    outComeCol = suppressWarnings(as.numeric(outcomeColumns[outcome]))
    stateData[, outComeCol] <- suppressWarnings(as.numeric(stateData[, outComeCol]))
    sortedData <- stateData[order(stateData[outComeCol],
                                  stateData$Hospital.Name),]

    # best hospital name should now be row 1
    hospitalNameCol  <- 2  # hospital name is column 2
    bestHospitalName <- sortedData[1, hospitalNameCol]
    bestHospitalRate <- sortedData[1, outComeCol]
    #print(paste("best hospital in", state, "for", outcome, "is", bestHospitalName,
    #            "with a rate of", bestHospitalRate))
    return(bestHospitalName)
}