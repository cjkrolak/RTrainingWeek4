rankall <- function(outcome, num = "best") {
    ## return a table of hospital names per state at <num> rank for specified outcome.
    ##
    ## inputs:
    ## outcome(str): name of the desired outcome, e.g. "heart attack", "heart failure", or "pneumonia"
    ## num(int):  rank number, if "best" than report 1
    ## returns:
    ## 2 column data frame (Hospital.Name, state) containing the hospital in each state taht has the ranking specified
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv")

    # column numbers for the outcome fields
    outcomeColumns <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)

    ## Check that outcome is valid
    if (!(outcome %in% names(outcomeColumns))) {
        stop("invalid outcome")
    }
    outcomeCol = as.numeric(outcomeColumns[outcome])

    # state list
    stateColumn <- 7  # column number for the State field
    states <- sort(unique(outcomeData[, stateColumn]))

    # validate num input, "best", "worst", 1 to 99999
    if (!((num %in% c("best", "worst")) || (is.numeric(num) && (num >= 1 && num <= 99999)))) {
        stop("invalid rank")
    }

    # output buffer
    df <- data.frame("hospital"=character(), "state"=character())
    hospitalNameCol  <- 2  # hospital name is column 2

    ## For each state, find the hospital of the given rank
    for (st in sort(states)) {
        # filter state data
        stateData <- outcomeData[which(outcomeData[stateColumn]==st),]
        # sort state data by outcome(ASC) and then by hospital name(ASC)
        stateData[, outcomeCol] <- suppressWarnings(as.numeric(stateData[, outcomeCol]))
        sortedData <- stateData[order(stateData[outcomeCol],
                                      stateData$Hospital.Name),]
        # determine "worst" rank for this state
        numberOfHospitals = length(sortedData[,outcomeCol])
        worstRank <- which.max(sortedData[,outcomeCol])
        if (num=="worst") {
            targetNum <- worstRank
        }
        else if (num=="best") {
            targetNum <- 1
        }
        else {
            targetNum <- num
        }
        # if rank exceeds number of hospitals, return "NA"
        if (targetNum > numberOfHospitals) {
            rankHospitalName <- "NA"
            rankHospitalRate <- -1
        }
        else {
            # grab specified value
            rankHospitalName <- sortedData[targetNum, hospitalNameCol]
            rankHospitalRate <- sortedData[targetNum, outcomeCol]
        }
        #print(paste("hospital ranked", targetNum, "in", st, "for", outcome, "is", rankHospitalName,
        #             "with a rate of", rankHospitalRate))
        # save ranked value
        df <- rbind(df, data.frame("hospital"=rankHospitalName, "state"=st))
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    df
}
