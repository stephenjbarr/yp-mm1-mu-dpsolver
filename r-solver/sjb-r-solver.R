## A simple port of Program7.p of Linn Sennott's Pascal code



N      <- 48
NUMACT <- 3
lambda <- 2.0
H      <- 1.0

## Matrix to hold the state space
fillact <- function() { return(matrix(c(1,4,7),nrow=1, ncol=NUMACT))}
act <- fillact()

fillcostact <- function() { return(matrix(c(1,50.0,500.0),nrow=1, ncol=NUMACT))}
CostAct <- fillcostact()

## Calculate tau, and multiply the action space by this to create TAct
tau  <- 0.5 * (1 / (lambda + act[length(act)]));
TAct <- tau * act;
arr  <- tau * lambda;

## Fill the u matrix (Fun) with zeros
Fun <- matrix(data=0, nrow=1, ncol=(N+1))




## CreateWFun
##   This populates the WFun and OptAct arrays,
##   It depends on Fun, arr, H, CostAct, TAct
##
##   OptAct, the optimal policy, gets recomputed every time
createWFun <- function() {
    WFun     <- matrix(data=0, nrow=1, ncol=(N+1))
    OptAct   <- matrix(data=0, nrow=1, ncol=(N+1))
    WFun[1]  <- (1 - arr) * Fun[1] + arr * Fun[2];
    New      <- 0.0
    for (i in (2:(N+1))) {
        if(i < (N+1)) {
            New = Fun[i+1]
        } else {
            New = Fun[N+1]
        }
        action_costs = CostAct + TAct * Fun[i - 1] + (1 - arr - TAct) * Fun[i];
        WFun[i]   = H * i + arr * New + (min(action_costs));
        oai <- which.min(action_costs)
        OptAct[i] = TAct[oai];
        OptActIdx[i] = oai
    }
    return(list(wf = WFun, oa = OptAct, oai = OptActIdx))
}


## Initial setup for the iteration
Old     <- 1;
delta   <- 1;
iter    <- 1;
S       <- 8;
epsilon <- 0.0001;
MAXITER <- 20000;


WFun        <- matrix(data=0, nrow=1, ncol=(N+1))
OptAct      <- matrix(data=0, nrow=1, ncol=(N+1))
OptActIdx   <- matrix(data=0, nrow=1, ncol=(N+1))

while ((delta > epsilon) && (iter < MAXITER)) {
    x      <- createWFun()
    WFun   <- x$wf
    OptAct <- x$oa
    OptActIdx <- x$oai
    delta  <- abs(WFun[1] - Old)
    Old    <- WFun[1]

    ## UpdateFun
    Fun[1] <- 0
    Fun[2:(N+1)] <- WFun[2:(N+1)] - WFun[1]
    iter <-  iter + 1
    print(paste("Iter: ", iter, ", ERR: ", delta))
}
