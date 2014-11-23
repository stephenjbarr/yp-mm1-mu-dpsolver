## This is a generalized version of the solver


################################################################################
## Parameterize the problem. 
problem_params_example <- list(
    N_state_size       = 48,  ## Max Queue Size
    N_action_size      = 3,
    service_rate       = 2.0,
    holding_cost_rate  = 1.0,
    epsilon            = 0.0001,
    MAXITER            = 20000,
    output_base        = "sample_output"
    )


################################################################################
#
## The action space and cost function c(mu) can
## be thought of as a list of pairs act, CostAct. 
## Alternatively, it can be a 4-tuple:
##  (c(mu), mu_min, mu_bar, NUMACT)
## 
## For now, assume linear interpolation between mu_min and mu_max
##
## The function 'make_act_and_costact' returns a list with these arrays
## and this list is extracted into the global namespace


ca_struct_example <- list(cmu    = function(x) { x^2 },  # cost fn
                          mu_min = 1.0,
                          mu_max = 7.0,
                          NUMACT = 10)


######################################################################
## make_act_and_costact - given the struct as constructed above,
## make the state and cost vectors, linearly interpolating over the
## suggested points
##  
make_act_and_costact <- function(cas) {
    pts   <- seq(from=cas$mu_min, to=cas$mu_max, length.out=cas$NUMACT)
    costs <- matrix(cas$cmu(pts),nrow=1)
    aac   <- list(act = pts, costact = costs)
    return(aac)
}


#######################################################################
##
## ac_map_example
##
## The ac_map requires 2 parallel arrays called act and costact
## act is the set of possible actions, and costact is the set of
## possible states
ac_map_example <- list(act     =  c(1,4,7),
                       costact = c(1,50,500)
                       )


######################################################################
##
## solve_dp
##
## Requires
##   1. A problem_params list with the following defined:
##      N_state_size       = 48,  ## Max Queue Size
##      N_action_size      = 3,
##      service_rate       = 2.0,
##      holding_cost_rate  = 1.0,
##      epsilon            = 0.0001,
##      MAXITER            = 20000,
##      output_base        = "sample_output"
##
##      holding_cost_rate can be NULL if holding_cost_fn is specified.
##
##   2. One of ca_struct OR ac_map.
##
##      ca stands for cost,action.
##      cmu is a function which returns, for a specified service rate,
##      the corresponding cost. It also asks for mu_min and mu_max, the lower
##      and upper bounds of the service rate, and NUMACT, the number of points
##      between the min and the max.
##      
##          cmu                = function(x) { x^2 },  # cost fn
##          mu_min             = 1.0,
##          mu_max             = 7.0,
##          NUMACT             = 10)
##
##      
##      ac_map is a list with the elements act and costact, which both have the
##      same length. ac_map$act defines the set of mus, and ac_map$costact defines
##      the corresponding costs.
##  
## OPTIONAL:
##  3.  holding_cost_fn
##         This is a function of the queue length and determines the holding cost.
##         This is an alternative to specifying the holding_cost_rate. If for some
##         reason both are specified, holding_cost_fn takes precedence
## 

solve_dp <-  function(
    problem_params,
    ca_struct = NULL,
    ac_map = NULL,
    holding_cost_fn = NULL) {

    ## Problem_Params and one of ca_struct, ac_map is needed
    if(missing(problem_params)) {
        stop("Problem Params must be specified.");
    }

    ## We need to construct the action space or have it supplied explicitly
    if( is.null(ca_struct) && is.null(ac_map) ) {
        stop("Must supply action space and costs, either explictly using an ac_map or supply a function.");
    }

    
    ## Check if we can specify the holding cost function
    if( is.null(problem_params$holding_cost_rate) && is.null(holding_cost_fn) ) {
        stop("Must specify either holding cost rate or holding cost function");
    }

    ## Extract the holding cost function
    ##
    if( is.null(holding_cost_fn) ) {
        hq <- function(q) { q * problem_params$holding_cost_rate; }
    } else {
        hq <- holding_cost_fn;
    }

    
    ######################################################################
    ## Extract the problem parameters
    N       <-     problem_params$N_state_size;       
    NUMACT  <-     problem_params$N_action_size;      ## this seems wrong here
    lambda  <-     problem_params$service_rate;       
    H       <-     problem_params$holding_cost_rate;  
    epsilon <-     problem_params$epsilon;
    MAXITER <-     problem_params$MAXITER;
    fname_base <-  problem_params$output_base;

    #####################################################
    ## Matrix to hold the actions and correspnding costs
    ## We get this either through a ca_struct or an ac_map
    ac <- list()    
    if(is.null(ac_map)) {
        ac  <- make_act_and_costact(ca_struct)
    } else {
        ac <- ac_map
    }
    act     <- ac$act
    CostAct <- ac$costact

    ## Calculate tau, and multiply the action space by this to create TAct
    tau  <- 0.5 * (1 / (lambda + act[length(act)]));
    TAct <- tau * act;     ## FillTAct equivalent
    arr  <- tau * lambda;

    ## Initial setup for the iteration
    Old     = 1;
    delta   = 1;
    iter    = 1;


    ## Fill the u matrix (Fun) with zeros ## InitialFun
    ## These are indexed as 1..N+1, rather than 0..N.
    ## Be aware when comparing with reference Problem7.p
    Fun         = matrix(data=0, nrow=1, ncol=(N+1))
    WFun        = matrix(data=0, nrow=1, ncol=(N+1))
    
    ## Indexed 1 .. N
    OptAct      = matrix(data=0, nrow=1, ncol=N)
    OptActIdx   = matrix(data=0, nrow=1, ncol=N)

    while ((delta > epsilon) && (iter < MAXITER)) {
 
        ## Put the createWFun function body here       
        WFun[1]    =  (1 - arr) * Fun[1] + arr * Fun[2];
        New        =  0.0

        for (i in 2:(N+1)) {
            if(i < (N+1)) {
                New = Fun[i+1];  ## 
            } else {
                New = Fun[N+1]; 
            }
            action_costs = CostAct + TAct * Fun[i - 1] + (1 - arr - TAct) * Fun[i];
            ## WFun[i]        = H * (i-1) + arr * New + (min(action_costs));
            WFun[i]        = hq(i-1) + arr * New + (min(action_costs));
            oai            = which.min(action_costs)
            OptAct[i-1]    = TAct[oai];
            OptActIdx[i-1] = oai
        }



        delta  = abs(WFun[1] - Old);
        Old    = WFun[1];

        ## UpdateFun
        Fun[1]       =  0;
        Fun[2:(N+1)] = WFun[2:(N+1)] - WFun[1];
        iter         =  iter + 1;
        print(paste("Iter: ", iter, ", ERR: ", delta));

    }

    soln <- list(
        WFun       = WFun,
        OptAct     = OptAct,
        OptActIdx  = OptActIdx,
        delta      = delta,
        optAvgCost = WFun[1],
        NIter      = iter
                 )

    save(soln, file = paste(fname_base, ".Robj", sep=""));
    return(soln)

}
















