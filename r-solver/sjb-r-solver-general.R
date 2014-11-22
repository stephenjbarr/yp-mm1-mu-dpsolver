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
ac_map_example <- list(act     =  c(1,4,7),
                       costact = c(1,50,500)
                       )


solve_dp <-  function(problem_params, ca_struct = NULL, ac_map = NULL) {

    ## Problem_Params and one of ca_struct, ac_map is needed
    if(missing(problem_params)) {
        stop("Problem Params must be specified.");
    }

    ## We need to construct the action space or have it supplied explicitly
    if( is.null(ca_struct) && is.null(ac_map) ) {
        stop("Must supply action space and costs, either explictly using an ac_map or supply a function.");
    }

    ######################################################################
    ## Extract the problem parameters
    N       <-     problem_params$N_state_size;       
    NUMACT  <-     problem_params$N_action_size;      ## this seems wrong here
    lambda  <-     problem_params$service_rate;       
    H       <-     problem_params$holding_cost_rate;  
    epsilon <-     problem_params$epsilon;
    MAXITER <-     problem_params$MAXITER;


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




    ## CreateWFun
    ##   This populates the WFun and OptAct arrays,
    ##   It depends on Fun, arr, H, CostAct, TAct
    ##
    ##   OptAct, the optimal policy, gets recomputed every time
    ##
    ##  Note, this is created within the scope of solve_dp.
    ##  Such is the curse with mutable state.


    ## createWFun <- function(FunPrev) {

    ##     WFun       =  matrix(data=0, nrow=1, ncol=(N+1))
    ##     OptAct     =  matrix(data=0, nrow=1, ncol=(N))
    ##     OptActIdx  =  matrix(data=0, nrow=1, ncol=(N))        
    ##     WFun[1]    =  (1 - arr) * FunPrev[1] + arr * FunPrev[2];
    ##     New        =  0.0

    ##     for (i in 2:(N+1)) {
    ##         if(i < N) {
    ##             New = FunPrev[i+2];  ## Adding + 2 to compensate for the 1-based indexing
    ##         } else {
    ##             New = FunPrev[N+1]; 
    ##         }
    ##         action_costs = CostAct + TAct * FunPrev[i - 1] + (1 - arr - TAct) * FunPrev[i];
    ##         WFun[i]        = H * i + arr * New + (min(action_costs));
    ##         oai            = which.min(action_costs)
    ##         OptAct[i-1]    = TAct[oai];
    ##         OptActIdx[i-1] = oai
    ##     }

    ##     return(list(wf = WFun, oa = OptAct, oai = OptActIdx))

    ## }


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
        ## x      = createWFun(Fun)       ## For this function, Fun is read-only
        ## WFun   = x$wf
        ## OptAct = x$oa
        ## OptActIdx = x$oai

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
            WFun[i]        = H * (i-1) + arr * New + (min(action_costs));
            oai            = which.min(action_costs)
            OptAct[i-1]    = TAct[oai];
            OptActIdx[i-1] = oai
        }



        delta  = abs(WFun[1] - Old)
        Old    = WFun[1]

        ## UpdateFun
        Fun[1]       =  0;
        Fun[2:(N+1)] = WFun[2:(N+1)] - WFun[1];   ## This fun is scoped above, so 
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

    return(soln)

}
















