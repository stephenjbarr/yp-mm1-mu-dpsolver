## SJB
##
## reproduce table 10.1 of Sennott

##




lambdas <- c(3,2,2,2,5,5,10,20)

svcrates <- matrix(
    data = c(
        2,4,8,
        1,4,7,
        1,4,7,
        1,4,7,
        5,5.5,5.8,
        5.1,5.3,6.0,
        10.2,10.6,12.0,
        24.0,27.0,30.0),
    ncol = 3,
    byrow = TRUE)

costs <- t(matrix(
    data = c(
        9.0, 1.0, 10, 1.0, 0.0, 0.0, 0.0, 1.0, 
        13.0, 50.0, 50.0, 50.0, 10.0, 10.0, 10.0, 1.5, 
        21.0,  500.0, 150.0, 100, 100, 25, 25, 5), 
    nrow = 3,
    byrow = TRUE))
    

problem_params_base <- list(
    N_state_size       = 48,  ## Max Queue Size
    N_action_size      = 3,
    service_rate       = 2.0,
    holding_cost_rate  = 1.0,
    epsilon            = 0.0001,
    MAXITER            = 20000,
    output_base        = "tab_scenario10-1"
    )


pp_list = list()
ac_list = list()
N_scenario = length(lambdas)

for (i in 1:N_scenario) {

    pp_tmp = problem_params_base;
    if(i > 1) {
        pp_tmp$N_state_size = 84;
    }
    pp_tmp$service_rate = lambdas[i];
    pp_tmp$output_base = paste(problem_params_base$output_base, "_n", i, ".txt", sep="");
    ac_tmp = list(act = svcrates[i,], costact = costs[i,]);
    pp_list[[i]] = pp_tmp;
    ac_list[[i]] = ac_tmp;

}

solns <- lapply(as.list(1:N_scenario), function(i) {solve_dp(pp_list[[i]], ac_map = ac_list[[i]])})
