setwd("~/GitHub/yp-mm1-mu-dpsolver/r-solver")
source("sjb-r-solver-general.R");
source("dp-plotter.R");    ## New plotting code

my_problem_params <- list(
  N_state_size       = 100,  ## Max Queue Size
  N_action_size      = 3, ## just ignore this
  arrival_rate       = 2.0,
  holding_cost_rate  = 1.0, ## default, just ignore this
  epsilon            = 0.0001,
  MAXITER            = 200000,
  output_base        = "yp-example-problem" ## file prefix
)

## hf_rate     = function(q) { 0.3 * q };

## hf_step = stepfun(x=c(5,10,15,20), y=c(1,3,4,5,8));

hf_step = function(q) { 0.01 * q };

## hf_penalty = function(q) {
## cost = 10 * q;
## if( q > 70) {  
## cost = cost + 50;
## }
## return(cost);
## }

my_ca_struct <- list(cmu    = function(x) { 20*x },  # cost fn
                     mu_min = 2,
                     mu_max = 100,
                     NUMACT = 1000);


solution = solve_dp(my_problem_params, ac_map = NULL, ca_struct=my_ca_struct, holding_cost_fn = hf_step) ## can also replace hf_step with hf_rate to use that function

## solution = solve_dp(my_problem_params, ac_map = NULL, ca_struct=my_ca_struct, holding_cost_fn = function(q) { q^3}); ##example of another way to run the solution

plotdp(solution, title="Experiment with a step function");








