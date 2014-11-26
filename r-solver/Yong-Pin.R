source("sjb-r-solver-general.R");
source("dp-plotter.R");    ## New plotting code



my_problem_params <- list(
  N_state_size       = 25,  ## Max Queue Size
  N_action_size      = 3,
  service_rate       = 2.0,
  holding_cost_rate  = 1.0,
  epsilon            = 0.0001,
  MAXITER            = 20000,
  output_base        = "yp-example-problem"
)

hf_rate     = function(q) { 0.3 * q };

hf_step = stepfun(x=c(5,10,15,20), y=c(10,3,1,4,8));


my_ca_struct <- list(cmu    = function(x) { 1.5*sqrt(x) },  # cost fn
                     mu_min = 1.0,
                     mu_max = 1.2,
                     NUMACT = 20);



solution = solve_dp(my_problem_params, ac_map = NULL, ca_struct=my_ca_struct, holding_cost_fn = hf_step)

plotdp(solution, title="Experiment with a step function");








