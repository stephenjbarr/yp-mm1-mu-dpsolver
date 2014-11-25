source("sjb-r-solver-general.R");

my_problem_params <- list(
  N_state_size       = 1000,  ## Max Queue Size
  N_action_size      = 3,
  service_rate       = 2.0,
  holding_cost_rate  = 1.0,
  epsilon            = 0.0001,
  MAXITER            = 20000,
  output_base        = "sample_output"
)

hf_square     = function(q) { q^2 };

my_ca_struct <- list(cmu    = function(x) { 10*x },  # cost fn
                          mu_min = 1.0,
                          mu_max = 7.0,
                          NUMACT = 10000)

solution=solve_dp(my_problem_params, ca_struct=my_ca_struct, holding_cost_fn = hf_square)

plot(x=1:length(solution$OptAct),  y=solution$OptAct, type="l")

