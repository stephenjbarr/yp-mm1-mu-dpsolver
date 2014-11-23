## SJB
##
## Experiment with variable holding cost rates

## Load the code for 
source("sjb-r-solver-general.R");

## Make a few different types of holding cost examples
hf_const      = function(q) { 20.0 };  
hf_linear     = function(q) { q };
hf_square     = function(q) { q^2 };
hf_sqrt       = function(q) { sqrt(q) };
hf_decreasing = function(q) { 84 - q };

hf_list  = list(hf_const, hf_linear, hf_square, hf_sqrt, hf_decreasing);
hf_names = list("Constant", "Linear", "Square", "SquareRoot", "Decreasing");
N_fns    = length(hf_names);

## Copy the problem_params_example and then modify parts of it
pp                   = problem_params_example;
pp$N_state_size      = 84;
pp$holding_cost_rate = NULL; # this is not strictly necessary but good practice

## Make a list of problem params objects.
## We are doing this so that each run has a different name
## based on its function
pp_list = list();
for(i in 1:N_fns) {
    pp_tmp             = pp;
    pp_tmp$output_base = paste("soln_", hf_names[[i]], sep="");
    pp_list[[i]]       = pp_tmp;    
}

##
ac_map = ac_map_example;


## Calculate the solutions
## 
solns = lapply(as.list(1:N_fns), function(i) { solve_dp(pp_list[[i]], ac_map = ac_map, holding_cost_fn = hf_list[[i]])});

######################################################################
##
## We can also generate functions algorithmicly. 
## For example, say were were interested in holding cost rates of the
## type
##           h(x) = a * x + b * x^2;
## where a in {0.5,1.5, 2.0} and b in { 0.33, 0.66 }
##
## We could do the following:

## Make a list of a's and b's, and get the Cartesian product
alist      = c(0.5,1.5, 2.0);
blist      = c(0.33, 0.66);
x          = expand.grid(a = alist, b = blist);

## Make a list of functions
hfn_list_2 = list()
for (j in 1:nrow(x)) {
    f               <- function(q) { };
    body(f)         <- parse(text=paste("q * ", x[j,]$a, "+ q^2 * ", x[j,]$b));
    hfn_list_2[[j]] <- f;
}

## Corresponding function names
fn_names    = as.list(paste("fn__a=", x$a, ",b=", x$b, sep=""))

## Make a list of problem parameters using the function names
pp_list_2 = list();
for(i in 1:nrow(x)) {
    pp_tmp = pp;
    pp_tmp$output_base = fn_names[[i]];
    pp_list_2[[i]] = pp_tmp;
}


## Get the solutions over all of these functions
solns2 = lapply(as.list(1:nrow(x)), function(i) { solve_dp(pp_list_2[[i]], ac_map = ac_map, holding_cost_fn = hfn_list_2[[i]])});
