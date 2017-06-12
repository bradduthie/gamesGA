library(gamesGA);
context("Fitness function");

agents      <- NULL;
history_vec <- c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1);
history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);

for(i in 1:10){
    agents[[i]] <- rep(x = 0, times = 9);
}

agents2     <- NULL;
for(i in 1:10){
    coop <- i - 1;
    agents2[[i]] <- c(rep(x = 1, times = coop), rep(x = 0, times = 10 - coop));
}
ref_fits_c <- c(51, 64,  177, 168, 156, 192, 153, 117, 168, 176);
ref_fits_R <- c(54, 112, 171, 173, 218, 246, 247, 282, 337, 384);

test_that("Equal fitnesses (c call) for the same strategies", {
    expect_equal( fitness( history       = history,
                           agents        = agents,
                           pay           = payoffs,
                           useC          = TRUE,
                           rounds        = 10,
                           num_opponents = 10
                           ), 
                  rep(x = 300, times = 10))
})

test_that("Equal fitnesses (R call) for the same strategies", {
    expect_equal( fitness( history       = history,
                           agents        = agents,
                           pay           = payoffs,
                           useC          = FALSE,
                           rounds        = 10,
                           num_opponents = 10
    ), 
    rep(x = 300, times = 10))
})


test_that("Increasing fitnesses (c call) for better strategies", {
    set.seed(1);
    expect_equal( fitness( history       = history,
                           agents        = agents2,
                           pay           = payoffs,
                           useC          = TRUE,
                           rounds        = 10,
                           num_opponents = 10
    ), 
    ref_fits_c)
})

test_that("Increasing fitnesses (R call) for better strategies", {
    set.seed(1);    
    expect_equal( fitness( history       = history,
                           agents        = agents2,
                           pay           = payoffs,
                           useC          = FALSE,
                           rounds        = 10,
                           num_opponents = 10
    ), 
    ref_fits_R)
})

set.seed(Sys.time())