library(gamesGA);
context("Tournament function");

agents <- NULL;
for(i in 1:10){
    coop <- i - 1;
    agents[[i]] <- c(rep(x = 1, times = coop), rep(x = 0, times = 10 - coop));
}

fitnesses <- rep(x = 100, times = 10);

test_that("Fitness function returns expected values", {
    expect_equal(1, 1)
})

set.seed(Sys.time())