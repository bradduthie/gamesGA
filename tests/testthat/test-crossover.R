library(gamesGA);
context("Strategy crossover");

agents <- NULL;
for(i in 1:10){
    agents[[i]] <- rep(x = 0, times = 9);
}
agents[[1]][1] <- 1;

test_that("No mutation occurs when equals is zero", {
    expect_equal(crossover(agents, 0), agents)
})

set.seed(Sys.time())