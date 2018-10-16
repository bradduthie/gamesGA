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

test_that("Mutation occurs given a set seed of 1", {
    set.seed(1);
    expect_equal(crossover(agents, 0.06)[[6]][1], 1);
    set.seed(1);
    expect_equal(sum(mutation(agents, 0.06)[[10]][9]), 1);
})

set.seed(Sys.time())