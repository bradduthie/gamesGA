library(gamesGA);
context("Round of games sampling");

agents <- NULL;
for(i in 1:10){
    agents[[i]] <- rep(x = 0, times = 9);
}

payoffs <- c(3, 0, 5, 1);

test_that("Accurate payoffs returned", {
    expect_equal(PD(0, 0, payoffs), payoffs[1])
    expect_equal(PD(0, 1, payoffs), payoffs[2])
    expect_equal(PD(1, 0, payoffs), payoffs[3])
    expect_equal(PD(1, 1, payoffs), payoffs[4])
})

test_that("Mutation occurs given a set seed of 1", {
    expect_equal(dim(sample_round(agents, 1, 1, payoffs, rounds = 10)),
                 c(10,4))
    expect_equal(sample_round(agents, 1, 1, payoffs, rounds = 10)[1,1], 0)
    expect_equal(sample_round(agents, 1, 1, payoffs, rounds = 10)[1,2], 0)
    expect_equal(sample_round(agents, 1, 1, payoffs, rounds = 10)[1,3], 3)
    expect_equal(sample_round(agents, 1, 1, payoffs, rounds = 10)[1,4], 3)
})
