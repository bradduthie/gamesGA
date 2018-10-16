library(gamesGA);
context("Tournament function");

agents <- NULL;
for(i in 1:10){
    coop <- i - 1;
    agents[[i]] <- c(rep(x = 1, times = coop), rep(x = 0, times = 10 - coop));
}

fitnesses <- rep(x = 100, times = 10);

test_that("Fitness function returns expected values", {
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[1]],
                 agents[[3]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[2]],
                 agents[[3]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[3]],
                 agents[[10]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[4]],
                 agents[[5]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[5]],
                 agents[[9]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[6]],
                 agents[[5]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[7]],
                 agents[[10]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[8]],
                 agents[[4]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[9]],
                 agents[[5]])
    set.seed(1);
    expect_equal(tournament(agents = agents, fitness = fitnesses)[[10]],
                 agents[[3]])
})

set.seed(Sys.time())