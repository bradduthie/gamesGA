library(gamesGA);
context("Strategy mutation");

agents      <- NULL;
history_vec <- c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1);
history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);
hist_pretty <- history;

hist_pretty[hist_pretty==0]   <- "C";
hist_pretty[hist_pretty=="1"] <- "D";
hist_print                    <- NULL;

for(i in 1:dim(hist_pretty)[1]){
    row        <- paste(hist_pretty[i,], collapse=" ");
    hist_print <- c(hist_print, row);
}
hist_print <- c(hist_print, "1st");

for(i in 1:10){
    agents[[i]] <- rep(x = 0, times = 9);
}

payoffs <- c(3, 0, 5, 1);

test_that("No mutation occurs when equals is zero", {
    expect_equal(mutation(agents, 0), agents)
})

test_that("Mutation occurs given a set seed of 1", {
    set.seed(1);
    expect_equal(mutation(agents, 0.06)[[10]][9], 1);
    set.seed(1);
    expect_equal(sum(mutation(agents, 0.06)[[10]][9]), 1);
})
