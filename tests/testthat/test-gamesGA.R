library(gamesGA);
context("gamesGA function");

test_that("Correct column in gamesGA return", {
    expect_equal( dim(games_ga(CC = 3, CD = 0, DC = 5, DD = 1, 
                           callC = TRUE, 
                           generations = 10, 
                           rounds = 10, 
                           num_opponents = 10,
                           cross_prob = 0.05, 
                           mutation_prob = 0.05
                          )$genos)[2], 
                  10)
})

test_that("Fitness vector length and type accurate", {
    expect_equal( is.numeric(games_ga(CC = 3, CD = 0, DC = 5, DD = 1, 
                               callC = TRUE, 
                               generations = 10, 
                               rounds = 10, 
                               num_opponents = 10,
                               cross_prob = 0.05, 
                               mutation_prob = 0.05
                             )$fitness), 
                   TRUE)
    expect_equal( length(games_ga(CC = 3, CD = 0, DC = 5, DD = 1, 
                                      callC = TRUE, 
                                      generations = 10, 
                                      rounds = 10, 
                                      num_opponents = 10,
                                      cross_prob = 0.05, 
                                      mutation_prob = 0.05
    )$fitness), 
    10)
})