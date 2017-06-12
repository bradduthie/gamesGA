#' 
#' Fitness function
#' 
#' This function assesses the fitness of each strategy through the use of a 
#' sequential game between each focal strategy and a fixed number of random 
#' opponents.
#' 
#' @param history A table of all possible prior moves of agents in sequence
#' @param agents A list of agents whose fitness will be assessed
#' @param payoffs A vector of the payoffs for CC (payoffs[1]), CD (payoffs[2]),
#' DC (payoffs[3]) and DD (payoffs[4]) combinations of play choices from the
#' focal player and the opponent
#' @param num_opponents The number of random opponents to match the focal agent
#'  against
#' @param rounds The number of rounds that will be played
#' @param useC A TRUE or FALSE value that determines whether or not c will be
#' called to calculate agent fitnesses
#' @return fitness A vector in which elements correspond to the accumlated
#' fitness (payoffs) of each agent
#' @export
fitness <- function(history, agents, payoffs, num_opponents, rounds, useC){
    pay <- payoffs;
    if(useC == TRUE){
        agent_vec   <- unlist(agents);
        agent_array <- matrix(data=agent_vec, nrow=length(agents), byrow=TRUE);
        parameters  <- c(num_opponents, rounds, pay);
        fitness     <- run_fitness(history, agent_array, parameters);
    } else{
        fitness    <- NULL;
        num_agents <- length(agents);
    
        for(foc in 1:num_agents){
        
            opponents <- sample(x=1:num_agents, size=num_opponents, 
                                replace=FALSE);
            foc_score <- rep(0, num_opponents);
            
            for(opps in 1:length(opponents)){
                opp     <- opponents[opps];
                agent_1 <- rep(0, rounds);
                agent_2 <- rep(0, rounds);
                payoff1 <- rep(0, rounds);
                payoff2 <- rep(0, rounds);
            
                # Special round 1 (not enough history);
                agent_1[1] <- agents[[foc]][9]; # First turn
                agent_2[1] <- agents[[opp]][9];
                payoff1[1] <- PD(agent_1[1], agent_2[1], pay);
                payoff2[1] <- PD(agent_2[1], agent_1[1], pay);
            
                # Special round 2 (not enough history);
                resp_1     <- which(history[1:2,3] == agent_2[1]);
                resp_2     <- which(history[1:2,3] == agent_1[1]);
                agent_1[2] <- agents[[foc]][resp_1];
                agent_2[2] <- agents[[opp]][resp_2];
                payoff1[2] <- PD(agent_1[2], agent_2[2], pay);
                payoff2[2] <- PD(agent_2[2], agent_1[2], pay);
            
                # Special round 3 (not enough history);
                resp_1     <- which( history[1:4,2] == agent_2[1] & 
                                     history[1:4,3] == agent_2[2]
                                   );
                resp_2     <- which( history[1:4,2] == agent_1[1] & 
                                     history[1:4,3] == agent_1[2]
                                   );
                agent_1[3] <- agents[[foc]][resp_1];
                agent_2[3] <- agents[[opp]][resp_2];
                payoff1[3] <- PD(agent_1[3], agent_2[3], pay);
                payoff2[3] <- PD(agent_2[3], agent_1[3], pay);
            
                # Remaining rounds (enough history for complete memory);
                for(round in 4:rounds){
                    mem1 <- round - 3;
                    mem2 <- round - 2;
                    mem3 <- round - 1;
                    resp_1     <- which( history[,1] == agent_2[mem1] & 
                                         history[,2] == agent_2[mem2] &
                                         history[,3] == agent_2[mem3]
                                       );
                    resp_2     <- which( history[,1] == agent_1[mem1] & 
                                         history[,2] == agent_1[mem2] &
                                         history[,3] == agent_1[mem3]
                                       );        
                    agent_1[round] <- agents[[foc]][resp_1];
                    agent_2[round] <- agents[[opp]][resp_2];
                    payoff1[round] <- PD(agent_1[round], agent_2[round], pay);
                    payoff2[round] <- PD(agent_2[round], agent_1[round], pay);
                }
                foc_score[opps] <- sum(payoff1);
            }
            fitness[foc] <- sum(foc_score);
        }
    }
    return(fitness);
}

run_fitness <- function(HISTORY, AGENTS, PARAMETERS){
    .Call("fitness", HISTORY, AGENTS, PARAMETERS);   
}

#' 
#' PD function
#' 
#' Returns the number of points (payoff) that a focal player accumlates from one
#' round of a game with an opponent given a payoff vector given a decision "C"
#' or "D" for each player
#' 
#' @param a1_play The play choice of the focal player (0 or 1)
#' @param a2_play The play choice of the opponent player (0 or 1)
#' @param payoffs A vector of payoffs (length = 4) to the focal player as a 
#' consequence of both players playing 0 (payoffs[1]), the focal player only
#' playing 0 (payoffs[2]), the focal player only playing 1 (payoffs[3]), and
#' both players playing 1 (payoffs[4])
#' @return fitness The payoff to the focal player (points accumulated from the
#' interaction)
PD <- function(a1_play, a2_play, payoffs){
  points <- 0;
  if(a1_play == 0 & a2_play == 0){
    points <- payoffs[1];
  }
  if(a1_play == 0 & a2_play == 1){
    points <- payoffs[2];
  }
  if(a1_play == 1 & a2_play == 0){
    points <- payoffs[3];
  }
  if(a1_play == 1 & a2_play == 1){
    points <- payoffs[4];
  }
  return(points);
}