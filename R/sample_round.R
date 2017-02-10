#' Sample round function
#' Function that simulates a sequence of rounds of a game played between two 
#' selected agents
#' @param agents A list of agents
#' @param foc_agent The index of the first agent that will play the game
#' @param opp_agent The index of the second agent that will play the game
#' @param rounds The number of rounds that the agents will play
#' @return round_hist The history of the game played between both agents
sample_round <- function(agents, foc_agent, opp_agent, rounds = 100){
    agent_1 <- rep(0, rounds);
    agent_2 <- rep(0, rounds);
    payoff1 <- rep(0, rounds);
    payoff2 <- rep(0, rounds);
    
    # Special round 1 (not enough history);
    agent_1[1] <- agents[[foc_agent]][9]; # First turn
    agent_2[1] <- agents[[opp_agent]][9];
    payoff1[1] <- PD(agent_1[1], agent_2[1]);
    payoff2[1] <- PD(agent_2[1], agent_1[1]);
    
    # Special round 2 (not enough history);
    resp_1     <- which(history[1:2,3] == agent_2[1]);
    resp_2     <- which(history[1:2,3] == agent_1[1]);
    agent_1[2] <- agents[[foc_agent]][resp_1];
    agent_2[2] <- agents[[opp_agent]][resp_2];
    payoff1[2] <- PD(agent_1[2], agent_2[2]);
    payoff2[2] <- PD(agent_1[2], agent_2[2]);
    
    # Special round 3 (not enough history);
    resp_1     <- which( history[1:4,2] == agent_2[1] & 
                         history[1:4,3] == agent_2[2]
                       );
    resp_2     <- which( history[1:4,2] == agent_1[1] & 
                         history[1:4,3] == agent_1[2]
                       );
    agent_1[3] <- agents[[foc_agent]][resp_1];
    agent_2[3] <- agents[[opp_agent]][resp_2];
    payoff1[3] <- PD(agent_1[3], agent_2[3]);
    payoff2[3] <- PD(agent_1[3], agent_2[3]);
    
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
        agent_1[round] <- agents[[foc_agent]][resp_1];
        agent_2[round] <- agents[[opp_agent]][resp_2];
        payoff1[round] <- PD(agent_1[round], agent_2[round]);
        payoff2[round] <- PD(agent_2[round], agent_1[round]);
    }
    
    round_hist <- data.frame(agent_1, agent_2, payoff1, payoff2);
    return(round_hist);
}