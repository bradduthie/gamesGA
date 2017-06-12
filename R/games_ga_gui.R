#' Games genetic algorithm (GUI): 
#' 
#' This function launches a graphical user interface (GUI) within a browser for 
#' the games_ga function, which runs a genetic algorithm that identifies 
#' sequential strategies for maximising payoffs given any two by two symmetrical 
#' payoff matrix. Simulated players remember three rounds into the past. 
#' Outputs include a table of survivng adaptive strategies and a plot of mean
#' strategy fitness over generations of the genetic algorithm. No arguments are
#' required (or available) for this function -- it only launces the GUI.
#' 
#'@return A table (visible in a browswer), the elements of which include: 
#' 1. A table of the genomes of strategies and their frequencies in the 
#' population and 2. A plot showing the mean fitness calculated over all players 
#' in the population in each generation. The fitness of one player is the number 
#' of points accrued over all rounds and opponents in a generation.
#'@useDynLib gamesGA
#'@importFrom stats rnorm rpois rbinom runif
#'@importFrom shiny inputPanel reactive renderTable renderPlot shinyUI fluidPage
#'    titlePanel br fluidRow h2 h4 mainPanel numericInput plotOutput runApp
#'    shinyApp shinyServer tableOutput
#'@importFrom graphics abline axis image mtext par plot points
#'@export
#'

games_ga_gui <- function(){
    
    ui <- shinyUI(fluidPage(
    
        titlePanel("gamesGA: Genetic algorithm for sequential symmetric games"),
    
        br(),
        
        fluidRow(
            
            h4("The gamesGA R package simulates a genetic algorithm for finding 
                adaptive strategies in sequentially played rounds of any two by 
                two sequential payoff matrix game iterated between players. 
                Below, the embedded shiny application recognises user inputs in 
                the `games_ga()` function and returns a table showing surviving 
                strategies and their corresponding frequencies. The plot below
                shows the mean fitness of strategies in each generation of the
                genetic algorithm"), 
            
            br(),

            inputPanel(
                numericInput("CC", label = "CC payoff", 3, width="50%"),
                numericInput("DC", label = "DC payoff", 5, width="50%"),
                numericInput("CD", label = "CD payoff", 0, width="50%"),
                numericInput("DD", label = "DD payoff", 1, width="50%"),
                numericInput("RnD", label = "Rounds", 100, width="50%"),
                numericInput("Gen", label = "Generations", 250, width="50%"),
                numericInput("Cro", label = "Pr(Crossover)", 0.05, width="50%"),
                numericInput("mu", label = "Pr(Mutation)", 0.05, width="50%"), 
                style='width: 900px; height: 150px'
            )   
        ),
    
        br(),
    
        mainPanel(br(), h2("Frequencies of adaptive strategies"), br(),
                  tableOutput(outputId = "table"),
                  br(), h2("Mean strategy fitness"), br(),
                  plotOutput(outputId="plot")
        )
    ))

    server <- shinyServer(function(input, output, session) {
        gres <- reactive({ 
            cc <- input$CC;
            dc <- input$DC;
            cd <- input$CD;
            dd <- input$DD;
            rd <- input$RnD;
            gn <- input$Gen;
            co <- input$Cro;
            mu <- input$mu;
        
            game_res <- games_ga(CC = cc, DC = dc, CD = cd, DD = dd, 
                                 rounds = rd, generations = gn, cross_prob = co, 
                                 mutation_prob = mu, num_opponents = 100);     
            all <- c(game_res$genos, "break_1",
                     rownames(game_res$genos), "break_2",
                     game_res$fitness);
        })
    
        output$table <- renderTable({
            break1   <- which(gres()=="break_1")[1] - 1;
            break2   <- which(gres()=="break_2")[1] - 1;
            geno_vec <- gres()[1:break1];
            geno_row <- gres()[(break1+2):break2];
            genos    <- matrix(data =  geno_vec, ncol = 10, byrow = FALSE);
            rownames(genos) <- geno_row;
            colnames(genos) <- c("CCC", "CCD", "CDC", "CDD", "DCC", "DCD", 
                                 "DDC", "DDD", "1st", "Final %");
            final_table <- genos;
            }, include.rownames=TRUE)
    
        output$plot <- renderPlot({
            break2    <- which(gres()=="break_2")[1]+1;
            end       <- length(gres());
            fitnesses <- gres()[break2:end];
            fitnesses <- as.numeric(fitnesses);
            mean_fit  <- fitnesses / (input$RnD * 100);
            maxpt     <- max(c(input$CC, input$CD, input$DC, input$DD));
            par(mar=c(5,5,1,1));
            plot(x = 1:length(mean_fit), y=mean_fit, type="l", ylim=c(0,maxpt), 
                 lwd = 3, xlab="Generation", cex.lab=1.5,  cex.axis=1.5, 
                 ylab = "Mean strategy fitness per round");
            })
    
    })

app <- shinyApp(ui, server) 
    runApp(app, launch.browser = TRUE)

}
