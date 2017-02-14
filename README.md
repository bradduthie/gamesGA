# gamesGA

A genetic algorithm for finding adaptive strategies in sequential games characterised by two by two symmetric payoff matrices. For example, in the classic case of the [Prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner's_dilemma), the payoff matrix can be defined below for the scenario in which both players cooperate (**CC**, upper left), the focal player cooperates but the opponent defects (**CD**, upper right), the focal player defects but the opponent cooperates (**DC**, lower left), and both players defect (**DD**, lower right).

|                         | Opponent cooperates | Opponent defects |
|-------------------------|---------------------|------------------|
| Focal player cooperates |   **3, 3**          |  **-1, 4**       |
| Focal player defects    |   **4, -1**         |  **0, 0**        |

Note that any *symmetrical* payoff combination is possible in `games_ga()`, as specified by `CC`, `DC`, `CD`, and `DD` (see below).

## Installation and use

To install this package, make sure that the `devtools` library is installed.

```
install.packages("devtools")
library(devtools)
```

Use `install_github` to install using `devtools`.

```
install_github("bradduthie/gamesGA")
```

**Example usage:**

To run the genetic algorithm and find strategies that evolve in a classical [Prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner's_dilemma) scenario, run the following:

```
games_ga(CC = 3, DC = 4, CD = -1, DD = 0)
```

## Run from your browser

The program gamesGA can also be run through a browser via [shiny](https://shiny.rstudio.com/). To do this, simply click the link below, which will redirect to a page that runs the application from the web.

> https://bradduthie.shinyapps.io/gameGA_browser/

For further assistance, contact brad.duthie@gmail.com.