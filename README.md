# gamesGA [![](http://www.r-pkg.org/badges/version/gamesGA)](https://cran.r-project.org/package=gamesGA) [![](http://cranlogs.r-pkg.org/badges/grand-total/gamesGA)](http://cranlogs.r-pkg.org/badges/grand-total/gamesGA)

A genetic algorithm for finding adaptive strategies in sequential games characterised by two by two symmetric payoff matrices. For example, in the classic case of the [Prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner's_dilemma), the payoff matrix can be defined below for the scenario in which both players cooperate (**CC**, upper left), the focal player cooperates but the opponent defects (**CD**, upper right), the focal player defects but the opponent cooperates (**DC**, lower left), and both players defect (**DD**, lower right).

|                         | Opponent cooperates | Opponent defects |
|-------------------------|---------------------|------------------|
| Focal player cooperates |   **3, 3**          |  **0, 5**       |
| Focal player defects    |   **5, 0**         |  **1, 1**        |

Note that any *symmetrical* payoff combination is possible in `games_ga()`, as specified by `CC`, `DC`, `CD`, and `DD` (see below).

## Installation and use

**Install from CRAN**

To install [this package](https://cran.r-project.org/web/packages/gamesGA/) from CRAN.

```
install.packages("gamesGA")
```

**Install with GitHub**

To install this package from GitHub, make sure that the `devtools` library is installed.

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
games_ga(CC = 3, DC = 5, CD = 0, DD = 1)
```

## Run from your browser

The program gamesGA can also be run through a browser via [shiny](https://shiny.rstudio.com/). To do this, run the `games_ga_gui()` function.

```
games_ga_gui()
```

Alternatively, the following link redirects to a page that runs the application from the web.

> https://bradduthie.shinyapps.io/gameGA_browser/

For further assistance, contact brad.duthie@gmail.com.
