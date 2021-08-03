#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Load Libraries
library(shiny)
library(tidyverse)
library(ggrepel)

library(rsconnect)
library(shinythemes)
library(directlabels)


# Load and Merge Data


all_players <- read_csv('all_nba_stats.csv')
all_players <- all_players %>% select(-X1, -slugSeason, -nameSchool, -idOrganization, isRookie, idTeam)
stat_choices <- c(
  "Points" = "pts",
  "Total Rebounds" = "treb",
  "Assists" = "ast",
  "Steals" = "stl",
  "Blocks" = "blk",
  "Games Played" = "gp",
  "Games Started" = "gs",
  "Field Goals Made" = "fgm",
  "Field Goals Attempted" = "fga",
  "Field Goal Percentage" = "pctFG",
  "3 Point Field Goals Made" = "fg3m",
  "3 Point Field Goals Attempted" = "fg3a",
  "3 Point Field Goal Percentage" = "pctFG3",
  "Free Throw Percentage" = "pctFT",
  "2 Point Field Goals Made" = "fg2m",
  "2 Point Field Goals Attempted" = "fg2a",
  "2 Point Field Goal Percentage" = "pctFG2",
  "Minutes Played" = "minutes",
  "Free Throws Made" = "ftm",
  "Free Throws Attempted" = "fta",
  "Offensive Rebounds" = "oreb",
  "Defensive Rebounds" = "dreb",
  "Turnovers" = "tov",
  "Personal Fouls" = "pf"
  
)

time_choices <- c("Number of Seasons Played" = "numberPlayerSeason", 
                  "Player Age" = "agePlayer", 
                  "Year" = "year")

career_choices <- c("Career Points per Game" = "c_ppg", 
                    "Career Assists per Game" ="c_ast", 
                    "Career Rebounds per Game" = "c_treb", 
                    "Career Steals per Game" = "c_stl",
                    "Career Blocks per Game" = "c_blk")

rank_choices <- c("Points Rank" = "ptsrank", 
                  "Assists Rank" = "astrank", 
                  "Rebounds Rank" = "trebrank", 
                  "Steals Rank" = "stlrank", 
                  "Blocks Rank" = "blkrank",
                  "Field Goal Percentage Rank" = "pctFGrank",
                  "Minutes Rank" = "minrank")

all_players <- all_players %>% mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
                                    group_by(year, playerName) %>% summarise(fgm = sum(fgm*gp)/sum(gp),
                                                                        fga = sum(fga*gp)/sum(gp),
                                                                        pctFG = sum(pctFG*gp)/sum(gp),
                                                                        fg3m = sum(fg3m*gp)/sum(gp),
                                                                        fg3a = sum(fg3a*gp)/sum(gp),
                                                                        fg2m = sum(fg2m*gp)/sum(gp),
                                                                        fg2a = sum(fg2a*gp)/sum(gp),
                                                                        pctFG3 = sum(pctFG3*gp)/sum(gp),
                                                                        pctFG2 = sum(pctFG2*gp)/sum(gp),
                                                                        pctFT = sum(pctFT*gp)/sum(gp),
                                                                        agePlayer = max(agePlayer),
                                                                        minutes = sum(minutes*gp)/sum(gp),
                                                                        ftm = sum(ftm*gp)/sum(gp),
                                                                        fta = sum(fta*gp)/sum(gp),
                                                                        oreb = sum(oreb*gp)/sum(gp),
                                                                        dreb = sum(dreb*gp)/sum(gp),
                                                                        treb = sum(treb*gp)/sum(gp),
                                                                        ast = sum(ast*gp)/sum(gp),
                                                                        stl = sum(stl*gp)/sum(gp),
                                                                        blk = sum(blk*gp)/sum(gp),
                                                                        tov = sum(tov*gp)/sum(gp),
                                                                        pf = sum(pf*gp)/sum(gp),
                                                                        pts = sum(pts*gp)/sum(gp),
                                                                        gp = sum(gp),
                                                                        gs = sum(gs), 
                                                                        numberPlayerSeason = max(numberPlayerSeason)
                                                                        )

all_players[all_players == 0] <- 0
all_players <- all_players %>% mutate(gp = ifelse(gp > 82, round(gp/2), gp),
                                      gs = ifelse(gs > 82, round(gs/2), gs))


hw <- read_csv('all_seasons.csv')
teams <- hw %>% select(player_name, team_abbreviation)
hw <- hw %>% group_by(player_name) %>% summarise(Height = mean(player_height),
                                                 Weight = mean(player_weight))

career_avg <-  all_players %>% mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
                               group_by(playerName) %>% summarise(c_ppg = sum(pts*gp)/sum(gp),
                                                                   c_ast = sum(ast*gp)/sum(gp),
                                                                   c_treb = sum(treb*gp)/sum(gp),
                                                                   c_stl = sum(stl*gp)/sum(gp),
                                                                   c_blk = sum(blk*gp)/sum(gp),
                                                                   c_pctFG = sum(pctFG*gp)/sum(gp))
hw_cavg <- inner_join(hw, career_avg, by=c("player_name" = "playerName"))

mj <- all_players
mj <- mj[1:4,]
mj[TRUE] <- NA
mj$year[1] = 1994
mj$year[2] = 1999
mj$year[3] = 2000
mj$year[4] = 2001
mj$agePlayer[1] = 30
mj$agePlayer[2] = 35
mj$agePlayer[3] = 36
mj$agePlayer[4] = 37
mj$playerName = "Michael Jordan"

all_players <- rbind(all_players, mj)


my_theme = theme_bw() + 
  theme(text = element_text(family = "Helvetica", size = 18),
        plot.title.position = "plot",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"))




names_df <- all_players %>% group_by(playerName) %>% summarise(total_pts = round(sum(pts*gp)))
player_names <- unique(names_df[order(names_df$total_pts, decreasing=TRUE),])$playerName




totals <- all_players %>% mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
                          mutate(season_pts = pts*gp, season_ast = ast*gp, season_treb = treb*gp, 
                                 season_stl = stl*gp, season_blk = blk*gp, season_fgm = fgm*gp,
                                 season_fga = fga*gp, season_fg3m = fg3m*gp, season_fg3a = fg3a*gp,
                                 season_fg2m = fg2m*gp, season_fg2a = fg2a*gp, season_gp = gp, season_gs = gs,
                                 season_ftm = ftm*gp, season_fta = fta*gp, season_oreb = oreb*gp, 
                                 season_dreb = dreb*gp, season_tov = tov*gp, season_pf = pf*gp,
                                 season_pctFG = pctFG*gp, season_pctFT = pctFT*gp, season_pctFG2 = pctFG2*gp, 
                                 season_pctFG3 = pctFG3*gp, season_minutes = minutes*gp
                                 ) %>%
                          group_by(playerName) %>% summarise(pts = round(sum(season_pts)),
                                                             ast = round(sum(season_ast)),
                                                             treb = round(sum(season_treb)),
                                                             stl = round(sum(season_stl)),
                                                             blk = round(sum(season_blk)),
                                                             gp = sum(season_gp),
                                                             gs = sum(season_gs),
                                                             fgm = round(sum(season_fgm)),
                                                             fga = round(sum(season_fga)),
                                                             fg3m = round(sum(season_fg3m)),
                                                             fg3a = round(sum(season_fg3a)),
                                                             fg2m = round(sum(season_fg2m)),
                                                             fg2a = round(sum(season_fg2a)),
                                                             minutes = round(sum(season_minutes)),
                                                             fta = round(sum(season_fta)),
                                                             ftm = round(sum(season_ftm)),
                                                             oreb = round(sum(season_oreb)),
                                                             dreb = round(sum(season_dreb)),
                                                             tov = round(sum(season_tov)),
                                                             pf = round(sum(season_pf)),
                                                             pctFG = sum(season_pctFG)/sum(season_gp),
                                                             pctFT = sum(season_pctFT)/sum(season_gp),
                                                             pctFG2 = sum(season_pctFG2)/sum(season_gp),
                                                             pctFG3 = sum(season_pctFG3)/sum(season_gp)
                                                            )

ranks <- all_players %>% group_by(year) %>% mutate(ptsrank = order(order(year, pts, decreasing=TRUE)), 
                                                   astrank = order(order(year, ast, decreasing=TRUE)), 
                                                   trebrank = order(order(year, treb, decreasing=TRUE)),
                                                   pctFGrank = order(order(year, pctFG, decreasing=TRUE)),
                                                   blkrank = order(order(year, blk, decreasing=TRUE)),
                                                   stlrank = order(order(year, stl, decreasing=TRUE)),
                                                   minrank = order(order(year, minutes, decreasing=TRUE)))


ranks <- ranks %>% mutate(ptsrank = ifelse(pts == 0, NA, ptsrank),
                          astrank = ifelse(ast == 0, NA, astrank),
                          trebrank = ifelse(treb == 0, NA, trebrank),
                          pctFGrank = ifelse(pctFG == 0, NA, pctFGrank),
                          blkrank = ifelse(blk == 0, NA, blkrank),
                          stlrank = ifelse(stl == 0, NA, stlrank),
                          minrank = ifelse(minutes == 0, NA, minrank))


# Define UI 
ui <-  navbarPage("NBA Stats Explorer",
                  tabPanel("Overview",
                           fluidPage(
                             h1("Overview of NBA Stats Explorer"),
                             mainPanel(
                               htmlOutput(outputId = "overview")
                              
                             ),
                             HTML("<div style = 'position: absolute; bottom: 0; right: 0; padding: 5px'>Author: Seth Keim</div>")
                             )
                           )
                           ,
                  tabPanel("Per Game Season Stats", 
                           fluidPage(theme = shinytheme('flatly'),
                                     
                                     
                                     
                                     
                                     # Sidebar layout with a input and output definitions
                                     sidebarLayout(
                                       # Inputs: Select variables to plot
                                       sidebarPanel(
                                         
                                         selectizeInput(inputId = "players1",
                                                        label = "Players: ",
                                                        choices = player_names,
                                                        selected = c("LeBron James", "Michael Jordan"),
                                                        multiple = TRUE,
                                                        options = list(maxItems = 8)),
                                         selectInput(inputId = "stat",
                                                     label = "Stat: ", 
                                                     choices = stat_choices,
                                                     selected = "pts",
                                                     multiple = FALSE),
                                         selectInput(inputId = "time", 
                                                     label = "Time: ",
                                                     choices = time_choices)
                                       ),
                                       
                                       #Output
                                       
                                       mainPanel(
                                         
                                         # Show scatterplot
                                         plotOutput(outputId = "scatterplot"),
                                         br(),        # a little bit of visual separation
                                         
                                       )
                                     )
                           ))
                  ,
                  tabPanel("Careet Total Stats", fluidPage(theme = shinytheme('flatly'),




                                                          # Sidebar layout with a input and output definitions
                                                          sidebarLayout(
                                                            # Inputs: Select variables to plot
                                                            sidebarPanel(

                                                              selectizeInput(inputId = "players2",
                                                                             label = "Players: ",
                                                                             choices = player_names,
                                                                             selected = c("LeBron James", "Michael Jordan"),
                                                                             multiple = TRUE,
                                                                             options = list(maxItems = 8)),
                                                              selectInput(inputId = "total_stat",
                                                                          label = "Stat: ",
                                                                          choices = stat_choices,
                                                                          selected = "pts",
                                                                          multiple = FALSE)
                                                            ),

                                                            #Output

                                                            mainPanel(

                                                              # Show scatterplot
                                                              plotOutput(outputId = "total_plot"),
                                                              br(),        # a little bit of visual separation

                                                            )
                                                          )
                                                        )
                  ),
                  tabPanel("Rank per year in Stats", fluidPage(theme = shinytheme('flatly'),
                                                               # Sidebar layout with a input and output definitions
                                                               sidebarLayout(
                                                                 # Inputs: Select variables to plot
                                                                 sidebarPanel(
                                                                   
                                                                   selectizeInput(inputId = "players3",
                                                                                  label = "Players: ",
                                                                                  choices = player_names,
                                                                                  selected = c("LeBron James", "Michael Jordan"),
                                                                                  multiple = TRUE,
                                                                                  options = list(maxItems = 8)),
                                                                   selectInput(inputId = "rank_stat",
                                                                               label = "Stat: ",
                                                                               choices = rank_choices,
                                                                               selected = "ptsrank",
                                                                               multiple = FALSE),
                                                                   selectInput(inputId = "time2", 
                                                                               label = "Time: ",
                                                                               choices = time_choices)
                                                                   # ,
                                                                   # sliderInput(inputId = "y_max",
                                                                   #             label = "Y-axis Limit: ",
                                                                   #             min = 5, max = 500,
                                                                   #             value = 25, step = 5)
                                                                 ),
                                                                 
                                                                 #Output
                                                                 
                                                                 mainPanel(
                                                                   
                                                                   # Show scatterplot
                                                                   plotOutput(outputId = "rank_plot"),
                                                                   br(),        
                                                                   
                                                                 )
                                                               )
                    
                                                     )
                    
                  ),
                  tabPanel("Height/Weight Relationship with Stats", 
                           fluidPage(theme = shinytheme('flatly'),
                                     sidebarLayout(
                                       # Inputs: Select variables to plot
                                       sidebarPanel(
                                         
                                         
                                         selectInput(inputId = "career_stat",
                                                     label = "Stat: ",
                                                     choices = career_choices,
                                                     selected = "c_ppg",
                                                     multiple = FALSE),
                                         selectInput(inputId = "phys", 
                                                     label = "Physical Measurement: ",
                                                     choices = c("Height", "Weight"))
                                         
                                       ),
                                       
                                       #Output
                                       
                                       mainPanel(
                                         
                                         # Show scatterplot
                                         plotOutput(outputId = "Height"),
                                         br(),   
                                         verbatimTextOutput(outputId = "sum")
                                         # verbatimTextOutput(outputId = "sum")
                                         # htmlOutput(outputId = "linear")
                                         
                                       )
                                     )
                                     )
                    
                  ),
                  tabPanel("Animation of Career Total Points",
                           fluidPage(
                             img(src="cumulative_pts.gif", align = "left",height='400px',width='500px')
                           )),
                  tabPanel("Data Source",
                           fluidPage(
                             uiOutput(outputId = "source")
                           ))
)
  
  

# Define server function --------------------------------------------
server <- function(input, output) {
  
  output$overview <- renderUI({
    "This shiny app is dedicated to providing National Basketball Association Statistics on every player that 
    has played in the league. 
    Each tab has a graph that allows you to choose which variables to plot. The first graph shows statistics 
    per game for each player that you select. This graph allows comparison of players' performance in these
    stats over time. The second graph shows career total statistics for players. The third graph shows the players'
    rank in the league for specific per game stats each year. This allows comparison of the dominance of players 
    in their respective leagues. The fourth graph shows physical attributes and stats. It plots a
    linear model to find a relationship between the variable.  For example, taller players typically have more blocks.
    The final chart is not interactive, but instead it is an animation of the cumulative scoring of the top 8 total scorers
    of all time in the NBA. One of the biggest debates in the NBA has been: 'Who is the Greatest of All Time (GOAT)? 
    There may not be a definitive answer, but check out these charts to see if you can come to your own conclusion."
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    

    
    ggplot(all_players %>% filter(playerName %in% input$players1), mapping = aes_string(x = input$time, y = input$stat, color = "playerName")) + 
      geom_line() +
      geom_dl(mapping = aes(label = playerName), method = "last.points") + 
      scale_color_brewer(palette="Set2") + 
      # scale_y_continuous(limits = c(0, 40),
      #                    breaks = c(0, 10, 20, 30, 40), 
      #                    labels = c("0", "10", "20", "30", "40 pts/\ngame")) + 
      ylim(0, NA) +  
      my_theme + 
      labs(title = paste(names(stat_choices)[stat_choices == input$stat], " Per Game Over Time", sep=""), color = "Player") +
      theme(axis.title.y = element_blank(),
            legend.position = "none") + 
      xlab(paste(names(time_choices)[time_choices == input$time])) + 
      xlim(ifelse(input$time == "agePlayer", 17, ifelse(input$time == "year", 1947, 0)), ifelse(input$time == "agePlayer", 50, ifelse(input$time == "year", 2030, 25)))
    
    
    
  })
  
  
  output$total_plot <- renderPlot({
    
    
    
    p2 <- ggplot(totals %>% filter(playerName %in% input$players2), aes_string(x = "playerName", y = input$total_stat, fill = "playerName")) + 
      geom_bar(stat="identity") + 
      scale_fill_brewer(palette="Set2") + 
      my_theme + 
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank()) + 
      #       axis.text.x = element_blank(),
      #       axis.title.x = element_blank(),
      #       axis.title.y = element_blank())+ 
      labs(title = paste("Career Total", names(stat_choices)[stat_choices == input$total_stat])) + 
      coord_flip() 
    
    p2
    
    
    
  })
  
  output$rank_plot <- renderPlot({
    

    
    p3 <- ggplot(ranks %>% filter(playerName %in% input$players3), mapping = aes_string(x = input$time2, y = input$rank_stat, color = "playerName")) + 
      geom_line() + 
      geom_dl(mapping = aes(label = playerName), method = "last.points") + 
      geom_point() + 
      scale_color_brewer(palette="Set2") +
      scale_y_reverse() +
      my_theme + 
      theme(legend.position = "none",
            axis.title.y = element_blank()) + 
      labs(title = paste(names(rank_choices)[rank_choices == input$rank_stat], "Over Time")) + 
      xlab(paste(names(time_choices)[time_choices == input$time2])) + 
      xlim(ifelse(input$time2 == "agePlayer", 17, ifelse(input$time2 == "year", 1947, 0)), ifelse(input$time2 == "agePlayer", 50, ifelse(input$time2 == "year", 2030, 25)))

 
    p3
    
    
    
  })
  
  output$Height <- renderPlot({

    
    
    p4 <- ggplot(hw_cavg, aes_string(x = input$phys, y = input$career_stat)) +
      geom_jitter(height = NULL, width = 1, color ="#66C2A4") +  
      geom_smooth(method = "lm", se = FALSE, color = "#FC8E62") +
      ylim(0, NA) +
      my_theme + 
      labs(title = paste(names(career_choices)[career_choices == input$career_stat], "Relationship with", input$phys)) +
      ylab(paste(names(career_choices)[career_choices == input$career_stat])) 
    
    p4
  })
  
  lm1 <- reactive({summary(lm(reformulate(input$phys, input$career_stat), data = hw_cavg))})
  
  output$sum <- renderText({ 
    lm1 <- reactive({summary(lm(reformulate(input$phys, input$career_stat), data = hw_cavg))})
    intercept <- round(lm1()$coefficients[1,1], digits = 3)
    coef <- round(lm1()$coefficients[2,1], digits = 3)
    r <- round(lm1()$r.squared, digits = 3)
    paste("Equation: ", names(career_choices)[career_choices == input$career_stat], " = ", coef, " * ", input$phys, " + ", intercept, "\nR-squared: ", r, sep="")
    
    
  })
  
  
  
  
  
  output$linear <- renderUI({
    
    lm1 <- reactive({lm(reformulate(input$phys, input$career_stat), data = hw_cavg)})
    
    HTML(tab_model(lm1()))
    
  })
  
  url <- a("Basketball Reference", href="https://www.basketball-reference.com/")
  url2 <- a("Kaggle", href="https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv")
  url3 <- a("nbastatR", href="https://github.com/abresler/nbastatR")
  
  output$source <- renderUI({
    tagList("Most data from this website was obtained using the ", url3, " package in R, which pulls 
    data from ", url, ". The height and weight data was obtained from ", url2, ".", sep = "")
  })
  
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui, server)



