# babynames
library(babynames)

shinyUI(fluidPage( 
  
  titlePanel("Given Names in USA"), 
  
  sidebarLayout(
    sidebarPanel( width = 3,
      
      radioButtons("gender1", strong("Gender:"),
             c("Female" = "F", "Male" = "M"), inline = TRUE),
      textInput( inputId = "nameID1"
          , label = strong("Name:")
          , value = "Bonnie"
      ),
      h5(htmlOutput("stat1")),
      br(),

      radioButtons("gender2", strong("Gender:"),
             c("Female" = "F", "Male" = "M"), inline = TRUE, selected = "M"),
      textInput( inputId = "nameID2"
          , label = strong("Name:")
          , value = "Clyde"
      ),
      h5(htmlOutput("stat2")),
      br(),
      sliderInput("yearRange", "Range of Years:", 1880, 2013, c(1880, 2013)
                  , step = NULL, round = FALSE, format = "###0."
                  , locale = "us", ticks = TRUE, animate = FALSE, width = NULL),

# widgets for smoothing control
    radioButtons("smooth", strong("Smoother"),
             c("None" = "N", "LOESS" = "L", "High Pass Filter" = "B")),
    sliderInput("lospan", "LOESS Span:", 0.06, 0.5, 0.2
                  , step = NULL, round = FALSE, format = "0.00"
                  , locale = "us", ticks = TRUE, animate = FALSE, width = NULL),
    sliderInput("filtfreq", "Filter frequency:", 4, 15, 8
                  , step = NULL, round = FALSE, format = "0"
                  , locale = "us", ticks = TRUE, animate = FALSE, width = NULL)

), # end sidebarPanel

    mainPanel(
      tabsetPanel(type = "pills",
        tabPanel("Acknowledgments",
          h4(htmlOutput("acks"))
        ), # end Acknowledgments tabPanel
        tabPanel("Profiles",
          htmlOutput("error1"),
          plotOutput("namePlot", height = 380),
          plotOutput("diffPlot", height = 280)
        ), # end Profiles tabPanel
        tabPanel("Lists",
          fluidRow(
            column(4,
              radioButtons("genderL", strong("Gender:"),
                 c("Female" = "F", "Male" = "M"), inline = TRUE)
            ),
            column(8,
              sliderInput("yearRangeL",
                    strong("Range of Years:"), 1880, 2013, c(1880, 2013)
                 , step = 1, round = FALSE, format = "###0."
                 , locale = "us", ticks = TRUE, animate = FALSE, width = "400px")
            )
          ),  # end first fluidRow
          fluidRow(
            column(8, offset = 4,
              sliderInput("minPercent",
                    strong("Minimum Percent in a Year:"), 0, 1.5, 0.2
                 , step = 0.01, round = FALSE, format = "#0.00"
                 , locale = "us", ticks = TRUE, animate = FALSE, width = "400px")
            )
          ),  # end second fluidRow
          fluidRow(
            column(6, radioButtons("sortL", strong("Sort by:"),
                 c("Year & Percent" = "Y", "Percent" = "P"),
                 inline = TRUE)
            ),
            column(5, radioButtons("aggrL", strong("Aggregate by:"),
                 c("None" = "N", "Year" = "Y", "Name" = "M"), inline = TRUE)
            )
          ),  # end third fluidRow
          dataTableOutput("tabl")
        ) # end Lists tabPanel
      )  # end tabsetPanel
    ) # end mainPanel
  )
)  # end fluidPage

)  # end shinyUI