###############################################
# HW #6                               11/3/21
#
# Shrey Patel                         Data 607
###############################################




#install.packages("shiny")
#install.packages("shinythemes")

packageVersion("shiny")

library(shiny)
library(shinythemes)

##### Loading data set #####

#wines <- read.csv("wine_data/wine.data", header=FALSE)
wines <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header=FALSE)

# assigning column names
colnames(wines) <- c("Class","Alcohol","Malic acid","Ash","Alcalinity of ash",
                     "Magnesium","Total phenols","Flavanoids",
                     "Nonflavanoid phenols","Proanthocyanins","Color intensity",
                     "Hue","OD280/OD315 of diluted wines","Proline")

# list of independent variables
features <- setdiff(names(wines),"Class")


##### Shiny app #####

# defining the user interface (front-end)
ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  # Application title
  navbarPage("K-means Clustering Using Wine Data"),
  
  p("This shiny app creates and displays three clusters of wines based on a combination of chemical characteristics provided as the input."),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      # input functions
      strong(p("Provide the inputs below: ")),
      selectInput("xcol", label = "X variable", features),
      selectInput("ycol", label = "Y variable", features, selected = features[[2]])
    ),
    
    mainPanel(
      # output functions in tab layout
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      ),
    
    # link to the source  
    tags$a(href="https://archive.ics.uci.edu/ml/datasets/Wine", "Data set source"),
    )
  )
)


# defining the behavior of the app (back-end)
server <- function(input, output, session) {
  
  #  creating reactive expression(s)
  dataset <- reactive({
    wines[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(dataset(), 3)
  })
  
  # recipe for "summary" output - renderPrint produces text
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  # recipe for "table" output - renderTable produces a table
  output$table <- renderTable({
    dataset()
  })

  # recipe for plotting clusters
  output$plot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A"))
    plot(dataset(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}


# running the app using defined ui and behavior
shinyApp(ui = ui, server = server)


