###############################################
# Final Project                       11/3/21
#
# Shrey Patel                         Data 607
###############################################




#install.packages("shiny")
#install.packages("shinythemes")

packageVersion("shiny")

library(shiny)
library(shinythemes)

##### Loading data set #####

sales <- read.csv("rollingsales_manhattan.csv", skip=5, header=FALSE)
colnames(sales) <- c("borough", "neighborhood", "building_class_category", "tax_class_at_present", "block", "lot", "easement", 
                     "building_class_at_present", "address", "apartment_number", "zipcode", "residential_units", "commercial_units", 
                     "total_units", "land_sqft", "gross_sqft", "year_built", "tax_class_at_sale", "building_class_at_sale",
                     "sale_price","sale_date")

# list of independent variables
features <- names(sales)


##### Shiny app #####

# defining the user interface (front-end)
ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  # Application title
  navbarPage("K-means Clustering Using Rolling Sales Data of Manhattan, NY"),
  
  p("This shiny app creates and displays clusters of sales based on a combination of variables provided as the input."),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      # input functions
      strong(p("Provide the inputs below: ")),
      selectInput("xcol", label = "X variable", features),
      selectInput("ycol", label = "Y variable", features, selected = features[[2]]),
      sliderInput("clusters", "Number of clusters:", min = 2, max = 6, value=3, round=TRUE)
    ),
    
    mainPanel(
      # output functions in tab layout
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      ),
      
      # link to the source  
      tags$a(href="https://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page", "Data set source"),
    )
  )
)


# defining the behavior of the app (back-end)
server <- function(input, output, session) {
  
  #  creating reactive expression(s)
  dataset <- reactive({
    sales[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(dataset(), input$clusters)
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


