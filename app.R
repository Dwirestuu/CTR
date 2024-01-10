# Load necessary libraries
library(shiny)
library(tidyverse)
library(DT)

# Define UI
ui <- fluidPage(
  navbarPage(
    "Ad Placement Analysis",
    tabPanel("View & Analyze Data",
             sidebarLayout(
               sidebarPanel(
                 h3("Ad Placement Analysis"),
                 p("Select an ad placement location to view CTR statistics."),
                 selectInput("ad_placement_select", "Ad Placement:", choices = NULL),
                 fileInput("file_upload", "Upload CSV File")
               ),
               tabsetPanel(
                 tabPanel("View Data",
                          mainPanel(
                            fluidRow(
                              column(
                                width = 12,
                                h3("View Data:"),
                                DTOutput("view_data"),
                                h4("CTR Statistics:"),
                                verbatimTextOutput("ctr_stats"),
                                h4("Summary Statistics:"),
                                verbatimTextOutput("summary_statistics")
                              )
                            )
                          )
                 ),
                 tabPanel("ANOVA Results",
                          mainPanel(
                            fluidRow(
                              column(
                                width = 12,
                                h4("ANOVA Results:"),
                                verbatimTextOutput("anova_results")
                              )
                            )
                          )
                 )
               )
             )
    ),
    
    tabPanel("Add New Data",
             sidebarLayout(
               sidebarPanel(
                 numericInput("left_sidebar_input", "Left Sidebar CTR:", value = 0),
                 numericInput("center_page_input", "Center Page CTR:", value = 0),
                 numericInput("right_sidebar_input", "Right Sidebar CTR:", value = 0),
                 actionButton("add_data_button", "Add Data", class = "btn-primary")
               ),
               mainPanel(
                 plotOutput("barplot"),
                 verbatimTextOutput("anova_output")
               )
             )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  user_data <- reactiveVal(data.frame())
  
  observeEvent(input$file_upload, {
    if (!is.null(input$file_upload)) {
      data(read.csv(input$file_upload$datapath))
      updateSelectInput(session, "ad_placement_select", choices = unique(data()$AdPlacement))
      
      # Display data structure
      output$data_structure <- renderPrint({
        str(data())
      })
    }
  })
  
  observeEvent(input$add_data_button, {
    left_sidebar <- isolate(input$left_sidebar_input)
    center_page <- isolate(input$center_page_input)
    right_sidebar <- isolate(input$right_sidebar_input)
    
    new_data <- data.frame(AdPlacement = c("Left Sidebar", "Center Page", "Right Sidebar"),
                           CTR = c(left_sidebar, center_page, right_sidebar))
    
    user_data(rbind(user_data(), new_data))
  })
  
  # ANOVA Results
  output$anova_results <- renderPrint({
    anova_model <- aov(CTR ~ AdPlacement, data = data())
    print(summary(anova_model))
  })
  
  # View Data - Render DataTable
  output$view_data <- renderDataTable({
    if (!is.null(data())) {
      selected_ad_placement <- filter(data(), AdPlacement == input$ad_placement_select)
      DT::datatable(selected_ad_placement)
    }
  })
  
  # CTR Statistics
  output$ctr_stats <- renderPrint({
    selected_ad_placement <- filter(data(), AdPlacement == input$ad_placement_select)
    summary(selected_ad_placement$CTR)
  })
  
  # Summary Statistics
  output$summary_statistics <- renderPrint({
    summary_stats <- summary(data())
    summary_stats
  })
  
  # Barplot
  output$barplot <- renderPlot({
    ggplot(user_data(), aes(x = AdPlacement, y = CTR, fill = AdPlacement)) +
      geom_bar(stat = "identity") +
      labs(title = "CTR Barplot by Ad Placement")
  })
  
  # ANOVA Results
  output$anova_output <- renderPrint({
    anova_model <- aov(CTR ~ AdPlacement, data = user_data())
    summary(anova_model)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
