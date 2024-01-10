# Load necessary libraries
library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)

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