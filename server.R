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