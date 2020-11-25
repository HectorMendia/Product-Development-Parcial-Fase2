
dataset <-
  read.csv('bank_churn_db.csv',
           stringsAsFactors = F,
           header = T)

server <- function(input, output) {
  output$output_range_age <- renderUI({
    min_age <- dataset %>% summarise(value = min(Age), .groups = 'drop')
    max_age <-
      dataset %>% summarise(value = max(Age), .groups = 'drop')
    sliderInput(
      "slide_range_age",
      "Select a range age:",
      min = min_age$value,
      max = max_age$value,
      value = c(40, 60)
    )
  })
  
  output$output_list_gender <- renderUI({
    list_gender <- dataset %>% distinct(Gender)
    items_gender <- as.character(list_gender$Gender)
    radioButtons("rdb_list_gender",
                 "Select a gender:",
                 choices = items_gender)
  })
  
  output$output_list_number_prods <- renderUI({
    count_prods <- dataset %>% distinct(NumOfProducts) %>% nrow()
    min_count_prods <-
      dataset %>% summarise(value = min(NumOfProducts), .groups = 'drop')
    max_count_prods <-
      dataset %>% summarise(value = max(NumOfProducts), .groups = 'drop')
    numericInput(
      inputId = "num_list_prods",
      label = "Number of products:",
      value = count_prods,
      min = min_count_prods$value,
      max = max_count_prods$value
    )
  })
  
  output$output_list_country <- renderUI({
    list_country <- dataset %>% distinct(Geography)
    items_country <- as.character(list_country$Geography)
    checkboxGroupInput("chk_list_country",
                       "Select a country:",
                       choices = items_country)
  })
  
  observe({
    checked_country <- input$chk_list_country
    selected_range <- input$slide_range_age
    selected_gender <- input$rdb_list_gender
    selected_number_prod <- input$num_list_prods
    count_exited <- 0
    total_balance <- 0
    count_active <- 0
    
    if (!is.null(selected_range) &
        !is.null(selected_gender) & !is.null(selected_number_prod)) {
      count_exited <-
        dataset %>% filter(Age >= selected_range[1] &
                             Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod) %>% nrow()
      total_balance <-
        dataset %>% filter(Age >= selected_range[1] &
                             Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod) %>%  summarise(value = sum(Balance), .groups = 'drop')
      total_balance <- total_balance$value
      count_active <-
        dataset %>% filter(Age >= selected_range[1] &
                             Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod) %>% filter(IsActiveMember == 1) %>% nrow()
      
    }
    
    if (!is.null(checked_country) &
        !is.null(selected_range) &
        !is.null(selected_gender) & !is.null(selected_number_prod)) {
      count_exited <-
        dataset %>% filter(Geography %in% checked_country) %>% filter(Age >=
                                                                        selected_range[1] &
                                                                        Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod) %>% filter(Exited == 1) %>% nrow()
      total_balance <-
        dataset %>% filter(Geography %in% checked_country) %>% filter(Age >=
                                                                        selected_range[1] &
                                                                        Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod) %>% summarise(value = sum(Balance), .groups = 'drop')
      count_active <-
        dataset %>% filter(Geography %in% checked_country) %>% filter(Age >=
                                                                        selected_range[1] &
                                                                        Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod) %>% filter(IsActiveMember == 1) %>% nrow()
      
      
    }
    
    output$output_exited <- renderValueBox({
      valueBox(
        formatC(count_exited, format = "d", big.mark = ',')
        ,
        paste('Count of exited')
        ,
        icon = icon("share-alt", lib = 'glyphicon')
        ,
        color = "purple"
      )
      
    })
    
    output$output_balance <- renderValueBox({
      valueBox(
        prettyNum(total_balance, big.mark = ',')
        ,
        'Total balance'
        ,
        icon = icon("usd", lib = 'glyphicon')
        ,
        color = "green"
      )
      
    })
    
    output$output_active <- renderValueBox({
      valueBox(
        formatC(count_active, format = "d", big.mark = ',')
        ,
        paste('Count of active members')
        ,
        icon = icon("ok", lib = 'glyphicon')
        ,
        color = "yellow"
      )
      
    })
  })
  
  data_plot <- reactive({
    checked_country <- input$chk_list_country
    selected_range <- input$slide_range_age
    selected_gender <- input$rdb_list_gender
    selected_number_prod <- input$num_list_prods
    
    if (!is.null(selected_range) &
        !is.null(selected_gender) & !is.null(selected_number_prod)) {
      data_graph <-
        dataset %>% filter(Age >= selected_range[1] &
                             Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod)
    }
    if (!is.null(checked_country) &
        !is.null(selected_range) &
        !is.null(selected_gender) & !is.null(selected_number_prod)) {
      data_graph <-
        dataset %>% filter(Geography %in% checked_country) %>% filter(Age >= selected_range[1] &
                                                                        Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod)
    }
    else{
      if (is.null(checked_country) &
          !is.null(selected_range) &
          !is.null(selected_gender) & !is.null(selected_number_prod)) {
        data_graph <-
          dataset  %>% filter(Age >= selected_range[1] &
                                Age <= selected_range[2]) %>% filter(Gender == selected_gender) %>% filter(NumOfProducts == selected_number_prod)
      }
      else{
        data_graph <-
          dataset %>% filter(Age >= 40 &
                               Age <= 60) %>% filter(Gender == "Female") %>% filter(NumOfProducts == 4)
      }
    }
    
  })
  
  output$render_plot <- renderPlot({
    ggplot(data = data_plot(),
           aes(x = Age, y = EstimatedSalary)) +
      geom_point(aes(colour = factor(Geography))) +
      labs(colour = "Geography")
  })
  
  output$render_box_plot <- renderPlot({
    ggplot(data = data_plot(),
           aes(
             x = Gender,
             y = Balance,
             fill = Geography,
             group = 1
           )) +
      geom_boxplot() +
      facet_wrap( ~ Geography)
  })
  
  
  #creating the plotOutput content
  
  # output$revenuebyPrd <- renderPlot({
  #     ggplot(data = recommendation,
  #            aes(x=Product, y=Revenue, fill=factor(Region))) +
  #         geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") +
  #         xlab("Product") + theme(legend.position="bottom"
  #                                 ,plot.title = element_text(size=15, face="bold")) +
  #         ggtitle("Revenue by Product") + labs(fill = "Region")
  # })
  #
  #
  # output$revenuebyRegion <- renderPlot({
  #     ggplot(data = recommendation,
  #            aes(x=Account, y=Revenue, fill=factor(Region))) +
  #         geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") +
  #         xlab("Account") + theme(legend.position="bottom"
  #                                 ,plot.title = element_text(size=15, face="bold")) +
  #         ggtitle("Revenue by Region") + labs(fill = "Region")
  # })
  
  
  
}
