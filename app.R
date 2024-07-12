library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)

# Load Data
data <- read.csv("Indian Airlines.csv")

# Simple Linear Regression
linear_regression <- function(data) {
  lm(price ~ days_left, data = data)
}

# UI

ui <- fluidPage(
  
  titlePanel("Price Prediction Using Simple Linear Regression"),
  
  dashboardPage(
    
    dashboardHeader(title = "Price Prediction Dashboard"),
    
    dashboardSidebar(
      
      sidebarMenu(
        
        menuItem("Price Prediction", tabName = "price_prediction"),
        menuItem("Compare Airlines", tabName = "graphs"),
        menuItem("Analysis", tabName = "analysis")
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        tabItem(
          
          tabName = "price_prediction",
          
          sidebarPanel(
            
            selectInput("airline", "Airline:", choices = c("", unique(data$airline)), selected = "Indigo"),
            selectInput("source_city", "Source City:", choices = c("", unique(data$source_city)), selected = "Mumbai"),
            selectInput("departure_time", "Departure Time:", choices = c("", unique(data$departure_time)),selected = "Early Morning"),
            selectInput("destination_city", "Destination City:", choices = c("", unique(data$destination_city)), selected = "Delhi"),
            selectInput("class", "Class:", choices = c("", unique(data$class)), selected = "Economy"),
            sliderInput("days_left_range", "Days Left Range:", min = 1, max = 49, value = c(1, 14))
          ),
          
          mainPanel(
            
            h4("PREDICTED PRICE :"),
            textOutput("predicted_price")
          )
        ),
        
        tabItem(
          
          tabName = "graphs",
          
          fluidRow(
            column(12,
                   
                   fluidRow(
                     
                     column(3, selectInput("graph_source_city", "Source City:", choices = c("", unique(data$source_city)), selected = "Mumbai")),
                     column(3, selectInput("graph_departure_time", "Departure Time:", choices = c("", unique(data$departure_time)),selected = "Early Morning")),
                     column(3, selectInput("graph_destination_city", "Destination City:", choices = c("", unique(data$destination_city)), selected = "Delhi")),
                     column(3, selectInput("graph_class", "Class:", choices = c("", unique(data$class)), selected = "Economy"))
                   ),
                   
                   selectizeInput("selected_airline", "Select Airlines:", choices = unique(data$airline), selected = unique(data$airline), multiple = TRUE),
                   
                   plotlyOutput("plot")
                   
            )
          )
        ),
        
        tabItem(
          
          tabName = "analysis",
          
          sidebarLayout(
            sidebarPanel(
              
              selectInput("selected_graph", "Select Graph", 
                          choices = c("No. of flight by Airlines",
                                      "Availability of Tickets according to Class of Travel",
                                      "Distribution of Airlines in Economy Class",
                                      "Distribution of Cities Used in Business Class Tickets",
                                      "Airline ticket prices based on days left before buying the ticket",
                                      "Average price depending on duration of flight",
                                      "Price range according to Class of Travel",
                                      "Economy Vs Business Ticket Prices by Airlines",
                                      "Price of Airline tickets based on No. of Stops in Economy Class",
                                      "Price of Airline tickets based on No. of Stops in Business Class",
                                      "Price of Ticket depending on time of departure",
                                      "Price of Ticket depending on time of arrival",
                                      "Airline ticket prices based on the source and destination cities"
                                      )),
              width = 3
            ),
            
            mainPanel(
              plotOutput("analysis_plot"),
              width = 9
            )
          )
        )
      )
    )
  )
)


# Server

server <- function(input, output, session) {
  
  session$allowReconnect(TRUE)  # Allow reconnection
  
  # Predicted Price 
  
  output$predicted_price <- renderText({
    
    req(input$airline, input$source_city, input$departure_time, input$destination_city, input$class, input$days_left_range)
    
    filtered_data <- data %>%
      filter(airline == input$airline, 
             source_city == input$source_city,
             departure_time == input$departure_time, 
             destination_city == input$destination_city, 
             class == input$class,
             stops == "zero",
             days_left >= input$days_left_range[1] & days_left <= input$days_left_range[2])
    
    if (nrow(filtered_data) == 0) 
    {
      return("NO DATA AVAILABLE")
    } 
    else 
    {
      lm_model <- linear_regression(filtered_data)
      predicted_price <- predict(lm_model, newdata = data.frame(days_left = mean(input$days_left_range)))
      return(paste("Rs.", round(predicted_price, 0)))
    }
    
  })
  
  # Graph
  
  output$plot <- renderPlotly({
    
    req(input$selected_airline)
    
    plot_data <- data.frame() # Dataframe to plot graph
    
    for (selected_airline in input$selected_airline) 
    {
      airline_data <- data %>%
        filter(airline == selected_airline,
               source_city == input$graph_source_city,
               departure_time == input$graph_departure_time,
               destination_city == input$graph_destination_city,
               class == input$graph_class,
               stops == "zero")
      
      if (nrow(airline_data) > 0) 
      {
        predicteds <- numeric()
        days_left <- numeric()
        
        for (i in unique(airline_data$days_left)) 
        {
          filtered_data <- airline_data %>% filter(days_left == i)
          lm_model <- linear_regression(filtered_data)
          predicteds <- c(predicteds, predict(lm_model, newdata = data.frame(days_left = i)))
          days_left <- c(days_left, i)
        }
        
        plot_data_airline <- data.frame(days_left = days_left, predicted_price = predicteds, airline = selected_airline)
        
        plot_data <- bind_rows(plot_data, plot_data_airline)
      }
    }
    
    # If no data is available
    if (nrow(plot_data) == 0) 
    {
      return("NO DATA AVAILABLE")
    }
    
    # Plot Graph
    p <- plot_ly(plot_data,
                 x = ~days_left, y = ~predicted_price,
                 color = ~airline, type = 'scatter',
                 mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Days Left", tickmode = "linear", dtick = 5),
             yaxis = list(title = "Predicted Price"))
    
  })
  
  # Analysis Graphs
  
  output$analysis_plot <- renderPlot({
    
    if (input$selected_graph == "No. of flight by Airlines") 
    {
      ggplot(data, aes(x = airline, fill = airline)) +
        geom_bar() +
        labs(x = "Airline in India", y = "No. of flights", title = "No. of flight by Airlines") +
        theme_minimal() +
        scale_fill_manual(values = rainbow(length(unique(data$airline))))
    }
    
    else if (input$selected_graph == "Price range according to Class of Travel")
    {
      ggplot(data, aes(x = price, y = class, color = class)) +
        geom_point(position = "jitter", size = 3) +
        labs(x = "Ticket cost", y = "Class of Travel", title = "Price range according to Class of Travel") +
        theme_minimal() +
        scale_color_manual(values = rainbow(length(unique(data$class))))
    }
    
    else if (input$selected_graph == "Availability of Tickets according to Class of Travel")
    {
      ggplot(data, aes(x = class, fill = class)) +
        geom_bar() +
        labs(x = "Class of Travel", title = "Availability of Tickets according to Class of Travel") +
        theme_minimal() +
        scale_fill_manual(values = rainbow(length(unique(data$class))))
    } 
  
    else if (input$selected_graph == "Economy Vs Business Ticket Prices by Airlines")
    {
      ggplot(data, aes(x = reorder(airline, price), y = price, fill = class)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Airlines in India", y = "Price of Ticket", title = "Economy Vs Business Ticket Prices by Airlines") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    else if (input$selected_graph == "Airline ticket prices based on days left before buying the ticket")
    {
      df_temp <- data %>%
        group_by(days_left) %>%
        summarise(mean_price = mean(price))

      ggplot(df_temp, aes(x = days_left, y = mean_price)) +
        geom_point(data = subset(df_temp, days_left == 1), color = "blue") +  # Set point color for days_left == 1
        geom_smooth(data = subset(df_temp, days_left == 1), method = "lm", se = FALSE, color = "red") +  # Add linear regression line for days_left == 1
        geom_smooth(data = subset(df_temp, days_left > 1 & days_left < 20), method = "lm", se = FALSE, color = "green") +  # Add linear regression line for 1 < days_left < 20
        geom_smooth(data = subset(df_temp, days_left >= 20), method = "lm", se = FALSE, color = "orange") +  # Add linear regression line for days_left >= 20
        labs(x = "Tickets booked before X days", y = "Price of Ticket", title = "Airline ticket prices based on days left before buying the ticket")
    } 
    
    else if (input$selected_graph == "Average price depending on duration of flight")
    {
      ggplot(data, aes(x = duration, y = price)) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
        labs(x = "Duration of flight", y = "Price of Ticket", title = "Average price depending on duration of flight") +
        theme_minimal()
    }
    
    else if (input$selected_graph == "Price of Ticket depending on time of departure")
    {
      ggplot(data, aes(x = departure_time, y = price, fill = departure_time)) +
        geom_boxplot(show.legend = FALSE) +
        labs(x = "Departure Time", y = "Price of Ticket", title = "Price of Ticket depending on time of departure") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    else if (input$selected_graph == "Price of Ticket depending on time of arrival")
    {
      ggplot(data, aes(x = arrival_time, y = price, fill = arrival_time)) +
        geom_boxplot(show.legend = FALSE) +
        labs(x = "Arrival Time", y = "Price of Ticket", title = "Price of Ticket depending on time of arrival") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    else if (input$selected_graph == "Airline ticket prices based on the source and destination cities")
    {
      ggplot(data, aes(x = destination_city, y = price, color = destination_city)) +
        geom_line() +
        facet_wrap(~source_city, ncol = 3) +
        labs(x = "Destination City", y = "Price of Ticket", title = "Airline ticket prices based on the source and destination cities") +
        theme_minimal()
    }
    
    else if (input$selected_graph == "Price of Airline tickets based on No. of Stops in Economy Class")
    {
      economy_df <- subset(data, class == "Economy")
      
      ggplot() +
        geom_bar(data = economy_df, aes(x = airline, y = price, fill = stops), stat = "identity") +
        labs(x = "Airlines", y = "Price of Ticket", title = "Price of Airline tickets based on No. of Stops in Economy Class") +
        theme_minimal() +
        #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~class, ncol = 2)
    }
    
    else if (input$selected_graph == "Price of Airline tickets based on No. of Stops in Business Class")
    {
      business_df <- subset(data, class == "Business")
      
      ggplot() +
        geom_bar(data = business_df, aes(x = airline, y = price, fill = stops), stat = "identity") +
        labs(x = "Airlines", y = "Price of Ticket", title = "Price of Airline tickets based on No. of Stops in Business Class") +
        theme_minimal() +
        #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~class, ncol = 2)
    }
    
    else if (input$selected_graph == "Distribution of Airlines in Economy Class")
    {
      eco <- subset(data, class == "Economy")
      airline_counts <- table(eco$airline)
      
      ggplot(data.frame(airline = names(airline_counts), count = as.vector(airline_counts)), aes(x = "", y = count, fill = airline)) +
        geom_bar(stat = "identity") +
        coord_polar("y", start = 0) +
        labs(x = "", y = "", fill = "", title = "Distribution of Airlines in Economy Class") +
        theme_void() +
        theme(legend.position = "bottom")
    }
    
    else if (input$selected_graph == "Distribution of Cities Used in Business Class Tickets")
    {
      bus <- subset(data, class == "Business")
      source_city_counts <- table(bus$source_city)
      
      ggplot(data.frame(city = names(source_city_counts), count = as.vector(source_city_counts)), aes(x = "", y = count, fill = city)) +
        geom_bar(stat = "identity") +
        coord_polar("y", start = 0) +
        labs(x = "", y = "", fill = "", title = "Distribution of Cities Used in Business Class Tickets") +
        theme_void() +
        theme(legend.position = "bottom")
    }
    
  })
}

# Run App
shinyApp(ui = ui, server = server)
