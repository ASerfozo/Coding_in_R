# Wine Browser application

library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)

shinyServer(function(input, output, session) {
    
    # Reset filters button ----------------------------------------------------
    
    observeEvent(input$button_reset,{
        updateCheckboxGroupInput(session, "wine_type", selected = wine_types)
        updatePickerInput(session, "country", selected = countries)
        updateSliderInput(session, "price_range", value = c(0, ceiling(max(wine_data$Price))))
        updateSliderInput(session, "rating_range", value = c(1,5))
        updateSliderInput(session, "year_range", value = c(1960,2021))
    })
    

    # Generate reactive data --------------------------------------------------

    my_reactive_df <- reactive({
        df<- get_data_by_year(input$wine_type, input$country, input$price_range[1], input$price_range[2], 
                              input$rating_range[1], input$rating_range[2], input$year_range[1], input$year_range[2])
        return(df)
    })
    
    # Elements of UI ----------------------------------------------------------
    
    ### Info boxes
    output$nr_infobox <- renderInfoBox({
        infoBox(
            title = 'Number of Wines',
            value = h3(nrow(my_reactive_df())),
            icon = icon("wine-glass-alt", lib = "font-awesome"),
            color = "teal"
        )
    })
    
    output$price_infobox <- renderInfoBox({
        infoBox(
            title = 'Average Price',
            value = h3(paste0(round(mean(my_reactive_df()$Price),2)," €")),
            icon = icon("tags", lib = "font-awesome"),
            color = "olive"
        )
    })
    
    output$nr_rating_infobox <- renderInfoBox({
        infoBox(
            title = 'Usual Number of Ratings',
            value = h3(median(my_reactive_df()$NumberOfRating)),
            icon = icon("star", lib = "font-awesome"),
            color = "yellow"
        )
    })
    
    
    ### Plots
    output$type_rating_plot <- renderPlotly({
        ggplot(my_reactive_df(), aes(factor(Type),Rating)) +
            geom_boxplot(color = "black", fill = c("red3"), alpha = 0.5) +
            labs(title ="Distribution of wine Rating by Type of wine", x = "Type of wine", y = 'Rating')+
            theme_bw()
    })
    
    output$rating_plot <- renderPlotly({
        ggplot( my_reactive_df() , aes(x = Rating)) +
            geom_bar(fill='red3', col="black", alpha=0.5) +
            labs(y="Frequency", x = "Ratings", title="Distribution of Ratings")+
            theme_bw()
    })
    
    ### Did not include it as it significantly slows down the application
    # output$price_rating_plot <- renderPlotly({
    #     ggplot(my_reactive_df(), aes(y=Rating, x=lnPrice))+
    #         geom_point() +
    #         geom_smooth(method = 'lm',color='red3', formula = y ~ x, alpha = 0.7) +
    #         labs(title ="Distribution of wine Log Price and Rating", x = "Price", y = 'Rating')+
    #         theme_bw()
    # })
    
    output$year_price_plot <- renderPlotly({
        get_boxplot(my_reactive_df(),my_reactive_df()$Year,log(my_reactive_df()$Price))+
            labs(title ="Distribution of Log wine Prices by Year", x = "Year", y = 'Log price in EUR')
    })
    
    output$country_rating_plot <- renderPlotly({
        get_boxplot(my_reactive_df(),my_reactive_df()$Country,my_reactive_df()$Rating) +
            labs(title ="Distribution of wine Rating by Country", x = "Country", y = 'Rating')
    })
    
    
    ### Data table from htmlwidgets
    output$my_data <- DT::renderDataTable({
        my_reactive_df()
        # )
    })
    
})
