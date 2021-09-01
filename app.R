library(bslib)
library(plotly)
library(dplyr)
library(ggplot2)
library(shiny)

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "yeti"),
    titlePanel("Covid-19 vaccine doses administered in Germany"),
    br(),
    sidebarLayout(
        sidebarPanel("",
            selectInput("bund","Select a state:",
                        c("Baden-Wuerttemberg"="BW",
                        "Bavaria" = "BY",
                        "Berlin"="BE",
                        "Brandenburg"= "BB",
                        "Bremen (Hansestadt)"="HB",
                        "Hamburg (Hansestadt)"= "HH",
                        "Hesse"= "HE",
                        "Mecklenburg-Western Pomerania"="MV",
                        "Lower Saxony"="NI",
                        "North Rhine-Westphalia"="NW",
                        "Rhineland-Palatinate"= "RP",
                        "Saarland"="SL",
                        "Saxony"="SN",
                        "Saxony-Anhalt"= "ST",
                        "Schleswig-Holstein"="SH",
                        "Thuringia"="TH",
                        "Federal Department" =  "Bundesressort")
                        ),
            br(),
            dateRangeInput('dateRange',
                           label = 'Period (format: yyyy-mm-dd)',
                           start = "2020-12-27",
                           end = Sys.Date(),
                           min = "2020-12-27",
                           max = Sys.Date(),
                           language = "en",
                           separator = "to"
                           ),
            br(),
            submitButton("Update View", icon("refresh")),
            hr(),
            p("The graphics are created using the R package plotly. This makes it possible to display the graphics interactively and dynamically."),
            hr(),
            p(strong("References:")),
            a(href="https://plotly-r.com", "R-Package plotly"),
            br(),
            a(href="https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland", "RKI data source (Github)")
        ),
            
        mainPanel(
            p(strong("Vaccinations by vaccine manufacturer per state")),
            plotlyOutput("plot"),
            p(strong("Vaccination development in Germany by age group since December 27, 2021")),
            plotlyOutput("plot2")
          )
            
        )
    )




server <- function(input, output) {
    load("vaccinetype_state.rda")
    output$plot <- renderPlotly({
        data_temp <- data_bund %>% 
                            filter(Bundesland==input$bund) %>%  
                                 group_by(Impfdatum,Impfstoff) %>%
                                    summarise(Dosen=sum(Anzahl))
        ggplotly(ggplot(data_temp, aes(Impfdatum,Dosen, colour = Impfstoff)) +  
              geom_line() + 
              facet_wrap (~Impfstoff) +
              theme(legend.position="none") +
              labs(title = 'Doses of vaccine administered by manufacturer',x = 'Month', y = "Doses"))
    })
    output$plot2 <- renderPlotly({
        load("federalstate.rda")
        begin <- input$dateRange[1]
        end <- input$dateRange[2]
        ggplotly(
            ggplot(data_age, aes(Impfdatum,cum_sum, fill = Altersgruppe)) +  
                geom_area() +
                facet_wrap(~factor(Altersgruppe, levels = c("12-17 years", "18-59 years", "60+"))) + 
                labs(title = 'Doses of vaccine administered in 2021', x = 'Month', y = "Cumulative Count") +
                scale_fill_manual(values = c("12-17 years" = "indianred4",
                                             "18-59 years" = "indianred2",
                                             "60+" = "indianred")) +
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 9),
                      legend.title = element_blank()) +
                scale_x_date(limit=c(begin,end),date_breaks = "2 month",date_labels = "%B")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


