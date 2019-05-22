library(tidyverse)
library(ggthemes)
library(shiny)
library(miniUI)

# ggplot theme
old <- theme_set(theme_tufte() + theme(text = element_text(family = 'Menlo')))

cohort_gadget <- function(data, xvar, yvar) {
    
    ui <- miniUI::miniPage(
        gadgetTitleBar("Cohort Analysis"),
        miniContentPanel(
            # define layout here 
            selectInput("variable", "Variable", 
                        c("Quantity" = "n",
                          "Retention" = "retain",
                          "Percentage" = "pct",
                          "Difference" = "diff")),
            plotOutput("cohort", height = "100%")
        )
    )
    
    server <- function(input, output, session) {
        
        # adjust labels based on selection
        lab <- reactive({
            switch(input$variable,
                   "n" = "n",
                   "retain" = "scales::percent(retain)",
                   "pct" = "scales::percent(pct)",
                   "diff" = "diff"
            )
        })
        
        # display cohort
        output$cohort <- renderPlot({
            data %>% 
                ggplot(aes_string(xvar, yvar, fill = input$variable)) + 
                geom_raster() + 
                geom_text(aes_string(label = lab()), col = 'white', size = 3) + 
                scale_fill_viridis_c(guide = 'none') +
                labs(x = "", y = "")
        })
    }
    
    runGadget(ui, server)
    
}
cohort_gadget(df, "ind", "fct_rev(cohort)")

