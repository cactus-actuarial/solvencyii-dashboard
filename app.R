library("ggplot2")
library("shinydashboard")



# dashboard

header <- dashboardHeader(
        title = "Solvency II dashboard"
    )

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Balance Sheet", tabName = "balance_sheet", icon = icon("bar-chart")),
        menuItem("BEL", tabName = "bel", icon = icon("pie-chart")),
        menuItem("SCR", tabName = "scr", icon = icon("sitemap"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "balance_sheet",
            fluidRow(
                box(plotOutput("bs_plot"))
            )
        ),
        tabItem(tabName = "bel",
                "BEL data"),
        tabItem(tabName = "scr",
                "SCR data")
    )
)



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    
    output$bs_plot <- renderPlot({
        # data
        cols = 3
        rows = 5
        
        bs_data <- as.data.frame(matrix(rep(0, cols * rows), ncol = cols, nrow = rows))
        colnames(bs_data) <- c("item", "value", "type")
        bs_data$item <- factor(c("Assets", "Excess capital", "SCR", "RM", "BEL"),
                               levels = c("Assets", "Excess capital", "SCR", "RM", "BEL"))
        bs_data$value <- c(253, 53, 67, 12, 121)
        bs_data$type <- c("Assets", "Liabilities", "Liabilities", "Liabilities", "Liabilities")
        
        # plot
        ggplot(bs_data, aes(x = type, y = value, fill = item)) +
            geom_bar(stat = "identity", colour = "black") +
            scale_fill_brewer(palette = "Pastel1")
    })
}

shinyApp(ui = ui, server = server)