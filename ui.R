library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

header <- dashboardHeader(title = "Bank Churn Analysis")

sidebar <- dashboardSidebar(sidebarMenu(
    menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
    ),
    uiOutput("output_range_age"),
    uiOutput("output_list_gender"),
    uiOutput("output_list_number_prods"),
    uiOutput("output_list_country")
))


frow1 <- fluidRow(
    valueBoxOutput("output_exited"),
    valueBoxOutput("output_balance"),
    valueBoxOutput("output_active"),
    box(
        title = "Estimated Salary by Age",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput("render_plot", height = "250px", brush = 'mbrush')
    ),
    box(
        title = "Estimated Balance by Gender",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput("render_box_plot", height = "250px")
    ),
    box(
        title = "Result of brushed points",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        DT::dataTableOutput('table_output', height = "120px")
    ),
    
    box(
        title = "URL Parameters",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        verbatimTextOutput("urlText"),
        height="180px"
    )
    
)

body <- dashboardBody(frow1)

ui <-
    dashboardPage(title = 'Fase II', header, sidebar, body, skin = 'blue')