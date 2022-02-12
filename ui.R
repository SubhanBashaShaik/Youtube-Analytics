rm(list = ls())
require(shiny)
require(shinydashboard)
require(shinycssloaders)
library(plotly)
library(data.table)
library(DT)
library(lubridate)


dateInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Protiviti Assessment",
                  dropdownMenuOutput("messageIcon1"),
                  dropdownMenuOutput("messageIcon2"),
                  dropdownMenuOutput("messageIcon3"),
                  tags$li(HTML('<img src="protiviti-logo.png" class="logo-left" />'),
                          class = "dropdown")
  ),
  dashboardSidebar(
    includeCSS("style.css"),
    sidebarMenu(id ='sidebarmenu',
                HTML('<div class="col-4">'),
                # selectInput("dateMonthYearInput", label = "Year Month", 
                #             choices = year_month_Names
                # ),
                dateInput2("dateMonthYearInput", "Year Month", startview = "year", minview = "months", maxview = "decades", format = "MM-yyyy"),
                helpText("    Kindly select the data within 2017-2018"),
                HTML('</div>'),
                HTML('<div class="col-2">'),
                selectInput("tagSelectNames", label = "Tag Filter", 
                            choices = tagsExtract_Names
                ),
                
                HTML('</div>'),
                HTML('<div class="col-2">'),
                HTML('<div class="btn-color">'),actionButton("submit","Submit"),HTML('</div>'),
                HTML('</div>')
    )
  ),
  
  dashboardBody(
              fluidRow(
                conditionalPanel(condition= "input.submit", 
                HTML('<div class="row">'),
                HTML('<div class="col-md-12">'),
                
                box(title = "Views", solidHeader = TRUE,
                    width = 4,
                    
                    htmlOutput("text1"),
                    htmlOutput("text4")),
                box(title = "Likes", solidHeader = TRUE,
                    width = 4,
                    
                    htmlOutput("text2"),
                    htmlOutput("text5")),
                box(title = "Comments", solidHeader = TRUE,
                    width = 4,
                    
                    htmlOutput("text3"),
                    htmlOutput("text6")),
                HTML('</div>'),
                HTML('</div>'),
                HTML('<header>'),
                HTML('<h1>TOP 10 Channel</h1>'),
                HTML('</header>'),
                HTML('<div class="row">'),
                HTML('<div class="col-md-12">'),
                
                box(title = "Views", solidHeader = TRUE,
                    width = 4,
                    
                    plotlyOutput("Plot1")),
                box(title = "Likes", solidHeader = TRUE,
                    width = 4,
                    
                    plotlyOutput("Plot2")),
                box(title = "Comments", solidHeader = TRUE,
                    width = 4,
                    
                    plotlyOutput("Plot3")),
                HTML('</div>'),
                HTML('</div>'),
                
                HTML('<div class="row">'),
                HTML('<div class="col-md-12">'),
                box(title = "Overall", solidHeader = TRUE,
                    width = 12,
                    DT::dataTableOutput("table1")),
                HTML('</div>'),
                HTML('</div>')
                
              ))
      )
)