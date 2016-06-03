library(DT)
library(shiny)
library(rCharts)

options(digits=3)

load(url("http://s3.amazonaws.com/tcbdfs/tonights_games_mlb.Rdata"))
preds <- read.csv("http://s3.amazonaws.com/tcbdfs/preds_mlb.csv")
preds$line <- as.numeric(preds$line)
#if(length(which(is.na(preds$line))) > 0){
#	preds <- preds[-which(is.na(preds$line)),]
#}


shinyUI(pageWithSidebar(
  
  headerPanel(title=""),
  sidebarPanel(
     a(img(src="fanduel.png", align="center", width="170px", class="fd_logo"), href="https://www.fanduel.com/?invitedby=tcash21&cnl=da", target="_blank"),
     a(img(src="logo.png", align="center", width="170px", class="nav_logo"), href="http://tcbanalytics.com", target="_blank"),
     includeCSS('www/app.css'),
     numericInput("line", "Lines >= :", value=min(preds$line), min=min(preds$line),  max=max(preds$line)),
     sliderInput("value", "Value >= :", value=min(preds$value), min=min(preds$value), max=max(preds$value)),
     sliderInput("salary", "Salary >= :", value=c(min(preds$Salary), max(preds$Salary)), min=min(preds$Salary), max=max(preds$Salary)),
     checkboxGroupInput("games", "Games:", ug$title, selected=ug$title),
     actionButton(inputId = "delete.button", label = "Delete", icon = icon("minus")),
     actionButton(inputId = "refresh", label="Clear Exclusions"),
     textOutput('total')
    ),
  
  
  
  mainPanel(
	tabsetPanel(
		tabPanel("Lineup Optimizer", 
			column(6, dataTableOutput('results')), 
			column(5, htmlOutput("update"))
		),
		tabPanel("Help", htmlOutput("about"))
	)
#     dataTableOutput('results')
#     tableOutput('total')
                   
    )
    
))
