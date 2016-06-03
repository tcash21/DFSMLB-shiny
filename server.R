library(DT)
library(shiny)
library(rCharts)
library(lpSolve)

options(shiny.trace=TRUE, stringsAsFactors = FALSE)


shinyServer(function(input, output, session){

preds <- reactiveValues()
preds$df <-  read.csv("http://s3.amazonaws.com/tcbdfs/preds_mlb.csv")

refresh_data <- reactive({

        preds$df <- read.csv("http://s3.amazonaws.com/tcbdfs/preds_mlb.csv")

	return(preds$df)
})

filterData <- reactive({
 	
	#preds <- reactiveValues()
	#preds$df <-  read.csv("http://s3.amazonaws.com/tcbdfs/preds_mlb.csv")

	preds$df$name <- as.character(preds$df$name)
 	preds$df$team <-as.character(preds$df$team)
        preds$df$Position <- as.character(preds$df$Position)

	preds2 <- preds$df
	preds2 <- preds2[preds2$line >= input$line,]
	preds2 <- preds2[preds2$value >= input$value,]
	preds2 <- preds2[preds2$Salary >= input$salary[1] & preds2$Salary <= input$salary[2],]
	preds2 <- preds2[preds2$title %in% input$games,]
	
	obj <- preds2$nf_pred
	con <- rbind(t(model.matrix(~ Position + 0,preds2)), t(model.matrix(~ team + 0, preds2)), rep(1,nrow(preds2)), preds2$Salary)
	dir <- c("=","=","=","=","=", "=", "=", rep('<=',length(unique(preds2$team))),"=","<=")
	rhs <- c(1,1,1,1,3,1,1,rep(4,length(unique(preds2$team))),9,35000)
	result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)
	results <- preds2[which(result$solution == 1),]
	results$spread <- as.numeric(results$spread)
#	results <- results$spread * -1
	results <- results[,c("name", "Position", "team", "Salary", "nf_pred", "value", "title","line", "spread")]
		
	return(list(results, result))
   
})

deleteData <- function(rowID){
	preds$df <- preds$df[-match(rowID, rownames(preds$df)),]
}


deleteRow <- observeEvent(input$delete.button, {
	deleteData(input$results_rows_selected)
})

refresh <- observeEvent(input$refresh, {
	refresh_data()
})

output$results <- renderDataTable({
	datatable(filterData()[[1]], options=list(autowidth=TRUE, selection="single", columnDefs = list(list(width='200px', targets="_all"))), rownames=TRUE)
})

output$update <- renderPrint({
	the_time <- Sys.time() - (3600 * 4)
	if(Sys.time() < format(Sys.time(), '%Y-%m-%d %H:30:00')){
		last_update <- paste0("<b>Last Updated:</b> ", as.Date(Sys.time()), " ", format(the_time, "%H:00"), " EST")
	} else{
		last_update <- paste0("<b>Last Updated:</b> ", as.Date(Sys.time()), " ", format(the_time, "%H:30"), " EST")
	}
	cat(as.character(HTML(last_update)))
})

output$total <- renderPrint({
  r <- filterData()[[2]]
  
  info <-  paste0("Total:", sum(filterData()[[1]]$nf_pred))
  cat(as.character(HTML(info)))
})

output$about <- renderPrint({
	info <- "This optimizer generates optimal lineups based on NumberFire projections:<p>
		<ul><li><b>numberFire</b>:  <a href = 'https://www.numberfire.com/nba/daily-fantasy/daily-baseball-projections/'>NumberFire.com</a> projections</li>
		<p>
		You can filter by games, salary and value. You can also exclude and lock certain players. Data is updated every 30 minutes.
		<p>
		Gam schedule data powered by  <a href = 'https://www.stattleship.com' target='_blank'>Stattleship</a>.
		</p>
		To take advantage of this tool, sign up for <a href = 'https://www.fanduel.com/?invitedby=tcash21&cnl=da' target='_blank'>FanDuel</a> to start playing today."
	cat(as.character(HTML(info)))
})

})




