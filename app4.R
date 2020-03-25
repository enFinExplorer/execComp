library(shiny)
library(tablerDash)
library(rvest)
library(xml2)
library(httr)
library(finreportr)
library(edgarWebR)
library(tidyverse)
library(dplyr)


opList1 <- c('APA',  'AR', 'AREX', 'BCEI', 'BP', 'CDEV', 'CHAP', 'CHK', 'CLR',
             'CNX', 'COG', 'COP', 'CPE',  'CRK',  'CXO', 'CVX', 'DVN', 'ECA','EOG',
             'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP', 'GUI', 'HES', 'HPR',  'LLEX', 'LPI', 'MGY', 'MR',
             'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP', 'RDS-A',
             'RRC', 'SBOW', 'SM', 'SWN',  'WLL', 'WPX', 'XEC', 'XOG', 'XOM')  

opLink <- data.frame(ticker = opList1, operator = c('Apache', 'Antero Resources',
                                                    'Approach Resources',  'Bonanza Creek Energy', 'BP','Centennial Resource Development',
                                                    'Chaparral Energy', 'Chesapeake Energy', 'Continental Resources',
                                                    'CNX Resources', 'Cabot Oil & Gas', 'ConocoPhillips', 'Callon Petroleum',
                                                    'Comstock Resources',  'Concho Resources', 'Chevron',
                                                    'Devon Energy', 'Encana Corporation', 'EOG Resources', 'EQT Corporation', 'Earthstone Energy',
                                                    'Diamondback Energy', 'Gulfport Energy', 'Goodrich Petroleum', 'Guidon Energy Management Services',
                                                    'Hess Corporation', 'HighPoint Resources', 
                                                    'Lilis Energy', 'Laredo Petroleum',
                                                    'Magnolia Oil & Gas Operating', 'Montage Resources', 'Marathon Oil',
                                                    'Matador Resources', 'Murphy Oil', 'Noble Energy', 'Oasis Petroleum',
                                                    'Occidental Petroleum', 'PDC Energy', 'Parsley Energy', 'Penn Virginia Corporation',
                                                    'Pioneer Natural Resources', 'QEP Resources', 'Shell', 'Range Resources',
                                                    'SilverBow Resources', 'SM Energy', 'Southwestern Energy', 'Whiting Petroleum Corporation',
                                                    'WPX Energy', 'Cimarex Energy', 'Extraction Oil & Gas', 'ExxonMobil'))

compScrape <- function(comp.ticker) tryCatch ({
  
    
  filingList <- data.frame(edgarWebR::company_details(comp.ticker, type = 'DEF 14A', count = 1))# %>% filter(!grepl('A', filings.type))
  if(comp.ticker == 'CHK'){
    filingList <- filingList[2:nrow(filingList),]
  }
  #accssionNo <- filingList$filings.accession_number[1]
  #checkNo <- filingList$information.cik[1]
  #filingList <- rbind(filingList, filingList1) %>% arrange(desc(filings.filing_date))
  compInfo <- finreportr::CompanyInfo(comp.ticker)
  filingList$Company <- compInfo$company
  #rm(filingList1)
  
  filingList$url1 <- lapply(filingList$filings.href,  url1)
  filingList$url1 <- gsub('/ix?doc=', '', filingList$url1, fixed=TRUE)
  #values$check <- filingList
  filingList <- filingList[,c('Company', 'filings.filing_date', 'filings.type', 'url1')] %>% arrange(desc(filings.filing_date))
  filingList$quarter <- lubridate::quarter(filingList$filings.filing_date) - 1
  filingList$year <- lubridate::year(filingList$filings.filing_date)
  filingList$quarter[filingList$quarter == 0] <- 4
  filingList$year[filingList$quarter == 4] <- filingList$year[filingList$quarter == 4]-1
  filingList$period <- paste0('Q', filingList$quarter, filingList$year)
  #updateSelectizeInput(session, 'Filing', choices = filingList$period)
  names(filingList)[1:4] <- c('Company', 'filingDate', 'type', 'url1')
  filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
  filingList <- data.frame(lapply(filingList, function(x){
    gsub("iXBRL", "", x)
  }))
  filingList <- data.frame(lapply(filingList, function(x){
    gsub("\\s+", "", x)
  }))
  
  filingList$type <- paste0('<a href="', filingList$url1, '" target="_blank">', filingList$type,'</a>')
  #values$filingList <- filingList
  filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
  names(filingList) <- c('Company', 'Filing Period', 'Filing Date', 'Report', 'URL')
  filingList <- as.data.frame(filingList)
  
  
  nodes <- xml2::read_html(filingList$URL[1]) %>% html_nodes('table')
  
  
  
  string1 <-  'Principal'
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  
  my_tables <- my_tables[[length(my_tables)]]
  return(my_tables)
},
error = function(e) {
  e
  NULL
})

#df <- readRDS('./data/test.rds')
shiny::shinyApp(
  ui = tablerDashPage(
    navbar = tablerDashNav(
      id = "mymenu",
      src = "",
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          tabName = "Home",
          icon = "home",
          "Home"
        )
      )
    ),
    footer = tablerDashFooter(
      #tablerIcon(name = "maestro", lib = "payment"),
      #tablerIcon(name = "mastercard", lib = "payment"),
      copyrights = "@EnFin Explorer, 2020"
    ),
    title = "tablerDash",
    body = tablerDashBody(
      
      #setZoom(class = "card"),
      #chooseSliderSkin("Modern"),
      tablerTabItems(
        tablerTabItem(
          tabName = "Home",
          fluidRow(
            column(
              width = 3,
              selectizeInput('operator', 'Select Operator', choices = opLink$operator)
            )
          ),
          fluidRow(
          column(
            width = 12,
            htmlOutput('plot')
          )
          )
          
        )
      )
      
    )
    ),
    server = function(input, output, session) {
      

      
      
      output$plot <- renderUI({
        comp.ticker <- opLink %>% filter(operator %in% input$operator)
        comp.ticker <- comp.ticker$ticker[1]
        df <- compScrape(comp.ticker)
        
        HTML(df$list1)
        })
    }
  )
  
