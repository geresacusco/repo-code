#packages
library(lubridate)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(curl)
library(data.table)
library(shinymanager)


# define some credentials
credentials <- data.frame(
  user = "comandoregioncusco", # mandatory
  password = "042020COV", # mandatory
  start = "2019-04-15", # optinal (all others)
  expire = NA,
  admin = FALSE,
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

set_labels(
  language = "en",
  "Please authenticate" = "Por favor autenticar",
  "Username:" = "Usuario:",
  "Password:" = "Contraseña:",
  "Login" = "Iniciar sesión"
)

# download data.table
table_recibidos <-fread('https://raw.githubusercontent.com/geresacusco/repositorio/master/tabla_recibidos.csv', sep = ";", header = TRUE)
table_emitidos_multiple <-fread('https://raw.githubusercontent.com/geresacusco/repositorio/master/tabla_emitidos_multiples.csv', sep = ",")
table_emitidos <-fread('https://raw.githubusercontent.com/geresacusco/repositorio/master/tabla_emitidos.csv', sep = ",")
table_otros <-fread('https://raw.githubusercontent.com/geresacusco/repositorio/master/tabla_otros.csv', sep = ";")

#load helper scripts
source("EEF scripts/dygraph-extra-shiny.R",local=TRUE)
source("EEF scripts/helper_funcs.r",local=TRUE)

#load data (locally)
load("EEF/index.rData",environment())
load("EEF/graph_data.rData",environment())

#load ui/server scripts
source("EEF scripts/ui_pack.r",local=TRUE)
source("EEF scripts/ui_graphs.r",local=TRUE)
source("EEF scripts/server_pack.r",local=TRUE)
#source("EEF scripts/server_graphs (OLD).r",local=TRUE) #no longer needed?
source("EEF scripts/server_graphs.r",local=TRUE)

#user interface
ui <- fluidPage( 
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
    HTML('<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB" crossorigin="anonymous">'),
    HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato">'),
    HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">'),
    HTML('<link href="https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900,400italic" rel="stylesheet" type="text/css">'),
    HTML('<link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">'),
    shinyjs::useShinyjs(),
    includeCSS("EEF scripts/tdstyles.css"),
    includeCSS("EEF scripts/dygraph.css"),
    includeCSS("EEF scripts/EEFextra.css"),
    tags$script(src = "javascript/dygraph-extra-modified-final.js"),
    tags$script(src="javascript/EEF.js"),
    tags$script(src = "javascript/jquery-ui-1-11-4.min.js"),
    tags$script('$(function(){$(".jui-tip").tooltip();});'),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }")
    
  ),
  
  ##UI components
  shinyjs::hidden(eefHelp()),
  eefHeader("equality"),
  npfText("eef eef-main eef-summ"),
  eefSection(NS("equality","NPF"),"Documentos recibidos",
             colour="eef-section-links",
             class="eef-section eef eef-main eef-summ",
             tabs = box( width = 12, DT::dataTableOutput("recibidos"))),
  br(),
  eefSection(NS("equality","NPF"),"Documentos emitidos",
             colour="eef-section-links",
             class="eef-section eef eef-main eef-summ",
             tabs = 
               box( width = 12, DT::dataTableOutput("emitidos")),
             box(width = 12, DT::dataTableOutput("emitidos_multiple"))),
  br(),
  eefSection(NS("equality","NPF"),"Otros documentos",
             colour="eef-section-links",
             class="eef-section eef eef-main eef-summ",
             tabs = box( width = 12, DT::dataTableOutput("otros"))),
  br(),br(),br(),
  eefFooter(NS("equality","top")),
  
)

# Wrap UI with secure_app

ui <- secure_app(ui,theme = shinythemes::shinytheme("flatly"))



#shiny server
graphOptions[["incPov-11"]]$updateNPF <- TRUE 
server <- function(input,output,session) {
  
# call the server part
# check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
#Tables
    
  table_recibidos$Descarga <- paste0("<a href='",table_recibidos$Descarga,"' target='_blank'>","Descarga","</a>")
  
  output$recibidos = DT::renderDataTable({
    datatable(table_recibidos, escape = FALSE,filter = 'top',
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 25,
                searchHighlight = TRUE
                ))
  })
  
  table_emitidos$Descarga <- paste0("<a href='",table_emitidos$Descarga,"' target='_blank'>","Descarga","</a>")
  
  
  output$emitidos = DT::renderDataTable({
    datatable(table_emitidos, escape = FALSE,filter = 'top',
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 25,
                searchHighlight = TRUE
              ))
  })

  table_emitidos_multiple$Descarga <- paste0("<a href='",table_emitidos_multiple$Descarga,"' target='_blank'>","Descarga","</a>")
  
  
  output$emitidos_multiple = DT::renderDataTable({
    datatable(table_emitidos_multiple, escape = FALSE,filter = 'top',
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 25,
                searchHighlight = TRUE
              ))
  })
  
    
  table_otros$Descarga <- paste0("<a href='",table_otros$Descarga,">RStudio</a>' target='_blank'>","Descarga","</a>")
  
  
  output$otros = DT::renderDataTable({
    datatable(table_otros, escape = FALSE,filter = 'top',
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 25,
                searchHighlight = TRUE
              ))
  })
  
 }

#run app
shinyApp(ui=ui,server=server)
