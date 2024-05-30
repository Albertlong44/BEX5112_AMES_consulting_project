#Part A library
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(shinydashboardPlus)
library(fpp3)
library(htmltools)
library(dygraphs)
library(plotly)
library(shinyWidgets)
library(DT)
library(readxl)
library(shinycssloaders)
library(colorspace)
library(tools)
library(knitr)

# Part B read data & wrangling
prod_exp<- read.csv("data/production.csv")

example<-read.csv("data/example.csv")

sea_freight<-read_excel("data/price_ratecard.xlsx", sheet = "sea_freight")|>
  mutate(Region =substring(POD, first =3))

d_transporation<-read_excel("data/price_ratecard.xlsx", sheet = "transportation")

storage<-read_excel("data/price_ratecard.xlsx", sheet = "storage")


AMES_storage<-read.csv("data/AMES_storage.csv")



AMES_dtrans <- read.csv("data/AMES_roadtransportation.csv")

AMES_sf<-read.csv("data/AMES_of.csv")



## Part C: Data cleaning and wranggling 

## Convert data to CBM level
AMES_storage_mt <- AMES_storage %>%
  mutate(
    CBM_cost = Pallet.Cost... / 1.3
  ) %>%
  rename(`RDC Pallet` = Pallet.Cost...,
         `RDC CBM` = CBM_cost)

##Add the data for CBM
AMES_storage_ndc<- AMES_storage_mt |> filter(Region =="MEL")

AMES_storage_mt$`NDC Pallet`<- AMES_storage_ndc$`RDC Pallet`

AMES_storage_mt$`NDC CBM`<-AMES_storage_ndc$`RDC CBM`


## dtrans cleaning

AMES_dtrans_mt<- AMES_dtrans  |> rename(`Pallet cost` =Average.Pallet.Freight.Cost,
                                        Origin =Origin.DC) |>
  mutate(`CBM cost` =`Pallet cost`/1.3)|>
  select(Combo, Origin, Destination, `Pallet cost`, `CBM cost`, Comment)

## SF cleaning

AMES_sf_mt<- AMES_sf |> rename(`RDC Pallet` =Pallet.cost) |>
  mutate(`RDC CBM` = `RDC Pallet`  /1.3)

AMES_sf_ndc<-AMES_sf_mt |> 
  filter(POD== "AUMEL") |>
  rename(`NDC Pallet`=`RDC Pallet`,
         `NDC CBM` =`RDC CBM`) |> 
  select(-POD)

AMES_sf_mt<- left_join(AMES_sf_mt, AMES_sf_ndc, 
                       by =c("POL","Sensitivity"))


## preset_list

product_list<-unique(prod_exp$Product)

region_list<- unique(prod_exp$Region)

model_list<-c("ARIMA","ETS","Rolling average")

container_list<-c("40FT High-cube"=76,"20FT Standard"=33,"40FT Standard"=67,"20FT High-cube"=37)

dt_color_list<-c("RDC: FCL for single sku" ="#AFE1AF",
                 "RDC: FCL for single sku by adjusting the volume quantity" ="#AFE1AF",
                 "RDC: FCL for consolidated sku" ="#AFE1AF",
                 "No or wrong predictive volumes detected" ="#F7D7DC",
                 "Other strategy to wait for FCL consolidation"="#F7D7DC",
                 "NDC: FCL for single sku"="#e7e0e8",
                 "NDC: FCL for single sku by adjusting the volume quantity" ="#e7e0e8",
                 
                 "NDC: FCL for consolidated sku"="#e7e0e8"
                 
)

serice_lvl_list<-c("95%"=1.96,"90%"=1.645, "85%"=1.44, "96%"=2.054,"97%"=2.17, "98%"=2.326,"99%"=2.576,"99.5%"=2.807)
                 
## Function

setSliderColor<-function(color, sliderId) {
  
  # some tests to control inputs
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))
  
  # the css class for ionrangeslider starts from 0
  # therefore need to remove 1 from sliderId
  sliderId <- sliderId - 1
  
  # create custom css background for each slider
  # selected by the user
  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i+1],
      "; border-top: 1px solid ", color[i+1],
      "; border-bottom: 1px solid ", color[i+1],
      ";}"
    )
  })
  
  # insert this custom css code in the head
  # of the shiy app
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

# Part C UI design
ui <- dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header= dashboardHeader(
    title= tags$img(src ="https://www.nssa.org.au/client_images/2430471.png" , 
                    height = "90px", width="90px", style = "margin-top: 3px;")
    
    
  ), ## end basket of header
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Demand forecasting", tabName = "dfore", icon = icon("gauge")),
      menuItem("Price ratecard", tabName ="price", icon = icon("table-list")),
      menuItem("Decision model", tabName = "decmodel", icon =icon("clipboard")),
      menuItem("About", tabName = "about", icon =icon("address-card"))
    )
  ),## end basket of sidebar
  
  ## Body content
  body= dashboardBody(
    tags$style(type = "text/css",
               ".item[data-value='37'] { background-color: #965bae !important; color: white !important; }",
               ".selectize-dropdown .option[data-value='37'] { background-color:#965bae !important; color: white !important; }",
               ".item[data-value='67'] { background-color: #acbe8d !important; color: white !important; }",
               ".selectize-dropdown .option[data-value='67'] { background-color:#acbe8d  !important; color: white !important; }",
               ".item[data-value='67'] { background-color: #acbe8d !important; color: white !important; }",
               ".item[data-value='33'] { background-color: #acbe8d !important; color: white !important; }",
               ".selectize-dropdown .option[data-value='33'] { background-color:#3183BE  !important; color: white !important; }",
               ".item[data-value='76'] { background-color: #5F9EA0 !important; color: white !important; }",
               ".selectize-dropdown .option[data-value='76'] { background-color:#5F9EA0  !important; color: white !important; }",
               "button#dropdown_data{font-weight: bold;background-color: #FF0000;color: white; border: black solid 2px; margin-left: 30px;}",
               "button#dropdown_model{ font-weight: bold; background-color: #00A65A; color: white; border: black solid 2px; margin-left: 20px;}",
               "button#dropdown_region {font-weight: bold;background-color: mediumpurple;color: white; border: black solid 2px; margin-left: 20px;}",
               "button#dropdown_product { font-weight: bold;color: white; border: black solid 2px; margin-left: 20px; }"
    ), ## End basket of tags$style
    tabItems(
      tabItem(tabName = "dfore",
              p(HTML('<h2 style="text-align: center; 
                     color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;"> AMES Project Report</h2>')),
              br(),
              br(),
              fixedRow(width =12,
                       column(width =3,
                              dropdownButton(
                                inputId = "dropdown_data",
                                label ="Data update",
                                circle=FALSE,
                                icon= icon("database"),
                                status ="warning",
                                fileInput("file5", "Choose supported File", accept = ".csv"),
                                fluidRow(
                                  column(width =6,
                                         radioButtons("filetype5", "File type:", 
                                                      choices =c(CSV="csv",Excel ="xlsx"), inline =TRUE)),
                                  column(width =6,
                                         downloadBttn(
                                           outputId = "downloadData5",
                                           label ="template",
                                           size = "xs",
                                           style = "bordered",
                                           color = "primary"
                                         ) )
                                )
                                
                              ) ##dropdown
                       ), ## coklumn
                       column(width =3,
                              dropdownButton(
                                inputId = "dropdown_product",
                                label ="Product",
                                circle=FALSE,
                                icon= icon("screwdriver-wrench"),
                                status ="warning",
                                selectizeInput("product","Products:",
                                               choices = product_list ,
                                               selected = product_list [1],
                                               multiple= TRUE) )    
                       ),
                       
                       column(width =3,
                              dropdownButton(
                                inputId = "dropdown_region",
                                label ="Region",
                                circle=FALSE,
                                icon= icon("earth-asia"),
                                status ="warning",
                                selectizeInput("region","Choices:",
                                               choices = region_list,
                                               selected ="MEL",
                                               multiple= FALSE) ) 
                              
                       ),
                       column(width =3,
                              dropdownButton(
                                inputId = "dropdown_model",
                                label ="Model",
                                circle=FALSE,
                                icon= icon("chart-column"),
                                status ="warning",
                                selectizeInput("model","Choices:",
                                               choices = model_list,
                                               selected ="ETS",
                                               multiple= FALSE)  ) 
                       )
              ), ##End of fluidrow
              br(),
              br(),
              
              tabBox(id = "tab", title = " ",width=12,
                     
                     tabPanel(title =" Pattern learning",
                              h3('Overall trend:'),
                              numericInputIcon("dygraphperiod","Tail period:", value=36, min = 1, max = 120, 
                                               icon =icon("sliders"), 
                                               help_text = "Out of bound",
                                               width ='30%'), 
                              withSpinner(dygraphOutput("dygraphresult")),
                              br(),
                              br(),
                              h3("Seasonality breakdown:"),
                              selectizeInput("seasontype","Season type:", 
                                             choices =c("Month", "Quarter"),
                                             selected= "Month",
                                             width ='30%'),
                              withSpinner(plotlyOutput("seasonalityresult"))
                              
                     ), ## END BRACKET OF tabPanel
                     tabPanel(title ="Demand forecasting",
                              h3('Seasonal/Trend/Residual decomposition'),
                              
                              selectizeInput("seasontypestl","Season type:", 
                                             choices =c("Month", "Quarter", "Year"),
                                             selected= "Month",
                                             width ='30%'),
                              withSpinner( plotOutput("stlresult")),
                              h3('Model prediction:'),
                              
                              fluidRow(width =12, 
                                       column(width =3,
                                              numericInputIcon("dforeperiod","Predicted period:", value=6, min = 1, max = 20, 
                                                               icon =icon("sliders"),
                                                               help_text = "Out of bound")
                                       ),
                                       column(width =3,
                                              numericInputIcon("modeltp","Tail period:", value=36, min = 1, max = 120, 
                                                               icon =icon("sliders"), 
                                                               help_text = "Out of bound")
                                       ),
                                       column(width=3,
                                             selectizeInput("servicelvl","Service level:", 
                                                            choices =serice_lvl_list,
                                                            selected= serice_lvl_list[1])),
                                       column(width =3,
                                              selectizeInput("seasontypest2","Season type:", 
                                                             choices =c("Month", "Quarter", "Year"),
                                                             selected= "Month")
                                       )
                              ),
                              
                              withSpinner( plotlyOutput("modelresult")
                     ) ,
                     downloadBttn(
                       outputId = "downloadData6",
                       label ="template",
                       size = "xs",
                       style = "bordered",
                       color = "primary"
                     )
                     ) ## END BRACKET OF tabPanel
                     
              ), ## End bracket of tabBox 
              br(),
              br(),
              htmlOutput("license_dfore")
      ), # End bracket of dfore tab
      
      tabItem(tabName ="price",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy; ">AMES Project Report</h2>')),
              br(),
              br(),
              p(HTML('<h3 style="margin-left:20px; font-weight: bold; ">Sea freight cost:</h3>')),
              br(),
              br(),
              sidebarLayout(
                
                sidebarPanel(
                  fileInput("file4", "Choose supported File", accept = ".csv"),
                  fluidRow(
                    column(width =6,
                           radioButtons("filetype4", "File type:", 
                                        choices =c(CSV="csv",Excel ="xlsx"), inline =TRUE)),
                    column(width =6,
                           downloadBttn(
                             outputId = "downloadData7",
                             label ="template",
                             size = "xs",
                             style = "bordered",
                             color = "primary"
                           ))
                  ) ##End bracket of fluidRow
                  
                ),## End bracket of sidebarPanel
                
                mainPanel( 
                  box(width=12,
                      title = "The summary table of sea freight cost",
                      status = "success", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(DTOutput("sfcost"))
                  )
                  
                ) ##End bracket of mainPanel
              ),##End bracket of sidebarPanel
              br(),
              br(),
              p(HTML('<h3 style="margin-left:20px; font-weight: bold; ">Storage cost:</h3>')),
              sidebarLayout(
                
                sidebarPanel(
                  fileInput("file2", "Choose supported File", accept = ".csv"),
                  fluidRow(
                    column(width =6,
                           radioButtons("filetype2", "File type:", 
                                        choices =c(CSV="csv",Excel ="xlsx"), inline =TRUE)),
                    column(width =6,
                           downloadBttn(
                             outputId = "downloadData2",
                             label ="template",
                             size = "xs",
                             style = "bordered",
                             color = "primary"
                           ))
                  ) ##End bracket of fluidRow
                  
                ),## End bracket of sidebarPanel
                
                mainPanel( 
                  box(width=12,
                      title = "The summary table of storage cost",
                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(DTOutput("storagecost"))
                  )
                  
                ) ##End bracket of mainPanel
              ),##End bracket of sidebarPanel
              br(),
              br(),
              p(HTML('<h3 style="margin-left:20px; font-weight: bold; ">Domestic transportation cost:</h3>')),
              sidebarLayout(
                
                sidebarPanel(
                  fileInput("file3", "Choose supported File", accept = ".csv"),
                  fluidRow(
                    column(width =6,
                           radioButtons("filetype3", "File type:", 
                                        choices =c(CSV="csv",Excel ="xlsx"), inline =TRUE)),
                    column(width =6,
                           downloadBttn(
                             outputId = "downloadData3",
                             label ="template",
                             size = "xs",
                             style = "bordered",
                             color = "primary"
                           ))
                  ) ##End bracket of fluidRow
                  
                ),## End bracket of sidebarPanel
                
                mainPanel( 
                  box(width=12,
                      title = "The summary table of domestic transportation cost",
                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(DTOutput("dtranscost"))
                  )
                  
                ) ##End bracket of mainPanel
              ), ## End bracket of sidebarPanel
             br(),
              br(),
              htmlOutput("license_price")
      ),  # End basket of price tab
      
      tabItem(tabName ="decmodel",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy; ">AMES Project Report</h2>')),
              br(),
              br(),
              
              sidebarLayout(
                
                sidebarPanel(
                  fileInput("file1", "Choose supported File", accept = ".csv"),
                  fluidRow(
                    column(width =6,
                           radioButtons("filetype", "File type:", 
                                        choices =c(CSV="csv",Excel ="xlsx"), inline =TRUE)),
                    column(width =6,
                           downloadBttn(
                             outputId = "downloadData",
                             label ="template",
                             size = "xs",
                             style = "bordered",
                             color = "primary"
                           ))
                  ),
                  selectizeInput("container","Container type:",
                                 choices=container_list,
                                 selected = 67,  # Corrected value
                                 multiple = FALSE),
                  br(),
                  setSliderColor(c("#FF4500" , "Teal"), c(1, 2)),
                  sliderInput("cost",
                              "Cost difference threshold(%):",
                              min = 0,
                              max = 30,
                              value = 5),
                  sliderInput("airvol",
                              "Space utilization(%):",
                              min = 0,
                              max = 100,
                              value = 80),
                  sliderInput("space",
                              "remaining space(%):",
                              min = 0,
                              max = 30,
                              value = 5)
                  
                ),
                
                mainPanel(
                  box(width=12,
                      title = "The summary table of decision model",
                      status = "success", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(DTOutput("DTmodel"))
                  )
                )
                
              ), #End basket for sidebarLayout
              br(),
              br(),
              htmlOutput("license_dcmodel")   
      ),  # End basket of dcmodel tab
      
      tabItem(tabName ="about",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">AMES Project Report</h2>')), 
              br(),
              br(),
              box(width = 12,status = "success", title = "Introduction", collapsible = TRUE, solidHeader = FALSE,
                  p(HTML('<b style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">Purpose</b>')),
                  p('This dashboard aims at the visualization and model build-up for the decision of  sku allocation(whether RDC or NDC)  :'),
                  p(HTML('🎯<b>Demand forecasting</b>: Visualizes demand trends and patterns and incorporate with advanced mathmatic prediction model  ETS/ARIMA.')),
                  p(HTML('🎯<b>Ratecard management</b>: Manages logistical costs and other stable key variables, separate tracking.')),
                  p(HTML('🎯<b>Decision model</b>:  Displays RDC/NDC results in nice format table with an interactive interface for uploading data and adjusting model parameters.'),
                   br(),
                   br(),
                   p(HTML('<b style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">Theoretical Model</b>')),
                   p('Below is the workflow of our theoretical model:'),
                   tags$img(src = "https://raw.githubusercontent.com/Albertlong44/BEX5112_AMES_consulting_project/main/Fig/model.png", 
                            height = "400px", width = "1000px"),
                   p(HTML('<b style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">Authors</b>')),
                   p('Members of BEX5112 AMES project team 2024 Semester 1:'),
                   p(HTML('<b>Yuhao Long</b>, <a href="loongyuho@gmail.com">loongyuho@gmail.com </a>, Master of Business Analytics, Monash University')),
                   p(HTML('<b>Varun Singh</b>, <a href="vsin0010@student.monash.edu">vsin0010@student.monash.edu</a>,Master of Management, Monash University')),
                   p(HTML('<b>Yuying Liu</b>, <a href="yliu0523@student.monash.edu">yliu0523@student.monash.edu</a>,Master of Business, Monash University')),
                   p(HTML('<b>Saud Jaber A Alfaifi</b>, <a href="salf0005@student.monash.edu">salf0005@student.monash.edu</a>,Master of Business, Monash University')),
                   p(HTML('<b style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">Supervisors</b>')),
                   p(HTML('<b>Richard Millson</b>,<a href="Richard.Millson@monash.edu">Richard.Millson@monash.edu</a>, Faculty of Business and Economics, Monash University'))
                    ), 
                  
                  tags$img(src = "https://raw.githubusercontent.com/Albertlong44/BEX5112_AMES_consulting_project/main/Fig/author.png", 
                           height = "200px", width = "250px"),
                  br(),
                  br(),
                  p(HTML('<b style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">Set-up</b>')),
                  p(icon("github"), HTML('<strong>Github:</strong>'),HTML('<a href="https://github.com/Albertlong44/BEX5112_AMES_consulting_project">github link</a>.')),
                  p(HTML('<b>Session documentation of build-up</b>')),
                  verbatimTextOutput("session_output")
                  ),
             htmlOutput("license_about")
              
      )    # End basket of about tab
      
    ), ##End basket for tabItems
    includeCSS("monashreport.css")
  ) ##End basket for body
  
) ## End basket  for UI


## Part D server setup
server <- function(input, output,session) { 
  
  
  ## Data file update
  data_example <- reactive({
    if (is.null(input$file1)) {
      # If no file is uploaded, use iris dataset
      example
    } else {
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == input$filetype, "Please upload a csv file"))
      
      read.csv(file$datapath, header = TRUE)
    }
    
  })
  
  data_storage <- reactive({
    if (is.null(input$file2)) {
      # If no file is uploaded, use iris dataset
      AMES_storage_mt
    } else {
      file <- input$file2
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == input$filetype, "Please upload a csv file"))
      
      data<- read.csv(file$datapath, header = TRUE)
      
      data_mt <- data %>%
        mutate(
          CBM_cost = Pallet.Cost... / 1.3
        ) %>%
        rename(`RDC Pallet` = Pallet.Cost...,
               `RDC CBM` = CBM_cost)
      
      ##Add the data for CBM
      data_ndc<- AMES_storage_mt |> filter(Region =="MEL")
      
      data_mt$`NDC Pallet`<- AMES_storage_ndc$`RDC Pallet`
      
      data_mt$`NDC CBM`<-AMES_storage_ndc$`RDC CBM`
      
      data_mt
      
    }
    
  })
  
  data_dtrans <- reactive({
    if (is.null(input$file3)) {
      # If no file is uploaded, use iris dataset
      AMES_dtrans_mt
    } else {
      file <- input$file3
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == input$filetype, "Please upload a csv file"))
      
      data<-read.csv(file$datapath, header = TRUE)
      
      data_mt<- data |> rename(`Pallet cost` =Average.Pallet.Freight.Cost,
                               Origin =Origin.DC) |>
        mutate(`CBM cost` =`Pallet cost`/1.3)|>
        select(Combo, Origin, Destination, `Pallet cost`, `CBM cost`, Comment)
      
      data_mt
    }
    
  })
  
  
  data_sf <- reactive({
    if (is.null(input$file4)) {
      # If no file is uploaded, use iris dataset
      AMES_sf_mt
    } else {
      file <- input$file4
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == input$filetype, "Please upload a csv file"))
      
      data<-read.csv(file$datapath, header = TRUE)
      
      
      data_mt<- data |> rename(`RDC Pallet` =Pallet.cost) |>
        mutate(`RDC CBM` = `RDC Pallet`  /1.3)
      
      data_ndc<-AMES_sf_mt |> 
        filter(POD== "AUMEL") |>
        rename(`NDC Pallet`=`RDC Pallet`,
               `NDC CBM` =`RDC CBM`) |> 
        select(-POD)
      
      data_mt<- left_join(AMES_sf_mt, AMES_sf_ndc, 
                          by =c("POL","Sensitivity")) 
      
      data_mt
    }
    
  })
  
  
  
  data_prod1 <- reactive({
    if (is.null(input$file5)) {
      # If no file is uploaded, use iris dataset
      prod_exp
      
      
    } else {
      file <- input$file5
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == input$filetype, "Please upload a csv file"))
      
      data<-read.csv(file$datapath, header = TRUE)
      
      
      # Update product_list with unique products in the new dataset
      product_list <- unique(data$Product)
      
      # Update choices in selectizeInput
      updateSelectizeInput(session, "product", choices = product_list, selected = product_list[1])
      
      
      # Update region_list with unique products in the new dataset
      region_list <- unique(data$Product)
      
      # Update choices in selectizeInput
      updateSelectizeInput(session, "product", choices = product_list, selected = product_list[1])
      
      data
      
      
      
    }
    
  })
  
  output$dygraphresult<-renderDygraph({
    
    ## Subset and mutation
    prod_exp_dygraph <- data_prod1() |>
      filter(Region ==input$region & Product%in% input$product) |>
      pivot_wider(names_from = Product, values_from =Volume)|> ##extend to wide format
      mutate(Month.num =match(Month, month.abb),
             Yearmonth =yearmonth( Yearmonth))
    
    ## Find the tail number
    prod_exp_dygraph_tail<- prod_exp_dygraph |>tail( n =input$dygraphperiod)
    
    ## Convert to ts object
    prod_exp_dygraph_ts<-prod_exp_dygraph |> 
      select(-Year,-Month, -Region, -Yearmonth, -Month.num) |>
      ts( start = c(prod_exp_dygraph$Year[1],
                    prod_exp_dygraph$Month.num[1] ), 
          frequency =12)
    
    ## The start/end date of filtering
    
    startdate<- paste0(prod_exp_dygraph_tail$Year[1],"-",  prod_exp_dygraph_tail$Month.num[1],"-01")
    
    enddate<- paste0(tail(prod_exp_dygraph_tail,1)$Year[1],"-",  tail(prod_exp_dygraph_tail,1)$Month.num[1],"-01")
    
    
    Sys.sleep(1)
    
    ##Convert to dygraph 
    dygraph_plot<-dygraph(prod_exp_dygraph_ts) |>
      dyRangeSelector(dateWindow= c(startdate,  enddate))
    
    return(  dygraph_plot)
    
    
  })
  
  
  output$seasonalityresult <- renderPlotly({
    
    
    
    ## Define time transformation function based on selected input
    transform_time <- switch(input$seasontype,
                             "Month" = yearmonth,
                             "Quarter" = yearquarter)
    
    
    ## Convert to tsibble function
    prod_exp_tsb<- data_prod1()|>
      filter(Region ==input$region & Product%in% input$product)|>
      mutate(season = transform_time(Yearmonth))|>
      group_by(season,Region, Product) |>
      summarise(Volume=sum(Volume)) |>
      as_tsibble(index =season, key =Product)
    
    Sys.sleep(1)
    ## Create a sub-series plot
    season_plot<-prod_exp_tsb |>  
      gg_subseries( Volume)+ 
      theme_bw()+
      labs(y = " ",x ="\n season") +
      theme(strip.background = element_rect(fill = "#90EE90"),
            strip.text.x = element_text(size = 12, face ="bold"),
            strip.text.y= element_text(size = 8),
            axis.text.x =element_text( angle =-45, hjust =-0.5) )
    
    ## Convert plotly to interactive plot
    ggplotly(season_plot) |>
      layout(
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        hoverinfo = "text+y",
        xaxis = list(spikemode = 'across')) 
    
  })
  
  
  
  
  output$stlresult<- renderPlot({
    ## Define time transformation function based on selected input
    transform_time <- switch(input$seasontypestl,
                             "Month" = yearmonth,
                             "Quarter" = yearquarter,
                             "Year" = function(x) year(yearmonth(x))
    )
    
    
    
    ## Convert to tsibble function
    prod_exp_tsb<- data_prod1() |>
      filter(Region ==input$region & Product%in% input$product)|>
      mutate(season = transform_time(Yearmonth))|>
      group_by(season,Region, Product) |>
      summarise(Volume=sum(Volume)) |>
      as_tsibble(index =season, key =Product)
    
    
    prod_exp_tsb |>
      model(STL( Volume~ trend(window =5)+season( window ="periodic")))  |> components() |> autoplot()+
      theme_bw() + 
      scale_color_discrete_sequential(palette = "Hawaii", 
                                      labels = unique(prod_exp_tsb$Product))+
      theme(strip.background = element_rect(fill = "#90EE90"),
            strip.text.x = element_text(size = 12, face ="bold"))
  })
  
 
  
  forecastData <- reactive({
    transform_time <- switch(input$seasontypest2,
                             "Month" = yearmonth,
                             "Quarter" = yearquarter,
                             "Year" = function(x) year(yearmonth(x))
    )   
    
    if (input$model == "Rolling average") {
      prod_exp <- data_prod1() |>
        filter(Region == input$region & Product %in% input$product) |>
        mutate(season = transform_time(Yearmonth)) |>
        group_by(season, Region, Product) |>
        summarise(Volume = sum(Volume))
      
      n <- input$dforeperiod
      future_date <- rep(0, n)
      
      for (i in 1:n) {
        future_date[i] <- as.character(max(prod_exp$season) + i)
      }
      
      unique_prod_len <- length(unique(prod_exp$Product))
      
      future_date_dt <- data.frame(season = rep(transform_time(future_date), unique_prod_len),
                                   Product = rep(unique(prod_exp$Product), each = n)) |>  
        arrange(season, Product)
      future_date_dt$Region <- "MEL"
      future_date_dt$Volume <- NA
      
      prod_exp_tail <- tail(prod_exp, 12 * unique_prod_len) |> 
        select(season, Product, Volume)
      
      prod_exp_rbind <- rbind(prod_exp_tail, future_date_dt)
      
      Start_point <- 12 * unique_prod_len + 1
      for (i in Start_point:nrow(prod_exp_rbind)) {
        prod_exp_rbind$Volume[i] <- round(sum(tail(prod_exp_rbind[prod_exp_rbind$Product == prod_exp_rbind$Product[i] & !is.na(prod_exp_rbind$Volume), ], 12)$Volume) / 12, digits = 0)
      }
      
      prod_exp_sigma <- 
        prod_exp_rbind |> 
        group_by(Product) |> 
        summarise(sigma = sd(Volume))
      
      forecast_result <- prod_exp_rbind |>  
        filter(season > max(prod_exp$season)) |> 
        left_join(prod_exp_sigma, by = "Product") |> 
        mutate(
          low80 = Volume - as.numeric(input$servicelvl) * sigma,
          high80 = Volume + as.numeric(input$servicelvl) * sigma
        )
      
      return(forecast_result)
    } else {
      transform_model <- switch(input$model,
                                "ETS" = ETS,
                                "ARIMA" = ARIMA)
      
      prod_exp_tsb <- data_prod1() |>
        filter(Region == input$region & Product %in% input$product) |>
        mutate(season = transform_time(Yearmonth)) |>
        group_by(season, Region, Product) |>
        summarise(Volume = sum(Volume)) |>
        as_tsibble(index = season, key = Product)
      
      forecast_result <- prod_exp_tsb |>
        model(transform_model(Volume)) |> 
        forecast(h = input$dforeperiod) |>  
        as.tibble() |> 
        mutate(
          range = Volume,
          Volume = .mean,
          var = as.numeric(str_extract(range, "(?<=,\\s)\\d+\\.?\\d*(?:[eE][-+]?\\d+)?")),
          sigma = sqrt(var),
          low= Volume - as.numeric(input$servicelvl) * sigma,
          high= Volume + as.numeric(input$servicelvl) * sigma
        )
      
      return(forecast_result)
    }
  })
  
 
  
  output$modelresult<- renderPlotly({
    ## Define time transformation function based on selected input
    transform_time <- switch(input$seasontypest2,
                             "Month" = yearmonth,
                             "Quarter" = yearquarter,
                             "Year" = function(x) year(yearmonth(x))
    )   
    
    
    if (input$model==  "Rolling average"){
      
      
      
      prod_exp<-data_prod1()  |>
        filter(Region ==input$region & Product %in% input$product)|>
        mutate(
          season  =transform_time ( Yearmonth))|>
        group_by(season,Region, Product) |>
        summarise(Volume=sum(Volume))
      
      n<-input$dforeperiod
      
      future_date <- rep(0, n)
      
      ## Iterate for upcoming month
      for (i in 1:n) {
        future_date[i] <- as.character(max(prod_exp$season) + i)
        
        
      }
      
      unique_prod_len<- length(unique(prod_exp$Product))
      
      future_date_dt<- data.frame(season = rep( transform_time (future_date),unique_prod_len),
                                  Product =rep( unique(prod_exp$Product), each = n))|>  
        arrange(season,Product)
      future_date_dt$Region<-"MEL"
      
      future_date_dt$Volume<-NA
      
      prod_exp_tail<-tail(prod_exp,12*  unique_prod_len)|> 
        select(season,Product,Volume )
      
      prod_exp_rbind<-rbind(prod_exp_tail,future_date_dt)
      
      ## Start point for Iteration
      Start_point<- 12*unique_prod_len +1
      ## Iterate for roll mean
      for ( i in  Start_point: nrow(prod_exp_rbind)) {
        
        prod_exp_rbind$Volume[i] <-round(sum(tail(prod_exp_rbind[prod_exp_rbind$Product == prod_exp_rbind$Product[i] & !is.na(prod_exp_rbind$Volume), ],12)$Volume  )/12 , digits =0)
      }
      
      ## Get sigma
      prod_exp_sigma<- 
        prod_exp_rbind |> 
        group_by(Product) |> 
        summarise(sigma =sd(Volume)) 
      
      forecast_result<-prod_exp_rbind |>  
        filter(season > max(prod_exp$season)) |> 
        left_join(
          prod_exp_sigma, by ="Product")|> 
        mutate(
          low =  Volume- as.numeric(input$servicelvl) *sigma,
          high=  Volume+ as.numeric(input$servicelvl) *sigma)
      
      prod_exp_tail_final<-prod_exp |> tail(n =as.numeric(input$modeltp)* unique_prod_len)
      
      prod_exp_bind <-rbind(prod_exp_tail_final|> 
                              select(Product,season, Volume),
                            forecast_result |> 
                              select( Product,season, Volume))
      
    } else {
      
      
      ## Define model transformation function based on selected input
      transform_model<- switch(input$model,
                               "ETS" = ETS,
                               "ARIMA" = ARIMA)
      
      
      
      ## Convert to tsibble function
      prod_exp_tsb<-   data_prod1() |>
        filter(Region ==input$region & Product%in% input$product)|>
        mutate(season = transform_time(Yearmonth))|>
        group_by(season,Region, Product) |>
        summarise(Volume=sum(Volume)) |>
        as_tsibble(index =season, key =Product)
      
      ## Model prediction
      forecast_result<- prod_exp_tsb |>
        model( transform_model( Volume)) |> 
        forecast(h = input$dforeperiod) |>  
        as.tibble()  |> mutate(range =Volume,
                               Volume=.mean,
                               var =as.numeric(str_extract(range , 
                                                           "(?<=,\\s)\\d+\\.?\\d*(?:[eE][-+]?\\d+)?")),
                               sigma =sqrt(var),
                               low=  Volume- as.numeric(input$servicelvl) *sigma,
                               high =  Volume+ as.numeric(input$servicelvl) *sigma
        )## Calculate the upper/lower CI
      
      
      prod_exp_tsb_tail<- prod_exp_tsb|>tail( n =input$modeltp)
      
      ## Combine the data
      prod_exp_bind <-rbind(as.tibble(prod_exp_tsb) |> select(Product,season, Volume),
                            forecast_result |> select( Product,season, Volume)
      )  |> filter( season >=  prod_exp_tsb_tail$season[1])
      
      
      
      
      
    }
    
    
    Sys.sleep(1)
    
    predictplot<-ggplot(data = forecast_result) +
      geom_line(data=prod_exp_bind, 
                aes(season, Volume,color =Product), 
                size =0.8,linetype ="dotdash") +
      theme_bw() +
      geom_ribbon( aes(x = season, ymin =low,  
                       ymax =high,fill = Product ), alpha =0.5) + 
      geom_line( aes( season, Volume,color =Product),  size =0.8)+
      theme(legend.position = "bottom")
    
    ggplotly(predictplot) |>
      layout(legend = list(orientation = 'h')) |>
      layout(
        hovermode = "closest",
        hoverlabel = list(bgcolor = "white"),
        hoverinfo = "text+y",
        xaxis = list(spikemode = 'across')) %>% 
      event_register("plotly_hover") ## set up the connection
    
    
    
  })
  
  
  
  output$downloadData6 <- downloadHandler(
  

    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Demand_forecast_result", Sys.Date(), ".csv")
    },
    content = function(file) {
      forecast_demand<-  forecastData()
      write.csv( forecast_demand, file)
    }
  ) 
  
  
  
  
  
  output$sfcost<-renderDT({
    
    AMES_sf_mt<- data_sf() |>
      mutate(`RDC Pallet` =if_else(POD== "AUMEL",NA,`RDC Pallet`),
             `RDC CBM`=if_else(POD== "AUMEL",NA,`RDC CBM`))
    
    AMES_sf_dt<-AMES_sf_mt |>  datatable(
      callback=JS('
    $("button.buttons-copy").css({
        "background": "#81D8D0",
         "border": "2px solid #000000",
         "color":"white",
         "fontweight":"bold",  
        "border-radius": "10px"
    });
    $("button.buttons-csv").css({
        "background": "#FF7900",
         "border": "2px solid #000000",  
        "border-radius": "10px", 
         "color":"white",
         "fontweight":"bold"
    });
    
    return table;
')
      
      ,
      extensions = c('Buttons', 'Scroller','FixedColumns'),
      options = list(
        scrollY = 200,
        scroller = TRUE,
        dom = 'Bfrtip',
        scrollX = TRUE,
        fixedColumns = TRUE,
        
        buttons = list( list(extend = "copy", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="14" viewBox="0 0 448 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M384 336H192c-8.8 0-16-7.2-16-16V64c0-8.8 7.2-16 16-16l140.1 0L400 115.9V320c0 8.8-7.2 16-16 16zM192 384H384c35.3 0 64-28.7 64-64V115.9c0-12.7-5.1-24.9-14.1-33.9L366.1 14.1c-9-9-21.2-14.1-33.9-14.1H192c-35.3 0-64 28.7-64 64V320c0 35.3 28.7 64 64 64zM64 128c-35.3 0-64 28.7-64 64V448c0 35.3 28.7 64 64 64H256c35.3 0 64-28.7 64-64V416H272v32c0 8.8-7.2 16-16 16H64c-8.8 0-16-7.2-16-16V192c0-8.8 7.2-16 16-16H96V128H64z"/></svg>'), 
                        list(extend = "csv", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 512 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M288 32c0-17.7-14.3-32-32-32s-32 14.3-32 32V274.7l-73.4-73.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l128 128c12.5 12.5 32.8 12.5 45.3 0l128-128c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L288 274.7V32zM64 352c-35.3 0-64 28.7-64 64v32c0 35.3 28.7 64 64 64H448c35.3 0 64-28.7 64-64V416c0-35.3-28.7-64-64-64H346.5l-45.3 45.3c-25 25-65.5 25-90.5 0L165.5 352H64zm368 56a24 24 0 1 1 0 48 24 24 0 1 1 0-48z"/></svg>')) ,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3A5311', 'color': '#fff'});",
          "}"))) %>%
      formatCurrency(c("RDC CBM", "RDC Pallet", "NDC CBM","NDC Pallet"), digits = 2)
    
    return( AMES_sf_dt)
  })
  
  output$dtranscost<-renderDT({
    
    AMES_dtrans_mt<-data_dtrans()
    
    
    dtrans_dt<- AMES_dtrans_mt|>
      
      datatable(
        callback=JS('
    $("button.buttons-copy").css({
        "background": "#81D8D0",
         "border": "2px solid #000000",
         "color":"white",
         "fontweight":"bold",  
        "border-radius": "10px"
    });
    $("button.buttons-csv").css({
        "background": "#FF7900",
         "border": "2px solid #000000",  
        "border-radius": "10px", 
         "color":"white",
         "fontweight":"bold"
    });
    
    return table;
')
        
        ,
        extensions = c('Buttons', 'Scroller','FixedColumns'),
        options = list(
          scrollY = 200,
          scroller = TRUE,
          dom = 'Bfrtip',
          scrollX = TRUE,
          fixedColumns = TRUE,
          
          buttons = list( list(extend = "copy", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="14" viewBox="0 0 448 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M384 336H192c-8.8 0-16-7.2-16-16V64c0-8.8 7.2-16 16-16l140.1 0L400 115.9V320c0 8.8-7.2 16-16 16zM192 384H384c35.3 0 64-28.7 64-64V115.9c0-12.7-5.1-24.9-14.1-33.9L366.1 14.1c-9-9-21.2-14.1-33.9-14.1H192c-35.3 0-64 28.7-64 64V320c0 35.3 28.7 64 64 64zM64 128c-35.3 0-64 28.7-64 64V448c0 35.3 28.7 64 64 64H256c35.3 0 64-28.7 64-64V416H272v32c0 8.8-7.2 16-16 16H64c-8.8 0-16-7.2-16-16V192c0-8.8 7.2-16 16-16H96V128H64z"/></svg>'), 
                          list(extend = "csv", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 512 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M288 32c0-17.7-14.3-32-32-32s-32 14.3-32 32V274.7l-73.4-73.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l128 128c12.5 12.5 32.8 12.5 45.3 0l128-128c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L288 274.7V32zM64 352c-35.3 0-64 28.7-64 64v32c0 35.3 28.7 64 64 64H448c35.3 0 64-28.7 64-64V416c0-35.3-28.7-64-64-64H346.5l-45.3 45.3c-25 25-65.5 25-90.5 0L165.5 352H64zm368 56a24 24 0 1 1 0 48 24 24 0 1 1 0-48z"/></svg>')) ,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3A5311', 'color': '#fff'});",
            "}"))) %>%
      formatCurrency(c("Pallet cost","CBM cost"), digits = 2)
    
    return(dtrans_dt)
    
  })
  
  
  output$storagecost <- renderDT({
    
    
    
    AMES_storage_mt<- data_storage() |> 
      mutate(`RDC Pallet` =if_else(Region =="MEL",NA,`RDC Pallet`),
             `RDC CBM`=if_else(Region =="MEL",NA,`RDC CBM`))
    
    
    Sys.sleep(1)
    
    
    storage_dt <- AMES_storage_mt |>
      datatable(
        callback=JS('
    $("button.buttons-copy").css({
        "background": "#81D8D0",
         "border": "2px solid #000000",
         "color":"white",
         "fontweight":"bold",  
        "border-radius": "10px"
    });
    $("button.buttons-csv").css({
        "background": "#FF7900",
         "border": "2px solid #000000",  
        "border-radius": "10px", 
         "color":"white",
         "fontweight":"bold"
    });
    
    return table;
')
        
        ,
        extensions = c('Buttons', 'Scroller','FixedColumns'),
        options = list(
          scrollY = 200,
          scroller = TRUE,
          dom = 'Bfrtip',
          scrollX = TRUE,
          fixedColumns = TRUE,
          
          buttons = list( list(extend = "copy", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="14" viewBox="0 0 448 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M384 336H192c-8.8 0-16-7.2-16-16V64c0-8.8 7.2-16 16-16l140.1 0L400 115.9V320c0 8.8-7.2 16-16 16zM192 384H384c35.3 0 64-28.7 64-64V115.9c0-12.7-5.1-24.9-14.1-33.9L366.1 14.1c-9-9-21.2-14.1-33.9-14.1H192c-35.3 0-64 28.7-64 64V320c0 35.3 28.7 64 64 64zM64 128c-35.3 0-64 28.7-64 64V448c0 35.3 28.7 64 64 64H256c35.3 0 64-28.7 64-64V416H272v32c0 8.8-7.2 16-16 16H64c-8.8 0-16-7.2-16-16V192c0-8.8 7.2-16 16-16H96V128H64z"/></svg>'), 
                          list(extend = "csv", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 512 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M288 32c0-17.7-14.3-32-32-32s-32 14.3-32 32V274.7l-73.4-73.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l128 128c12.5 12.5 32.8 12.5 45.3 0l128-128c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L288 274.7V32zM64 352c-35.3 0-64 28.7-64 64v32c0 35.3 28.7 64 64 64H448c35.3 0 64-28.7 64-64V416c0-35.3-28.7-64-64-64H346.5l-45.3 45.3c-25 25-65.5 25-90.5 0L165.5 352H64zm368 56a24 24 0 1 1 0 48 24 24 0 1 1 0-48z"/></svg>')) ,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3A5311', 'color': '#fff'});",
            "}"))) %>%
      formatCurrency(c("RDC CBM", "RDC Pallet", "NDC CBM","NDC Pallet"), digits = 2)
    
    return(storage_dt)
  })
  
  
  output$DTmodel<- renderDT({
    
    ## call the value of the vector
    
    
    
    ## Data wranggling
    
   
    
    example_mt<-  data_example()|> 
      group_by(Product.code,Item_description,Manufacturing.type,Short.description,Brand_name,
               Supplier.Factory.code,POL,Target.service.level,Region,Forecast.demand,Actual.weight.kg,
               Length.cm, Height.cm,Width.cm,Packaging.type,`Product.volume.cm.`,Packaging.Length,
               Packaging.width,Packaging.height,Sensitivity)|> 
      summarise(Forecast.demand =mean(Forecast.demand))|> 
      ungroup() |>
      mutate( 
              Vm= if_else(Packaging.type=="Without packaging", 
               Length.cm*Height.cm*Width.cm/6000,
               Packaging.Length*Packaging.width*Packaging.height/6000), 
                Charge_weight= if_else(Vm>Actual.weight.kg, Vm,Actual.weight.kg ), 
                cbm=  round(Forecast.demand*Charge_weight/1000000,digits=2),# CBM calculation
              leftover_space = (1-cbm/ as.numeric(input$container ))*100,
             pkg_air_vol= if_else(Packaging.type=="Without packaging",
             Product.volume.cm./(  Length.cm* Height.cm*Width.cm),Product.volume.cm./(Packaging.Length*Packaging.width*Packaging.height)),
             pkg_utilization= 100- round(pkg_air_vol*Forecast.demand*100/1000000,digits=3)
      )
    
   
    
    
    ## Summarise the mean
    example_sum<-example_mt  |>
      group_by(Supplier.Factory.code,Region) |> 
      summarise(consolidated_cbm = round(sum(cbm),digits =2),
                consolidated_pkg_utilization =weighted.mean(pkg_utilization,cbm) ) 
    
    ## Join the data
    example_join<-left_join(example_mt, example_sum,
                            by = c("Supplier.Factory.code", "Region")) |>
      mutate(allocation_result_node_pr = case_when( 
        cbm <= 0 ~"No or wrong predictive volumes detected",
        Region == "MEL" & cbm>0  ~ "NDC check",
        Region != "MEL"& cbm >=  as.numeric(input$container)  ~ "RDC: FCL for single sku",
        Region != "MEL"& cbm<  as.numeric(input$container)   &leftover_space>0 & leftover_space<= as.numeric(input$space)  ~ "RDC: FCL for single sku by adjusting the volume quantity",
        Region != "MEL"& cbm <  as.numeric(input$container)  & consolidated_cbm >=   as.numeric(input$container)  &leftover_space> as.numeric(input$space) ~"RDC: FCL for consolidated sku",
        Region != "MEL"& consolidated_cbm <  as.numeric(input$container)  ~"NDC check",
        TRUE ~ NA_character_
      ),  Warning_message= case_when( cbm <= 0 ~"No or wrong predictive volume detected, please check your data.",
                                      allocation_result_node_pr!="NDC check"& consolidated_pkg_utilization<=as.numeric(input$airvol) ~" Warning: This batch of shipment carries high air volumes.",
                                      leftover_space>0 & leftover_space<= as.numeric(input$space)  ~"Warning: The current CBM delivery is just below the FCL threshold. Consider adjusting the volume quantity to meet the FCL requirement.")
      
      ) 
    ##NDC round check  
    
    example_sum_ndc<-example_join |> filter(allocation_result_node_pr =="NDC check") |>  
      group_by(Supplier.Factory.code) |>
      summarise(consolidated_cbm_ndc =round(sum(cbm), digits = 2),
                consolidated_pkg_utilization_ndc =weighted.mean(pkg_utilization,cbm) 
                )|> ungroup()
    
    
    example_join<- left_join(example_join, example_sum_ndc,  by = c("Supplier.Factory.code")) |> 
      mutate(
        allocation_result_node1 = case_when( 
          allocation_result_node_pr == "NDC check" & cbm >=  as.numeric(input$container)  ~ "NDC: FCL for single sku",
          allocation_result_node_pr == "NDC check" & cbm <  as.numeric(input$container)   & leftover_space > 0 & leftover_space <= as.numeric(input$space)  ~ "NDC: FCL for single sku by adjusting the volume quantity",
          allocation_result_node_pr == "NDC check" & cbm <  as.numeric(input$container)  & leftover_space > input$space & consolidated_cbm_ndc >= as.numeric(input$container)   ~ "NDC: FCL for consolidated sku",
          allocation_result_node_pr == "NDC check" & consolidated_cbm_ndc <  as.numeric(input$container)  ~ "Other strategy to wait for FCL consolidation",
          TRUE~allocation_result_node_pr
        ),
        Warning_message= case_when(allocation_result_node1 =="Other strategy to wait for FCL consolidation"~"Warning: The maximium of  consolidation cannot fulfill FCL. Other strategy  is required", 
                                   allocation_result_node_pr=="NDC check"&consolidated_pkg_utilization_ndc<= as.numeric(input$airvol) ~ " Warning: This batch of shipment carries high air volumes.",                       
                                   TRUE ~  Warning_message)
        
      )
    
    
    
    ## Call the price ratecard
    
    
    AMES_dtrans_exp<- data_dtrans() 
    
    AMES_dtrans_exp<- AMES_dtrans_exp |>
      filter( Origin =="VIC") 
    
    
    AMES_storage_exp<- data_storage() 
    
    AMES_storage_exp<- AMES_storage_exp |> 
      rename( rdc_storage=`RDC CBM`,
              ndc_storage =`NDC CBM`)
    
    
    
    AMES_sf_exp<- data_sf()
    
    
    AMES_sf_exp<-  AMES_sf_exp  |>
      mutate(Region =substring(POD, first =3)) |> 
      rename( rdc_sf=`RDC CBM`,
              ndc_sf =`NDC CBM`)
    
    
    ## Cost analysis
    example_cost_study <- example_join |> 
      left_join(AMES_sf_exp, by =c("POL","Region","Sensitivity")) |> 
      left_join(AMES_storage_exp, by="Region")|>
      left_join(AMES_dtrans_exp, by = c("State" = "Destination"))
    
    
    
    ## Result aggregation
    example_final_result <- example_cost_study |>
      mutate( 
        Reg_cost =   as.numeric(input$container ) *rdc_sf  + cbm *rdc_storage ,
        NDC_cost = consolidated_cbm * (ndc_sf+ rdc_storage + `CBM cost`),
        cost_difference =  if_else(Region == "MEL" |allocation_result_node1 %in% c("RDC: FCL for single sku", "RDC: FCL for consolidated sku","Other strategy to wait for FCL consolidation"), 
                                   NA, NDC_cost*(1+ as.numeric(input$cost)/100)-Reg_cost),
        allocation_result_node_final = if_else( !is.na(cost_difference)& cost_difference >= 0,"RDC: revise for cost analysis", allocation_result_node1),
        cost_difference =if_else( is.na(cost_difference),"Not appliable", paste0("$", round(cost_difference,digits = 2)))
        
      )
    
    
    Sys.sleep(2)
    
    tb_model<-  example_final_result |> rename(`Product code`=Product.code,
                                               `Short description` =Short.description,
                                               Brand =Brand_name,
                                               CBM = cbm,
                                               `Forecast demand` =Forecast.demand,
                                               `Consol CBM` =consolidated_cbm,
                                               `NDC Consol CBM` =consolidated_cbm_ndc,
                                               `Cost diff` =cost_difference ,
                                               `Allocation result`= allocation_result_node_final ,
                                               `Warning message`=Warning_message) |>
      select(`Product code`, `Allocation result`,`Warning message`,`Short description`,`Forecast demand`,
             `Cost diff`,CBM,`Consol CBM`,`NDC Consol CBM`)|>
      datatable(
        callback=JS('
    $("button.buttons-copy").css({
        "background": "#81D8D0",
         "border": "2px solid #000000",
         "color":"white",
         "fontweight":"bold",  
        "border-radius": "10px"
    });
    $("button.buttons-csv").css({
        "background": "#FF7900",
         "border": "2px solid #000000",  
        "border-radius": "10px", 
         "color":"white",
         "fontweight":"bold"
    });
    
    return table;
')
        
        ,
        extensions = c('Buttons', 'Scroller','FixedColumns'),
        options = list(
          scrollY = 500,
          scroller = TRUE,
          dom = 'Bfrtip',
          scrollX = TRUE,
          fixedColumns = TRUE,
          
          buttons = list( list(extend = "copy", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="14" viewBox="0 0 448 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M384 336H192c-8.8 0-16-7.2-16-16V64c0-8.8 7.2-16 16-16l140.1 0L400 115.9V320c0 8.8-7.2 16-16 16zM192 384H384c35.3 0 64-28.7 64-64V115.9c0-12.7-5.1-24.9-14.1-33.9L366.1 14.1c-9-9-21.2-14.1-33.9-14.1H192c-35.3 0-64 28.7-64 64V320c0 35.3 28.7 64 64 64zM64 128c-35.3 0-64 28.7-64 64V448c0 35.3 28.7 64 64 64H256c35.3 0 64-28.7 64-64V416H272v32c0 8.8-7.2 16-16 16H64c-8.8 0-16-7.2-16-16V192c0-8.8 7.2-16 16-16H96V128H64z"/></svg>'), 
                          list(extend = "csv", text = '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 512 512"><!--!Font Awesome Free 6.5.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2023 Fonticons, Inc.--><path d="M288 32c0-17.7-14.3-32-32-32s-32 14.3-32 32V274.7l-73.4-73.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l128 128c12.5 12.5 32.8 12.5 45.3 0l128-128c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L288 274.7V32zM64 352c-35.3 0-64 28.7-64 64v32c0 35.3 28.7 64 64 64H448c35.3 0 64-28.7 64-64V416c0-35.3-28.7-64-64-64H346.5l-45.3 45.3c-25 25-65.5 25-90.5 0L165.5 352H64zm368 56a24 24 0 1 1 0 48 24 24 0 1 1 0-48z"/></svg>')) ,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3A5311', 'color': '#fff'});",
            "}")))|> formatStyle(
              c("Allocation result","Warning message"), "white-space" = "pre-line"
            ) |> formatStyle("Warning message", 
                             color = "red")  |> 
      formatString(c("Consol CBM","NDC Consol CBM", "CBM"), suffix= HTML(' m<sup>3</sup>')) |> 
      formatStyle("Allocation result", 
                  BackgroundColor =  styleEqual(names(dt_color_list),dt_color_list),
                  fontWeight ="bold"  ) 
    
  })
  
  

  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(example, file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("storage_template", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(AMES_storage, file)
    }
  ) 
  
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("domestic_transportation_template", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(AMES_dtrans, file)
    }
  ) 
  
  
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("sea_freight_template", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(AMES_sf, file)
    }
  ) 
  
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("forecast_template", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(prod_exp, file)
    }
  ) 
  
  
  output$license_dfore<-renderUI({
    HTML( '<p style="color =grey; margin-left:10px;"> Version 1.0 | This webpage is licensed by <a href="https://creativecommons.org/licenses/by/3.0/"><i class="fa-brands fa-creative-commons"></i> CC 3.0</a> | Monash AMES project team')
  })
  
  
  output$license_price<-renderUI({
    HTML( '<p style="color =grey; margin-left:10px;"> Version 1.0 | This webpage is licensed by <a href="https://creativecommons.org/licenses/by/3.0/"><i class="fa-brands fa-creative-commons"></i> CC 3.0</a> | Monash AMES project team')
  })
  
  output$license_dcmodel<-renderUI({
    HTML( '<p style="color =grey; margin-left:10px;"> Version 1.0 | This webpage is licensed by <a href="https://creativecommons.org/licenses/by/3.0/"><i class="fa-brands fa-creative-commons"></i> CC 3.0</a> | Monash AMES project team')
  })
  
  output$license_about<-renderUI({
    HTML( '<p style="color =grey; margin-left:10px;"> Version 1.0 | This webpage is licensed by <a href="https://creativecommons.org/licenses/by/3.0/"><i class="fa-brands fa-creative-commons"></i> CC 3.0</a> | Monash AMES project team')
  })
  
  output$session_output <- renderPrint({
    sessioninfo::session_info()
  })
  
}

shinyApp(ui, server)