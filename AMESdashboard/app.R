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


## preset_list

product_list<-unique(prod_exp$Product)

region_list<- unique(prod_exp$Region)

model_list<-c("ARIMA","ETS","Rolling average")

container_list<-c("40FT High-cube"=76,"20FT Standard"=33,"40FT Standard"=67,"20FT High-cube"=37)


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
               "button#dropdown_model{ font-weight: bold; background-color: #00A65A; color: white; border: black solid 2px; margin-left: 10px;}",
               "button#dropdown_region {font-weight: bold;background-color: mediumpurple; border: black solid 2px; margin-left: 20px;}",
               "button#dropdown_product { font-weight: bold; border: black solid 2px; margin-left: 30px; }"
    ), ## End basket of tags$style
    tabItems(
      tabItem(tabName = "dfore",
              p(HTML('<h2 style="text-align: center; 
                     color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;"> AMES Project Report</h2>')),
              br(),
              br(),
              fluidRow(width =12,
                       column(width =3,
                              dropdownButton(
                                inputId = "dropdown_product",
                                label ="Product",
                                circle=FALSE,
                                icon= icon("screwdriver-wrench"),
                                status ="warning",
                                selectizeInput("product","Products:",
                                               choices = product_list,
                                               selected =product_list[1],
                                               multiple= TRUE) )    
                       ),
                       
                       column(width =3,
                              dropdownButton(
                                inputId = "dropdown_region",
                                label ="region",
                                circle=FALSE,
                                icon= icon("earth-asia"),
                                status ="warning",
                                selectizeInput("region","Choices:",
                                               choices = region_list,
                                               selected =region_list[1],
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
                       
              ), ## fluidrow end bracket
              br(),
              br(),
              box(width =10, status ="success", title ="Pattern learning",  collapsible = FALSE, solidHeader = TRUE,
                  h3('Overall trend:'),
                  numericInputIcon("dygraphperiod","Period:", value=36, min = 1, max = 120, 
                                   icon =icon("sliders"), width ='30%'), 
                  withSpinner(dygraphOutput("dygraphresult")),
                  br(),
                  br(),
                  h3("Seasonality breakdown:"),
                  selectizeInput("seasontype","Season type:", 
                                 choices =c("Month", "Quarter"),
                                 selected= "Month",
                                 width ='30%'),
                  withSpinner(plotlyOutput("seasonalityresult"))
              ), # box end bracket
              br(),
              br(),
              
              box(width =10, status ="primary", title ="Demand forecasting",  collapsible = FALSE, solidHeader = TRUE,
                  h3('Seasonal/Trend/Residual decomposition'),
                  
                  selectizeInput("seasontypestl","Season type:", 
                                 choices =c("Month", "Quarter", "Year"),
                                 selected= "Month",
                                 width ='30%'),
                  withSpinner( plotOutput("stlresult")),
                  h3('Model prediction:'),
                  numericInputIcon("dforeperiod","Prediction period:", value=6, min = 1, max = 20, 
                                   icon =icon("sliders"), width ='30%'),
                  withSpinner( plotlyOutput("modelresult"))
              )    
              
      ), # End basket of dfore tab
      
      tabItem(tabName ="price",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy; ">AMES Project Report2</h2>')),
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
                             outputId = "downloadData4",
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
                    status = "success", solidHeader = TRUE, collapsible = TRUE,
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
              title = "The summary table of storage cost",
              status = "success", solidHeader = TRUE, collapsible = TRUE,
              withSpinner(DTOutput("dtranscost"))
          )
          
        ) ##End bracket of mainPanel
      ) ## End bracket of sidebarPanel
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
                              value = 80)
                  
                ),
                
                mainPanel(
                  box(width=12,
                      title = "The summary table of decision model",
                      status = "success", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(DTOutput("DTmodel"))
                  )
                )
                
              ) #End basket for sidebarLayout
              
      ),  # End basket of dcmodel tab
      
      tabItem(tabName ="about",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy;">AMES Project Report3</h2>'))
      )    # End basket of about tab
      
    ), ##End basket for tabItems
    includeCSS("monashreport.css")
  ) ##End basket for body
  
) ## End basket  for UI


## Part D server setup
server <- function(input, output) { 
  

  
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
  
  output$dygraphresult<-renderDygraph({
    
    ## Subset and mutation
    prod_exp_dygraph <-prod_exp |>
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
    prod_exp_tsb<-prod_exp |>
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
    prod_exp_tsb<-prod_exp |>
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
  
  output$modelresult<- renderPlotly({
    
    ## Define time transformation function based on selected input
    transform_time <- switch(input$seasontypestl,
                             "Month" = yearmonth,
                             "Quarter" = yearquarter,
                             "Year" = function(x) year(yearmonth(x))
    )
    
    ## Convert to tsibble function
    prod_exp_tsb<-prod_exp |>
      filter(Region ==input$region & Product%in% input$product)|>
      mutate(season = transform_time(Yearmonth))|>
      group_by(season,Region, Product) |>
      summarise(Volume=sum(Volume)) |>
      as_tsibble(index =season, key =Product)
    
    ## Model prediction
    forecast_result<- prod_exp_tsb |>
      model(ETS( Volume)) |> 
      forecast(h = input$dforeperiod) |>  
      as.tibble()  |> mutate(range =Volume,
                             Volume=.mean,
                             var =as.numeric(str_extract(range , 
                                                         "(?<=,\\s)\\d+\\.?\\d*(?:[eE][-+]?\\d+)?")),
                             sigma =sqrt(var),
                             low80 =  Volume- 1.282 *sigma,
                             high80 =  Volume+ 1.282 *sigma,
                             low90 =  Volume- 1.645*sigma,
                             high90 =  Volume+ 1.645 *sigma
      )## Calculate the upper/lower CI
    
    
    prod_exp_tsb_tail<- prod_exp_tsb|>tail( n =36)
    
    ## Combine the data
    prod_exp_bind <-rbind(as.tibble(prod_exp_tsb) |> select(Product,season, Volume),
                          forecast_result |> select( Product,season, Volume)
    )  |> filter( season >=  prod_exp_tsb_tail$season[1])
    
    
    
    
    Sys.sleep(1)
    
    predictplot<-ggplot(data = forecast_result) +
      geom_line(data=prod_exp_bind, 
                aes(season, Volume,color =Product), 
                size =0.8,linetype ="dotdash") +
      theme_bw() +
      geom_ribbon( aes(x = season, ymin =low80,  
                       ymax =high80,fill = Product ), alpha =0.8) + 
      geom_ribbon( aes(x = season, ymin =low90, 
                       ymax =high90, fill = Product ), alpha =0.2) + 
      scale_fill_discrete_qualitative(palette ="Dynamic") +
      geom_line( aes( season, Volume,color =Product),  size =0.8)
    
    ggplotly(predictplot) 
  })
  
  output$sfcost<-renderDT({
    
    AMES_sf_mt<- AMES_sf |> rename(`RDC Pallet` =Pallet.cost) |>
      mutate(`RDC CBM` = `RDC Pallet`  /1.3)
    
    AMES_sf_ndc<-AMES_sf_mt |> 
      filter(POD== "AUMEL") |>
      rename(`NDC Pallet`=`RDC Pallet`,
             `NDC CBM` =`RDC CBM`) |> 
      select(-POD)
    
    AMES_sf_mt<-left_join(AMES_sf_mt, AMES_sf_ndc, by =c("POL","Sensitivity")) 
    
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
   AMES_dtrans_mt<-AMES_dtrans |> rename(`Pallet cost` =Average.Pallet.Freight.Cost,
                                         Origin =Origin.DC) |>
     mutate(`CBM cost` =`Pallet cost`/1.3)|>
     select(Combo, Origin, Destination, `Pallet cost`, `CBM cost`, Comment)
   
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
    
    AMES_storage_mt<- AMES_storage_mt %>% mutate(`RDC Pallet` =if_else(Region =="MEL",NA,`RDC Pallet`),
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
    example_mt<- data_example()  |> mutate(Profit = Wholesale.price-COGs, ## Calculate the Cost
                                           Month =yearmonth(Month), ## Convert to year month
                                           Vm= if_else(Packaging.type=="Without packaging", 
                                                       Length.cm*Height.cm*Width.cm/6000,
                                                       Packaging.Length*Packaging.width*Packaging.height/6000), 
                                           Charge_weight= if_else(Vm>Actual.weight.kg, Vm,Actual.weight.kg ), 
                                           cbm=  Forecast.demand*Charge_weight/1000000,# CBM calculation
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
        cbm <= 0 ~" No or wrong predictive orders detected",
        Region == "MEL" & cbm>0  ~ "NDC check",
        Region != "MEL"& cbm >=  input$container  ~ "RDC: FCL for single sku",
        Region != "MEL"& cbm<  input$container   &leftover_space>0 & leftover_space<=5 ~ "RDC: FCL for single sku",
        Region != "MEL"& cbm <  input$container  & consolidated_cbm >=  input$container   &leftover_space>5 ~"RDC: FCL for consolidated sku",
        Region != "MEL"& consolidated_cbm <  input$container  ~"NDC check",
        TRUE ~ NA_character_
      ),  Warning_message= case_when( cbm <= 0 ~"No or wrong predictive orders detected, please check your data.",
                                      consolidated_pkg_utilization<= input$airvol ~" Warning: This batch of shipment carries highair volumes.",
                                      leftover_space>0 & leftover_space<=5  ~"Warning: The current CBM delivery is just below the FCL threshold. Consider adjusting the volume quantity to meet the FCL requirement.")
      
      ) 
    ##NDC round check  
    
    example_sum_ndc<-example_join |> filter(allocation_result_node_pr =="NDC check") |>  
      group_by(Supplier.Factory.code) |>
      summarise(consolidated_cbm_ndc =sum(cbm))|> ungroup()
    
    
    example_join<- left_join(example_join, example_sum_ndc,  by = c("Supplier.Factory.code")) |> 
      mutate(
        allocation_result_node1 = case_when( 
          allocation_result_node_pr == "NDC check" & cbm >= input$container  ~ "NDC: FCL for single sku",
          allocation_result_node_pr == "NDC check" & cbm <  input$container   & leftover_space > 0 & leftover_space <= 5 ~ "NDC: FCL for single sku",
          allocation_result_node_pr == "NDC check" & cbm <  input$container  & leftover_space > 5 & consolidated_cbm_ndc >= input$container  ~ "NDC: FCL for consolidated sku",
          allocation_result_node_pr == "NDC check" & consolidated_cbm_ndc <  input$container  ~ "Other strategy to wait for FCL consolidation",
          TRUE~allocation_result_node_pr
        ),
        Warning_message= if_else(allocation_result_node1 =="Other strategy to wait for FCL consolidation",
                                 "Warning: The maximium of  consolidation cannot fulfill FCL. Other strategy  is required", Warning_message)
        
      )
    
    
    ## Cost analysis
    example_cost_study <- example_join |> 
      left_join(sea_freight, by =c("POL","Region","Sensitivity")) |> 
      left_join(d_transporation, by = c("Region" = "Destination")) |> 
      left_join(storage, by=c("Region" = "Facility"))
    
    
    ## Result aggregation
    example_final_result <- example_cost_study |>
      mutate( 
        Reg_cost =   as.numeric(input$container )  * (`Sea freight Cost per Cubic Meter` + `Inventory Cost per Cubic Meter per day` + `Other fixed_miscellaneous`),
        NDC_cost = cbm * (`NDC sea freight` + `Land truck Cost per Cubic Meter`+ `NDC Inventory`),
        cost_difference =  NDC_cost*(1+ as.numeric(input$cost)/100)-Reg_cost,
        allocation_result_node_final = if_else(Region != "MEL" & cost_difference <= 0 & allocation_result_node1 %in% c("NDC: FCL for single sku", "NDC: FCL for consolidated sku"), "RDC: revise for cost analysis", allocation_result_node1)
      )
   
    
    Sys.sleep(2)
    
    tb_model<-  example_final_result |> rename(`Product code`=Product.code,
                                               `Short description` =Short.description,
                                               Brand =Brand_name,
                                               `Consol CBM` =consolidated_cbm,
                                               `Cost diff` =cost_difference ,
                                               `Allocation reult`= allocation_result_node_final ,
                                               `Warning message`=Warning_message)|>
      mutate(Month =as.character(Month)) |>
      select(`Product code`,`Short description`, Month,Region,`Consol CBM`,`Cost diff` ,`Allocation reult`,`Warning message`) |> 
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
              c("Allocation reult","Warning message"), "white-space" = "pre-line"
            ) |> formatStyle("Warning message", 
                             color = "red") |> 
      formatCurrency("Cost diff", digits =2) |> 
      formatString("Consol CBM", suffix= HTML(' m<sup>3</sup>')) 
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
  
}

shinyApp(ui, server)