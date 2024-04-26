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
library(tools)

# Part B read data & wrangling
prod_exp<- read.csv("data/production.csv")

example<-read.csv("data/example.csv")

sea_freight<-read_excel("data/price_ratecard.xlsx", sheet = "sea_freight")%>%
  mutate(Region =substring(POD, first =3))

d_transporation<-read_excel("data/price_ratecard.xlsx", sheet = "transportation")

storage<-read_excel("data/price_ratecard.xlsx", sheet = "storage")

## Mutate the data


prod_exp_clear<- prod_exp %>% 
  mutate(Yearmonth=yearmonth(Yearmonth)) %>% 
   pivot_longer(Product.A:Product.F, 
               names_to = "Product", 
               values_to ="Volume") %>% 
  mutate(Product = str_replace(Product,"\\.", " "))

## Convert as  tsibble (temporal data）
prod_exp_clear_ts<-prod_exp_clear %>% 
  as_tsibble(index=Yearmonth, key=Product)


## preset_list

product_list<-unique(prod_exp_clear$Product)

model_list<-c("ARIMA","ETS","Rolling average")

container_list<-c("40FT High-cube"=76*0.8,"20FT Standard"=33*0.8,"40FT Standard"=67*0.8,"20FT High-cube"=37*0.8)


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
              "button#dropdown_product { font-weight: bold; border: black solid 2px; margin-left: 20px; }"
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
                                multiple= TRUE)
              )   ),
              
              
              
              column(width =3,
                     dropdownButton(
                       inputId = "dropdown_model",
                       label ="Model",
                       circle=FALSE,
                       icon= icon("screwdriver-wrench"),
                       status ="warning",
                       selectizeInput("model","Choices:",
                                      choices = model_list,
                                      selected ="ETS",
                                      multiple= TRUE)
                     )  )
              
           
              
              ),#End basket for fluidRow
              
              plotlyOutput("Result")
              
      ), # End basket of dfore tab
      
      tabItem(tabName ="price",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy; ">AMES Project Report2</h2>'))
      ),  # End basket of price tab
      
      tabItem(tabName ="decmodel",
              p(HTML('<h2 style="text-align: center;color:#8b0000;
                     font-weight: bold;
                     font-family: fantasy; ">AMES Project Report</h2>')),
              br(),
              br(),
              
              sidebarLayout(
                
              sidebarPanel(
                fileInput("file1", "Choose CSV File", accept = ".csv"),
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
                selected = 67*0.8,  # Corrected value
                multiple = FALSE),
                br(),
               setSliderColor(c("#FF4500" , "Teal"), c(1, 2)),
               sliderInput("profit",
                           "Profit threshold(%):",
                           min = 0,
                           max = 100,
                           value = 10),
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
  
  data <- reactive({
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
  
output$DTmodel<- renderDT({
  
  
  ## Data wranggling
  example_mt<- data() %>% mutate(Profit = Wholesale.price-COGs, ## Calculate the Cost
                                 Month =yearmonth(Month), ## Convert to year month
                                 Vm= if_else(Packaging.type=="Without packaging", 
                                             Length.cm*Height.cm*Width.cm/6000,
                                             Packaging.Length*Packaging.width*Packaging.height/6000), 
                                 Charge_weight= if_else(Vm>Actual.weight.kg, Vm,Actual.weight.kg ),                                        upcome_order= Forecast.demand - Occupied.quantity,
                                 cbm=  Forecast.demand*Charge_weight/1000000,# CBM calculation
                                 pkg_air_vol= if_else(Packaging.type=="Without packaging",
                                                      Product.volume.cm./(  Length.cm* Height.cm*Width.cm),
                                                      Product.volume.cm./(Packaging.Length*Packaging.width*Packaging.height)),                                   pkg_utilization= 100- round(pkg_air_vol*upcome_order*100/1000000,digits=3)
  )

 ## Summarise the mean
  example_sum<-example_mt  %>%
    group_by(Supplier.Factory.code,Region) %>% 
    summarise(consolidated_cbm =sum(cbm),
              consolidated_pkg_utilization =weighted.mean(pkg_utilization,cbm) )
  
  ## Join the data
  example_join<-left_join(example_mt, example_sum,
                          by = c("Supplier.Factory.code", "Region"))   %>% 
    mutate(allocation_result_node_pr = case_when( 
      cbm <= 0 ~" No or wrong predictive orders detected",
      Region == "MEL" & cbm>0  ~ "NDC",
      Region != "MEL"& consolidated_cbm >= input$container ~ "RDC",
      Region != "MEL"& consolidated_cbm <  input$container & consolidated_cbm>0 ~ "Other strategy to wait for FCL consolidation",
      TRUE ~ NA_character_
    ))
  
  example_sum_ndc<-example_join %>% filter(allocation_result_node_pr !="RDC") %>%  
    group_by(Supplier.Factory.code) %>%
    summarise(consolidated_cbm_ndc =sum(cbm))
  
  example_join<- left_join(example_join, example_sum_ndc,  by = c("Supplier.Factory.code"))%>% 
    mutate(allocation_result_node1 = case_when( 
      allocation_result_node_pr %in% c("NDC","Other strategy to wait for FCL consolidation") & consolidated_cbm >40 ~ "NDC",
      TRUE~allocation_result_node_pr),
      Warning_message= case_when( cbm <= 0 ~"No or wrong predictive orders detected, please check your data.",
                                  allocation_result_node1=="Other strategy to wait for FCL consolidation"~ "Warning: The maximium of  consolidation cannot fulfill FCL. Other strategy  is required",
                                  consolidated_pkg_utilization<= input$airvol ~" Warning: This batch of shipment carries highair volumes."))
  ## Node2 cost&profit study
  example_cost_stdy <- example_join %>% 
    left_join(sea_freight, by =c("POL","Region","Sensitivity")) %>% 
    left_join(d_transporation, by = c("Region" = "Destination")) %>% 
    left_join(storage, by=c("Region" = "Facility"))
  
  ## Combine the result
  example_final_result<- example_cost_stdy %>%
    mutate( Rev = Profit* Forecast.demand,
            Reg_cost =  cbm*(`Sea freight Cost per Cubic Meter`+`Land truck Cost per Cubic Meter`+`Inventory Cost per Cubic Meter per day` + `Other fixed_miscellaneous`),
            NDC_cost= cbm*(`NDC Inventory`+`NDC Miscellenous`+ `NDC sea freight`),
            Profit_Rec =if_else(allocation_result_node1 =="RDC", 
                                (Rev-Reg_cost)/(Wholesale.price*Forecast.demand)*100,NA ),
            Profit_NDC =(Rev-NDC_cost)/(Wholesale.price*Forecast.demand)*100,
            allocation_result_node2=if_else(Profit_NDC -Profit_Rec >=input$profit, "NDC","RDC"),
            allocation_result_final =if_else(is.na(allocation_result_node2), allocation_result_node1,allocation_result_node2),
            Final_profit =if_else(allocation_result_final=="RDC",Profit_Rec, Profit_NDC) )
  
  
  Sys.sleep(2)
  tb_model<-  example_final_result %>% rename(`Product code`=Product.code,
                                              `Short description` =Short.description,
                                              Brand =Brand_name,
                                              `Consol CBM` =consolidated_cbm,
                                              `Net profit` =Final_profit ,
                                              `Allocation reult`=allocation_result_final,
                                              `Warning message`=Warning_message)%>%
    mutate(Month =as.character(Month)) %>%
    select(`Product code`,`Short description`, Month,Region,`Consol CBM`,`Net profit` ,`Allocation reult`,`Warning message`) %>% 
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
          "}")))%>% formatStyle(
            c("Allocation reult","Warning message"), "white-space" = "pre-line"
          ) %>% formatStyle("Warning message", 
                            color = "red") %>%
    formatRound(c("Net profit","Consol CBM"),digits = 2)%>% 
    formatString("Consol CBM", suffix= HTML(' m<sup>3</sup>')) %>% 
    formatCurrency("Net profit")
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


