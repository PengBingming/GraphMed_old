
library('RColorBrewer')
library('ggrepel')
library('sf') 
library('geojsonsf')

library('showtext') # 解决画图中文乱码
showtext_auto()

library('readxl')
library('rhandsontable')
library('ggspatial')

library('plotly')
library('ggplot2')
library('shinyWidgets')
library('htmlwidgets')

# lang <- Translator$new(translation_csvs_path = "./lang/info/")

mapUI <- function(id) {
  ns <- NS(id)
  # shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
     tabPanel(title = 'Data',
              fluidRow(
              box(title=lang$t("地图数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                  splitLayout(cellWidths = c("60%"), rHandsontableOutput(ns("table") ) ) 
                  ),
              box(width = 3,status="success",
                  lang$t("数据来源："),a("GeoAtlas,v3", href = "http://datav.aliyun.com/portal/school/atlas/area_selector"),
                  textInput(ns("adcode"),  lang$t("地区1 adcode"),value = '100000'),
                  column( width = 12, selectInput(ns("quyu"), lang$t("地区1 子区域"), choices = c('with'='you','without'='wu'),selected = "you" ) ),
                  textInput(ns("adcode2"), lang$t("地区2 adcode"),value = ''),
                  column( width = 12, selectInput(ns("quyu2"),lang$t("地区2 子区域"), choices = c('with'='you','without'='wu'),selected = "you" ) ),
                  actionBttn(ns("submit"), label = lang$t("开始画图"),style = "fill", color = "primary", size = "sm" ),
                  hr(),
                  dropdownButton(label = lang$t("使用说明"), icon = icon('tachometer-alt'), circle = FALSE,
                                 br(),br(),
                                 h6("不需要下载数据，依据 adcode 画图。"),
                                 h6("先点击“开始画图”，默认画整个中国地图（省级）。"),
                                 h6("可依据子区域的 adcode 画省、市及县的地图。"),
                                 h6("地区1和地区2的 adcode 均输入内容则为拼图。"),
                                 h6("name 列为标签，value 列为数据，可编辑修改。")
                  ) )
      ) ),
     tabPanel(title = 'Plot',
      fluidRow(
       box(title = lang$t("中国地图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
           splitLayout(cellWidths = c("100%"),plotOutput(ns("plot0"),height = 500 ) ) 
       ),
       box(width=3,status="success",
           dropdownButton( label = lang$t("图形标签"),icon = icon('image'),circle = FALSE,width = NULL, 
                           br(),br(),
                           textInput(inputId = ns('title'),label = lang$t('标题'), value = 'ChinaMap' ),
                           textInput(inputId = ns('value'),label = lang$t('图例'), value = 'value'    ),
                           numericInput(inputId = ns('size'),label = lang$t('标签字号'),value = 2     ), 
                           selectInput(inputId = ns("low") , lang$t("低值颜色"), colors() , selected = "white" ),
                           selectInput(inputId = ns("high"), lang$t("高值颜色"), colors() , selected = "red"   ),
                           selectInput(ns("netline"), lang$t("经纬度线"),  c('with'='you','without'='wu'),selected = "wu" )
           ),br() ,
           dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                          br(),br() ,
                          numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 15),
                          numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 15),
                          numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
                          downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                          downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE )
           )
       )
     )
     ),
     tabPanel(title = 'Plotly',
              fluidRow(
                box(title = lang$t("中国地图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                    splitLayout(cellWidths = c("100%"),plotlyOutput(ns("plot1") ) ) 
                ),
                box(width=3,status="success",
                    actionBttn(ns("submitPlotly"), label = lang$t("开始画图"),style = "fill", 
                               color = "primary", size = "sm" ),hr(),
                    downloadButton(ns("downloadplot"),  label = ".html")
                    )
              )
     )
    ) # tabsetPanel
  ) # tagList
} # function(id)



mapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      
      observeEvent(input$submit, {
        
        # 数据：读取输入文件或参考数据
        df1 <- reactive({
          adcode <- input$adcode
          
          if( input$quyu=='you' ){
            
            if(adcode=='100000'){
              
              df1 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[,c('adcode','name','center','parent','geometry')]
              df2 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/100000.json")[,c('adcode','name','center','parent','geometry')]
              df2[,2]<- ''
              df1 <- na.omit(df1)
              df1 <- rbind(df2,df1)
              
            }else{
              
              site <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/",adcode,"_full.json")
              n <-  try(  read_sf( site ) )  
              logi <- 'try-error' %in% class( n ) 
              
              if(logi==T){ df1 <- NULL } # if( is.null(df3) ) 判断 adcode 是否正确
              else{
                df1 <- read_sf(site)[,c('adcode','name','center','parent','geometry')]
              }
            }
            
            
          }else{
            site <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/",adcode,".json")
            n <-  try(  read_sf( site) ) 
            logi <- 'try-error' %in% class( n ) 
            
            if(logi==T){ df1 <- NULL } # if( is.null(df3) ) 判断 adcode 是否正确
            else{
              df1 <- read_sf(site)[,c('adcode','name','center','parent','geometry')]
            }
          }
          
          
          adcode2 <- input$adcode2
          
          if( input$quyu2=='you' ){
            
            if(adcode2=='100000'){
              
              df3 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[,c('adcode','name','center','parent','geometry')]
              df4 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/100000.json")[,c('adcode','name','center','parent','geometry')]
              df4[,2]<- ''
              df3 <- na.omit(df3)
              df3 <- rbind(df4,df3)
              
            }else{
              
              site2 <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/",adcode2,"_full.json")
              n2 <-  try(  read_sf( site2 ) )  
              logi2 <- 'try-error' %in% class( n2 ) 
              
              if(logi2==T){ df3 <- NULL } # if( is.null(df3) ) 判断 adcode 是否正确
              else{
                df3 <- read_sf(site2)[,c('adcode','name','center','parent','geometry')]
              }
            }
            
            
          }else{
            site2 <- paste0("https://geo.datav.aliyun.com/areas_v3/bound/",adcode2,".json")
            n2 <-  try(  read_sf( site2) ) 
            logi2 <- 'try-error' %in% class( n2 ) 
            
            if(logi2==T){ df3 <- NULL } # if( is.null(df3) ) 判断 adcode 是否正确
            else{
              df3 <- read_sf(site2)[,c('adcode','name','center','parent','geometry')]
            }
          }
          
          df1 <- rbind(df1,df3)
          if( is.null(df1) ){  return(NULL) }
          df1$value <- 1:nrow(df1) -1
          df1$jd <- matrix(unlist(df1$center),ncol = 2, byrow = T)[,1]
          df1$wd <- matrix(unlist(df1$center),ncol = 2, byrow = T)[,2]
          return(df1)
          
        })
        
        df <- reactive({
          
          if( is.null(df1() ) ){return(NULL)}
          
          df <- df1()
          df <- cbind(df$adcode,df$name,df$value)
          colnames(df) <- c('adcode','name','value')
          return(df)
          
        })

        output$table <- renderRHandsontable(
          if( is.null(df1() ) ){return(NULL)}else{
            return(        rhandsontable(df(),rowHeaderWidth = 22,height = 500) %>% 
                             hot_cols(columnSorting = TRUE) %>% 
                             hot_col("adcode", readOnly = TRUE) %>% 
                             hot_col('value',type = 'numeric'))
          }
          
        )
        
          # 整合数据 china_data
          data_map <- reactive({
            
            if( is.null(df1() ) ){return(NULL)}
            
            df <- hot_to_r( input$table )
            df <- as.data.frame(df)
            data_map <- df1()
            
            if( !nrow(data_map)==nrow(df) ){
              return(data_map)
            }
            
            data_map[,1:2] <- df[,1:2]
            data_map$value <- as.numeric(df$value )
            
            return(data_map)
            
          })
          # 编写函数
          myfun <- reactive({
            
            if( is.null(df1() ) ){return(NULL)}
            
            data_map <- data_map() 
            
            
            p <- ggplot(data=data_map)+ geom_sf(aes(fill = value) ) + 
              
              fixed_plot_aspect(ratio = 1.25) + labs(title=input$title)+ 
              # coord_sf(crs = 4326)
              
              annotation_scale(location='bl')+   # 添加比例尺
              # 添加指北针
              annotation_north_arrow(location = "tl", which_north = "false",
                                     style = north_arrow_fancy_orienteering) +
              # 颜色.l
              scale_fill_gradient(name =input$value , low= input$low, high= input$high )+
              # 标签
              geom_text(data = data_map,aes(x=jd,y=wd,label= name),
                        position = "identity", size=input$size)
            
            if(input$netline=='wu'){  # 一、无经纬度网格线
              p <- p + theme_void()+  theme(  plot.title = element_text(hjust = 0.5,size=25 ) )
              
            }
            else if(input$netline=='you'){ # 二、添加经纬度网格线
              p <- p + theme( plot.title = element_text(hjust = 0.5,size=25 )  )
              
            }
            
            return( p)
            
          } )
          
          output$plot0 <- renderPlot({
            if( is.null(df1() ) ){return(NULL)}
            return( myfun() )
            
          }  )

          
          if(T){
            output$pdf <- downloadHandler(
              filename="map.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(myfun() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="map.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="map.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="map.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              }
            )
          }
          
          observeEvent(input$submitPlotly, {
            output$plot1 <- renderPlotly({
              if( is.null(df1() ) ){return(NULL)}
              return( ggplotly(myfun(),height = 550,width = 700) )
              
            }  )

            # # 下载图形 .html
            output$downloadplot <- downloadHandler(
              filename = function() {
                paste("ChinaMap.html")
              },
              content = function(file) {
                p <- myfun()
                htmlwidgets::saveWidget(as_widget(ggplotly(p)), file)
                
              }  )
          })
          
        
        # 下载参考数据
        output$downloadtable <- downloadHandler(
          filename = function() {
            paste('ChinaMap_data.csv')
          },
          content = function(file) {
            
            sample <- hot_to_r( input$table )
            write.csv(sample, file,  row.names = F, fileEncoding = 'GB18030')
          } )
        
      } )  #  observeEvent(input$submit, {
      

    } # function(input, output, session)
) # moduleServer
} # function(id) 




