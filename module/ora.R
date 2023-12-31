
library("ReactomePA")

library('clusterProfiler') # 基因 ID 转换、富集
library('org.Hs.eg.db')    # 人数据库 hsa
library('org.Mm.eg.db')    # 鼠数据库 mmu
library('org.Rn.eg.db')    # 鼠数据库 rat

geneName <- intersect(columns(org.Mm.eg.db),columns(org.Hs.eg.db) )

library('rhandsontable')

library("showtext") #中文问题
showtext_auto()

geneName <- intersect(columns(org.Mm.eg.db),columns(org.Hs.eg.db) ) #  基因ID类型

oraUI <- function(id) {
  ns <- NS(id)
  tagList(
      bs4Dash::tabsetPanel(
        tabPanel(title = 'Data',
                 fluidRow(
                   box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("DEG") ) ) 
                   ),
                   box(width = 3,status="success",
                       fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                       h6(lang$t('格式：.csv .xlsx .xls')),
                       actionBttn( inputId = ns("show"), label = "Show Data", 
                                   style = "fill", color = "primary", size = "sm" ), hr(),
                       fluidRow(
                         column(width = 6,selectInput(inputId = ns("ID"),lang$t('基因列'),c("")) ),
                         column(width = 6,selectInput(inputId = ns("species"), lang$t("物种"),selected = "hsa",
                                                      choices = c('human'= "hsa", 'mouse' = "mmu", "rat" ="rat") ) )
                       ),
                       selectInput(ns('type'),lang$t('输入数据类型'),selected = 'gene',choices = c('diff gene'='gene','diff matrix'='DEG')),
                       conditionalPanel(
                         condition = "input.type=='DEG'",ns = NS(id),
                         dropdownButton(circle=FALSE,label=lang$t("差异矩阵参数"),  br(),br(),
                                        fluidRow(
                                          column(width = 7, selectInput( inputId = ns("pvalue_col"),  lang$t('pvalue 列'), c("")) ),
                                          column(width = 5, numericInput(inputId = ns("pvalue"),      label = "pvalue",    value = 0.05)),
                                          column(width = 7, selectInput( inputId = ns("padj_col"),    lang$t('padj 列'),   c(""))),
                                          column(width = 5, numericInput(inputId = ns("padj")  ,      label = "padj"  ,    value = 0.1)),
                                          column(width = 7, selectInput( inputId = ns("logFC_col"),   lang$t('logFC 列'),  c(""))),
                                          column(width = 5, numericInput(inputId = ns("logFC_cutoff"),label = "logFC",     value = 1)),
                                          column(width = 12, selectInput( inputId = ns("kegg_gene"),   lang$t("富集基因"),  selected = 'diff',
                                                                          choices =  c('diff gene'='diff', 'up gene'='up','down gene'='down')  ) ))
                         ) ) ,hr(),
                       downloadButton(ns("downloadSampleData"), lang$t("参考数据")),br(),br(),
                       dropdownButton( label = lang$t("使用说明"), icon = icon('tachometer-alt'), circle = FALSE,br(),br(),
                                       h6("1、输入数据支持“差异基因”和“差异矩阵”。"),
                                       h6("2、首先确定物质和ID列。“差异矩阵”还需要需要确定 pvalue、padj、logFC 列以及对应参数。"),
                                       h6("3、最后确定基因名类型，运行。若不知道基因名类型，可选择 unknown。
                             运行时间较长，点击运行后请耐心等待小会儿。")
                       )
                   ) )
                 ),
        tabPanel(title = 'ORA',
                 fluidRow(
                   box(title=lang$t("ORA 分析"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_kegg0"),height = 500 ) ) ),
                   box(width = 3,status="success",
                       actionBttn( inputId = ns("submit1"), label = "Analyze Data", 
                                   style = "fill", color = "primary", size = "sm" ), hr(),
                       selectInput(ns("geneid"),label = lang$t('ID列基因'), c(geneName,"unknown") , selected = "SYMBOL"),
                       selectInput(ns("plot_kegg_id1"), lang$t("图形选择："), choices = c("p_dot","p_bar", "p_emap") ),
                       dropdownButton(circle=FALSE,label=lang$t("差异矩阵参数"),  br(),br(),
                                      numericInput(inputId = ns("pvalue"), label = "pvalue", value = 0.05),
                                      numericInput(inputId = ns("padj")  , label = "padj"  , value = 0.1),
                                      numericInput(inputId = ns("logFC_cutoff"), label = "log2FoldChange",value = 1) ,
                                      selectInput(ns("kegg_gene"), lang$t("富集基因"), selected = 'diff',
                                                  choices =  c('diff gene'='diff', 'up gene'='up','down gene'='down')  ) ),br(),
                       numericInput(inputId = ns("kegg_num0"),  label = lang$t('通路数目'), value = 10),
                       dropdownButton(circle=FALSE,label=lang$t("富集参数"),  br(),br(),
                                      numericInput(inputId = ns("kegg_min"),   label = 'minGSSize', value = 10 ),
                                      numericInput(inputId = ns("kegg_max"),   label = 'maxGSSize', value = 500 ),
                                      numericInput(inputId = ns("pvalue_kegg"),label = lang$t('通路 P 值'), value = 1 ),
                                      numericInput(inputId = ns("padj_kegg"),  label = lang$t('通路 P.adj'), value = 1 ) 
                       ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                      br(),br() ,
                                      numericInput(inputId = ns('w0'),label = lang$t('下载图宽'),value = 15),
                                      numericInput(inputId = ns('h0'),label = lang$t('下载图高'),value = 15),
                                      numericInput(inputId = ns('ppi0'),label = lang$t('分辨率'),value = 72),
                                      downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE )
                       )
                   ) )
        ),
        tabPanel(title = 'Pathways',
                 fluidRow(
                   box(title=lang$t("通路选择"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_kegg1"),height = 500 ) ) ),
                   box(width = 3,status="success",
                       actionBttn(ns("submit_kegg1"), lang$t("开始画图"),style = "fill", 
                                  color = "primary", size = "sm"),hr(),
                       selectInput( inputId = ns("kegg_num1"),  label = lang$t('通路选择'), c(""),multiple = T ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                      br(),br() ,
                                      numericInput(inputId = ns('w1'),label = lang$t('下载图宽'),value = 15),
                                      numericInput(inputId = ns('h1'),label = lang$t('下载图高'),value = 15),
                                      numericInput(inputId = ns('ppi1'),label = lang$t('分辨率'),value = 72),
                                      downloadBttn(outputId = ns("pdf1") , label = "PDF" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("png1") , label = "PNG" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("jpeg1"), label = "JPEG", size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("tiff1"), label = "TIFF", size='sm', block=TRUE )
                       )
                   ) )
        ),
        tabPanel(title = 'Genes',
                 fluidRow(
                   box(title=lang$t("通路基因"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_kegg2"),height = 500 ) ) ),
                   box(width = 3,status="success",
                       actionBttn(ns("submit_kegg2"), lang$t("开始画图"),style = "fill", 
                                  color = "primary", size = "sm"),hr(),
                       selectInput(ns("kegg_kk"), lang$t("通路选择"), c(""), multiple = T ),
                       selectInput( ns("plot_kegg_id2"), lang$t("图形选择："), c('cnetplot'='p1', 'cnetplot_circ'= 'p2') ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                      br(),br() ,
                                      numericInput(inputId = ns('w2'),label = lang$t('下载图宽'),value = 15),
                                      numericInput(inputId = ns('h2'),label = lang$t('下载图高'),value = 15),
                                      numericInput(inputId = ns('ppi2'),label = lang$t('分辨率'),value = 72),
                                      downloadBttn(outputId = ns("pdf2") , label = "PDF" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("png2") , label = "PNG" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("jpeg2"), label = "JPEG", size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("tiff2"), label = "TIFF", size='sm', block=TRUE )
                       )
                   )
                 ) # fluidRow
        )
        )
  ) # NS(id)
} # function(id) 

oraServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      observeEvent(input$show, { 
        
        # 输入数据
        DEG <- reactive({
          file1 <- input$file1
          if(is.null(file1) ){
            ifelse(input$type=="DEG",
                   DEG <- read.csv(('./www/DEG.csv'))
                   ,
                   DEG <- data.frame("ID"=read.csv(('./www/DEG.csv'))[1:200,1])
            )
          }
          else{
            
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            
            if(d=='csv'){
              DEG <- data.frame( read.csv(file1$datapath, fileEncoding = "GB18030") )
            } else{
              DEG <- data.frame( read_excel(file1$datapath,1) ) 
            } 
          }
          
          # lis <- strsplit(DEG$ID,'[.]')
          # for (i in 1:length(lis) ) {
          #   DEG$ID[i] <- lis[[i]][1]
          # } ; rm(i)
          
          return( DEG )
          
        })
        
        # 输入数据
        DEG <- reactive({
          file1 <- input$file1
          if(is.null(file1) ){
            ifelse(input$type=="DEG",
                   DEG <- read.csv(('./www/DEG.csv'))
                   ,
                   DEG <- data.frame("ID"=read.csv(('./www/DEG.csv'))[1:200,1])
            )
          }
          else{
            
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            
            if(d=='csv'){
              DEG <- data.frame( read.csv(file1$datapath, fileEncoding = "GB18030") )
            } else{
              DEG <- data.frame( read_excel(file1$datapath,1) )
            }
          }
          
          # lis <- strsplit(DEG$ID,'[.]')
          # for (i in 1:length(lis) ) {
          #   DEG$ID[i] <- lis[[i]][1]
          # } ; rm(i)
          
          return( DEG )
          
        })
        
        if(!is.null(DEG() )){ 
          
          observe({
            
            DEG <- DEG()
            
            updateSelectInput(session ,inputId = 'ID',label = '基因列',
                              choices = colnames(DEG),selected = colnames(DEG)[1])
            if(!input$ID==''){
              output$DEG <- renderRHandsontable(
                rhandsontable(DEG,rowHeaderWidth = 50, height = 500) %>%
                  hot_cols(columnSorting = TRUE) %>%
                  hot_col(setdiff(colnames(DEG ),input$ID),type = 'numeric') %>%
                  hot_col(input$ID,type = 'dropdown')
              )
            }
            
            if(input$type=='DEG'){
              updateSelectInput(session ,inputId = 'pvalue_col',label = 'pvalue 列',
                                choices = colnames(DEG),selected = '' )
              updateSelectInput(session ,inputId = 'padj_col',  label = 'padj 列',
                                choices = colnames(DEG),selected = '' )
              updateSelectInput(session ,inputId = 'logFC_col', label = 'logFC 列',
                                choices = colnames(DEG),selected = '' )
            }
            
            # ORA 富集
            observeEvent(input$submit1, {
              
              OrgDb.db <- reactive({
                
                if(input$species == 'hsa'){ OrgDb.db <- org.Hs.eg.db }
                else if(input$species == 'mmu'){ OrgDb.db <- org.Mm.eg.db }
                else if(input$species == 'rat'){ OrgDb.db <- org.Rn.eg.db }
                
                return(OrgDb.db)
              })
              
              observeEvent(input$DEG, { 
                
                DEG <- as.data.frame( hot_to_r( input$DEG ) )
                DEG <- dplyr::rename(DEG, c('ID'=input$ID))
                
                # if(input$type=='DEG'){
                #   if(length(intersect(colnames(DEG),"logFC") ) > 0){
                #     colnames(DEG)[which(colnames(DEG)=="logFC")] <- 'log2FoldChange'
                #   }
                #   if(length(intersect(colnames(DEG),"P.Value") ) > 0){
                #     colnames(DEG)[which(colnames(DEG)=="P.Value")] <- 'pvalue'
                #   }
                #   if(length(intersect(colnames(DEG),"adj.P.Val") ) > 0){
                #     colnames(DEG)[which(colnames(DEG)=="adj.P.Val")] <- 'padj'
                #   }
                # }
                
                
                # 自动判断输入基因名类型
                name <- reactive({
                  if(input$geneid=='unknown'){
                    dat1 <- DEG
                    if(nrow(dat1)>200){
                      dat2 <- dat1[1:200,]
                    }
                    else{
                      dat2 <- dat1
                    }
                    
                    dfName <- data.frame()
                    
                    for (i in geneName){
                      n <- try(bitr(unique(dat2$ID), fromType <- i, toType <- "MAP", OrgDb <- OrgDb.db() ),silent=T)
                      logi <- 'try-error' %in% class( n ) 
                      
                      if(logi== F ){
                        if(nrow(dfName) < nrow(n)){
                          dfName <-  n
                          name <- colnames(dfName)[1]
                        }
                      }
                    } 
                    
                  }
                  else{
                    name <- input$geneid
                  }
                  
                  return(name)
                })
                
                # 选择 "ENTREZID" 基因
                gene_list <- reactive({
                  
                  if(is.null(input$DEG )){return(NULL)}
                  
                  dat1 <- DEG
                  name <- name()
                  
                  if(name=="ENTREZID"){
                    dat1$ENTREZID <- dat1$ID
                    DEG <- dat1
                  }
                  else if(!name=="ENTREZID"){
                    dfName <- bitr(unique(dat1$ID), fromType <- name, toType <- "ENTREZID" , OrgDb <- OrgDb.db())
                    DEG <- merge(dat1, dfName, by.x='ID', by.y= name())
                  }
                  
                  if(input$type=='DEG'){
                    if(!input$logFC_col==""&!input$pvalue_col==""&!input$padj_col==''){        
                      expr <- paste0("ifelse(DEG$",input$pvalue_col, "<" ,input$pvalue,"&","abs(DEG$",input$logFC_col,")>",input$logFC_cutoff,
                                     ",ifelse( DEG$",input$logFC_col,">",input$logFC_cutoff,",'UP','DOWN'),'NOT')")
                      
                      DEG$change <- as.factor( eval(parse(text = expr ) ) )
                      
                      # DEG$change <- as.factor(
                      #   ifelse(
                      #     DEG$pvalue < input$pvalue & abs(DEG$log2FoldChange) > input$logFC_cutoff,ifelse( DEG$log2FoldChange> input$logFC_cutoff,'UP','DOWN'),
                      #     'NOT'))
                      
                      # 3.1 选出上调基因的'ENTREZID' ID
                      gene_up <- DEG[DEG$change == 'UP','ENTREZID']
                      
                      # 3.2 下调基因ID
                      gene_down <- DEG[DEG$change == 'DOWN','ENTREZID']
                      
                      # 3.3 差异基因ID
                      gene_diff <- c(gene_up,gene_down)
                      
                      # 3.4 所有基因ID
                      gene_all <- as.character(DEG[ ,'ENTREZID'] )
                      
                      # 3.4 geneList: LogFC # DEG$log2FoldChange 
                      geneList <- eval(parse(text = paste0("DEG$",input$logFC_col) ) ) # 把 DEG 数据logFC列值赋值给数据geneList
                      
                      names(geneList) <- DEG$ENTREZID # 把ID赋值给geneList数据的名字
                      geneList <- sort(geneList, decreasing = T) # 把数据进行排序
                      
                      # 3.5 选取 logFC ＞ logFC_cutoff 的基因
                      gene <- names(geneList)[abs(geneList) > input$logFC_cutoff ]
                      
                      # 保存数据待用
                      gene_list <- list()
                      
                      gene_list$gene_up <- gene_up
                      gene_list$gene_down <- gene_down
                      gene_list$gene_diff <- gene_diff
                      
                      gene_list$gene <- gene
                      gene_list$gene_all <- gene_all
                      
                      gene_list$geneList <- geneList
                    }
                  }
                  else{
                    gene_list <- DEG[,'ENTREZID']
                  }
                  
                  return(gene_list)
                })
                
                organism <- reactive({
                  if(input$species=='hsa'){return('human')}
                  else if(input$species=='mmu'){return('mouse')}
                  else if(input$species=='rat'){return('rat')}
                })
                
                # ORA 富集、网页展示
                kk <- reactive({
                  
                  if(is.null(gene_list() )){return(NULL)}
                  
                  gene_list <- gene_list()
                  
                  if(input$type=='DEG'){
                    # 基因选择
                    
                    if(input$kegg_gene=='diff'){
                      gene_kegg <- gene_list$gene_diff
                    }
                    else if(input$kegg_gene=='up'){
                      gene_kegg <- gene_list$gene_up
                    }
                    else if(input$kegg_gene=='down'){
                      gene_kegg <- gene_list$gene_down
                    }
                    
                    geneList <- gene_list$geneList
                    
                    # 2.1 差异基因富集
                    kk <- enrichPathway(gene          = gene_kegg ,
                                        organism      = organism(),
                                        pvalueCutoff  = input$pvalue_kegg,
                                        qvalueCutoff  = input$padj_kegg ,
                                        universe      = names(geneList),
                                        pAdjustMethod = "BH",
                                        minGSSize     = input$kegg_min,
                                        maxGSSize     = input$kegg_max,
                                        readable      = T)
                    
                    kk <- setReadable(kk, OrgDb = OrgDb.db(), keyType = "ENTREZID")
                  }
                  else{
                    # 基因选择
                    gene_kegg <- gene_list
                    
                    # 2.1 差异基因富集
                    kk <- enrichPathway(gene          = gene_kegg ,
                                        organism      = organism(),
                                        pvalueCutoff  = input$pvalue_kegg,
                                        qvalueCutoff  = input$padj_kegg ,
                                        pAdjustMethod = "BH",
                                        minGSSize     = input$kegg_min,
                                        maxGSSize     = input$kegg_max,
                                        readable      = T)
                    
                    kk <- setReadable(kk, OrgDb = OrgDb.db(), keyType = "ENTREZID")
                  }
                  return(kk)
                  
                } )
                
                observe({
                  
                  if(is.null(kk() ) ) {return(NULL) }  
                  
                  kk <-kk()
                  
                  if(input$plot_kegg_id1 == "p_emap"){
                    kk_emap <- enrichplot::pairwise_termsim(kk)
                    
                    updateSelectInput(session, "kegg_num1",label = '通路选择',
                                      choices = kk_emap@result[["Description"]], selected = kk_emap@result[["Description"]][1:10] )
                  }
                  else if(!input$plot_kegg_id1 == "p_emap"){
                    updateSelectInput(session, "kegg_num1",label = '通路选择',
                                      choices = kk@result[["Description"]], selected = kk@result[["Description"]][1:10] )
                  }
                  
                  
                  updateSelectInput(session, "kegg_kk",label =   '通路选择',
                                    choices = kk@result[["Description"]], selected = kk@result[["Description"]][1] )
                  
                } ) 
                
                observeEvent(input$submit1, { 
                  
                  kegg_plot0 <- reactive({
                    if(is.null( kk() )){return(NULL)}
                    kk <- kk()
                    
                    if(input$plot_kegg_id1 == "p_dot") {
                      
                      p <- dotplot(kk, showCategory = input$kegg_num0 )
                    }
                    else if(input$plot_kegg_id1 == "p_bar"){
                      p <- barplot(kk, showCategory = input$kegg_num0 )
                    }
                    else { 
                      p <-  emapplot(enrichplot::pairwise_termsim(kk), 
                                     showCategory = input$kegg_num0, 
                                     layout.params = list(layout = 'kk'), 
                                     edge.params = list(min = 0.8), 
                                     cex.params = list(category_node = 1.5),
                                     max.overlaps =10 ) 
                    }
                    
                    return(p)
                  })
                  
                  
                  output$plot_kegg0 <- renderPlot({
                    if (is.null( kk() ) ){ return() }
                    
                    return( kegg_plot0() )
                  })
                  
                  
                  if(T){
                    output$pdf0 <- downloadHandler(
                      filename="ora0.pdf",
                      content = function(file){
                        pdf(file,width=input$w0,height=input$h0)
                        print( kegg_plot0() )
                        dev.off()
                      }
                    )
                    output$png0 <- downloadHandler(
                      filename="ora0.png",
                      content = function(file){
                        png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                        print( kegg_plot0() )
                        dev.off()
                      }
                    )
                    output$jpeg0 <- downloadHandler(
                      filename="ora0.jpeg",
                      content = function(file){
                        jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                        print(kegg_plot0() )
                        dev.off()
                      }
                    )
                    output$tiff0 <- downloadHandler( 
                      filename="ora0.tiff",
                      content = function(file){
                        tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
                        print(kegg_plot0() )
                        dev.off()
                      }
                    )
                  }
                  
                  
                })
                
                observeEvent(input$submit_kegg1, { 
                  
                  kegg_plot1 <- reactive({
                    if(is.null( kk() )){return(NULL)}
                    kk <- kk()
                    
                    if(input$plot_kegg_id1 == "p_dot") {
                      
                      p <- dotplot(kk, showCategory = input$kegg_num1  )
                    }
                    else if(input$plot_kegg_id1 == "p_bar"){
                      p <- barplot(kk, showCategory = input$kegg_num1  )
                    }
                    else { 
                      p <- emapplot(enrichplot::pairwise_termsim(kk), 
                                    showCategory = input$kegg_num1, 
                                    layout.params = list(layout = 'kk'), 
                                    edge.params = list(min = 0.8), 
                                    cex.params = list(category_node = 1.5),
                                    max.overlaps =10 ) 
                    }
                    
                    return(p)
                  })
                  
                  
                  # 3.1 展示 ORA 1 
                  output$plot_kegg1 <- renderPlot({
                    
                    if (is.null( kk() ) ){ return() }
                    return( kegg_plot1() )
                  })
                  
                  if(T){
                    output$pdf1 <- downloadHandler(
                      filename="ora1.pdf",
                      content = function(file){
                        pdf(file,width=input$w1,height=input$h1)
                        print( kegg_plot1() )
                        dev.off()
                      }
                    )
                    output$png1 <- downloadHandler(
                      filename="ora1.png",
                      content = function(file){
                        png(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                        print( kegg_plot1() )
                        dev.off()
                      }
                    )
                    output$jpeg1 <- downloadHandler(
                      filename="ora1.jpeg",
                      content = function(file){
                        jpeg(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                        print(kegg_plot1() )
                        dev.off()
                      }
                    )
                    output$tiff1 <- downloadHandler( 
                      filename="ora1.tiff",
                      content = function(file){
                        tiff(file,width=input$w1,height=input$h1,units="in",res=input$ppi1)
                        print(kegg_plot1() )
                        dev.off()
                      }
                    )
                  }
                  
                  
                })
                
                observeEvent(input$submit_kegg2, {
                  
                  kegg_plot2 <- reactive({
                    
                    if(is.null( kk() )){return(NULL)}
                    kk <- kk()
                    
                    if(input$type=='DEG'){
                      if(input$plot_kegg_id2 == "p1"){
                        y1 <- input$kegg_kk
                        gene_list <- gene_list()
                        p = cnetplot(kk, color.params = list(foldChange = gene_list$geneList, edge = T ),
                                     showCategory = y1, layout = "gem", node_label="gene")
                      }
                      else if(input$plot_kegg_id2 == "p2"){
                        y1 <- input$kegg_kk
                        gene_list <- gene_list()
                        p = cnetplot(kk, circular = T, showCategory = y1, layout = "gem", node_label="gene",
                                     color.params = list(foldChange = gene_list$geneList, edge = T )
                        )
                      }
                      
                    }
                    else{
                      if(input$plot_kegg_id2 == "p1"){
                        y1 <- input$kegg_kk
                        gene_list <- gene_list()
                        p = cnetplot(kk, color.params = list(edge = T ),
                                     showCategory = y1, layout = "gem", node_label="gene")
                      }
                      else if(input$plot_kegg_id2 == "p2"){
                        y1 <- input$kegg_kk
                        gene_list <- gene_list()
                        p = cnetplot(kk, circular = T, showCategory = y1, layout = "gem", node_label="gene",
                                     color.params = list( edge = T )
                        )
                      }
                    }
                    return(p)
                    
                    
                  })
                  
                  
                  # 3.1 展示ora 1 
                  output$plot_kegg2 <- renderPlot({
                    if (is.null( kk() ) ){ return() }
                    
                    return( kegg_plot2() )
                  })
                  
                  if(T){
                    output$pdf2 <- downloadHandler(
                      filename="ora2.pdf",
                      content = function(file){
                        pdf(file,width=input$w2,height=input$h2)
                        print( kegg_plot2() )
                        dev.off()
                      }
                    )
                    output$png2 <- downloadHandler(
                      filename="ora2.png",
                      content = function(file){
                        png(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                        print( kegg_plot2() )
                        dev.off()
                      }
                    )
                    output$jpeg2 <- downloadHandler(
                      filename="ora2.jpeg",
                      content = function(file){
                        jpeg(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                        print(kegg_plot2() )
                        dev.off()
                      }
                    )
                    output$tiff2 <- downloadHandler( 
                      filename="ora2.tiff",
                      content = function(file){
                        tiff(file,width=input$w2,height=input$h2,units="in",res=input$ppi2)
                        print(kegg_plot2() )
                        dev.off()
                      }
                    )
                  }
                  
                  
                })
                
              })
              
            }) #  observeEvent(input$submit1
            
            
          })

        }
        
      } ) # show
      
      # 5.3 下载参考数据 DEseq2  Excel
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('DEG.csv')
        },
        content = function(file) {
          ifelse(input$type=='DEG',  
                 DEG <- read.csv('./www/DEG.csv'),
                 DEG <- data.frame('ID'=read.csv('./www/DEG.csv')[1:200,1])
          )
          
          write.csv(DEG, file, row.names = F, fileEncoding = "GB18030")
        }
      )
      
      
    } # function(input, output, session) 
  ) # moduleServer
} # function(id)

