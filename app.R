
library('shiny') 
library('bs4Dash')
library('shinyWidgets')
library('shinycssloaders')
library('shinyFiles')
library('DT')
library('shinyjs')
library('readxl')

library('ggplot2')     # 画图
library('ggrepel')     # 画图，添加注释信息
library('ggplotify')   # 画图，交互动图
library('plotly')      # 画图，交互动图，网页呈现
library("ggpubr")
library("pheatmap")

# 通路富集相关
library("forcats")
library("ggstance")
library("patchwork")
library("pathview")
library("ReactomePA")
library("enrichplot")

library("tidyr") 
library("showtext") #中文问题
showtext_auto()

library('viridis')
library('grid')
library('waiter')
library('shiny.i18n') # 语言切换

source("module/info.R",encoding = "utf-8")

source("module/elisa.R",encoding = "utf-8")
source("module/qpcr.R",encoding = "utf-8")

source("module/reps.R",encoding = "utf-8")
source("module/geneID.R",encoding = "utf-8")
source("module/expr.R", encoding = "utf-8")
source("module/heatmap.R", encoding = "utf-8")
source("module/volcano.R",encoding = "utf-8")

source("module/limma.R",encoding = "utf-8")
source("module/deseq2.R",encoding = "utf-8")

source("module/kegg.R",encoding = "utf-8")
source("module/gseakegg.R",encoding = "utf-8")
source("module/go.R", encoding = "utf-8")
source("module/gseago.R", encoding = "utf-8")
source("module/ora.R", encoding = "utf-8")

source("module/anova.R",encoding = "utf-8")
source("module/forestplot.R",encoding = "utf-8")
source("module/logistics.R",encoding = "utf-8")
source("module/lm.R",encoding = "utf-8")
source("module/surv.R",encoding = "utf-8")
source("module/roc.R",encoding = "utf-8")
source("module/map.R",encoding = "utf-8")

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")
# lang <- Translator$new(translation_json_path = "./data/translation.json")
lang$set_translation_language("cn") # here you select the default translation to display

ui <- bs4DashPage(
  preloader <- list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  
  title = "GraphMed",
  fullscreen = T,
  header = bs4DashNavbar(
    shiny.i18n::usei18n(lang),
    column(width = 2, 
           materialSwitch(inputId = "lang",label = lang$t("中文"), 
                          status = "primary",value = F, right = T )
                           ),
    title = dashboardBrand(
      title = lang$t("重医儿院"),
      color = "primary",
      href = "https://stu.chcmu.aisa",
      image = "./logo_chcmu.png", 
      opacity=1
    ),
    disable = FALSE,
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("table-cells"),
    fixed = FALSE,
    # Dropdown menu for notifications
    leftUi = dropdownMenu(type = "notifications", badgeStatus = "warning",
                          
                          notificationItem(icon = icon("user", lib = "glyphicon"),
                                           status = "danger", "Bingm"
                          ),
                          notificationItem(icon = icon("lungs",lib = "font-awesome"), status = "info",
                                           "Lab. of Pediatric Respir. Medicine",
                                           href = "https://stu.chcmu.asia"
                          ),
                          notificationItem(icon = icon("envelope", lib = "glyphicon"), status = "danger",
                                           "2020111042@stu.cqmu.edu.cn"
                          )
    )
    
  ),
  ## Sidebar content
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    bs4SidebarUserPanel(
      image = "./doupi.jpg", 
      name = lang$t("欢迎使用 GraphMed")
    ),
    sidebarMenu(id="sidebar",
                sidebarHeader(title = lang$t("医学数据可视化") ),
                menuItem(lang$t("使用说明"), tabName = "help", icon = ionicon(name="information-circle")),
                menuItem(lang$t("联系我们"), tabName = "contact", icon = ionicon(name="call") ),
                menuItem(lang$t("常见问题"), tabName = "faq", icon = ionicon(name="help-circle")),
                # 
                menuItem(lang$t("Elisa 分析"), tabName = "elisa", icon = ionicon(name="flask")),
                menuItem(lang$t("Q-PCR 结果分析"), tabName = "qpcr", icon = ionicon(name="flask") ),
                
                menuItem(lang$t("基因去重复"), tabName = "reps", icon = ionicon(name="flask") ),
                menuItem(lang$t("基因 ID 转换"), tabName = "geneID", icon = ionicon(name="flask") ),
                menuItem(lang$t("表达箱图"), tabName = "expr", icon = ionicon(name="flask") ),
                menuItem(lang$t("热图"), tabName = "heatmap", icon = ionicon(name="flask") ),
                menuItem(lang$t("火山图"), tabName = "volcano", icon = ionicon(name="flask")),
                
                menuItem("RNAseq：limma", tabName = "limma", icon = ionicon(name="flask") ),
                menuItem("RNAseq：DESeq2", tabName = "deseq2", icon = ionicon(name="flask") ),
                menuItem(lang$t("KEGG 分析"), tabName = "kegg", icon = ionicon(name="flask") ),
                menuItem(lang$t("GSAE 分析(KEGG 库)"), tabName = "gseakegg", icon = ionicon(name="flask") ),
                menuItem(lang$t("GO 分析"), tabName = "go", icon = ionicon(name="flask") ),
                menuItem(lang$t("GSEA 分析(GO库)"), tabName = "gseago", icon = ionicon(name="flask") ),
                menuItem(lang$t("ORA 分析"), tabName = "ora", icon = ionicon(name="flask") ),
                
                menuItem(lang$t("单因素差异分析"), tabName = "anova", icon = ionicon(name="bed") ),
                menuItem(lang$t("Logistics 回归"), tabName = "logis", icon = ionicon(name="bed") ),
                menuItem(lang$t("简易森林图"), tabName = "forestplot", icon = ionicon(name="bed") ),
                menuItem(lang$t("两变量相关性"), tabName = "lm", icon = ionicon(name="bed") ),
                menuItem(lang$t("生存分析"), tabName = "surv", icon = ionicon(name="bed") ),
                menuItem(lang$t("ROC 曲线"), tabName = "roc", icon = ionicon(name="bed") ),
                menuItem(lang$t("中国地图"), tabName = "map", icon = ionicon(name="bed") )

                
    )
  ),
  footer = dashboardFooter(
    left = a(
      href = "https://shiny.chcmu.com.cn/graphmed/",
      target = "_blank", "GraphMed |" ,a(
        href = "https://beian.miit.gov.cn/",
        target = "_blank", "渝ICP备2023006607号"
      ),
    ),
    right = "2023"   
    
  ),
  controlbar = dashboardControlbar(),
  body  = dashboardBody(
    tabItems(
      tabItem(tabName= "help",       helpUI("help")),
      tabItem(tabName= "contact",    contactUI("contact")),
      tabItem(tabName= "faq",        faqUI("faq")),
      
      tabItem(tabName= "elisa",      elisaUI("elisa")),
      tabItem(tabName= "qpcr",       qpcrUI("qpcr")),
      
      tabItem(tabName= "reps",      repsUI("reps")),
      tabItem(tabName= "geneID",      geneIDUI("geneID")),
      tabItem(tabName= "expr",        exprUI("expr")),
      tabItem(tabName= "heatmap",     heatmapUI("heatmap")),
      tabItem(tabName= "volcano",      volcanoUI("volcano")),
      
      tabItem(tabName= "limma",      limmaUI("limma")),
      tabItem(tabName= "deseq2",      deseq2UI("deseq2")),

      tabItem(tabName= "kegg",        keggUI("kegg")),
      tabItem(tabName= "gseakegg",    gseakeggUI("gseakegg")),
      tabItem(tabName= "go",        goUI("go")),
      tabItem(tabName= "gseago",        gseagoUI("gseago")),
      tabItem(tabName= "ora",         oraUI("ora")),
      
      tabItem(tabName= "anova",      anovaUI("anova")),
      tabItem(tabName= "logis",        logisUI("logis")),
      tabItem(tabName= "forestplot", forestplotUI("forestplot")),
      tabItem(tabName= "lm",         lmUI("lm")),
      tabItem(tabName= "surv",       survUI("surv")),
      tabItem(tabName= "roc",        rocUI("roc")),
      tabItem(tabName= "map",        mapUI("map"))

    )
  )
)

server <- function(input, output, session) {
  waiter_hide()
  
  observeEvent(input$lang, {
    # observeEvent(input$selected_language, {
    if(input$lang==F){ 
      shiny.i18n::update_lang('cn' )
    }
    else{
      shiny.i18n::update_lang('en' )
    }
    
  })
  
  elisaServer("elisa")
  qpcrServer("qpcr")
  
  geneIDServer("geneID")
  repsServer("reps")
  exprServer("expr")
  heatmapServer("heatmap")
  volcanoServer("volcano")
  
  limmaServer("limma")
  deseq2Server("deseq2")
  keggServer("kegg")
  gseakeggServer("gseakegg")
  goServer("go")
  gseagoServer("gseago")
  oraServer("ora")
  
  anovaServer("anova")
  logisServer("logis")
  forestplotServer("forestplot")
  lmServer("lm")
  survServer("surv")
  rocServer("roc")
  mapServer("map")



}

shinyApp(ui, server)

