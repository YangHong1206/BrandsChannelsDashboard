#brandchannels----
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#library----
library(shiny)
library(ggplot2)
library(readxl)
library(vroom)
library(tidyverse)
library(bslib)
library(lubridate)
library(zip)
library(openxlsx)
library(writexl)
library(httr)

#设定上传文件容量。----
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

# 自定义函数 ----
fn_drop_parentheses <- function(clean_parentheses_a) {
  clean_parentheses_a <- gsub("\\(.*\\)", "", clean_parentheses_a)
  clean_parentheses_a <- gsub("\\（.*\\）", "", clean_parentheses_a)
  return(clean_parentheses_a)
}

fn_format_h <- function(time_a) {
  time_a <- format(as.POSIXct(time_a, format="%Y-%m-%d %H:%M:%S"),"%H")
  return(time_a)
}

历史订单明细url <- "https://cdn.jsdelivr.net/gh/YangHong1206/Yixin_dataset@main/%E5%8E%86%E5%8F%B2%E8%AE%A2%E5%8D%95%E6%98%8E%E7%BB%86%E8%84%B1%E6%95%8F%E7%89%88.csv"
店面拜访url <- "https://cdn.jsdelivr.net/gh/YangHong1206/Yixin_dataset@main/visit_record.csv"
预审订单明细 <- "https://cdn.jsdelivr.net/gh/YangHong1206/Yixin_dataset@main/%E9%A2%84%E5%AE%A1%E8%AE%A2%E5%8D%95%E6%98%8E%E7%BB%86.csv"
渠道管理列表 <- "https://cdn.jsdelivr.net/gh/YangHong1206/Yixin_dataset@main/channel.csv"
花名册 <- "https://cdn.jsdelivr.net/gh/YangHong1206/Yixin_dataset@main/%E5%8D%8E%E4%B8%9C%E5%8C%BA%E8%8A%B1%E5%90%8D%E5%86%8C.csv"

#UI----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal", version = 5),
  #https://bootswatch.com/ choose preference theme
  titlePanel("易鑫福州品牌项目"),
  tabsetPanel(
    tabPanel(
      "3月过程目标",
      hr(),
      #actionButton("refresh", "刷新数据"),
      #selectInput("brand", "选择品牌", choices = NULL),

      DT::dataTableOutput("March过程目标DF_theme"),
      downloadButton("Download_March过程目标", "下载过程目标数据"),
      hr(),
      strong(br(), "直秒闪过必过逾期订单----"),
      tableOutput("testdf"),
      #downloadButton("Download_March过程目标", "下载逾期数据"),
      #tableOutput("tempdf")
    ),
    tabPanel(
      "数据源",
      hr(),
      actionButton("refresh", "刷新数据"),
      selectInput("brand", "选择品牌", choices = NULL),
      
      DT::dataTableOutput("本月品牌渠道预进生数据df"),
      downloadButton("Download_本月品牌渠道预进生数据df", "下载本月品牌数据表"),
      hr(),
      strong(br(),"品牌渠道月度成交"),
      DT::dataTableOutput("全年品牌渠道成交数据DF")
    )
  )
)


server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)



# Server----
server <- function(input, output, session) {
  #input rawdata----
  rawdata_raw <- eventReactive(input$refresh, {
    temp_file <- tempfile(fileext = ".csv")
    response <- GET(历史订单明细url, write_disk(temp_file, overwrite = TRUE))
    
    if (response$status_code == 200) {
      data <- read.csv(temp_file, fileEncoding = "UTF-8")
      unlink(temp_file)
      data
    } else {
      stop("数据下载失败，状态码：", response$status_code)
    }
  })
  
  #input channel----
  channel_raw <- eventReactive(input$refresh, {
    temp_file <- tempfile(fileext = ".csv")
    response <- GET(渠道管理列表, write_disk(temp_file, overwrite = TRUE))
    
    if (response$status_code == 200) {
      data <- read.csv(temp_file, fileEncoding = "UTF-8")
      unlink(temp_file)
      data
    } else {
      stop("数据下载失败，状态码：", response$status_code)
    }
  })
  
  #input visit record----
  visit_record_raw <- eventReactive(input$refresh, {
    temp_file <- tempfile(fileext = ".csv")
    response <- GET(店面拜访url, write_disk(temp_file, overwrite = TRUE))
    
    if (response$status_code == 200) {
      data <- read.csv(temp_file, fileEncoding = "UTF-8")
      unlink(temp_file)
      data
    } else {
      stop("数据下载失败，状态码：", response$status_code)
    }
  })
  
  #input pre_apr----
  pre_apr_raw <- eventReactive(input$refresh, {
    temp_file <- tempfile(fileext = ".csv")
    response <- GET(预审订单明细, write_disk(temp_file, overwrite = TRUE))
    
    if (response$status_code == 200) {
      data <- read.csv(temp_file, fileEncoding = "UTF-8")
      unlink(temp_file)
      data
    } else {
      stop("数据下载失败，状态码：", response$status_code)
    }
  })
  
  #SCname list----
  name_list <- eventReactive(input$refresh, {
    temp_file <- tempfile(fileext = ".csv")
    response <- GET(花名册, write_disk(temp_file, overwrite = TRUE))
    
    if (response$status_code == 200) {
      data <- read.csv(temp_file, fileEncoding = "UTF-8")
      unlink(temp_file)
      data
    } else {
      stop("数据下载失败，状态码：", response$status_code)
    }
  })
  
  
  
  #tidy rawdata----
  rawdata_tidied <- reactive({
    rawdata_raw() %>%
      mutate(
        融资金额 = 融资金额 / 10000,
        进件日期 = ymd(进件日期),
        合同生效日期 = ymd(合同生效日期),
        金融经理名称 = fn_drop_parentheses(金融经理名称),
        金融顾问名称 = fn_drop_parentheses(金融顾问名称)
      ) %>%
      arrange(desc(合同生效日期)) %>%
      rename("渠道编号" = "渠道代码")
  })
  
  #tidy pre_apr----
  pre_apr_tidied <- reactive({
    pre_apr_raw() %>%
      mutate(
        进件时点 = fn_format_h(进件时间),
        预审提交时间 = as.Date(预审提交时间),
        金融经理 = fn_drop_parentheses(金融经理),
        金融顾问 = fn_drop_parentheses(金融顾问),
        进件时间 = as.Date(进件时间)
      ) %>%
      rename("渠道编号" = "渠道编码")

  })

  #tidy channels-----
  channel_tidied <- reactive({
    channel_raw() %>%
      rename('负责员工' = '负责员工.金融顾问.') %>%
      mutate(
        负责员工 = fn_drop_parentheses(负责员工),
        业务上级 = fn_drop_parentheses(业务上级),
        初始生效日期 = ymd(初始生效日期),
        渠道关停日期 = ymd(渠道关停日期)
      )
  })

  #tidy visit_record----
  visit_record_tidied <- reactive({
    visit_record_raw() %>%
      mutate(
        拜访开始时间 = as.POSIXct(拜访开始时间),
        拜访结束时间 = as.POSIXct(拜访结束时间),
        拜访用时 = as.numeric(拜访用时),
        拜访人 = fn_drop_parentheses(拜访人),
        业务上级 = fn_drop_parentheses(业务上级)
      )
  })

  #计算全量表----
  单号生效日 <- reactive({
    rawdata_tidied() %>%
      select(申请编号, 合同生效日期, 申请状态, 产品类目) %>%
      filter(申请状态 != "订单已取消")
  })

  pre_apr_tidied_combined <- reactive({
    pre_apr_tidied() %>%
    left_join(单号生效日(), by = "申请编号")
  })

  全维度预审量 <- reactive({
    req(pre_apr_tidied_combined()) # 添加数据验证
    pre_apr_tidied_combined() %>%
      mutate(
        预审月 = format(预审提交时间, "%Y%m"),
        预审结果通过订单 = case_when(预审结果 == "自动通过" ~ 1, TRUE ~ 0),
        是否预审复议是 = case_when(是否预审复议 == "是" ~ 1, TRUE ~ 0),
        预审通过 = 预审结果通过订单 + 是否预审复议是
      ) %>%
      group_by(
        预审月, 店面城市, 产品类别, 产品类目, 产品名称, 合作项目,
        车辆类型, 主品牌, 金融经理, 金融顾问, 渠道二级科目名称,
        渠道编号, 店面主体
      ) %>%
      summarise(
        预审量 = n(),
        预审通过 = sum(预审结果通过订单)
      )
  })

  全维度进件量 <- reactive({
    pre_apr_tidied_combined() %>%
      mutate(
        进件月 = format(进件时间, "%Y%m"),
        最后审批通过量 = if_else(
          最后一次人工审批时间 > 最后一次自动审批时间,
          case_when(最后一次人工审批结果 == "通过" ~ 1, TRUE ~ 0),
          case_when(最后一次自动审批结果 == "通过" ~ 1, TRUE ~ 0)
        )
      ) %>%
      group_by(
        进件月, 店面城市, 产品类别, 产品类目, 产品名称, 合作项目,
        车辆类型, 主品牌, 金融经理, 金融顾问, 渠道二级科目名称,
        渠道编号, 店面主体
      ) %>%
      summarise(
        进件量 = n(),
        进件通过 = sum(最后审批通过量)
      )
  })

  全维度生效量 <- reactive({
    rawdata_tidied() %>%
      mutate(
        生效月 = format(合同生效日期, "%Y%m"),
        生效量 = case_when(!is.na(合同生效日期) & 申请状态 != "订单已取消" ~ 1, TRUE ~ 0),
        生效额 = case_when(!is.na(合同生效日期) & 申请状态 != "订单已取消" ~ 融资金额, TRUE ~ 0)
      ) %>%
      group_by(
        生效月, 店面城市, 产品类别, 产品类目, 产品方案名称, 合作项目,
        车辆类型, 品牌, 金融经理名称, 金融顾问名称, 渠道二级科目,
        渠道编号, 店面主体
      ) %>%
      summarise(
        生效量 = sum(生效量),
        生效额 = sum(生效额, na.rm = TRUE)
      )
  })

  df <- reactive({
    full_join(
      全维度预审量(), 全维度进件量(),
      by = c(
        "预审月" = "进件月", "店面城市", "产品类别", "产品类目",
        "产品名称", "合作项目", "车辆类型", "主品牌", "金融经理",
        "金融顾问", "渠道二级科目名称", "渠道编号", "店面主体"
      )
    )
  })

  df2 <- reactive({
    req(df(), 全维度生效量())
    
    
    full_join(
      df(), 全维度生效量(),
      by = c(
        "预审月" = "生效月", "店面城市", "产品类别", "产品类目",
        "产品名称" = "产品方案名称", "合作项目", "车辆类型", "主品牌" = "品牌",
        "金融经理"= "金融经理名称", "金融顾问" = "金融顾问名称", 
        "渠道二级科目名称" = "渠道二级科目",
        "渠道编号", "店面主体"
      )
    )
  })
  
  
  #品牌渠道表----
  # 启辰----
  启辰 <- data.frame(
    品牌 = c("启辰","启辰","启辰","启辰","启辰"),
    店面名称 = c("福州福沁汽车销售服务有限公司","福州市汇京时代汽车销售服务有限公司",
             "福建省汇京鸿福汽车销售服务有限公司","莆田市华宝投资有限公司","福建万星商贸有限公司"),
    地址 = c("福建省福州市仓山区大浦路12号福汽新能源小镇6号楼","福建省福州市仓山区建新北路146号",
           "福州市晋安区龙头路","福建省莆田市荔城区西天尾镇涵西大道899号","福建省连江县江南镇达江路66号")
  ) 
  
  #曹操项目----
  曹操 <- data.frame(
    品牌 = c("曹操","曹操","曹操"),
    店面名称 = c("福州道禧客新能源汽车服务有限公司","福州优行集众商务服务有限公司","福州车中宝汽车信息咨询有限公司"),
    地址 = c("福州晋安区国家电双乓峰悦享充电站","福州海西科技金融大厦7楼","福州仓山区红江路8号浦上工业园34栋302室")
  ) 
  
  
  
  #长安欧尚----
  长安欧尚 <- data.frame(
    品牌 = c("长安欧尚","长安欧尚","长安欧尚","长安欧尚","长安欧尚","长安欧尚","长安欧尚","长安欧尚"),
    店面名称 = c("福州长安汽车销售有限公司","福州锐霖汽车销售有限公司","福清长安汽车有限公司",
             "莆田裕晟长安汽车","莆田市益众汽车销售服务有限公司","长元长安汽车","龙祥长安轿车","宁德融瑞汽车有限公司"),
    地址 = c("福州","福州","福清",
           "莆田","莆田","三明","南平","宁德"))
  
  #上汽大通MAXUS----
  上汽大通MAXUS <- data.frame(
    品牌 = c("上汽大通MAXUS","上汽大通MAXUS"),
    店面名称 = c("福州上汽大通汽车销售服务有限公司","福建世骏汽车有限公司（福州）"),
    地址 = c("福州市仓山区城门镇大浦路12号安腾商业广场（一区）新能源汽车产业园18地块13号楼1层02店面","福州市铜盘路388号"))
  
    #山海----
  山海 <- data.frame(
    品牌 = c("捷途","捷途","捷途","捷途","捷途"),
    店面名称 = c("莆田盛世元通新能源汽车销售服务有限公司","福建赢宸捷新能源汽车销售有限公司","福州中科瑞捷汽车销售服务有限公司",
             "福州市亿海广际汽车销售有限公司","三明中源汽车销售服务有限公司"),
    地址 = c("福建省莆田市涵江区白塘镇埭里村城涵西大道3269号","福建省闽侯县尚干镇祥通路13-1号一层","福州市仓山区大浦路12号安腾商业广场（一区）新能源汽车产业园18号地块6号楼1层03店面",
           "福州市仓山区金洲北路金华路 15 号","福建省三明市三元区陈大镇德安工业区16号"))
    
  #北汽制造BAW项目----
  BAW <- data.frame(
    品牌 = c("BAW","BAW","BAW","BAW","BAW","BAW","BAW","BAW"),
    店面名称 = c("福建万商平治房车销售有限公司","福州犇牛汽车服务有限公司",
             "福建腾顺汽车维修服务有限公司","福建华强车业集团有限公司",
             "福州恒润兴汽车贸易有限公司","福建菱德汽车销售服务有限公司",
             "福建省丰华车业有限公司","莆田市丰众汽车贸易有限公司"),
    地址 = c("福建省福州市仓山区则徐大道481号","福建省福州市闽侯县祥谦镇泮洋村口解放修理厂","福州晋安区鼓山镇福马路698号",
           "福建省福州市福清市海口镇东阁村村西606号","福州市仓山区则徐大道葫芦阵名星汽车城","福建省莆田市荔城区西天尾镇荔涵大道1065号",
           "福建省莆田市城厢区霞林街道华东大道2535号迎和加油站隔壁","莆田市仙游县枫亭镇工业路"))
    
  #智己汽车----
  智己汽车 <- data.frame(
    品牌 = c("智己汽车","智己汽车"),
    店面名称 = c("福州东百爱琴海体验中心",
             "福州青口体验及交付服务中心"),
    地址 = c("福建省福州市仓山区金山街道浦上大道198号",
           "福建省福州市闽侯县福州市闽侯县尚干镇祥通路9号青口汽车城"))
  
  
  #小米汽车----
  小米汽车 <- data.frame(
    品牌 = c("小米汽车","小米汽车"),
    店面名称 = c("小米之家福州市台江区万象生活城汽车体验店",
             "小米汽车福州市闽侯县青口交付中心"),
    地址 = c("福建福州市台江区上海街道西环中路691号万象九宜城F1",
           "福建福州市闽侯县青口镇闽侯县青口镇新榕路37号"))
  
  #菱势汽车----
  菱势汽车 <- data.frame(
    品牌 = c("菱势汽车","菱势汽车"),
    店面名称 = c(
             "福建合一新能源汽车服务有限公司",
             "三明市捷顺新能源技术有限公司"),
    地址 = c(
           "江滨南路六号",
           "福建省莆田市涵江区白塘镇埭里村"
           ))

  #阿维塔----
  阿维塔 <- data.frame(
    品牌 = c("阿维塔","阿维塔","阿维塔"),
    店面名称 = c(
      "阿维塔中心 福州仓山店","阿维塔体验中心 福州青口店",
      "阿维塔体验中心 福州东二环泰禾广场店"),
    地址 = c(
      "江滨南路六号","福建省莆田市涵江区白塘镇埭里村",
      "福州市晋安区东二环泰禾城市广场22号楼一层L140铺"
    ))

  #蔚来----
  蔚来 <- data.frame(
    品牌 = c("蔚来","蔚来","蔚来","蔚来","蔚来"),
    店面名称 = c("蔚来空间 | 福州华润万象城", "蔚来空间 | 福清福和万达广场",
             "蔚来中心 | 福州东二环", "蔚来空间 | 莆田正荣财富中心", "蔚来空间 | 福州烟台山"),
    地址 = c("福建省福州市鼓楼区工业路526号华润万象城L127a","福建省福清市音西街道福和路9号福和万达广场1030号铺",
           "福建省福州市晋安区岳峰镇竹屿路6号东二环泰禾广场西区L131号",
           "福建省莆田市荔城区荔园东路 1688 号正荣财富中心A 座 一层 B1008号商铺",
           "仓前路309号烟台山商业漫步街区X2-15"))

  #特斯拉----
  特斯拉 <- data.frame(
    品牌 = c("特斯拉","特斯拉","特斯拉","特斯拉"),
    店面名称 = c("福州东二环泰禾广场体验店", "福州仓山万达体验店",
             "福州仓山特斯拉中心", "莆田特斯拉中心"),
    地址 = c("福建省福州市晋安区岳峰镇竹屿路6号东二环泰禾广场", "福建省福州市仓山区浦上大道274号仓山万达广场一楼1-15，116A",
           "福建省福州市仓山区高旺路6号",
           "福建省莆田市荔城区城涵西大道南面2068号"))

  #捷豹----
  捷豹 <- data.frame(
    品牌 = c("捷豹"),
    店面名称 = c("福州捷众汽车有限公司"),
    地址 = c("中国福建省福州市闽侯青口海峡汽车城旁（福州兰莆高速出口执行300米)"))

  #路虎----
  路虎 <- data.frame(
    品牌 = c("路虎"),
    店面名称 = c("福州捷众汽车有限公司"),
    地址 = c("中国福建省福州市闽侯青口海峡汽车城旁（福州兰莆高速出口执行300米)"))

  #别克----
  别克 <- data.frame(
    品牌 = c("别克","别克","别克","别克"),
    店面名称 = c("福建通德汽车销售服务有限公司", "福州永达汽车销售服务有限公司",
             "福建省直中机中泰汽车有限公司", "莆田市中机中泰汽车销售有限公司"),
    地址 = c("福建省福州市福清市宏路宏扬汽车广场内", "福建省福州市仓山区盖山投资区高仕路10号",
           "福建省福州市鼓楼区屏西路29号",
           "福建省莆田市涵江区白塘镇埭里村"))

  #吉利银河----
  吉利银河 <- data.frame(
    品牌 = c("吉利银河","吉利银河","吉利银河","吉利银河","吉利银河","吉利银河","吉利银河"),
    店面名称 = c("荔城 · 莆田建发店", "吉行驿丨福鼎·宁德盛源店",
             "仓山 · 福州鑫际店", "仓山 · 福远鑫瀚店",
             "福清 · 福州鑫业店", "三明盛世银行汽车销售服务有限公司", "建瓯 · 建瓯鑫启翔店"),
    地址 = c("莆田市荔城区西天尾镇洞湖城涵西大道1299号", "福建省宁德市福鼎市星火工业园区51号",
           "福州市仓山区建新镇金洲北路30号","仓山区潘墩路88-77号福远汽车广场内",
           "福清市宏路宏扬广场内","福建省三明市三元区东乾二路1号（万达广场）","南平市建瓯市东安路13-6号"))


  
  
  
  #红旗----
  红旗 <- data.frame(
    品牌 = c("红旗","红旗","红旗","红旗","红旗","红旗","红旗"),
    店面名称 = c(
      "福州榕泰鸿旗汽车销售有限公司",
      "福州华奥鸿旗汽车销售有限公司",
      "福建华奥红旗汽车销售有限公司",
      "福建赢宸汽车销售服务有限公司",
      "宁德华奥宏旗汽车销售有限公司",
      "莆田星奥汽车销售服务有限公司",
      "三明市旗展汽车销售服务有限公司"),
    地址 = c(
      "福建省福州市仓山区盖山投资区高仕路7号福州福达仪表有限公司厂房B第一、二层",
      "福州市闽侯县青口镇镜上村新榕路19号",
      "福州市仓山区齐安路769号华奥红旗",
      "福建省建瓯市东安路15号",
      "宁德市东侨经济开发区工业路5号（宁德市华强丰田汽车销售服务有限公司厂区内",
      "莆田市荔城区西天尾镇城涵西大道889号",
      "三明市梅列区德安工业园16号一号"))
  
  #ARCFOX极狐----
  ARCFOX极狐 <- data.frame(
    品牌 = c("ARCFOX极狐","ARCFOX极狐"),
    店面名称 = c(
      "中创华丰（福建）汽车销售有限公司",
      "福州市新薛航汽车贸易有限公司"),
    地址 = c(
      "晋安区长乐中路88号世欧广场A3-1-48/49",
      "福建省福州市台江区西环中路691号万象九宜城一层117商铺"))
  
  
  
  
  
  
  品牌渠道表 <- rbind(曹操, 启辰, 长安欧尚, 上汽大通MAXUS, 山海, BAW, 智己汽车, 小米汽车, 菱势汽车, 红旗
                 ,吉利银河, 别克, 路虎, 捷豹, 特斯拉, 蔚来, 阿维塔, ARCFOX极狐
                 )#每增加一个品牌需要合并表格----
  
  品牌渠道表_raw <- reactive({
    as.data.frame(品牌渠道表)
  })
  
  
  
  品牌渠道表_df <- reactive({
    
    品牌渠道表_raw() %>% 
      left_join(channel_tidied() %>% 
                  select(主体名称, 渠道编号, 负责员工,业务上级,状态), 
                by = c("店面名称" = "主体名称")) %>% 
      select(品牌,店面名称,地址,渠道编号,负责员工,业务上级,状态)
    })

  #合并渠道信息和全量数据表计算----
  
  combined_df <- reactive({
    df2() %>% 
      filter(
        主品牌 == input$brand,
        预审月 == format(Sys.Date(), "%Y%m")#LOOK AT 这里 可以选择替换预审月变量----
      ) %>% 
      group_by(主品牌,预审月,渠道编号) %>% 
      summarise(预审量 = sum(预审量,na.rm = TRUE),
                预审通过 = sum(round(预审通过,0),na.rm = TRUE),
                进件量 = sum(进件量,na.rm = TRUE),
                进件通过 = sum(round(进件通过,0),na.rm = TRUE),
                生效量 = sum(round(生效量,0),na.rm = TRUE),
                生效额 = sum(round(生效额,0),na.rm = TRUE)
      ) %>% 
      right_join(品牌渠道表_df(), by = c("渠道编号" = "渠道编号" )) %>% 
      filter(品牌 == input$brand) %>% 
      select(预审月,渠道编号,店面名称,地址,负责员工,业务上级,状态,预审量,预审通过,进件量,进件通过,生效量,生效额)
  })
  
  #生成合计行 create row of sum
  sum_row_combined_df <- reactive({
    combined_df() %>% 
      summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
      mutate(渠道编号 = "合计", 预审月 = format(Sys.Date()-20, "%Y%m")) %>% 
      filter(!is.na(主品牌))
  })
  
  本月品牌渠道预进生数据 <- reactive({
    rbind(combined_df(), sum_row_combined_df())
  }) 
  
  
  
  #设定选择器内容----
  observeEvent(品牌渠道表_df(), {
    # 将向量转换为列表
    brand_choices <- as.list(unique(品牌渠道表_df()$品牌))
    updateSelectInput(session, "brand", choices = brand_choices)
  })
  
  
  #全年品牌渠道成交数据----

   全年品牌渠道成交数据 <- reactive({
     df2() %>%
       filter(
         主品牌 == input$brand
         #预审月 == format(Sys.Date(), "%Y%m") 
         ) %>%
       group_by(主品牌,预审月,渠道编号) %>% 
       summarise(
                 生效量 = sum(round(生效量,0),na.rm = TRUE)
                 )%>%
       right_join( 品牌渠道表_df(), by = c("渠道编号" = "渠道编号" )) %>% 
       filter(品牌 == input$brand) %>% 
       select(预审月,渠道编号,店面名称,地址,负责员工,业务上级,状态,生效量) %>% 
       spread(key = 预审月 , value = 生效量) #LOOK AT 生效量也可以变为变量----
})
  
  # LOOKAT这里输出----
  # output$全年品牌渠道成交数据DF <- renderTable({ #输出查看
  #   req(rawdata_raw())
  #   head(全年品牌渠道成交数据(),5)
  # })
  
  
  output$全年品牌渠道成交数据DF <- DT::renderDataTable({
    req(rawdata_raw())
    
    month_columns <- names(全年品牌渠道成交数据()) %>% #设定动态列名
      grep(pattern = "^\\d{6}$", value = TRUE)  # 假设月份格式为 6 位数字（如 202408）
    
    
    全年品牌渠道成交数据() %>% 
      DT::datatable(
        class = "stripe hover order-column compact", #表格样式类
        options = list(dom = "tip", scrollX = TRUE, ordering = TRUE, 
                       autoWidth = TRUE), 
        filter = "top",
        rownames = TRUE
      ) %>% 
      # 品牌
      DT::formatStyle(
        columns = c("主品牌"), 
        color = "#333", 
        backgroundColor = "#a1d76a"
      ) %>% 
      # 门店信息
      DT::formatStyle(
        columns = c("渠道编号", "店面名称", "地址", "负责员工", "业务上级", "状态"),
        color = "#333", 
        backgroundColor = "#f7f7f7"  
      ) %>% 
      # 门店详细数据
      DT::formatStyle(
        columns = month_columns, 
        color = "#333", 
        #backgroundColor = "#e9a3c9"
        backgroundColor = DT::styleInterval(
          cuts = c(1, 2), 
          values = c("#fde0dd", "#fa9fb5", "#c51b8a")
        )
      ) 
  })
  
  
  #------
  #3月过程目标----
 
  
  渠道活跃_经理 <- reactive({
    req(rawdata_tidied())
    rawdata_tidied() %>%
      mutate(生效年月 = format(合同生效日期, "%Y-%m"))  %>%
       filter(between(生效年月,"2024-01", "2024-12")) %>% #筛选全年
       dplyr::group_by(金融经理员工工号, `金融经理名称`) %>%
       dplyr::summarise(生效渠道_24年 = n_distinct(渠道编号))
  })
  
  渠道活跃本月_经理 <- reactive({
    rawdata_tidied() %>% 
      mutate(生效年月 = format(合同生效日期, "%Y-%m")) %>% 
      filter(生效年月 == format(Sys.Date(), "%Y-%m")) %>% #筛选本月
      group_by(金融经理员工工号,金融经理名称) %>% 
      summarise(生效渠道_本月 = n_distinct(渠道编号))  %>% 
       left_join(渠道活跃_经理(), join_by("金融经理员工工号"=="金融经理员工工号",
                                  "金融经理名称" == "金融经理名称")) %>%
       filter(金融经理名称 != "成彦生") %>% 
       mutate("门店月度活跃%" = round(`生效渠道_本月`/ `生效渠道_24年`, 2)*100,
              门店活跃得分 = case_when(`门店月度活跃%` >= 50 ~ 60*0.3,
                                 `门店月度活跃%` <= 40 ~ 0*0.3,
                                 TRUE ~ (`门店月度活跃%` - 40) * 6) * 0.3
       )
    
  })
  
  经理人数2月底 <- reactive({
    name_list() %>% 
      filter(六级部门 == "易鑫融资租赁福州分公司") %>% 
      group_by(`直接上级`, `直接上级姓名`) %>% 
      summarise(人数 = length(`姓名`))
  })
  
  本月新增加门店数 <- reactive({ 
    channel_tidied() %>% 
      mutate(门店初始生效月 = format(初始生效日期, "%Y-%m"),
             业务上级工号 = as.integer(业务上级工号)) %>% 
      filter(门店初始生效月 == format(Sys.Date(), "%Y-%m")) %>% 
      group_by(业务上级工号, 业务上级) %>% 
       summarise(本月新开渠道数 = length(渠道编号)) %>% 
       
       left_join(经理人数2月底(), join_by("业务上级工号" == "直接上级",
                                  "业务上级" == "直接上级姓名")) %>% 
      ungroup() %>%
      mutate(`人均新增门店%` = round((`本月新开渠道数`/ `人数`)*100,1) ,
             人均新增加门店排名 = row_number(desc(`人均新增门店%`)),
             人均新增加门店得分 = case_when(
               `人均新增加门店排名` == 1 ~ 40* 0.3,
               `人均新增加门店排名` == 2 ~ 30* 0.3,
               `人均新增加门店排名` == 3 ~ 20* 0.3,
               `人均新增加门店排名` == 4 ~ 10* 0.3,
               T ~ 0
             ),
             业务上级工号 = as.integer(`业务上级工号`)
      )
   })
    
  直秒闪过订单数 <- reactive({  
    rawdata_tidied() %>%
      mutate(订单编号 = case_when( 合同生效日期 >= "2024-01-01" & 
                                 (直秒类型 == "固定比例" | 
                                    闪过选择 != "" |
                                    !is.na(必过选择类型))
                               ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 ) %>% 
      group_by(金融经理员工工号, 金融经理名称) %>% 
      summarize(直秒闪过订单数 = n())
  })
  
  直秒闪过必过订单逾期得分 <- reactive({ 
    rawdata_tidied() %>%
      mutate(订单编号 = case_when( 合同生效日期 >= "2024-01-01" & 
                                 (直秒类型 == "固定比例" | 
                                    闪过选择 != "" |
                                    !is.na(必过选择类型)) &
                                 最新逾期天数 >= 1 #逾期1天就算逾期
                               ~申请编号, .default = FALSE)
      ) %>% 
       subset(订单编号 > 0 ) %>% 
       group_by(金融经理员工工号, 金融经理名称) %>% 
       summarize(直秒闪过订单逾期数 = n())  %>% 
       left_join(直秒闪过订单数(), join_by(
         "金融经理员工工号"=="金融经理员工工号", 
         "金融经理名称"== "金融经理名称")) %>%
       ungroup() %>%
       mutate(逾期率 = round((直秒闪过订单逾期数 / 直秒闪过订单数)*100,1),
              直秒闪过必过订单排名 = row_number(逾期率),
              直秒闪过必过订单得分 = case_when(
                #`直秒闪过必过订单排名` == NA ~ 20,
                `直秒闪过必过订单排名` == 1 ~ 15,
                `直秒闪过必过订单排名` == 2 ~ 10,
                `直秒闪过必过订单排名` == 3 ~ 5,
                T ~ 20 )
       )
  })
    
  直秒闪过必过逾期订单 <- reactive({
    rawdata_tidied() %>%
    mutate(订单编号 = case_when( 合同生效日期 >= "2024-01-01" & 
                               (直秒类型 == "固定比例" | 
                                  闪过选择 != "" |
                                  !is.na(必过选择类型)) &
                               最新逾期天数 >= 1 #逾期1天就算逾期
                             ~申请编号, .default = FALSE),
           进件日期 = format(进件日期, "%Y-%m-%d"),
           申请编号 = as.character(`申请编号`) #调整申请编号--------
           ) %>% 
    subset(订单编号 > 0 ) %>% 
    #as.Date(`进件日期`, format = "%Y-%m") %>% 
    select(`当前逾期级别`, `已还款期数`, `产品方案名称`, `渠道编号`, `店面主体`, `申请编号`,
           `进件日期`, `当前销售口径剩余本金`, `融资金额`, `客户姓名`, `金融顾问名称`, `金融经理名称`)
  })
  
    
  LCV_成交得分 <- reactive({ 
    rawdata_tidied() %>% 
      mutate(生效年月 = format(合同生效日期, "%Y-%m")) %>%
      filter(是否LCV == "是" & 
               车辆类型 == "新车") %>% 
      group_by(生效年月, 金融经理员工工号, 金融经理名称) %>% 
      summarise(成交量 = length(申请编号)) %>% 
      filter(生效年月 == format(Sys.Date(), "%Y-%m") |
               生效年月 == format(Sys.Date()-30, "%Y-%m")
      ) %>% 
      spread(key = 生效年月, value = 成交量) %>% 
      mutate(环比 = round(((`2025-03` / `2025-02`)-1) * 100, 1),#取小数点后1位
             LCV成交排名 = row_number(desc(环比)),
             LCV成交得分 = case_when(
               `LCV成交排名` == 1 ~ 20,
               `LCV成交排名` == 2 ~ 15,
               `LCV成交排名` == 3 ~ 10,
               `LCV成交排名` == 4 ~ 5,
               TRUE ~ 0
             )
      )
  })
    
  理想成交得分 <- reactive({ 
    rawdata_tidied() %>% 
      mutate(生效年月 = format(合同生效日期, "%Y-%m")) %>%
      
       filter(`合作项目` == "理想汽车" & 
                `车辆类型` == "新车" 
              #& grepl("常规", 产品方案名称) #筛选 产品方案名称中带有常规的订单 
       )  %>% 
       group_by(生效年月, 金融经理员工工号, 金融经理名称) %>% 
       summarise(成交量 = length(申请编号)) %>% 
       filter(生效年月 == format(Sys.Date(), "%Y-%m")) %>% #筛选本月
       spread(key = 生效年月, value = 成交量) %>% 
       mutate(理想成交排名 = row_number(desc("2025-03")),
              理想成交得分 = case_when(
                `理想成交排名` == 1 ~ 10,
                `理想成交排名` == 2 ~ 7.5,
                `理想成交排名` == 3 ~ 5,
                `理想成交排名` == 4 ~ 2.5,
                TRUE ~ 0
                )
              ) %>% 
      rename( "理想本月成交" = `2025-03`)
  })
    
  
  
  二手车融资额小于6万 <- reactive({
    rawdata_tidied() %>% 
      mutate(生效年月 = format(合同生效日期, "%Y-%m")) %>%
      filter( `融资金额` <= 6  & 
                `车辆类型` == "二手车") %>% 
        group_by(生效年月, 金融经理员工工号, 金融经理名称) %>% 
         summarise(成交量 = length(申请编号))  %>% 
         filter(生效年月 == format(Sys.Date(), "%Y-%m") |
                生效年月 == format(Sys.Date()-30, "%Y-%m")) %>% 
       spread(key = 生效年月, value = 成交量)  %>%
      rename(二手车小6万本月成交 = `2025-03`) %>% 
      rename(二手车小6万上月成交 = `2025-02`) %>% 
       filter(金融经理名称 != "成彦生") %>%
        ungroup() %>%
        mutate(环比 = case_when(
                                is.na(`二手车小6万上月成交`) & !is.na(`二手车小6万本月成交`) ~ 100.0,
                                `二手车小6万上月成交` == 0 & `二手车小6万本月成交` == 0 ~ 0.0,
                                TRUE ~ round(((`二手车小6万本月成交` / `二手车小6万上月成交`) - 1) * 100, 1)
                                ),
               二手车融资额小于6万排名 = row_number(desc(环比)),
               二手车融资额小于6万得分 = case_when(
               `二手车融资额小于6万排名` == 1 ~ 20,
               `二手车融资额小于6万排名` == 2 ~ 15,
               `二手车融资额小于6万排名` == 3 ~ 10,
               `二手车融资额小于6万排名` == 4 ~ 5,
               TRUE ~ 0)
               )
      
  })
  
 
  
  March过程目标 <- reactive({ 
    渠道活跃本月_经理() %>% 
      left_join(本月新增加门店数()
                , join_by("金融经理员工工号"=="业务上级工号",
                          "金融经理名称"=="业务上级"))  %>% 
       left_join(直秒闪过必过订单逾期得分(), join_by(
         "金融经理员工工号"=="金融经理员工工号","金融经理名称"=="金融经理名称"
       )) %>% 
       left_join(LCV_成交得分(), join_by(
         "金融经理员工工号"=="金融经理员工工号","金融经理名称"=="金融经理名称"
       )) %>% 
       rename( "LCV上月成交" = `2025-02`,
               "LCV本月成交" = `2025-03`) %>% 
        left_join(理想成交得分(), join_by(
          "金融经理员工工号"=="金融经理员工工号","金融经理名称"=="金融经理名称"
        )) %>% 
       #rename("理想本月成交" = '2025-03') %>% 
       left_join(二手车融资额小于6万(), join_by(
         "金融经理员工工号"=="金融经理员工工号","金融经理名称"=="金融经理名称"
       )) %>% 
      mutate(across(everything(), ~ replace_na(., 0))) %>%
      mutate(
        
        总得分 = (门店活跃得分 + 人均新增加门店得分) * 0.3 + 
          直秒闪过必过订单得分 + LCV成交得分 + 
          理想成交得分 + 二手车融资额小于6万得分
      )
  })
    
   
    
    output$March过程目标DF <- renderTable({ #输出查看
      req(rawdata_tidied())
      head(March过程目标(),4) 
    })
    
    output$testdf <- renderTable({ #输出查看
      req(channel_tidied())
      head(直秒闪过必过逾期订单(),10) #直秒闪过必过逾期订单
    })
    
    
      
  output$March过程目标DF_theme <- DT::renderDataTable({
        req(March过程目标())
        
        March过程目标() %>% 
          DT::datatable(
            class = "stripe hover order-column",
            options = list(dom = "tip", scrollX = TRUE),
            filter = "none"
          ) %>% 
          # 总得分列
          DT::formatStyle(
            columns = c("金融经理员工工号", "金融经理名称", "总得分"), 
            color = "white", 
            backgroundColor = "#4d4d4d"
          ) %>% 
          # 门店活跃得分列
          DT::formatStyle(
            columns = c("生效渠道_本月", "生效渠道_24年", "门店月度活跃%", "门店活跃得分"),
            color = "#333", 
            backgroundColor = "#b2182b"  
          ) %>% 
          # 人均新增加门店得分列
          DT::formatStyle(
            columns = c("本月新开渠道数", "人数","人均新增门店%" ,"人均新增加门店排名" ,"人均新增加门店得分"), 
            color = "#333", 
            backgroundColor = "#ef8a62" 
          ) %>% 
          DT::formatStyle(
            columns = c("直秒闪过订单逾期数", "直秒闪过订单数", "逾期率" ,"直秒闪过必过订单排名" ,"直秒闪过必过订单得分"), 
            color = "#333", 
            backgroundColor = "#fddbc7"  
          ) %>% 
          DT::formatStyle(
            columns = c("LCV上月成交", "LCV本月成交", "环比.x" ,"LCV成交排名" ,"LCV成交得分"), 
            color = "#333", 
            backgroundColor = "#ffffff"  
          ) %>% 
          DT::formatStyle(
            columns = c("理想本月成交", "理想成交排名", "理想成交得分"), 
            color = "#333", 
            backgroundColor = "#e0e0e0"  
          ) %>%
          DT::formatStyle(
            columns = c("二手车小6万上月成交", "二手车小6万本月成交", "环比.y", 
                        "二手车融资额小于6万排名", "二手车融资额小于6万得分"), 
            color = "#333", 
            backgroundColor = "#999999" 
          )
        })
      
  #Download March过程目标()
      output$Download_March过程目标 <- downloadHandler(
        filename = function() {
          paste("金融经理过程目标数据_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          multi_sheets = list(
            
            "金融经理过程目标数据"= March过程目标(),
            
            "直秒闪过必过逾期数据" = 直秒闪过必过逾期订单() )
          write_xlsx(multi_sheets, path = file)
        }
      )


  
  
  
  #定义输出表格----
  output$rawdataDF <- renderTable({ #输出查看
    req(rawdata_raw())
    head(df2(),4)
  })
  


  output$本月品牌渠道预进生数据df <- DT::renderDataTable({
    req(rawdata_raw())
    
    本月品牌渠道预进生数据() %>% 
      DT::datatable(
        class = "stripe hover order-column compact", #表格样式类
        options = list(dom = "tip", scrollX = TRUE, ordering = TRUE, 
                       autoWidth = TRUE), 
        filter = "top",
        rownames = TRUE
      ) %>% 
      # 品牌
      DT::formatStyle(
        columns = c("主品牌"), 
        color = "white", 
        backgroundColor = "#fc8d59"
      ) %>% 
      # 门店信息
      DT::formatStyle(
        columns = c("预审月", "渠道编号", "店面名称", "地址", "负责员工", "业务上级", "状态"),
        color = "#333", 
        backgroundColor = "#ffffbf"  
      ) %>% 
      # 门店详细数据
      DT::formatStyle(
        columns = c("预审量", "预审通过","进件量" ,"进件通过" ,"生效量", "生效额"), 
        color = "#333", 
        backgroundColor = "#91bfdb" 
      ) 
  })
  
  
  
  #Download_本月品牌渠道预进生数据df
  
  output$Download_本月品牌渠道预进生数据df <- downloadHandler(
    filename = function() {
      paste("本月品牌渠道预进生数据_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      multi_sheets = list(

        "本月品牌渠道预进生数据"= 本月品牌渠道预进生数据(),
        
        "全年品牌渠道成交数" = combined_df() )
      write_xlsx(multi_sheets, path = file)
    }
  )
  
   output$tempdf <- renderTable({
     req(rawdata_raw())
     combined_df()
     })
  
  

}#结束server
  
  
  


shinyApp(ui = ui, server = server)