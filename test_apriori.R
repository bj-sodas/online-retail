library(tidyverse)
library(arules)
library(arulesViz)
library(RColorBrewer)

# 数据集转换 --------------------------------------------------------------

# dat <- read.transactions('retail.csv', format = 'single', sep = ',', header = TRUE, cols = c('invoice_no', 'stock_code'))
#dat <- read.transactions('retail.csv', format = 'single', sep = ',', header = TRUE, cols = c('invoice_no', 'description'))

# MyList<-list( 
#     
#     c("豆奶", "莴苣"),
#     
#     c("莴苣","尿布","葡萄酒","甜菜"),
#     
#     c("豆奶","尿布","葡萄酒","橙汁"),
#     
#     c("莴苣","尿布","豆奶","葡萄酒"),
#     
#     c("莴苣","尿布","豆奶","橙汁")
# )
# dat <- as(MyList,"transactions")



# 数据集转换 方法2 ---------------------------------------------------------------
raw <- readRDS('raw.rds')
dat <- raw %>% 
    filter(!is.na(customer_id)) %>% 
    select(invoice_no, description) %>% 
    as.data.frame()

dat <- as(split(dat[, "description"], dat[, "invoice_no"]), "transactions")


# preview data ------------------------------------------------------------

# par(family='Noto Sans CJK SC')

inspect(dat[1:5])

summary(dat)

itemFrequency(dat[,1:3])
itemFrequencyPlot(dat, support = 0.02)
itemFrequencyPlot(dat, topN = 20)

image(dat[1:5])


# 使用apriori算法生成关联规则 ------------------------------------------------------------------

dat_rules <- apriori(data = dat, parameter = list(support = 0.02, confidence = 0.7, minlen = 2))

summary(dat_rules)
inspect(dat_rules)

inspect(sort(dat_rules, by = 'lift'))

# 也可以用subset做规则的筛选,取"右手边"含有whole milk且lift大于1.2的规则 
# sub.rules=subset(rules, subset = rhs %in% "whole milk" &lift > 1.2)
sub.rules = subset(dat_rules, lift > 1.2)


# 可视化 ------------------------------------------------------------------

plot(dat_rules, measure = "confidence", method = "graph", 
     control = list(type = "items"), shading = "lift")

plot(sub.rules, measure = "confidence", method = "graph",
     control = list(type = "items"), shading = "lift")

plot(dat_rules, measure = "confidence", method = "graph", engine = "htmlwidget",
     control = list(type = "items"), shading = "lift")
