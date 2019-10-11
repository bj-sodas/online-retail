library(sparklyr)
library(dplyr)
library(readr)
library(purrr)
library(igraph)
library(visNetwork)


# FPGrowth in Spark -------------------------------------------------------


# Spark properties
conf <- spark_config()
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "8G"
conf$`spark.memory.fraction` <- 0.9
sc <- spark_connect(master = "local", version = "2.2.0", config = conf)

# this is our data from Instacart
orders <- spark_read_csv(sc, "orders", "instacart_2017_05_01/order_products__prior.csv")
orders_wide <- orders %>% 
    group_by(order_id) %>% 
    summarise(items = collect_list(product_id))

# use FP Growth
fpg.fit <- ml_fpgrowth(orders_wide, items_col = "items", min_confidence = .03, min_support = .01)
rules <- ml_association_rules(fpg.fit) %>% collect()

# these are our rules
asso <-
    tibble(
        antecedent = unlist(rules$antecedent),
        consequent = unlist(rules$consequent),
        confidence = rules$confidence
    )

# remember to close connection
spark_disconnect_all()


# iGraph ------------------------------------------------------------------

# get product names
products <- read_csv("instacart_2017_05_01/products.csv")

# bind to nodes
nodes <- data.frame(id = unique(asso$antecedent, asso$consequent)) %>% 
    distinct() %>% 
    left_join(products, by = c("id" = "product_id")) %>% 
    select(id, label = product_name)

edges <- asso %>% mutate(weight = confidence * 10)

df.g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
plot(
    df.g,
    edge.arrow.size = .1,
    edge.curved = .3,
    edge.width = edges$weight,
    vertex.color = "lightblue",
    vertex.label.color = "darkblue",
    vertex.label.cex = .7,
    edge.label.cex = .7
)


# VisNetwork --------------------------------------------------------------


nodes <- data.frame(id = unique(asso$antecedent, asso$consequent)) %>% 
    distinct() %>% 
    left_join(products, by = c("id" = "product_id")) %>% 
    select(id, label = product_name)

edges <- asso %>% 
    mutate(width = confidence * 20, 
           smooth = TRUE, arrows = "to",
           label = format(confidence, digits = 2)) %>% 
    rename(from = antecedent, to = consequent)

visNetwork(nodes, edges, height = "600px", width = "100%")


# Finding Subgroups -------------------------------------------------------

 
net.sym <- as.undirected(df.g, mode = "collapse", 
                         edge.attr.comb = list(weight = "sum", "ignore"))
ceb <- cluster_edge_betweenness(net.sym)
dendPlot(ceb, mode = "hclust")
plot(ceb, net.sym)
