library(data.table)
library(dtplyr)
library(dplyr)
library(dimRed)
library(ggplot2)
library(patchwork)
library(hdrcde)
library(igraph)
library(MASS)
library(ggraph)
library(plotly)

load('../../data/nn_embed_function.rda')
load('../../data/neighbors_1id336tow.rda')
# LLE with sparse matrices
knn <- 20
eps <- 0.5
knng <- makeKNNgraph(x = indata, k = knn, eps = eps)
igraph::is.connected(knng)
Y <- nn_embed(indata, knn = knn, eps = 1.1, ndim = 2)
Y_lle <- dimRed::embed(indata, 'LLE', knn = knn)@data@data
all.equal(Y, -Y_lle)

p1 <- e1 %>% 
  ggplot(aes(x=LLE1, y=LLE2)) + 
  geom_point() 
p2 <- e2 %>% 
  ggplot(aes(x=LLE1, y=LLE2)) + 
  geom_point() 

(p2 + p1)

# ann_lle_k20_1id336tow.png



### Laplacian Eigenmap
Y_lapeig <- dimRed::embed(indata, 'LaplacianEigenmaps', .mute = c("message", "output"), knn = knn)@data@data
### Hessian LLE
# knn >= 5 for HLLE
Y_hlle <- dimRed::embed(indata, 'HLLE', .mute = c("output"), knn=knn)@data@data

week <- spdemand[,c('id', 'tow')][, dow := .(as.factor(ceiling(tow/48)))]
e1 <- cbind(week, Y_lle)
e2 <- cbind(week, Y_lapeig)
e3 <- cbind(week, Y)

p1 <- e1 %>% 
  ggplot(aes(x=LLE1, y=LLE2)) + 
  geom_point() 
p2 <- e2 %>% 
  ggplot(aes(x=LEIM1, y=LEIM2, color=dow)) + 
  geom_point() 
p3 <- e3 %>% 
  ggplot(aes(x=HLLE1, y=HLLE2, color=dow)) + 
  geom_point() 

p <- ggraph(knng) + 
  geom_edge_link(colour = 'grey30') + 
  geom_node_point() + 
  geom_node_text(aes(label = V(knng)), colour = blues9[7],
                 hjust = -.4, check_overlap = TRUE)
(p + p1) / 
  (p2 + p3)




# plot typical households
# load("~/git/eda/data/DT.rda")
# head(DT)
# DT1 <- DT[id == 1003 | id == 1539,]
# save(DT1, file = "docs/confirmation/Slides/DT1.eda")

# # Read combined file
# tmp <- fread(file.path("SmartMeterData/Data.txt"))
# # Consider only residential data
# info <- readxl::read_excel("SmartMeterData/SME and Residential allocations.xlsx")
# residential <- as.matrix(info[info[,2] == 1,1]) 
# # 4225 resid, 485 SME, 1735 others
# k <- is.element(tmp$V1, residential)
# tmp <- tmp[k,]
# # Keep only ids where we have complete data
# keepids <- table(tmp[,V1]) >= 25726   # deleted 586 ids
# keepids <- as.numeric(names(keepids)[keepids])
# keep <- is.element(tmp[,V1], keepids)
# tmp <- tmp[keep,]
# tmp <- tmp[V1 == 1003 | V1 == 1539,]
# save(tmp, file = "docs/confirmation/Slides/tmp.eda")

load("docs/confirmation/Slides/tmp.eda")
DT1 <- tmp
summary(DT1)
class(DT1)
p <- DT1 %>% 
  # lazy_dt() %>%
  # group_by(id) %>%
  # mutate(Time=paste(round(day/7),tow,sep='_')) %>%
  as.data.frame() %>% 
  ggplot(aes(x = V2, y = V3, group = V1)) + 
    geom_line() + 
    facet_grid(V1 ~ .) + 
    labs(x = "Time period", y = "Demand (kWh)")

save(p, file = "docs/confirmation/Slides/smartmeter_2id.rda")
load(here::here("docs/confirmation/Slides/smartmeter_2id.rda"))
plotly::ggplotly(p, width = 600, height = 400)






# swissroll manifold
sr1000 <- loadDataSet("Swiss Roll", n = 3000)
plot(sr1000, type = "3vars")


n <- 8000; sigma <- 0.5
x <- stats::runif(n, 1.5 * pi, 4.5 * pi)
y <- stats::runif(n, 0, 30)
srmap <- dimRed:::swissRollMapping(x, y)
sr <- dimRed:::swissRoll(n, sigma)

marker <- list(color = ~x+y+z, colorscale = c('#FFE1A1', '#683531'), 
               showscale = FALSE)
srp <- plotly::plot_ly(data.frame(srmap), x = ~x, y = ~y, z = ~z, 
        size = 4, marker = marker)  

# plotly::plot_ly(data.frame(sr@data), x = ~x, y = ~y, z = ~z, size = 4) %>%
  # layout( title = paste("Swiss Roll Data with Noise n =", n))

htmlwidgets::saveWidget(srp,paste0("p.html"))

# ggplotly(srp, width=600, height=400) # not an ggplot2 object





## 6 subplot embedding
dataset <- loadDataSet("Swiss Roll", n = 1000)
embedded_data <- list()
embed_methods <- dimRedMethodList()[c(17, 18, 9, 13, 12)]

for (e in embed_methods) {
  message("embedding: ", e)
  embedded_data[[e]] <- embed(dataset, e, .mute = c("message", "output"))
}

### plotting
# save the plot as pdf
# pdf("inst/sr100_embedding.pdf", width = 10, height = 10)
par(mfrow = c(2,3))
## plot the original data set
plot(dataset, type = "3vars")
## plot the embedding of both methods
lapply(embedded_data, plot, type = "2vars")
# dev.off()