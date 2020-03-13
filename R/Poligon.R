library(visNetwork)
library(dplyr)
library(tidyr)
# df <- Titanic
# View(Titanic)
df1 <- as.data.frame(datasets::Seatbelts)
# View(head(as.data.frame(df1)))
corelations <- cor(df1)
coldict <- setNames(1:ncol(df1), colnames(df1))
no_corelation_approx <- 0.05

nodes <- data.frame(id = coldict,
              label = names(coldict))
edges <- cor(df1) %>% as.data.frame() %>%
  pivot_longer(cols = 1:ncol(df1),
               names_to = "to",
               values_to = "strength") %>%
  mutate(from = rep(colnames(df1), each = ncol(df1))) %>%
  filter(from != to) %>%
  mutate(length = (1.05 - strength) ^ 2 * 1000,
         physics = strength > no_corelation_approx,
         color = if_else(physics, 'blue', 'red'),
         hidden = abs(strength) < no_corelation_approx,
         label = as.character(round(strength, 2)),
         from = coldict[from],
         to = coldict[to]) %>%
  filter(from >= to)
# nodes <- data.frame(id = coldict,
#                     label = names(coldict)
#                      mass = sapply(coldict, function(id) filter(edges, (from == id) | (to == id),  
#                                                                 strength < 0) %>%
#                                      select(strength) %>% abs() %>% sum() + 1)
#)
net <- visNetwork(nodes, select(edges, -strength), height = 600, width = 1000) %>%
  # visEdges(length = )  %>% 
  visLayout(randomSeed = 123) 

net

