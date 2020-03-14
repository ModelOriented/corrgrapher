library(visNetwork)
library(dplyr)
library(tidyr)

# df <- Titanic
# View(Titanic)
df <- as.data.frame(datasets::Seatbelts) %>%
  select(-law)
# View(head(as.data.frame(df)))
corelations <- cor(df)
coldict <- setNames(1:ncol(df), colnames(df))
no_corelation_approx <- 0.15
negative_corelation_handler <- function(x) x

nodes <- data.frame(id = coldict,
              label = names(coldict))
edges <- cor(df) %>% as.data.frame() %>%
  pivot_longer(cols = 1:ncol(df),
               names_to = "to",
               values_to = "strength") %>%
  mutate(from = rep(colnames(df), each = ncol(df))) %>%
  filter(from != to) %>%
  mutate(length = negative_corelation_handler(1.1 - strength) * 500,
         hidden = abs(strength) < no_corelation_approx,
         physics = !hidden,
         color = if_else(strength >= 0, 'blue', 'red'),
         label = as.character(round(strength, 2)),
         from = coldict[from],
         width = abs(strength) + 3,
         to = coldict[to]) %>%
  filter(from >= to)
# nodes <- data.frame(id = coldict,
#                     label = names(coldict)
#                      mass = sapply(coldict, function(id) filter(edges, (from == id) | (to == id),  
#                                                                 strength < 0) %>%
#                                      select(strength) %>% abs() %>% sum() + 1)
#)
net <- visNetwork(nodes, select(edges, -strength), height = 600, width = 1000) %>%
  visNodes(size = 20) %>% 
  visEdges(smooth = FALSE) # %>%
  # visPhysics(barnesHut = list(repulsion = -500))

net

