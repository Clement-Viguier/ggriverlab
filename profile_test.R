
library(ggriverlab)
library(riverdist)
library(ggplot2)

river <- as.data.frame(Gulk$lines[4])
colnames(river) <- c("x", "y")

ggplot(river, aes(x,y)) + geom_path() + coord_fixed()
ggplot(river, aes(x,y)) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk", offset = 14000, dist = 1500, vpos = 2000, win = 3000))

# the full fiver

branches <- 1:14
branch_list <- lapply(branches, function(b, Gulk){
  river <- as.data.frame(Gulk$lines[b])
  colnames(river) <- c("x", "y")
  river$branch <- b
  return(river)
}, Gulk)
full_river<- do.call(rbind, branch_list)

ggplot(full_river, aes(x,y, group = branch, colour = as.factor(branch))) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk", offset = 6000, dist = 2000, vpos = 3000, win = 3000))


Rprof(tmp <- tempfile())
ggplot(full_river, aes(x,y, group = branch, colour = as.factor(branch))) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk", offset = 6000, dist = 2000, vpos = 3000, win = 3000), relative = F)
Rprof(NULL)
tree <- prof.tree(tmp)
print(tree, limit = 100)



unlink(tmp)
