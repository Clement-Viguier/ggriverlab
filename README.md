Labels for rivers in the ggplot2 framework
------------------------------------------

ggriverlab aims to introduce labels that follow lines to creat nice maps
with ggplot2. While this function is common in GIS software, I could not
find satisfying solutions for R plotting, so here is my take on it.

Installation
------------

The package can be installed from github with the help of the devtool
package.

    # install.packages("devtools")
    devtools::install_github("Clement-Viguier/ggriverlab")

Demo
----

    library(ggriverlab)
    library(riverdist)
    library(ggplot2)
    library(dplyr)


    river <- as.data.frame(Gulk$lines[4])
    colnames(river) <- c("x", "y")

    ggplot(river, aes(x,y)) + geom_path() + coord_fixed()
    ggplot(river, aes(x,y)) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk", offset = 0.24), reverse = F)

    ?geom_river_label

    # Plotting the full river

    branches <- 1:14
    branch_list <- lapply(branches, function(b, Gulk){
      river <- as.data.frame(Gulk$lines[b])
      colnames(river) <- c("x", "y")
      river$branch <- b
      return(river)
    }, Gulk)
    full_river<- do.call(rbind, branch_list)

    ggplot(full_river, aes(x,y, group = branch, colour = as.factor(branch))) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk"), check_length = F) +
     theme_minimal() + theme(legend.position = "none")

    # Check_length avoids to print labels that cannot fit the river portion.
    ggplot(full_river, aes(x,y, group = branch, colour = as.factor(branch))) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk"), check_length = T) +
      theme_minimal() +theme(legend.position = "none")

    ggplot(full_river, aes(x,y, group = branch, colour = as.factor(branch))) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk"), offset = 0.15, dist = 0.01, check_length = T) +
      theme_minimal() +theme(legend.position = "none")

The `geom_river_label` function can be used to plot multiple river, or
branches at once. If they are too short, the label can be exclude thanks
to the option `check_length = T`. The above code produce the following
map of the river:

![river labeling with
ggriverlab](https://github.com/Clement-Viguier/ggriverlab/blob/master/Images/river_branches.png)

    # install.packages('devtools')
    # devtools::install_github('thomasp85/gganimate')
    library(gganimate)

    # generate data for the animation
    offsets <- seq(0.05, 0.9, length.out = 20)
    list_data <- lapply(offsets, function(os, river){
      river$off <- os
      return(river)
    }, river)
    data_offset <- do.call(rbind, list_data)

    anim <- ggplot(data_offset, aes(x,y)) + geom_path() + coord_fixed() + geom_river_label(aes(label = "Gulk", offset = off), relative = T, check_length = F, reverse = F, repeated = F)+
      transition_states(off, 1, 3)
    anim
    anim_save("Images/moving_label_smooth.gif", last_animation())

The effect of the different parameters on the label position and shape
can be seen here:

![offset parameter effect on river
label](https://github.com/Clement-Viguier/ggriverlab/blob/master/Images/moving_label_offset.gif)
![dist parameter effect on river
label](https://github.com/Clement-Viguier/ggriverlab/blob/master/Images/moving_label_dist.gif)
![vpos parameter effect on river
label](https://github.com/Clement-Viguier/ggriverlab/blob/master/Images/moving_label_vpos.gif)

### Larger scale data

Oceans and rivers

    library(raster)
    library(tidyr)

    ne_rivers <- shapefile("./data/ne_50m_rivers_lake_centerlines.shp")
    ne_lands  <- shapefile("./data/ne_50m_land.shp")
    ne_lakes  <- shapefile("./data/ne_50m_lakes.shp")

    data <- ne_rivers@data
    data$id <- rownames(data)
    rivers_df <- fortify(ne_rivers) %>% left_join(data, by = "id")

    lands <- fortify(ne_lands)


    ggplot(rivers_df, aes(long, lat, group = group)) +
       geom_path(aes(), colour = "steelblue") + geom_river_label(aes(label = label), colour = "steelblue") +
      coord_fixed() + 
      facet_wrap(~id, scales = "free")+
      theme_linedraw()+
      theme_classic() + theme(panel.background = element_rect(fill = "steelblue"))

### TO DO LIST

-   ✓ make it work with point: place points along the path in place of
    letters;
-   ✓ include in geom;
-   □ make it work with entire labels as an option;
-   ✓ make it work with letters;

### TO DO if the rest is done and clean

-   □ make river specific geom (size~order);
-   □ make interactive tool to test the parameters;
-   ✓ find good default parameters;
-   □ scale against text size or plot size ?;
