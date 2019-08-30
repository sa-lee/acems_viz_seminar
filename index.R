## /* custom.css */

## :root{

##   --main-color1: #2f4c7a;

##   --main-color2: #bcbddc;

##   --main-color3: #efedf5;

##   --main-color4: #9DDAE5;

##   --text-color3: black;

##   --text-color4: black;

##   --code-inline-color: #4e5054;

##   --link-color: #006CAB;

##   --logo: url(http://www.fragiletoagile.com.au/wp-content/uploads/2018/02/monash-university-logo-transparent.png);

## }

## .huge { font-size: 300% }

## .large { font-size: 150% }

## .largeish { font-size: 120% }

## .summarystyle { font-size: 150%;

##   line-height:150%;}

## 
## .left-code {

##   color: #777;

##   width: 48%;

##   height: 92%;

##   float: left;

## }

## .right-plot {

##   width: 50%;

##   float: right;

##   padding-left: 1%;

## }


## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE, width = 120)


knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE,
                      fig.align = "center",
                      fig.height = 6,
                      fig.width = 4,
                      cache.path = "./cache/",
                      fig.path = "./figures/",
                      dev.args = list(bg = "transparent")
                      )
library(knitr)
# preload data
library(tidyverse)
tb <- read_rds(here::here("data", "tb_tidy.rds"))
tb_au <- filter(tb, country == "Australia", !is.na(age_group))



hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
   lines <- options$output.lines
   if (is.null(lines)) {
     return(hook_output(x, options))  # pass to default hook
   }
   x <- unlist(strsplit(x, "\n"))
   more <- "..."
   if (length(lines)==1) {        # first n lines
     if (length(x) > lines) {
       # truncate the output, but add ....
       x <- c(head(x, lines), more)
     }
   } else {
     x <- c(more, x[lines], more)
   }
   # paste these lines together
   x <- paste(c(x, ""), collapse = "\n")
   hook_output(x, options)
 })

theme_set(
  theme_grey(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent")
  )
)


## ----tb-01, eval = TRUE, echo = TRUE-------------------------------------
library(tidyverse)
tb <- read_rds("data/tb_tidy.rds")
tb_au <- filter(tb, country == "Australia", !is.na(age_group))
tb_au


## ----echo=FALSE, fig.width=12, fig.height=5------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2") 


## ----plot1, eval=FALSE, echo=TRUE----------------------------------------
## ggplot(tb_au) #<<


## ----output1, ref.label="plot1", echo=FALSE, cache=TRUE, fig.height = 5, fig.width=10----


## ----plot2, eval=FALSE, echo=TRUE----------------------------------------
## ggplot(tb_au) +
##   aes(x = year, #<<
##       y = count, #<<
##       fill = gender) #<<


## ----output2, ref.label="plot2", echo=FALSE, cache=TRUE, fig.height = 5, fig.width=10----


## ----plot3, eval=FALSE, echo=TRUE----------------------------------------
## ggplot(tb_au) +
##   aes(x = year,
##       y = count,
##       fill = gender) +
##   geom_bar(stat = "identity", #<<
##            position = "fill") #<<


## ----output3, ref.label="plot3", echo=FALSE, cache=TRUE, fig.height = 5, fig.width=10----


## ----plot4, eval=FALSE, echo=TRUE----------------------------------------
## ggplot(tb_au) +
##   aes(x = year,
##       y = count,
##       fill = gender) +
##   geom_bar(stat = "identity",
##            position = "fill") +
##   scale_fill_brewer(  #<<
##     palette = "Dark2")  #<<
## 


## ----output4, ref.label="plot4", echo=FALSE, cache=TRUE, fig.height = 5, fig.width=10----


## ----plot5, eval=FALSE, echo=TRUE----------------------------------------
## ggplot(tb_au) +
##   aes(x = year,
##       y = count,
##       fill = gender) +
##   geom_bar(stat = "identity",
##            position = "fill") +
##   scale_fill_brewer(
##     palette = "Dark2") +
##   facet_grid(. ~ age_group) #<<
## 


## ----output5, ref.label="plot5", echo=FALSE, cache=TRUE, fig.height = 5, fig.width=10----


## ----echo=TRUE, fig.width=12, fig.height=4.5-----------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ----echo=TRUE, fig.width=12, fig.height=4.5-----------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
##   geom_bar(stat = "identity", position = "stack") +
##   facet_grid(~ age_group) +
##   scale_fill_brewer(palette="Dark2")


## ----echo=TRUE, fig.width=12, fig.height=4.5-----------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
##   geom_bar(stat = "identity", position="dodge") +
##   facet_grid(~ age_group) +
##   scale_fill_brewer(palette="Dark2")


## ----plotPolar, eval=TRUE, echo=TRUE, fig.width=12, fig.height=4.5-------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(gender ~ age_group) +
  scale_fill_brewer(palette="Dark2") + 
  coord_polar() 


## ----plotPolarC, eval=FALSE, echo=TRUE-----------------------------------
## ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
##   geom_bar(stat = "identity") +
##   facet_grid(gender ~ age_group) +
##   scale_fill_brewer(palette="Dark2") +
##   coord_polar()


## ----plotPie, eval=TRUE, echo=TRUE, fig.width=12, fig.height=4.5---------
ggplot(tb_au, aes(x = 1, y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position="fill") +
  facet_grid(gender ~ age_group) +
  coord_polar(theta = "y")


## ----plotPie2, eval=FALSE, echo=TRUE-------------------------------------
## ggplot(tb_au, aes(x = 1, y = count, fill = factor(year))) +
##   geom_bar(stat = "identity", position="fill") +
##   facet_grid(gender ~ age_group) +
##   coord_polar(theta = "y")


## ----both, echo=FALSE, fig.height=5, fig.width=14------------------------
a <- tb_au %>% filter(year == 2012) %>%
  ggplot() +
  aes(x=gender, y=count, fill=gender) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~age_group, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement A")
b <- tb_au %>% filter(year == 2012) %>%
  ggplot() +
  aes(x=age_group, y=count, fill=age_group) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~gender, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement B")
library(gridExtra)
grid.arrange(a, b, nrow=1)


## ---- echo = FALSE, fig.height=5, fig.width=7----------------------------
a


## ---- echo = FALSE, fig.height=5, fig.width=7----------------------------
b


## ----arrA, echo=TRUE, fig.height=5, fig.width=7--------------------------
tb_au %>% filter(year == 2012) %>%
  ggplot() +
  aes(x=gender, y=count, fill=gender) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~age_group, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement A")


## ----arrB, echo=TRUE, fig.height=5, fig.width=7--------------------------
tb_au %>% filter(year == 2012) %>%
  ggplot() +
  aes(x=age_group, y=count, fill=age_group) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~gender, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement B")


## ------------------------------------------------------------------------
# we need some geographical coordinates
world_data <- map_data("world")
head(world_data)


## ----reldata, echo = FALSE-----------------------------------------------
tb_rel <- tb %>% 
  group_by(country, year) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  mutate(
    diff = lag(count, 1), 
    reldiff = if_else(diff == 0, 0, (count - diff) / diff)
  ) %>% 
  filter(year != first(year)) %>% 
  ungroup() %>% 
  mutate(
    country=recode(country, 
                   "United States of America"="USA", 
                   "United Kingdom of Great Britain and Northern Ireland"="UK",
                   "Russian Federation"="Russia")
  )
tb_map <- left_join(
  world_data, 
  tb_rel, 
  by = c("region" = "country")
)
head(tb_map)


## ----chloromap, echo = TRUE, eval = FALSE--------------------------------
## library(ggthemes)
## map_2012 <- tb_map %>%
##   filter(year == 2012)
## map <- map_2012 %>%
##   ggplot(aes(x = long, y = lat, group = group, fill = reldiff)) +
##   geom_polygon() +
##   coord_map(xlim = c(-180, 180)) +
##   theme_map()
## map


## ----chloro-out, ref.label="chloromap", echo=FALSE, cache=TRUE, fig.width=12, fig.height=6----


## ----vc, echo = TRUE, eval = FALSE---------------------------------------
## library(viridis)
## map <- map + scale_fill_viridis(option = "magma")
## map


## ----viridis-out, ref.label="vc", echo=FALSE, cache=TRUE, fig.height=6, fig.width=12----


## ----facetMaps, echo = TRUE, eval =  FALSE-------------------------------
## library(ggthemes)
## tb_map %>%
##   filter(year %in% 2007:2012) %>%
##   ggplot(aes(x = long, y = lat, group = group, fill = reldiff)) +
##   geom_polygon() +
##   coord_map(xlim = c(-180, 180)) +
##   scale_fill_viridis(limits = c(-1, 4), option = "magma") +
##   facet_wrap(~ year) +
##   theme_map()


## ----facetMaps-out, ref.label="facetMaps", echo = FALSE, cache = TRUE, fig.height=6, fig.width = 12----


## ----animateMaps, echo = TRUE, eval = FALSE------------------------------
## library(gganimate)
## tb_map %>%
##   filter(year %in% 2007:2012) %>%
##   ggplot(aes(x = long, y = lat, group = group, fill = reldiff)) +
##   geom_polygon() +
##   coord_map(xlim = c(-180, 180)) +
##   scale_fill_viridis(limits = c(-1, 4), option = "magma") +
##   theme_map() +
##   transition_time(year)


## ----animateMaps-out, ref.label="animateMaps", fig.width = 12, fig.height = 6, echo = FALSE, cache = TRUE----


## ----plotly, echo = TRUE, fig.height=6, fig.width=12---------------------
library(plotly)
ggplotly(map)

