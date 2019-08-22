## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE,
                      fig.align = "center",
                      fig.height = 6,
                      fig.width = 4,
                      root.dir = here::here()
                      )
library(knitr)
# preload data
library(tidyverse)
tb <- read_rds(here::here("data", "tb_tidy.rds"))
tb_au <- filter(tb, country == "Australia", !is.na(age_group))


## ----tb-01, eval = FALSE, echo = TRUE------------------------------------
## library(tidyverse)
## tb <- read_rds("data/tb_tidy.rds")
## tb

## ---- echo = FALSE-------------------------------------------------------
tb


## ----tb-02, eval = FALSE-------------------------------------------------
## tb_au <- filter(tb, country == "Australia")
## tb_au


## ----echo=FALSE, fig.width=8, fig.height=2-------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ------------------------------------------------------------------------
p <- ggplot(tb_au, aes(x = year, y = count, fill = gender))


## ------------------------------------------------------------------------
p


## ---- fig.width=8--------------------------------------------------------
p <- p + geom_bar(stat = "identity", position = "fill")


## ---- fig.width=8--------------------------------------------------------
p


## ---- fig.height=8, fig.width=6------------------------------------------
p <- p + scale_fill_brewer(palette = "Dark2")
p


## ---- fig.width=8, fig.height = 2----------------------------------------
p <- p + facet_grid(. ~ age_group) 
p


## ------------------------------------------------------------------------
p <- p + facet_grid( age_group ~ .) 
p


## ----echo=TRUE, fig.width=8, fig.height=2--------------------------------
p <- ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")
p


## ----echo=FALSE, fig.width=8, fig.height=2-------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ----echo=FALSE, fig.width=8, fig.height=2-------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ---- eval = FALSE, echo = TRUE------------------------------------------
## library(tidyverse)
## tb <- read_rds("data/tb_tidy.rds")
## tb_au <- filter(tb,
##                 country == "Australia",
##                 !is.na(age_group))


## ----echo=TRUE, fig.width=8, fig.height=2--------------------------------
p <- ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")
p


## ----echo=FALSE, fig.width=8, fig.height=2-------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ----echo=TRUE, fig.width=8, fig.height=2--------------------------------
p <- ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ age_group) +
  scale_fill_brewer(palette="Dark2")
p


## ----echo=FALSE, fig.width=8, fig.height=3-------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(gender ~ age_group) +
  scale_fill_brewer(palette="Dark2")


## ----echo=TRUE, fig.width=8, fig.height=2--------------------------------
p <- ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(gender ~ age_group) +
  scale_fill_brewer(palette="Dark2")
p


## ----echo=FALSE, fig.width=8, fig.height=3-------------------------------
ggplot(tb_au, aes(x = 1, y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position="fill") +
  facet_grid(gender ~ age_group) 


## ----echo=TRUE, fig.width=8, fig.height=3--------------------------------
rainbow <- ggplot(tb_au, aes(x = 1, y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position="fill") +
  facet_grid(gender ~ age_group) 
rainbow


## ----echo=FALSE, fig.width=8, fig.height=3-------------------------------
ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(gender ~ age_group) +
  scale_fill_brewer(palette="Dark2") + 
  coord_polar() 


## ----echo=TRUE, fig.width=8, fig.height=3--------------------------------
p <- ggplot(tb_au, aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(gender ~ age_group) +
  scale_fill_brewer(palette="Dark2") + 
  coord_polar() 
p


## ----echo=TRUE, fig.width=8, fig.height=3--------------------------------
rainbow +
  coord_polar(theta = "y")


## ----compare counts of ages, fig.height=12, echo=FALSE, eval=FALSE-------
## #ggplot(tb_fr, aes(x=year, y=count, fill=age)) +
## #  geom_bar(stat="identity") +
## #  facet_wrap(sex~age, ncol=2) +
## #  scale_fill_brewer("", palette="Dark2") +
## #  scale_x_continuous("year", breaks=seq(1995, 2012, 5), labels=c("95", "00", "05", "10"))


## ----eval=FALSE----------------------------------------------------------
## - Not what we want
## - Same information as previous
## - We'd like to directly compare male femal for each year, and age, side-by-side bar chart


## ----focus on one year gender, side-by-side bars of males/females, echo=TRUE, fig.height=3----
tb_au %>% filter(year == 2012) %>%
  ggplot(aes(x=gender, y=count, fill=gender)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~age_group, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement A")


## ----focus on one year age, side-by-side bars of age group, echo=TRUE, fig.height=3----
tb_au %>% filter(year == 2012) %>%
  ggplot(aes(x=age_group, y=count, fill=age_group)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~gender, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement B")


## ----eval=FALSE----------------------------------------------------------
## - Arrangement A makes it easier to directly compare male and female counts, separately for each age group. Generally, male counts are higher than female counts. There is a big difference between counts in the 45-54 age group, and over 65 counts are almost the same.
## - Arrangement B makes it easier to directly compare counts by age group, separately for females and males. For females, incidence drops in the middle years. For males, it is pretty consistently high across age groups.

