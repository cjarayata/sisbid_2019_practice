fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"


download.file(fileUrl,
              destfile="./data/cameras.csv",
              method="curl")
list.files("./data")


cameras <- read.csv(here::here("data/cameras.csv"))
install.packages("tidyverse")

library(readr)
setwd("data/ufo")
ufo <- read_csv("ufo_data_complete.csv", stringsAsFactors = F, na.strings = c("", " "))
ufo <- read_csv("ufo_data_complete.csv")

names(ufo) <- stringr::str_replace_all(names(ufo), " ", "_")

# Day 2

cars <- cars

cars <- mtcars

cars <- mutate(cars,
               disp_cat = ifelse(
                 disp <= 200, "Low",
                 ifelse(disp <= 400, "Medium", "High")
               ))

github_url <- "https://api.github.com/users/jtleek/repos"

install.packages("jsonlite")
library(jsonlite)

jsonData <- fromJSON(github_url)
date_read <- date()
date_read
dim(jsonData)
jsonData$name
names(jsonData$owner)

install.packages("rvest")
library(rvest)
htmlfile <- read_html("http://bowtie-bio.sourceforge.net/recount/")

nds <- html_nodes(htmlfile,                
                 xpath='//*[@id="recounttab"]/table')
dat <- html_table(nds)
dat <- as.data.frame(dat)
head(dat)
names(dat) <- dat[1, ]
dat <- dat[-1, ]

install.packages("httr")
library(httr)

req <- GET("https://api.github.com/search/repositories?q=created:2014-08-13+language:r+-user:cran&type")
req
names(content(req))

pacman::p_load(gapminder)

glimpse(gapminder)

# get random sample
set.seed(10)
sample_n(gapminder, 10)


gapminder %>% 
  group_by(continent) %>% 
  summarise(aveLife = mean(lifeExp),
            n = n())

# Create a variable that multiplies life expectancy by 1.2 for the Asian countries, and 1.3 for the African countries

gapminder <- gapminder %>% 
  mutate(newlifeExp = 
           case_when(continent == "Africa" ~ lifeExp*1.3,
                     continent == "Asia" ~ lifeExp*1.2,
                     TRUE ~ lifeExp))

# It's useful to do as much as you can do on the database beforehand so that pulling into R is as small as possible

install.packages("dbplyr")
install.packages("babynames")
install.packages("pryr")
install.packages("RSQLite")
library(dbplyr)
library(babynames)
library(pryr)
library(RSQLite)

str(babynames)
object_size(babynames)

library(dplyr)
my_db <- dplyr::src_sqlite("my_db.sqlite3", 
                    create = TRUE)
babys_sqlite <- dplyr::copy_to(my_db, 
                        babynames, temporary = FALSE)
dplyr::src_tbls(my_db)
tbl(my_db,"babynames")

newtbl = my_db %>% 
  tbl("babynames")%>% 
  filter(name=="Hilary") %>% 
  select(year,n,name)
newtbl # this is only the first 10 rows as a preview

newtbl %>% count() # now we know its 193 rows
# collect actually gets the query
output <- newtbl %>% collect()

popular <- babynames %>% 
  group_by(name) %>% 
  summarise(N = sum(n)) %>% 
  arrange(desc(N)) %>% 
  top_n(100)

# this doesn't work because top_n function is R-specific
pop2 <- my_db %>% 
  tbl("babynames")%>%
  group_by(name) %>% 
  summarise(N = sum(n, na.rm = T)) %>% 
  arrange(desc(N)) %>% 
  top_n(100) %>% 
  collect()

# this works, order of operations
pop3 <- my_db %>% 
  tbl("babynames")%>%
  group_by(name) %>% 
  summarise(N = sum(n, na.rm = T)) %>% 
  arrange(desc(N)) %>% 
  collect() %>% 
  top_n(100) # do the R-command last

# find the crosswalk
translate_sql(filter(name=="James"))
translate_sql(mean(x, na.rm = T))

how_female <- my_db %>% 
  tbl("babynames") %>% 
  group_by(name) %>% 
  summarize(m=mean(sex=="F", na.rm = T))

explain(how_female) # through whole commaand

