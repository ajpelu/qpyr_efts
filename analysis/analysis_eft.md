Read data
---------

``` r
library("plyr")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("lubridate")
```

    ## Warning: package 'lubridate' was built under R version 3.2.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

Read data
=========

``` r
# Read data
evi <- read.csv(file=paste(di, "/data/evi_attributes.csv", sep= ""), header = TRUE, sep = ',')
```

Classify the data according 4x4x4 (See Alcaraz et al. 2013): \* Primary production (*Annual EVI Mean*): ranging from A to D for low to high (increasing) \* Seasonality (*CV EVI*): ranging from a to d for high (decreasing) \* Phenological indicator (*Date of Maximun EVI*): 1–4: spring, summer, autumn and winter.

``` r
## For Date of MAX, we used a julian day approach (with lubridate)
# VI (1–4: spring, summer, autumn and winter).
# >81 <=173 --> 1
# >173 <=265 --> 2
# > 265 <=356 --> 3 
# > 356 and < 81 --> 4 
# Get julian day
sp <-lubridate::yday(as.Date("2000-03-21"))
su <- lubridate::yday(as.Date("2000-06-21"))
au <-  lubridate::yday(as.Date("2000-09-21"))
wi <- lubridate::yday(as.Date("2000-12-21"))
season_julian <- c(sp,su,au,wi)



qu <- evi %>% group_by(year) %>%
  mutate(q_evi_mean=ntile(evi_mean, 4),
         q_evi_cv=ntile(evi_cv, 4),
         n=n(), 
         jday=lubridate::yday(date_max)) %>%
  select(iv_malla_modi_id, year, q_evi_mean, q_evi_cv, n, date_max, jday) %>% 
  mutate(q_max = ifelse(jday > 81 & jday <= 173, 1,
                        ifelse(jday > 173 & jday <= 265, 2, 
                               ifelse(jday > 265 & jday <= 356, 3, 4))))  

# Add specific funtion to compute the mode
# http://stackoverflow.com/questions/28931435/calculate-mode-in-r
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


eft <- qu %>% 
  group_by(iv_malla_modi_id) %>% 
  summarise(mq_evi_mean = Mode(q_evi_mean),
         mq_evi_cv = Mode(q_evi_cv),
         mq_max = Mode(q_max))


# --- test
# Select data for pixel 142799
test <- qu[qu$iv_malla_modi_id == 142799, ]
# Select computation modes for pixel 142799
testeft <- eft[eft$iv_malla_modi_id==142799,]

# Mode Q max
test$q_max
```

    ##  [1] 1 2 1 3 2 1 1 1 1 4 1 3 2 3

``` r
Mode(test$q_max) == testeft$mq_max
```

    ## [1] TRUE

``` r
testeft$mq_max
```

    ## [1] 1

``` r
# Mode Q evi mean
test$q_evi_mean
```

    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
Mode(test$q_evi_mean) == testeft$mq_evi_mean
```

    ## [1] TRUE

``` r
testeft$mq_evi_mean
```

    ## [1] 2

``` r
# Mode Q evi cv
test$q_evi_cv
```

    ##  [1] 2 2 2 3 2 3 1 3 2 1 2 2 3 2

``` r
Mode(test$q_evi_cv) == testeft$mq_evi_cv
```

    ## [1] TRUE

``` r
testeft$mq_evi_cv
```

    ## [1] 2

``` r
# --- End test 


### Rename values
eft_final <- eft %>% 
  mutate(mq_evi_meanL = mapvalues(mq_evi_mean, 
                          from=c(1,2,3,4), to = c("A","B", "C", "D")),
         mq_evi_cvL = mapvalues(mq_evi_cv, 
                          from=c(1,2,3,4), to = c("a","b", "c", "d"))) %>%
  mutate(clu = paste(mq_evi_meanL,mq_evi_cvL,mq_max, sep=""))

# Export dataframe
write.csv(eft_final, file=paste(di, "/data/eft.csv", sep=""), row.names = FALSE)
```

Create raster

``` r
x <- read.csv(file=paste(di, "/data/raw_iv.csv", sep= ""), header = TRUE, sep = ',')

aux <- x %>% 
  group_by(iv_malla_modi_id) %>%
  dplyr::select(iv_malla_modi_id, lng, lat, poblacion) 

df <- as.data.frame(dplyr::inner_join(aux, eft_final, by="iv_malla_modi_id"))


library(sp)

dfsp <- SpatialPointsDataFrame(coords = df[,c("lat", "lng")],
                               data=df)



# VAS POR AQUI CANTINFLAS 
library(raster)
```

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select
