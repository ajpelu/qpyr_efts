Read data
---------

``` r
library("lubridate") 
```

    ## Warning: package 'lubridate' was built under R version 3.2.3

``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Read and prepare data
=====================

``` r
# Read data
rawdata <- read.csv(file=paste(di, "/data/raw_iv.csv", sep= ""), header = TRUE, sep = ',')

# Create variables of year and month 
# Apply scale factor https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1 
rawdata <- rawdata %>% 
  mutate(myevi = evi * 0.0001,
         year = lubridate::year(fecha),
         month = lubridate::month(fecha))



#------------------------ test
# Pixels with negative evi values?
# Pixels with positive evi values?
rawdata %>% 
  summarise(pixel_pos=sum(myevi >= 0),
            pixel_neg=sum(myevi < 0),
            pixels=n())
```

    ##   pixel_pos pixel_neg pixels
    ## 1    551689      3395 555084

``` r
#------------------------ End test


### Get by pixel and year the EVI
eviyear <- rawdata %>% 
  group_by(iv_malla_modi_id, year) %>%
  summarise(evi_min=min(myevi),
            evi_max=max(myevi), 
            evi_mean = sum(myevi[myevi >=0]),
            evi_mean_all = sum(myevi), 
            evi_cv = ((evi_max - evi_min) / evi_mean ))
```

`TODO` Duda domingo ??? (minimo)
--------------------------------

``` r
#------------------------ test
# Test by pixel e.g. 142799 
test <- rawdata[rawdata$iv_malla_modi_id == 142799 & rawdata$year ==2001,]

dply_test <- eviyear %>% 
  filter(iv_malla_modi_id == 142799, year ==2001) %>%
  select(evi_min, evi_mean, evi_max)

# Is the pixel the same?? 
unique(test$iv_malla_modi_id) == dply_test$iv_malla_modi_id
```

    ## [1] TRUE

``` r
# Are the minimun, maximun and mean values well computed?
min(test$myevi) == dply_test$evi_min
```

    ## [1] TRUE

``` r
max(test$myevi) == dply_test$evi_max
```

    ## [1] TRUE

``` r
sum(test$myevi) == dply_test$evi_mean
```

    ## [1] TRUE

``` r
#------------------------ End test
```

``` r
mes_max <- rawdata %>% 
  select(iv_malla_modi_id, year, myevi, fecha, month) %>% 
  group_by(iv_malla_modi_id, year) %>%
  slice(which.max(myevi)) %>%
  ungroup %>%
  mutate(date_max = fecha,
         month_max = month) %>%
  select(iv_malla_modi_id, year, date_max, month_max)


# Joins dataframes
evi_attributes <- dplyr::inner_join(x=eviyear, y=mes_max, by=c("iv_malla_modi_id", "year"))

# Export dataframe
write.csv(evi_attributes, file=paste(di, "/data/evi_attributes.csv", sep=""), row.names = FALSE)
```