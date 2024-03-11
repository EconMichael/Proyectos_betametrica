Se aprende haciendo
================
Econ. Michael Solórzano
2024-03-01

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- read.csv("C:\\Users\\bama1\\Desktop\\PROGRAMA CIENTIFICO DE DATOS\\Modulo 1\\BASES DE DATOS\\Iowa_Liquor_Sales_YG84CD.csv", stringsAsFactors = F, header = T)

datos <- data %>% 
  mutate(Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
         City=toupper(City),
         Store.Name=toupper(Store.Name),
         Date=as.Date(Date,format="%m/%d/%Y"),
         anio=lubridate::year(Date)) %>% 
  rename(ventas=Sale..Dollars.,
         ciudad=City,
         Categoria=Category.Name,
         nombre_tienda=Store.Name)
```

``` r
datos %>% 
  group_by(ciudad,nombre_tienda,anio) %>% 
  summarise(Promedio_ventas=mean(ventas)) %>% 
  filter(ciudad=="CEDAR RAPIDS" & anio==2016) %>% 
  arrange(-Promedio_ventas) %>% 
  filter(Promedio_ventas>161)
```

    ## `summarise()` has grouped output by 'ciudad', 'nombre_tienda'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   ciudad, nombre_tienda [5]
    ##   ciudad       nombre_tienda                       anio Promedio_ventas
    ##   <chr>        <chr>                              <dbl>           <dbl>
    ## 1 CEDAR RAPIDS SAM'S CLUB 8162 / CEDAR RAPIDS      2016            354.
    ## 2 CEDAR RAPIDS FAREWAY STORES #151 / CEDAR RAPIDS  2016            338.
    ## 3 CEDAR RAPIDS BENZ DISTRIBUTING                   2016            171.
    ## 4 CEDAR RAPIDS LEO1  /  CEDAR RAPIDS               2016            163.
    ## 5 CEDAR RAPIDS TARGET STORE T-1771 / CEDAR RAPIDS  2016            162.

``` r
#El top de las 5 tiendas que mas vendieron en la ciudad de CEDAR RAPIDS en el 2016 fueron: SAM'S CLUB 8162, FAREWAY STORES #151, BENZ DISTRIBUTING, LEO1 y TARGET STORE T-1771 
```

``` r
datos %>% 
  group_by(Vendor.Name,anio,ciudad) %>% 
  summarise(promedio_ventas=mean(ventas)) %>% 
  filter(anio==2016 & ciudad=="DAVENPORT") %>% 
  arrange(promedio_ventas) %>% 
  filter(promedio_ventas<43)
```

    ## `summarise()` has grouped output by 'Vendor.Name', 'anio'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   Vendor.Name, anio [5]
    ##   Vendor.Name                    anio ciudad    promedio_ventas
    ##   <chr>                         <dbl> <chr>               <dbl>
    ## 1 Luxco-St Louis                 2016 DAVENPORT            36.5
    ## 2 A HARDY USA LTD                2016 DAVENPORT            37.0
    ## 3 Rumcoqui and Co                2016 DAVENPORT            38.3
    ## 4 Prestige Wine & Spirits Group  2016 DAVENPORT            42.1
    ## 5 Dehner Distillery              2016 DAVENPORT            42.7

``` r
#Los vendedores que menos vendieron en la ciudad de DAVENPORT en el 2016 fueron: Luxco-St Louis, A HARDY USA LTD, Rumcoqui and Co, Prestige Wine & Spirits Group y Dehner Distillery
```

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.2

    ## Warning: package 'tidyr' was built under R version 4.3.2

    ## Warning: package 'readr' was built under R version 4.3.2

    ## Warning: package 'purrr' was built under R version 4.3.2

    ## Warning: package 'forcats' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
datos %>%
  group_by(Item.Description,anio,ciudad) %>% 
  summarise(mas_vendidos=max(ventas)) %>% 
  filter(anio==2016) %>%
  arrange(-mas_vendidos) %>% 
  pivot_wider(names_from = ciudad,
              values_from = mas_vendidos,
              values_fill = 0)
```

    ## `summarise()` has grouped output by 'Item.Description', 'anio'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 299 × 5
    ## # Groups:   Item.Description, anio [299]
    ##    Item.Description                       anio `CEDAR RAPIDS` DAVENPORT WATERLOO
    ##    <chr>                                 <dbl>          <dbl>     <dbl>    <dbl>
    ##  1 "Cedar Ridge Barrel Aged Rum 5YR Res…  2016          8250         0        0 
    ##  2 "Malibu Coconut Rum"                   2016          7555.     3778.    5666.
    ##  3 "Rumchata"                             2016          4412.     5882.    4500 
    ##  4 "Carolan's Irish Cream Liqueur"        2016           188.     4576.     148.
    ##  5 "Smirnoff Kissed Caramel"              2016          4160.      149.     149.
    ##  6 "Bailey's Original Irish Cream"        2016          3727.     3564     3564 
    ##  7 "New Amsterdam Peach"                  2016          1490.     3402     1863 
    ##  8 "Uv Blue (raspberry) Vodka"            2016          3375      2430     3375 
    ##  9 "Ryan's Cream Liqueur"                 2016          2463.      121.    3125.
    ## 10 "Rumchata \"GoChatas\""                2016           148.      148.    2970 
    ## # ℹ 289 more rows

``` r
#El top de ventas en el 2017 de los productos mas vendidos en las tres ciudades fueron: Cedar Ridge Barrel Aged Rum 5YR Reserve (este producto solo se vendio en CEDAR RAPIDS), Malibu Coconut Rum, Rumchata, Bailey's Original Irish Cream y Uv Blue (raspberry) Vodka


datos %>%
  group_by(Item.Description,anio,ciudad) %>% 
  summarise(mas_vendidos=max(ventas)) %>% 
  filter(anio==2017) %>%
  arrange(-mas_vendidos) %>% 
  pivot_wider(names_from = ciudad,
              values_from = mas_vendidos,
              values_fill = 0) 
```

    ## `summarise()` has grouped output by 'Item.Description', 'anio'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 87 × 5
    ## # Groups:   Item.Description, anio [87]
    ##    Item.Description                anio DAVENPORT WATERLOO `CEDAR RAPIDS`
    ##    <chr>                          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1 Godiva Liqueur                  2017      306      306            25.5
    ##  2 Godiva White Chocolate Liqueur  2017      306      306            25.5
    ##  3 Bailey's Original Irish Cream   2017      297      297           297  
    ##  4 Bailey's Chocolate Cherry       2017      225        0            20.2
    ##  5 Bailey's Salted Caramel         2017      225      225           225  
    ##  6 Bailey's Vanilla Cinnamon       2017      225        0           225  
    ##  7 Baileys Expresso Creme          2017      225        0           225  
    ##  8 Rumchata                        2017      225      225           225  
    ##  9 Tequila Rose Liqueur            2017      207      207           207  
    ## 10 Malibu Coconut Rum              2017      189.     189.          189. 
    ## # ℹ 77 more rows

``` r
#El top de ventas en el 2017 de los productos mas vendidos en las tres ciudades fueron: Godiva Liqueur, Godiva White Chocolate Liqueur, Bailey's Original Irish Cream, Bailey's Salted Caramel y Rumchata
```
