# mnschooldata

**[Documentation](https://almartin82.github.io/mnschooldata/)** \|
**[Getting
Started](https://almartin82.github.io/mnschooldata/articles/quickstart.html)**
\| **[Enrollment
Stories](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks.html)**

Fetch and analyze Minnesota school enrollment data from the Minnesota
Department of Education (MDE) in R or Python.

This package is part of the [state-schooldata
project](https://github.com/almartin82?tab=repositories&q=schooldata),
inspired by [njschooldata](https://github.com/almartin82/njschooldata) –
providing a simple, consistent interface for accessing state-published
school data across all 50 states.

## What can you find with mnschooldata?

**873,175 students. 389 districts. 68 charter schools.** Minnesota is
far more diverse than most people think – 41% students of color, major
Hmong and Somali communities, meatpacking towns that are
majority-Hispanic, and tribal schools that are 96% Native American. Here
are fifteen stories hiding in the numbers:

------------------------------------------------------------------------

### 1. Minnesota’s 873,000 students are 41% students of color

Minnesota enrolled 873,175 students in 2022-23 across 389 districts.
While still majority-white at 59%, the state’s schools are far more
diverse than many assume – 41% of students identify as Black, Hispanic,
Asian, Native American, multiracial, or Pacific Islander.

``` r
library(mnschooldata)
library(dplyr)
library(ggplot2)
library(scales)

data("enr_multi_example", package = "mnschooldata")
enr <- enr_multi_example

state_demo <- enr %>%
  filter(is_state, grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(state_demo) > 0)
state_demo
#> # A tibble: 8 x 3
#>   subgroup         n_students     pct
#>   <chr>                 <dbl>   <dbl>
#> 1 total_enrollment     873175 1      
#> 2 white                518783 0.594  
#> 3 black                110312 0.126  
#> 4 hispanic             105538 0.121  
#> 5 asian                 62538 0.0716 
#> 6 multiracial           59516 0.0682 
#> 7 native_american       15372 0.0176 
#> 8 pacific_islander       1116 0.00128
```

![State
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/state-demographics-chart-1.png)

State demographics

------------------------------------------------------------------------

### 2. St. Paul schools are 28% Asian – the Hmong capital of America

St. Paul has the largest Hmong community in the United States. In the
St. Paul Public School District, 9,022 students (27.5%) are Asian – more
than any other racial group except Black (24.1%) and White (22.4%). Two
charter schools, Hmong College Prep Academy (96% Asian, 2,456 students)
and Prairie Seeds Academy (88% Asian, 942 students), serve primarily
Hmong families.

``` r
stpaul <- enr %>%
  filter(is_district,
         district_name == "ST. PAUL PUBLIC SCHOOL DISTRICT",
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(stpaul) > 0)
stpaul
#> # A tibble: 8 x 3
#>   subgroup         n_students      pct
#>   <chr>                 <dbl>    <dbl>
#> 1 total_enrollment      32750 1       
#> 2 asian                  9022 0.275   
#> 3 black                  7886 0.241   
#> 4 white                  7348 0.224   
#> 5 hispanic               5077 0.155   
#> 6 multiracial            3155 0.0963  
#> 7 native_american         254 0.00776 
#> 8 pacific_islander          8 0.000244
```

![St. Paul
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/stpaul-demographics-chart-1.png)

St. Paul demographics

------------------------------------------------------------------------

### 3. St. Cloud is 43% Black – Somali refugees reshaped a rural city

St. Cloud, a city of 70,000 in central Minnesota, saw dramatic
demographic change as Somali refugees settled there starting in the late
1990s. Today 4,399 Black students make up 43% of the district’s
enrollment of 10,232, making it one of the most demographically
transformed districts in the country.

``` r
stcloud <- enr %>%
  filter(is_district,
         grepl("ST. CLOUD PUBLIC", district_name),
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(stcloud) > 0)
stcloud
#> # A tibble: 8 x 3
#>   subgroup         n_students     pct
#>   <chr>                 <dbl>   <dbl>
#> 1 total_enrollment      10232 1      
#> 2 black                  4399 0.430  
#> 3 white                  3462 0.338  
#> 4 hispanic               1248 0.122  
#> 5 multiracial             800 0.0782 
#> 6 asian                   245 0.0239 
#> 7 native_american          47 0.00459
#> 8 pacific_islander         31 0.00303
```

![St. Cloud
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/stcloud-demographics-chart-1.png)

St. Cloud demographics

------------------------------------------------------------------------

### 4. Anoka-Hennepin is Minnesota’s largest district at 38,336 students

Anoka-Hennepin School District, north of Minneapolis, serves more
students than any other Minnesota district. At 38,336 students, it is
larger than Minneapolis (30,079) and St. Paul (32,750). The suburban
district is 53.5% white, with growing Black (17.6%), Asian (11.6%), and
multiracial (8.9%) populations.

``` r
anoka <- enr %>%
  filter(is_district,
         grepl("ANOKA-HENNEPIN", district_name),
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(anoka) > 0)
anoka
#> # A tibble: 8 x 3
#>   subgroup         n_students      pct
#>   <chr>                 <dbl>    <dbl>
#> 1 total_enrollment      38336 1       
#> 2 white                 20520 0.535   
#> 3 black                  6737 0.176   
#> 4 asian                  4437 0.116   
#> 5 multiracial            3405 0.0888  
#> 6 hispanic               3005 0.0784  
#> 7 native_american         207 0.00540 
#> 8 pacific_islander         25 0.000652
```

![Anoka-Hennepin
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/anoka-chart-1.png)

Anoka-Hennepin demographics

------------------------------------------------------------------------

### 5. Worthington is 57% Hispanic – meatpacking built a new Minnesota

Worthington, a city of 14,000 in southwest Minnesota, is home to a JBS
pork processing plant that attracted immigrants from Latin America and
East Africa. Today 57% of the district’s 3,958 students are Hispanic,
making it one of the most Latino school districts in the Upper Midwest.

``` r
worthington <- enr %>%
  filter(is_district,
         grepl("WORTHINGTON", district_name),
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(worthington) > 0)
worthington
#> # A tibble: 8 x 3
#>   subgroup         n_students      pct
#>   <chr>                 <dbl>    <dbl>
#> 1 total_enrollment       3958 1       
#> 2 hispanic               2263 0.572   
#> 3 white                  1083 0.274   
#> 4 asian                   281 0.0710  
#> 5 black                   196 0.0495  
#> 6 multiracial             106 0.0268  
#> 7 pacific_islander         26 0.00657 
#> 8 native_american           3 0.000758
```

![Worthington
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/worthington-chart-1.png)

Worthington demographics

------------------------------------------------------------------------

### 6. Minneapolis is minority-majority: 64% students of color

Minneapolis Public Schools enrolled 30,079 students in 2022-23, with
white students making up just 36%. Black students are the largest group
at 27%, followed by Hispanic (22%), multiracial (8%), and Asian (3%).
The city’s schools reflect decades of immigration from Somalia, Latin
America, and Southeast Asia.

``` r
mpls <- enr %>%
  filter(is_district,
         district_name == "MINNEAPOLIS PUBLIC SCHOOL DISTRICT",
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(mpls) > 0)
mpls
#> # A tibble: 8 x 3
#>   subgroup         n_students      pct
#>   <chr>                 <dbl>    <dbl>
#> 1 total_enrollment      30079 1       
#> 2 white                 10935 0.364   
#> 3 black                  8056 0.268   
#> 4 hispanic               6723 0.224   
#> 5 multiracial            2326 0.0773  
#> 6 native_american        1080 0.0359  
#> 7 asian                   940 0.0313  
#> 8 pacific_islander         19 0.000632
```

![Minneapolis
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/mpls-demographics-chart-1.png)

Minneapolis demographics

------------------------------------------------------------------------

### 7. Red Lake is 96% Native American

Red Lake Public School District, on the Red Lake Band of Chippewa
reservation in northern Minnesota, enrolls 1,517 students – 1,455 of
whom (96%) are Native American. Nearby Cass Lake-Bena (94% Native
American) and Waubun-Ogema-White Earth (81%) serve similarly
concentrated Indigenous communities. Together, 15,372 Native American
students attend Minnesota schools statewide.

``` r
native <- enr %>%
  filter(is_district, subgroup == "native_american",
         grade_level == "TOTAL") %>%
  left_join(
    enr %>% filter(is_district, subgroup == "total_enrollment",
                    grade_level == "TOTAL") %>%
      select(district_name, total = n_students),
    by = "district_name"
  ) %>%
  filter(total > 100) %>%
  arrange(desc(pct)) %>%
  head(10) %>%
  select(district_name, n_students, pct, total)

stopifnot(nrow(native) > 0)
native
#> # A tibble: 10 x 4
#>    district_name                       n_students   pct total
#>    <chr>                                    <dbl> <dbl> <dbl>
#>  1 RED LAKE PUBLIC SCHOOL DISTRICT           1455 0.959  1517
#>  2 CASS LAKE-BENA PUBLIC SCHOOLS             1034 0.939  1101
#>  3 WAUBUN-OGEMA-WHITE EARTH SCHOOLS           575 0.805   714
#>  4 MAHNOMEN PUBLIC SCHOOL DISTRICT             468 0.718   652
#>  5 VOYAGEURS EXPEDITIONARY                      67 0.583   115
#>  6 BROWNS VALLEY PUBLIC SCHOOL DIST            116 0.566   205
#>  7 TREKNORTH HIGH SCHOOL                       121 0.482   251
#>  8 KELLIHER PUBLIC SCHOOL DISTRICT             145 0.468   310
#>  9 ONAMIA PUBLIC SCHOOL DISTRICT               227 0.448   507
#> 10 NORTHLAND COMMUNITY SCHOOLS                 118 0.375   315
```

![Native American
concentrations](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/native-chart-1.png)

Native American concentrations

------------------------------------------------------------------------

### 8. 68 charter schools serve 37,782 students

Minnesota passed the nation’s first charter school law in 1991. Today 68
charter school districts serve 37,782 students (4.3% of the state
total). The largest is Minnesota Transitions Charter School (6,425
students), followed by Hmong College Prep Academy (2,456). Many charters
serve specific immigrant communities.

``` r
charter <- enr %>%
  filter(is_charter, subgroup == "total_enrollment",
         grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  select(district_name, n_students) %>%
  head(15)

stopifnot(nrow(charter) > 0)
charter
#> # A tibble: 15 x 2
#>    district_name                          n_students
#>    <chr>                                       <dbl>
#>  1 MINNESOTA TRANSITIONS CHARTER SCH            6425
#>  2 HMONG COLLEGE PREP ACADEMY                   2456
#>  3 HIAWATHA ACADEMIES                           1683
#>  4 METRO SCHOOLS CHARTER                        1643
#>  5 EAGLE RIDGE ACADEMY CHARTER SCHOOL           1538
#>  6 PACT CHARTER SCHOOL                          1457
#>  7 PARNASSUS PREPARATORY CHARTER SCH            1362
#>  8 LAKES INTERNATIONAL LANGUAGE ACADEM          1315
#>  9 ST. CROIX PREPARATORY ACADEMY                1229
#> 10 HIGHER GROUND ACADEMY                        1177
#> 11 NOVA CLASSICAL ACADEMY                       1032
#> 12 SPECTRUM HIGH SCHOOL                          982
#> 13 PRAIRIE SEEDS ACADEMY                         942
#> 14 GREAT RIVER SCHOOL                            811
#> 15 COMMUNITY OF PEACE ACADEMY                    803
```

![Largest charter
schools](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/charter-chart-1.png)

Largest charter schools

------------------------------------------------------------------------

### 9. The suburban ring enrolls 211,000+ students

Thirteen suburban ring districts surrounding Minneapolis-St. Paul
collectively enroll more than 211,000 students – nearly a quarter of the
state total. Anoka-Hennepin (38,336), Rosemount-Apple Valley-Eagan
(29,229), and Osseo (21,385) are the biggest, as families continue
moving from the urban core to the suburbs.

``` r
suburban_names <- c(
  "ANOKA-HENNEPIN SCHOOL DISTRICT",
  "ROSEMOUNT-APPLE VALLEY-EAGAN",
  "OSSEO PUBLIC SCHOOL DISTRICT",
  "SOUTH WASHINGTON COUNTY SCHOOLS",
  "ELK RIVER SCHOOL DISTRICT",
  "WAYZATA PUBLIC SCHOOL DISTRICT",
  "LAKEVILLE PUBLIC SCHOOL DISTRICT",
  "MOUNDS VIEW PUBLIC SCHOOL DISTRICT",
  "NORTH ST. PAUL-MAPLEWOOD OAKDALE",
  "ROBBINSDALE PUBLIC SCHOOL DISTRICT",
  "BLOOMINGTON PUBLIC SCHOOL DISTRICT",
  "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT",
  "EASTERN CARVER COUNTY PUBLIC SCHOOL"
)

suburban <- enr %>%
  filter(is_district, district_name %in% suburban_names,
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  select(district_name, n_students)

stopifnot(nrow(suburban) > 0)
suburban
#> # A tibble: 13 x 2
#>    district_name                       n_students
#>    <chr>                                    <dbl>
#>  1 ANOKA-HENNEPIN SCHOOL DISTRICT           38336
#>  2 ROSEMOUNT-APPLE VALLEY-EAGAN             29229
#>  3 OSSEO PUBLIC SCHOOL DISTRICT             21385
#>  4 SOUTH WASHINGTON COUNTY SCHOOLS          19705
#>  5 ELK RIVER SCHOOL DISTRICT                14785
#>  6 WAYZATA PUBLIC SCHOOL DISTRICT           13217
#>  7 LAKEVILLE PUBLIC SCHOOL DISTRICT         12105
#>  8 MOUNDS VIEW PUBLIC SCHOOL DISTRICT       11935
#>  9 NORTH ST. PAUL-MAPLEWOOD OAKDALE         10666
#> 10 ROBBINSDALE PUBLIC SCHOOL DISTRICT       10326
#> 11 BLOOMINGTON PUBLIC SCHOOL DISTRICT       10317
#> 12 EDEN PRAIRIE PUBLIC SCHOOL DISTRICT       9681
#> 13 EASTERN CARVER COUNTY PUBLIC SCHOOL       9195
```

![Suburban ring
districts](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/suburban-chart-1.png)

Suburban ring districts

------------------------------------------------------------------------

### 10. Rochester’s 17,320 students reflect Mayo Clinic’s global draw

Rochester Public Schools, anchored by the Mayo Clinic, enrolled 17,320
students in 2022-23. The district is remarkably diverse for an outstate
city: 51% white, 17% Black, 13% Hispanic, 9% Asian, and 9% multiracial.
Mayo Clinic’s global workforce brings families from around the world to
southeast Minnesota.

``` r
rochester <- enr %>%
  filter(is_district,
         district_name == "ROCHESTER PUBLIC SCHOOL DISTRICT",
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(rochester) > 0)
rochester
#> # A tibble: 8 x 3
#>   subgroup         n_students     pct
#>   <chr>                 <dbl>   <dbl>
#> 1 total_enrollment      17320 1      
#> 2 white                  8821 0.509  
#> 3 black                  2981 0.172  
#> 4 hispanic               2299 0.133  
#> 5 multiracial            1644 0.0949 
#> 6 asian                  1486 0.0858 
#> 7 native_american          73 0.00421
#> 8 pacific_islander         16 0.000924
```

![Rochester
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/rochester-chart-1.png)

Rochester demographics

------------------------------------------------------------------------

### 11. Duluth: 75% white, but diversifying in the Northland

Duluth Public Schools enrolled 8,807 students in 2022-23 in Minnesota’s
third-largest city. At 75% white, it is the least diverse of the state’s
large districts, but multiracial students (10.6%) are now the
second-largest group, reflecting changing demographics in northern
Minnesota.

``` r
duluth <- enr %>%
  filter(is_district,
         grepl("DULUTH PUBLIC", district_name),
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(duluth) > 0)
duluth
#> # A tibble: 8 x 3
#>   subgroup         n_students      pct
#>   <chr>                 <dbl>    <dbl>
#> 1 total_enrollment       8807 1       
#> 2 white                  6646 0.755   
#> 3 multiracial             933 0.106   
#> 4 black                   419 0.0476  
#> 5 hispanic                386 0.0438  
#> 6 native_american         338 0.0384  
#> 7 asian                    77 0.00874 
#> 8 pacific_islander          8 0.000908
```

![Duluth
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/duluth-chart-1.png)

Duluth demographics

------------------------------------------------------------------------

### 12. Columbia Heights: The most diverse district in Minnesota

Columbia Heights Public Schools, a small inner-ring suburb of
Minneapolis, enrolled 3,449 students with the most evenly distributed
demographics of any district in the state: 51% Hispanic, 13% white, 13%
multiracial, 12% Black, and 10% Asian. No single group dominates.

``` r
colhts <- enr %>%
  filter(is_district,
         grepl("COLUMBIA HEIGHTS", district_name),
         grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))

stopifnot(nrow(colhts) > 0)
colhts
#> # A tibble: 8 x 3
#>   subgroup         n_students      pct
#>   <chr>                 <dbl>    <dbl>
#> 1 total_enrollment       3449 1       
#> 2 hispanic               1774 0.514   
#> 3 multiracial             448 0.130   
#> 4 white                   448 0.130   
#> 5 black                   421 0.122   
#> 6 asian                   336 0.0974  
#> 7 native_american          18 0.00522 
#> 8 pacific_islander          4 0.00116 
```

![Columbia Heights
demographics](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/columbia-heights-chart-1.png)

Columbia Heights demographics

------------------------------------------------------------------------

### 13. 55 districts have fewer than 250 students

Rural Minnesota continues to consolidate. Fifty-five districts enroll
fewer than 250 students, while 16 districts have over 10,000. The 16
largest districts serve 294,000 students (34% of the state), while the
55 smallest collectively serve just 8,052.

``` r
size_dist <- enr %>%
  filter(is_district, subgroup == "total_enrollment",
         grade_level == "TOTAL") %>%
  mutate(size_cat = case_when(
    n_students < 250 ~ "Under 250",
    n_students < 500 ~ "250-499",
    n_students < 1000 ~ "500-999",
    n_students < 5000 ~ "1,000-4,999",
    n_students < 10000 ~ "5,000-9,999",
    TRUE ~ "10,000+"
  )) %>%
  group_by(size_cat) %>%
  summarize(
    n_districts = n(),
    total_students = sum(n_students),
    .groups = "drop"
  )

stopifnot(nrow(size_dist) > 0)
size_dist
#> # A tibble: 6 x 3
#>   size_cat    n_districts total_students
#>   <chr>             <int>          <dbl>
#> 1 1,000-4,999         119         253651
#> 2 10,000+              16         293943
#> 3 250-499              66          24322
#> 4 5,000-9,999          24         172196
#> 5 500-999             109          78015
#> 6 Under 250            55           8052
```

![District size
distribution](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/size-distribution-chart-1.png)

District size distribution

------------------------------------------------------------------------

### 14. The top 10 most diverse large districts

Among districts with over 3,000 students, the least-white are Columbia
Heights (13% white), Richfield (22%), Burnsville (35%), St. Paul (22%),
and Brooklyn Center (20%). Several of these inner-ring suburbs
diversified rapidly as immigrant and refugee families moved out of
Minneapolis and St. Paul.

``` r
diverse_large <- enr %>%
  filter(is_district, subgroup == "white", grade_level == "TOTAL") %>%
  left_join(
    enr %>% filter(is_district, subgroup == "total_enrollment",
                    grade_level == "TOTAL") %>%
      select(district_name, total = n_students),
    by = "district_name"
  ) %>%
  filter(total > 3000, !is_charter) %>%
  arrange(pct) %>%
  head(10) %>%
  select(district_name, n_students, pct, total)

stopifnot(nrow(diverse_large) > 0)
diverse_large
#> # A tibble: 10 x 4
#>    district_name                       n_students    pct total
#>    <chr>                                    <dbl>  <dbl> <dbl>
#>  1 COLUMBIA HEIGHTS PUBLIC SCHOOL DIST        448 0.130   3449
#>  2 BROOKLYN CENTER SCHOOL DISTRICT            424 0.204   2074  
#>  3 ST. PAUL PUBLIC SCHOOL DISTRICT           7348 0.224  32750
#>  4 RICHFIELD PUBLIC SCHOOL DISTRICT           876 0.220   3984
#>  5 BURNSVILLE PUBLIC SCHOOL DISTRICT         2644 0.351   7536
#>  6 MINNEAPOLIS PUBLIC SCHOOL DISTRICT       10935 0.364  30079
#>  7 ST. CLOUD PUBLIC SCHOOL DISTRICT          3462 0.338  10232
#>  8 ROBBINSDALE PUBLIC SCHOOL DISTRICT        4429 0.429  10326
#>  9 WILLMAR PUBLIC SCHOOL DISTRICT            1297 0.317   4091
#> 10 FARIBAULT PUBLIC SCHOOL DISTRICT          1297 0.417   3111
```

![Most diverse large
districts](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/diverse-large-chart-1.png)

Most diverse large districts

------------------------------------------------------------------------

### 15. Outstate districts are still 85%+ white

While the Twin Cities metro diversified rapidly, outstate Minnesota
districts remain overwhelmingly white. Among districts with 3,000+
students, New Prague (92%), Chisago Lakes (90%), Princeton (89%), and
Alexandria (88%) have barely changed, creating a stark geographic divide
in who Minnesota students learn alongside.

``` r
whitest <- enr %>%
  filter(is_district, subgroup == "white", grade_level == "TOTAL") %>%
  left_join(
    enr %>% filter(is_district, subgroup == "total_enrollment",
                    grade_level == "TOTAL") %>%
      select(district_name, total = n_students),
    by = "district_name"
  ) %>%
  filter(total > 3000, !is_charter) %>%
  arrange(desc(pct)) %>%
  head(10) %>%
  select(district_name, n_students, pct, total)

stopifnot(nrow(whitest) > 0)
whitest
#> # A tibble: 10 x 4
#>    district_name                        n_students   pct total
#>    <chr>                                     <dbl> <dbl> <dbl>
#>  1 NEW PRAGUE AREA SCHOOLS                    3673 0.917  4007
#>  2 CHISAGO LAKES SCHOOL DISTRICT              2929 0.903  3245
#>  3 PRINCETON PUBLIC SCHOOL DISTRICT           3009 0.886  3396
#>  4 ALEXANDRIA PUBLIC SCHOOL DISTRICT          3474 0.879  3951
#>  5 WACONIA PUBLIC SCHOOL DISTRICT             3429 0.875  3917
#>  6 BRAINERD PUBLIC SCHOOL DISTRICT            5163 0.873  5915
#>  7 ORONO PUBLIC SCHOOL DISTRICT               2606 0.867  3005
#>  8 BUFFALO-HANOVER-MONTROSE PUBLIC SCH        4381 0.856  5119
#>  9 GRAND RAPIDS PUBLIC SCHOOL DISTRICT        3281 0.848  3870
#> 10 SARTELL-ST. STEPHEN SCHOOL DISTRICT        3248 0.833  3901
```

![Whitest large
districts](https://almartin82.github.io/mnschooldata/articles/enrollment_hooks_files/figure-html/whitest-large-chart-1.png)

Whitest large districts

------------------------------------------------------------------------

## Installation

``` r
# install.packages("remotes")
remotes::install_github("almartin82/mnschooldata")
```

## Quick start

### R

``` r
library(mnschooldata)
library(dplyr)

# Fetch one year (requires network access to MDE)
enr_2023 <- fetch_enr(2023, use_cache = TRUE)

# State totals
enr_2023 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# Largest districts
enr_2023 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(15)

# Minneapolis demographics
enr_2023 %>%
  filter(grepl("MINNEAPOLIS", district_name), grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students, pct)
```

### Python

``` python
import pymnschooldata as mn

# Check available years
years = mn.get_available_years()
print(f"Data available from {years['min_year']} to {years['max_year']}")

# Fetch one year
enr_2023 = mn.fetch_enr(2023)

# State totals
state_total = enr_2023[
    (enr_2023['is_state'] == True) &
    (enr_2023['subgroup'] == 'total_enrollment') &
    (enr_2023['grade_level'] == 'TOTAL')
]

# Largest districts
districts = enr_2023[
    (enr_2023['is_district'] == True) &
    (enr_2023['subgroup'] == 'total_enrollment') &
    (enr_2023['grade_level'] == 'TOTAL')
].sort_values('n_students', ascending=False).head(15)
```

## Data Notes

### Source

Data is sourced from the Minnesota Department of Education (MDE): -
MDEAnalytics Portal:
<https://pub.education.mn.gov/MDEAnalytics/Data.jsp> - MN Report Card:
<https://rc.education.mn.gov/>

### Available Years

| Years         | Source    | Notes                                        |
|---------------|-----------|----------------------------------------------|
| **2007-2024** | MDE MARSS | Requires successful download from MDE portal |

**Note:** MDE uses a WebFOCUS (ibi_apps) portal that does not provide
stable direct-download URLs. Data availability depends on portal
accessibility. The bundled example data contains verified 2022-23
enrollment from a successful download.

### Census Day

All enrollment data represents **October 1 official counts** (Census
Day), reported via MARSS (Minnesota Automated Reporting Student System).

### Suppression Rules

- Small cell sizes (typically \<10 students) may be suppressed for
  privacy
- Suppressed values appear as `NA` in the data

### What’s Included

- **Levels:** State, District (~389)
- **Demographics:** White, Black, Hispanic, Asian, Native American,
  Pacific Islander, Multiracial
- **District types:** Independent (01), Special (03 – Minneapolis, South
  St. Paul), Charter (07)
- **Grade levels:** TOTAL (grade-level breakdowns available when MDE
  portal provides them)

### Minnesota-Specific Notes

- **District Types:**
  - 01: Independent School Districts (300 districts)
  - 03: Special School Districts (Minneapolis, South St. Paul)
  - 07: Charter Schools (68 charters)
- **MARSS:** Minnesota Automated Reporting Student System
- **Hmong community:** St. Paul has the largest Hmong population in the
  US (27.5% of St. Paul Public Schools)
- **Somali community:** Significant populations in Minneapolis,
  St. Cloud, and Faribault

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data
in Python and R.

**All 50 state packages:**
[github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (<almartin@gmail.com>)

## License

MIT
