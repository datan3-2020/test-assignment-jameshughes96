Test statistical assignment
================
James Hughes
22 January 2020

## Introduction

Please change the author and date fields above as appropriate. Do not
change the output format. Once you have completed the assignment you
want to knit your document into a markdown document in the
“github\_document” format and then commit both the .Rmd and .md files
(and all the associated files with graphs) to your private assignment
repository on Github.

## Reading data (40 points)

First, we need to read the data into R. For this assignment, I ask you
to use data from the youth self-completion questionnaire (completed by
children between 10 and 15 years old) from Wave 9 of the Understanding
Society. It is one of the files you have downloaded as part of SN6614
from the UK Data Service. To help you find and understand this file you
will need the following documents:

1)  The Understanding Society Waves 1-9 User Guide:
    <https://www.understandingsociety.ac.uk/sites/default/files/downloads/documentation/mainstage/user-guides/mainstage-user-guide.pdf>
2)  The youth self-completion questionnaire from Wave 9:
    <https://www.understandingsociety.ac.uk/sites/default/files/downloads/documentation/mainstage/questionnaire/wave-9/w9-gb-youth-self-completion-questionnaire.pdf>
3)  The codebook for the file:
    <https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/youth/wave/9>

<!-- end list -->

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------- tidyverse 1.3.0 --

    ## <U+2713> ggplot2 3.2.1     <U+2713> purrr   0.3.3
    ## <U+2713> tibble  2.1.3     <U+2713> dplyr   0.8.3
    ## <U+2713> tidyr   1.0.0     <U+2713> stringr 1.4.0
    ## <U+2713> readr   1.3.1     <U+2713> forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# This attaches the tidyverse package. If you get an error here you need to install the package first. 

Data <- read_tsv("C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w9/i_youth.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
# You need to add between the quotation marks a full path to the required file on your computer.
```

## Tabulate variables (10 points)

In the survey children were asked the following question: “Do you have a
social media profile or account on any sites or apps?”. In this
assignment we want to explore how the probability of having an account
on social media depends on children’s age and gender.

Tabulate three variables: children’s gender, age (please use derived
variables) and having an account on social media.

``` r
table(Data$i_sex_dv)
```

    ## 
    ##    0    1    2 
    ##    2 1411 1408

``` r
table(Data$i_age_dv)
```

    ## 
    ##   9  10  11  12  13  14  15  16 
    ##   1 460 496 467 463 491 434   9

``` r
table(Data$i_ypsocweb)
```

    ## 
    ##   -9    1    2 
    ##   14 2277  530

## Recode variables (10 points)

We want to create a new binary variable for having an account on social
media so that 1 means “yes”, 0 means “no”, and all missing values are
coded as NA. We also want to recode gender into a new variable with the
values “male” and “female” (this can be a character vector or a factor).

``` r
NewData <- mutate(Data,
                  soc_med = i_ypsocweb %>% 
                    recode(`1` = 1,
                           `2` = 0,
                           `-9` = NA_real_)
                  %>% 
                    as.factor(),
                  
                  gender = i_sex_dv %>% 
                    recode(`1` = "Male",
                           `2` = "Female", 
                           `0` = NA_character_)
                  %>% 
                    as.factor()
                  
)

summary(NewData$soc_med)
```

    ##    0    1 NA's 
    ##  530 2277   14

``` r
summary(NewData$gender)
```

    ## Female   Male   NA's 
    ##   1408   1411      2

## Calculate means (10 points)

Produce code that calculates probabilities of having an account on
social media (i.e. the mean of your new binary variable produced in the
previous problem) by age and gender.

``` r
prop.table(table(NewData$gender, NewData$soc_med), 1)
```

    ##         
    ##                  0         1
    ##   Female 0.1604850 0.8395150
    ##   Male   0.2173913 0.7826087

``` r
prop.table(table(NewData$soc_med, as.factor(NewData$i_age_dv)), 2)
```

    ##    
    ##              9         10         11         12         13         14
    ##   0 1.00000000 0.51310044 0.30101010 0.13449024 0.08478261 0.05316973
    ##   1 0.00000000 0.48689956 0.69898990 0.86550976 0.91521739 0.94683027
    ##    
    ##             15         16
    ##   0 0.04147465 0.00000000
    ##   1 0.95852535 1.00000000

## Write short interpretation (10 points)

The results indicate that a larger proportion of young females (84%)
have social media accounts than young males (78%). The results indicate
that as age increases (from 9-6) the proportion of youths with social
media accounts also increases. 0% of 9 year olds reported to having
social media accounts, in comparison to 100% of 16 year olds.

## Visualise results (20 points)

Create a statistical graph (only one, but it can be faceted)
illustrating your results (i.e. showing how the probability of having an
account on social media changes with age and gender). Which type of
statistical graph would be most appropriate for this?

``` r
plotdata1 <- prop.table(table(NewData$gender, NewData$i_age_dv), 2)

plotdata1 <- as.data.frame(plotdata1)

plot1 <- ggplot(data=plotdata1, aes(x=Var2, y=Freq))

plot1 <- plot1 + geom_bar(stat="identity") + facet_wrap( ~ Var1) 

plot1 <- plot1 + theme_bw() + labs(title = "The effects of age and gender on social media use",
                          x = "Age",
                          y = "Proportion")

plot(plot1)
```

![](testAssignment_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Conclusion

This is a test formative assignment and the mark will not count towards
your final mark. If you cannot answer any of the questions above this is
fine – we are just starting this module\! However, please do submit this
assignment in any case to make sure that you understand the procedure,
that it works correctly and you do not have any problems with summative
assignments later.
