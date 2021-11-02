Assignment B1
================

This assignment covers making a function in R, documenting it, and
testing it. For more details on this assignment see
<https://stat545.stat.ubc.ca/assignments/assignment-b1/>.

We begin by loading the `tidyverse` package below:

``` r
suppressMessages(library(tidyverse))
```

## Exercise 1: Make a Function

One of the plots that I have found very useful when working on a new
dataset is the distribution of variables. Below, I wrote a function to
plot the distribution given the data and the variable’s name. The
function handles both categorical and numerical data. Also, I have tried
to provide as much information as possible within the output plot. For
example, I am indicating the mean of the numerical variable with a
vertical line.

``` r
quick_dist <- function(data, variable_name, na.rm = TRUE){
  
  if (!is.data.frame(data)){
    stop('The parameter data should be a data frame object.\n',
         'You have provided an object of class: ', class(data)[1])
  }
  
  if (!is.character(variable_name)){
    stop('The parameter variable_name requires a character input.\n',
         'You have provided an object of class: ', class(variable_name)[1])
  }
  
  if (!variable_name %in% colnames(data)){
    stop('The variable name parameter does 
         not exist in the provided data frame.\n')
  }
  variable_name <- as.name(variable_name)
  var <- data[[variable_name]]
  
  if (!is.numeric(var) &&
      !is.factor(var) &&
      !is.character(var)){
    stop("The column specified by the parameter variable_name
         requries a numeric, factor or character variable, \n",
         'You have provided an object of class: ', class(var)[1])
      }
  
  if (is.numeric(var)){
    
    mu <- mean(var, na.rm = na.rm)
    plt <- ggplot(data = data) +
      geom_density(aes(var), na.rm = na.rm, fill = 'dodgerblue', alpha = 0.5) +
      geom_vline(aes(xintercept=mu), linetype="dashed", color='red') +
      theme_bw() +
      xlab(variable_name)
  }
  
  else if (is.factor(var) || is.character(var)){
    
    plt <- data %>%
        filter(!is.na(!!variable_name)) %>%
        ggplot(aes(!!variable_name)) +
        geom_bar(na.rm = na.rm, fill='dodgerblue', width = 0.5) +
        theme_bw() +
        xlab(variable_name)
  }
  return(plt)
}
```

## Exercise 2: Document your Function

``` r
title 
```

    ## function (main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    ##     line = NA, outer = FALSE, ...) 
    ## {
    ##     main <- as.graphicsAnnot(main)
    ##     sub <- as.graphicsAnnot(sub)
    ##     xlab <- as.graphicsAnnot(xlab)
    ##     ylab <- as.graphicsAnnot(ylab)
    ##     .External.graphics(C_title, main, sub, xlab, ylab, line, 
    ##         outer, ...)
    ##     invisible()
    ## }
    ## <bytecode: 0x7fee32929708>
    ## <environment: namespace:graphics>

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

## Exercise 3: Include examples

yoo

## Exercise 4: Test the Function