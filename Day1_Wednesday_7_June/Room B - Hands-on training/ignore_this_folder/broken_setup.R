library("tidyverse")
library("ggdag")
library("runjags")
library("knitr")
runjags.options(silent.jags=TRUE, silent.runjags=TRUE)

theme_set(theme_light())

set.seed(2021-06-22)

# Reduce the width of R code output for PDF only:
if(params$presentation) options(width=60)
knitr::opts_chunk$set(echo = TRUE, eval=TRUE)

# Reduce font size of R code output for Beamer:
if(params$presentation){
  knitr::knit_hooks$set(size = function(before, options, envir) {
    if(before){
      knitr::asis_output(paste0("\\", options$size))
    }else{
      knitr::asis_output("\\normalsize")
    }
  })
  knitr::opts_chunk$set(size = "scriptsize")
}

# Collapse successive chunks:
space_collapse <- function(x){ gsub("```\n*```r*\n*", "", x) }
# Reduce space between chunks:
space_reduce <- function(x){ gsub("```\n+```\n", "", x) }
knitr::knit_hooks$set(document = space_collapse)

# To collect temporary filenames:
cleanup <- character(0)

exercise_start <- function(){
	rv <- ""
	if(params$presentation & !exercise_current){
		if(params$latex) rv <- "\\begin{comment}" else rv <- "<!--"
		knitr::opts_chunk$set(eval = FALSE)
		exercise_current <<- TRUE
	}
	return(rv)
}
exercise_end <- function(){
	rv <- ""
	if(params$presentation & exercise_current){
		if(params$latex) rv <- "\\end{comment}" else rv <- "-->"
		knitr::opts_chunk$set(eval = TRUE)
		exercise_current <<- FALSE
	}
	return(rv)
}
exercise_current <- FALSE

solution_start <- function(){
	rv <- ""
	if(!params$solutions & !solution_current){
		if(params$latex) rv <- "\\begin{comment}" else rv <- "<!--"
		knitr::opts_chunk$set(eval = FALSE)
		solution_current <<- TRUE
	}
	return(rv)
}
solution_end <- function(){
	rv <- ""
	if(!params$solutions & solution_current){
		if(params$latex) rv <- "\\end{comment}" else rv <- "-->"
		knitr::opts_chunk$set(eval = TRUE)
		solution_current <<- FALSE
	}
	return(rv)
}
solution_current <- FALSE

