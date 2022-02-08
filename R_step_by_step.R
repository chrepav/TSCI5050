#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);
library(ggplot2); # plotting
library(dplyr); # data processing
library(GGally);
library(rio); # simple command for importing and exporting data
library(pander); # formats tables
library(printr); 
options(max.print=42); # sets the limit no how many lines of results are printed on console
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};
# R basic syntax ----
#'
#' # R basic syntax
#'
#' ## Assignment, Variables, and Data types.
#'
#' To store a value as variable `foo` in R the convention is to use the
#' `<-` operator, not `=`. This makes it easier to distinguish stand-alone
#' expressions from function arguments.

#+ assignment_operator
foo <- 500;
bar <- foo <- 500;
#' It's not a formal rule, it's rarely even written down, but `foo`, `baz`,
#' `bat`, etc. are throw-away variables people use for testing. If you need more
#' test variables, just make up three letter ones that start with `b`.
#' If you see one of those in a script you're reviewing it means it is left over
#' from when that code was being debugged. Example code shared with this class
#' will usually use `foo` and friends to represent the parts of an expression
#' that should be replaced with whatever values you are using instead of being
#' used literally. Shorter than having to write `YOURFUNCTIONHERE` or
#' `YOURARGUMENTHERE` each time.
#'
#' This is not specific to R-- it's just a little quirk of programming culture
#' in general. A quirk with a practical consequence: _never use `foo`, `bar`,
#' `baz`, `bat`, etc. in your production (i.e. finalized) code_ because
#' otherwise you or somebody else debugging your code will attempt to use those
#' names as test variables and in some situations this could overwrite the
#' existing variables!
#'
#'
#' ## Simple data types
#'
#' Numeric. You can do arithmetic on them: `+`, `-`, `/`, `*`, `^`, `log()`,
#' `exp()`, `sqrt()`

#+ assignment_numeric
foo <- 2 + 2; foo;
foo <- log(foo)
bar <- 5*5; bar;
bar <- exp(bar)
whatisthis(foo);
log(((3^2)*(2+3)/4)-1)
print(foo <- 42)

#' Character strings. Create these by wrapping single (`'`) or double (`"`)
#' quotes around the value.

#+ assignment_string # name variables or levels
"Do Not Panic"
'Do Not Panic'
"Don't Panic"
'The "Heart of Gold" comes equipped with heated leather seats and an infinite improbability drive'
'42'
#' Logical values are `TRUE` or `FALSE`. Can also be created using `>`, `<`,
#' `==`, `!=`, `>=`, `<=`

#+ assignment_logical 
## for data wrangling e.g. if treatment success (0,1) 
#if variable >0== "successful"
5<4
5==4
5!=5
5<=6

#' Missing values are represented by `NA` (no quotes for any of these). Null
#' values are _not_ the same as missing and they are represented by `NULL`. In
#' some circumstances you might also run into `Inf`, `-Inf`, and `NaN`. These
#' often indicate errors somewhere else in your code.

#' Dates and times. Can be created with the `Sys.Date()` or `Sys.time()`
#' functions or converted from a character string using `as.Date()`.

#+ assignment_datetime # use for time variables depending on date 
#+e.g calculate follow-up time starting on date x
Sys.Date()
Sys.time()
?as.Date
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "%m/%d/%y")
schedule <- as.Date(dates, "%m/%d/%y")
as.Date(schedule)
as.Date(schedule, "%m/%d/%y") #with comma separate arguments in the same command
#' Factors are basically integers that have labels. They are a human-readable
#' alternative to using integer codes for discrete data. These will make more
#' sense after we talk about vectors in the next section.

#+ factor_example

#+ assignment_wierd

#' ## Data structures
#'
#' Of course we usually need to work with variables bundled together, not single
#' values.
#'
#' ### Vectors
#'
#' The default data structure in R is a `vector`. You create one with the `c()`
#' command with any number of arguments. All items in a vector have to be the
#' same type.

#+ vectors_c  
#'function is the command before the (), argument is in ()
foo <- c(100,500, 4, 10)
foofoo <- c(5,11,14,20)
#' Since the default data structure in R is a `vector`, if you
#' create some sort of simple value you are creating a `vector` even if you are
#' not using `c()`... it just happens to be a `vector` of length 1. These
#' are identical, and both return `1` when used with the `length()` function.

#+ vectors_length1
length(foo)
#' If you want to create a sequence of consecutive integers, you can use the `:`
#' operator.

#+ vectors_sequence
25:100
#' In most other languages, you need to use a `for` loop in order to perform
#' some sort of change to a series of values. In R, you often don't have to
#' when you are working with vectors because a lot of functions (including all
#' the arithmetic and logical ones above) and be applied to a vector and they
#' work. If the function involves multiple vectors (e.g. `+`), usually you'll
#' want all of them to be either the same length or length 1.

#+ vectors_operators; #these work will all arithmetic operators
foo+6
foo+foofoo
foo/foofoo
foo>=100
foofoo<=10
#' You can assign names to some or all members of a vector when you create it.
#' You can also assign or re-assign names later using the `names()` function.

#+ vectors_names1
age <- c(a="young", b="mid", c="old")
print(age <- c(a="young", b="mid", c="old"))
age["b"]
age[c("b", "c")]
#' You can also use it to see the currently assigned names.

#+ vectors_names2
names(age)
names(age) <- c("kids", "adolescents", "adults") #rename elements of the vector
names(age)[3] <-"old peeps" #rename only one element
age
#' You can subset a vector by using `[...]` with the `...` replaced by _another_
#' vector, of integers indicating which positions you want to extract. Or you
#' could use a vector of names.

#+ vectors_subset1
foo[100]
#' If you just need a single value, use a single name or number.

#+ vectors_subset2

#' If you need a series of adjacent values, use `:`. If you need several
#' adjacent series with interruptions between them, use `c()` with `:`
#' expressions separated by commas `,`.

#+ vectors_subset3
foo[c(1,2,3)]
foo[1:3]
foo[c(1:2,3:4)]
#' Other useful functions for exploring vectors: `length()`, `summary()`,
#' `table()`, `head()`, `tail()`, `sum()`, `diff()`, `seq_along()`.

#+ vectors_explore
summary(foo) #summary stats for numeric vector
summary(age)
table(age) #frequency table
bat <- sample(1:10, 30, replace = TRUE)*1000
table(bat)
bat
head(bat) #first 6 elements of vector
tail(bat) #last 6 elements of vector
diff(bat) #dif between two consecutive values
sum(bat) #sum of all values
seq_along(bat) # generate ID numbers unique for each element
sum(bat,na.rm = TRUE) # to not account missing values
#' Here are some aggregation functions. For these, make sure to use `na.rm=T` if
#' your vector has `NA`s in it... `max()`, `min()`, `mean()`, `median()`,
#' `quantile()`.

#+ vectors_aggregate
quantile(bat)
quantile(bat,na.rm = TRUE)
#' ### Data Frames
#'
#' You can bundle several vectors of the same length together into a
#' `data.frame` using the `data.frame()` command. A `data.frame` is a tabular
#' data structure where the columns can be different types from each other
#' (though within each column the type will be uniform because each column is
#' still a vector). Most data in R is in the form of a `data.frame` or a class
#' that inherits from `data.frame`. The `dplyr` package makes working with
#' `data.frames` easier and a lot of attention will be devoted to `dplyr`
#' [below](#data-frames-indepth). For now, here are some basic commands for
#' exploring `data.frames`: `dim()`, `nrow()`, `ncol()`, `names()` and (for
#' small datasets) `plot()`.

#+ df_explore




