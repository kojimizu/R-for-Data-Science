---
output:
  word_document: default
  html_document: default
  pdf_document: default
---

R for Data Science (II)
II. Wrangle

# Chapter 9: Introduction  
We will learn about data wrangling, the art of getting the data into R in a useful form for visualization and modeling. 

The part of the book proceeds as follows:

- In `tibbles`, you’ll learn about the variant of the data frame that we use in this book: the tibble. You’ll learn what makes them different from regular data frames, and how you can construct them “by hand”.

- In `data import`, you’ll learn how to get your data from disk and into R. We’ll focus on plain-text rectangular formats, but will give you pointers to packages that help with other types of data.

- In `tidy data`, you’ll learn about tidy data, a consistent way of storing your data that makes transformation, visualisation, and modelling easier. You’ll learn the underlying principles, and how to get your data into a tidy form.

Data wrangling also encompasses data transformation, which you've already learned a little about. Now we'll focus on new skills for three specific types of data, tou will frequently encounter in practice:

- `Relational data` will give you tools for working with multiple interrelated datasets.
- `Strings` will introduce regular expressions, a powerful tool for manipulating strings.
- `Factors` are how R stores categorical data. They are used when a variable has a fixed set of possible values, or when you want to use a non-alphabetical ordering of a string.
- `Dates and times` will give you the key tools for working with dates and date-times.

# Chapter 10: Tibbles
## 10.1: Introduction
Throughout this book we work with “tibbles” instead of R’s traditional `data.frame`. Tibbles are data frames, but they tweak some older behaviours to make life a little easier. R is an old language, and some things that were useful 10 or 20 years ago now get in your way. It’s difficult to change base R without breaking existing code, so most innovation occurs in packages. Here we will describe the tibble package, which provides opinionated data frames that make working in the tidyverse a little easier. In most places, I’ll use the term tibble and data frame interchangeably; when I want to draw particular attention to R’s built-in data frame, I’ll call them `data.frame`s.

If this chapter leaves you wanting to learn more about tibbles, you might enjoy `vignette("tibble")`.

### 10.1.1: Prerequisites
```{r 10.1.1: Prerequisites}
library(tidyverse)
```

## 10.2: Creating tibbles
Almost all of the functions that we will use in this book produce tibbles, as tibbles are one of the unifying features of the tidyverse. Most other R packages use regular data frames, so you might want to coerce a data frame to a tibble. You can do that with `as_tibble()`:
```{r 10.2: creating tibbles 1}
as_tibble(iris)
```

You can create a new `tibble` from individual vectors with tibble(). tibble() will automatically recycle inputs of length 1, and allows you to refer to variables that you just created, as shown below.
```{rcreating tibbles 2}
tibble(
  x=1:5,
  y=1,
  z=x^2+y
)
```

If you’re already familiar with `data.frame()`, note that tibble() does much less: it never changes the type of the inputs (e.g. it never converts strings to factors!), it never changes the names of variables, and it never creates row names.

It’s possible for a tibble to have column names that are not valid R variable names, aka non-syntactic names. For example, they might not start with a letter, or they might contain unusual characters like a space. To refer to these variables, you need to surround them with backticks, ```:
```{r creating tibbles 3}
tb <- tibble(
  ':)'="smile",
  ' '="space",
  `2000`="number"
)
tb
```

You’ll also need the backticks when working with these variables in other packages, like ggplot2, dplyr, and tidyr.

Another way to create a tibble is with `tribble()`, short for transposed tibble. `tribble()` is customised for data entry in code: column headings are defined by formulas (i.e. they start with `~`), and entries are separated by commas. This makes it possible to lay out small amounts of data in easy to read form.
```{r creating tibbles 4}
tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a",2,3.6,
  "b",1,8.5
)
```

I often add a comment (the line starting with #), to make it really clear where the header is.

## 10.3 Tibbles vs. data.frame
There are two main differences in the usage of a tibble vs. a classic data.frame: printing and subsetting.

### 10.3.1 Printing
Tibbles have a refined print method that shows only the first 10 rows, and all the columns that fit on screen. This makes it much easier to work with large data. In addition to its name, each column reports its type, a nice feature borrowed from `str()`:
```{r Tibbles vs. data.frame 0}
tibble(
  a=lubridate::now()+runif(1e3)*86400,
  b=lubridate::today()+runif(1e3)*30,
  c=1:1e3,
  d=runif(1e3),
  e=sample(letters,1e3,replace=TRUE)
)
```

Tibbles are designed so that you don’t accidentally overwhelm your console when you print large data frames. But sometimes you need more output than the default display. There are a few options that can help.

First, you can explicitly `print()` the data frame and control the number of rows (`n`) and the `width` of the display. `width = Inf` will display all columns:
```{r Tibbles vs. data.frame 1}
nycflights13::flights %>% 
  print(n=10,width=Inf)
```

You can also control the default print behaviour by setting options:

- `options(tibble.print_max = n, tibble.print_min = m)`: if more than `m` rows, print only n rows. Use options(dplyr.print_min = Inf) to always show all rows.
- Use `options(tibble.width = Inf)` to always print all columns, regardless of the width of the screen.

You can see a complete list of options by looking at the package help with `package?tibble`.

A final option is to use RStudio’s built-in data viewer to get a scrollable view of the complete dataset. This is also often useful at the end of a long chain of manipulations.
```{r Tibbles vs. data.frame 2}
nycflights13::flights %>% 
  View()
```

### 10.3.2 Subsetting
So far all the tools you’ve learned have worked with complete data frames. If you want to pull out a single variable, you need some new tools, `$` and [[. [[ can extract by name or position; `$` only extracts by name but is a little less typing.
```{r Tibbles vs. data.frame 3}
df <- tibble(
  x=runif(5),
  y=rnorm(5)) %>% 
  print(n=3)

df$x
df[["x"]]
df[[1]]
```

To use these in a pipe, you’ll need to use the special placeholder `.`:
```{r Tibbles vs. data.frame 4}
df %>% .$x
df %>% .[["x"]]
```

Compared to a data.frame, tibbles are more strict: they never do partial matching, and they will generate a warning if the column you are trying to access does not exist.

## 10.4 Interacting with older code
Some older functions don’t work with tibbles. If you encounter one of these functions, use `as.data.frame()` to turn a tibble back to a `data.frame`:
```{r 10.4 Interacting with older code 1}
class(as.data.frame(tb))
```

## 10.5 Exercises
1. How can you tell if an object is a tibble? (Hint: try printing mtcars, which is a regular data frame).
- Visually you can tell by the printing method and how it stores the class of each variable underneath each variable. Or just get the class() of object and see whether it has tbl_df and tbl.

```{r}
is.tibble(mtcars)
class(mtcars)
```

2. Compare and contrast the following operations on a data.frame and equivalent tibble. What is different? Why might the default data frame behaviours cause you frustration?
```{r}
df <- data.frame(abc = 1, xyz = "a")
df$x # on a tibble this will throw a warning. Partial matching is not allowed
df[, "xyz"] # on a tibble this will be a data frame still
df[, c("abc", "xyz")] # This will be the same result in a tibble
```

The frustration is because in some situations a data frame will returns a different thing like in the last two previous lines of code. Tibble will return the same thing, providing consistency.

3. If you have the name of a variable stored in an object, e.g. var <- "mpg", how can you extract the reference variable from a tibble?
```{r}
var <- "mpg"
as_tibble(mtcars)
as_tibble(mtcars)[[var]]

# or
as_tibble(mtcars)[var]
```

4. Practice referring to non-syntactic names in the following data frame by:
- Extracting the variable called 1.
- Plotting a scatterplot of 1 vs 2.
- Creating a new column called 3 which is 2 divided by 1.
- Renaming the columns to one, two and three.
```{r}
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`)))

# extracting the variable called 1
annoying[[1]]
annoying$`1`

# plotting a scatterplot of 1 vs 2
ggplot2::ggplot(annoying,aes('1',`2`))+
  geom_point()

# creating a new column called 3 which is 2 divided by1
annoying <- 
  annoying %>% 
  mutate(`3`=`2`/`1`)

# Renaming the columns to one, two and three.
names(annoying) <- c("one","two","three")

# OR
#annoying
#annoying %>% 
 # rename(one=`1`,
  #       two=`2`,
   #      three=`3`)
```

5. What does tibble::enframe() do? When might you use it?
It turns named vectors or list to two-column data frames.

It's different from `as_tibble()` for lists because it creates a stacked data frame rather than a widy one. It all dependes on your data.
```{r}
lst <- list(female = 1, male = 2)
as_tibble(lst)
```

6. What option controls how many additional column names are printed at the footer of a tibble?
`options(tibble.width = Inf)` for all columns to be printed.

# Chapter 11: Data import
## 11.1: Introduction
Working with data provided by R packages is a great way to learn the tools of data science, but at some point you want to stop learning and start working with your own data. In this chapter, you’ll learn how to read plain-text rectangular files into R. Here, we’ll only scratch the surface of data import, but many of the principles will translate to other forms of data. We’ll finish with a few pointers to packages that are useful for other types of data.

### 11.1.1: Prerequisites
```{r Data import intro}
library(tidyverse)
```

## 11.2: Getting started
Most of readr’s functions are concerned with turning flat files into data frames:

1. `read_csv()` reads comma delimited files, `read_csv2()` reads semicolon separated files (common in countries where `,` is used as the decimal place), `read_tsv()` reads tab delimited files, and `read_delim()` reads in files with any delimiter.

2. `read_fwf()` reads fixed width files. You can specify fields either by their widths with `fwf_widths()` or their position with `fwf_positions()`. `read_table()` reads a common variation of fixed width files where columns are separated by white space.

3. `read_log()` reads Apache style log files. (But also check out webreadr which is built on top of `read_log()` and provides many more helpful tools.)

These functions all have similar syntax: once you have masted one, you can use the others with ease. For the rest of this chapter we’ll focus on `read_csv()`. Not only are csv files one of the most common forms of data storage, but once you understand `read_csv()`, you can easily apply your knowledge to all the other functions in readr.

The first argument to `read_csv()` is the most important: it’s the path to the file to read.

```{r data import 1}
# heights <- read_csv("data/heights.csv")
```

When you run `read_csv()` it prints out a column specification that gives the name and type of each column. That’s an important part of readr, which we’ll come back to in parsing a file.

You can also supply an inline csv file. This is useful for experimenting with readr and for creating reproducible examples to share with others
```{r data import 2}
read_csv("a,b,c
         1,2,3
         4,5,6")
```

In both cases `read_csv()` uses the first line of the data for the column names, which is a very common convention. There are two cases where you might want to tweak this behaviour:

1. Sometimes there are a few lines of metadata at the top of the file. You can use `skip = n` to skip the first n lines; or use `comment = "#"` to drop all lines that start with (e.g.) #.
```{r data import 3}
read_csv("The first line of metadata
         The second line of metadata
         x,y,z
         1,2,3",skip=2)

read_csv("# A comment I want to skip
         x,y,z
         1,2,3",comment="#")
```

2. The data might not have column names. You can use `col_names = FALSE` to tell `read_csv()` not to treat the first row as headings, and instead label them sequentially from `X1` to `Xn`:
```{r data import 5}
read_csv("1,2,3\n4,5,6",col_names = F)
```

("\n" is a convenient shortcut for adding a new line. You’ll learn more about it and other types of string escape in string basics.)

Alternatively, you can pass col_names a characer vector which will be used as the column names:
```{r data import 6}
read_csv("1,2,3\n4,5,6",col_names = c("a","b","c"))
```

This is all you need to know to read ~75% of CSV files that you’ll encounter in practice. You can also easily adapt what you’ve learned to read tab separated files with `read_tsv()` and fixed width files with `read_fwf()`. To read in more challenging files, you’ll need to learn more about how readr parses each column, turning them into R vectors.

### 11.2.1: Compared to base R
If you’ve used R before, you might wonder why we’re not using `read.csv()`. There are a few good reasons to favour readr functions over the base equivalents:

- They are typically much faster (~10x) than their base equivalents. Long running jobs have a progress bar, so you can see what’s happening. If you’re looking for raw speed, try `data.table::fread()`. It doesn’t fit quite so well into the tidyverse, but it can be quite a bit faster.

- They produce tibbles, they don’t convert character vectors to factors, use row names, or munge the column names. These are common sources of frustration with the base R functions.

- They are more reproducible. Base R functions inherit some behaviour from your operating system and environment variables, so import code that works on your computer might not work on someone else’s.

### 11.2.2 Exercises
1. What function would you use to read a file where fields were separated with “|”?
You use `read_delim` and specify "|" in the delim argument.

2. Apart from `file`, `skip`, and `comment`, what other arguments do `read_csv()` and `read_tsv()` have in common?
All arguments! But that's logical because they both use read_delim as the function doing the work. Both functions just call `read_delim` with a set of predefine options for the `csv` and `tsv` formats using the `tokenize_*` functions. The `tokenize_*` functions simply return a list with the charachteristics of each format.

3. What are the most important arguments to read_fwf()?
The most important argument is `col_positions` because that's how determine the width at which each column is separated. You can determine the width with the `fwf_*` helper functions.

4. Sometimes strings in a CSV file contain commas. To prevent them from causing problems they need to be surrounded by a quoting character, like `"` or `'`. By convention, `read_csv()` assumes that the quoting character will be ", and if you want to change it you’ll need to use `read_delim()` instead. What arguments do you need to specify to read the following text into a data frame?
```{r Getting started exercise 4}
read_csv("x,y\n1,'a,b'", quote = "'")
read_delim("x,y\n1,'a,b'", delim = ",",  quote = "'")
```

5. Identify what is wrong with each of the following inline CSV files. What happens when you run the code?
```{r Getting started exercise 5}
read_csv("a,b\n1,2,3\n4,5,6") # more rows then column names
read_csv("a,b\n1,2,3\n4,5,6", skip = 1, col_names = letters[1:3]) # fixed

read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b,c\n1,2\n1,2,3,4") # second row has only two values but the remaining lines have 3

read_csv("a,b\n\"1") # the second row is actually: ", 1. but it uses \" so it's a literal " and a comma is missing
read_csv('a,b\n\",1', quote = "'") # it should be something like this I think

read_csv("a,b\n1,2\na,b") # nothing wrong with this one. Maybe the column classes because a and b are column names errors
read_csv("a,b\n1,2\na,b", n_max = 1) # this is the correct format.

read_csv("a;b\n1;3") # this is ; deliminted
read_csv2("a;b\n1;3")
```

## 11.3 Parsing a vector
Before we get into the details of how readr reads files from disk, we need to take a little detour to talk about the `parse_*()` functions. These functions take a character vector and return a more specialised vector like a logical, integer, or date:

```{r 11.3 Parsing a vector 1}
str(parse_logical(c("TRUE","FALSE","FALSE")))
str(parse_integer(c(c("1","2","3"))))
str(parse_date(c("2010-01-01","1979-10-14")))
```

These functions are useful in their own right, but are also an important building block for readr. Once you’ve learned how the individual parsers work in this section, we’ll circle back and see how they fit together to parse a complete file in the next section.

Like all functions in the tidyverse, the `parse_*()` functions are uniform: the first argument is a character vector to parse, and the na argument specifies which strings should be treated as missing:

```{r 11.3 Parsing a vector 2}
parse_integer(c("1","231",".","456"))
parse_integer(c("1","231",".","456"),na=".")
```

If parsing fails, you will get a warning:
```{r 11.3 Parsing a vector 3}
x <- parse_integer(c("123","345","abc","123.45"))
```

And the failures will be missing in the output:
```{r 11.3 Parsing a vector 4}
x
```

If there are many parsing failures, you’ll need to use `problems()` to get the complete set. This returns a tibble, which you can then manipulate with dplyr.
```{r 11.3 parsing a vector 5}
problems(x)
```

Using parsers is mostly a matter of understanding what’s available and how they deal with different types of input. There are eight particularly important parsers:

1. `parse_logical()` and `parse_integer()` parse logicals and integers respectively. There’s basically nothing that can go wrong with these parsers so I won’t describe them here further.

2. `parse_double()` is a strict numeric parser, and `parse_number()` is a flexible numeric parser. These are more complicated than you might expect because different parts of the world write numbers in different ways.

3. `parse_character()` seems so simple that it shouldn’t be necessary. But one complication makes it quite important: character encodings.

4. parse_factor() create factors, the data structure that R uses to represent categorical variables with fixed and known values.

5. parse_datetime(), parse_date(), and parse_time() allow you to parse various date & time specifications. These are the most complicated because there are so many different ways of writing dates.

### 11.3.1 Numbers
It seems like it should be straightforward to parse a number, but three problems make it tricky:

1. People write numbers differently in different parts of the world. For example, some countries use . in between the integer and fractional parts of a real number, while others use ,.

2. Numbers are often surrounded by other characters that provide some context, like “$1000” or “10%”.

3. Numbers often contain “grouping” characters to make them easier to read, like “1,000,000”, and these grouping characters vary around the world.

To address the first problem, readr has the notion of a “locale”, an object that specifies parsing options that differ from place to place. When parsing numbers, the most important option is the character you use for the decimal mark. You can override the default value of `.` by creating a new locale and setting the `decimal_mark` argument:

```{r 11.1 11.3.1 Numbers 1}
library(tidyverse)

parse_double("1.23")
parse_double("1.23",local=locale(decimal_mark = "."))
```

readr’s default locale is US-centric, because generally R is US-centric (i.e. the documentation of base R is written in American English). An alternative approach would be to try and guess the defaults from your operating system. This is hard to do well, and, more importantly, makes your code fragile: even if it works on your computer, it might fail when you email it to a colleague in another country.

`parse_number()` addresses the second problem: it ignores non-numeric characters before and after the number. This is particularly useful for currencies and percentages, but also works to extract numbers embedded in text.
```{r 11.3.1 Numbers 2}
parse_number("$100")
parse_number("20%")
parse_number("It cost #123.45")
```

The final problem is addressed by the combination of `parse_number()` and the locale as `parse_number()` will ignore the “grouping mark”:

```{r 11.3.1 Numbers 3}
# used in America
parse_number("$123,456,789")
#> [1] 1.23e+08

# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))
#> [1] 1.23e+08

# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))
```

### 11.3.2 Strings
It seems like `parse_character()` should be really simple — it could just return its input. Unfortunately life isn’t so simple, as there are multiple ways to represent the same string. To understand what’s going on, we need to dive into the details of how computers represent strings. In R, we can get at the underlying representation of a string using `charToRaw()`:
```{r 11.3.2 Strings 1}
charToRaw("Hadley")
```

Each hexadecimal number represents a byte of information: `48` is H, `61` is a, and so on. The mapping from hexadecimal number to character is called the encoding, and in this case the encoding is called ASCII. ASCII does a great job of representing English characters, because it’s the American Standard Code for Information Interchange.

Things get more complicated for languages other than English. In the early days of computing there were many competing standards for encoding non-English characters, and to correctly interpret a string you needed to know both the values and the encoding. For example, two common encodings are Latin1 (aka ISO-8859-1, used for Western European languages) and Latin2 (aka ISO-8859-2, used for Eastern European languages). In Latin1, the byte b1 is “±”, but in Latin2, it’s “ą”! Fortunately, today there is one standard that is supported almost everywhere: UTF-8. UTF-8 can encode just about every character used by humans today, as well as many extra symbols (like emoji!).

readr uses UTF-8 everywhere: it assumes your data is UTF-8 encoded when you read it, and always uses it when writing. This is a good default, but will fail for data produced by older systems that don’t understand UTF-8. If this happens to you, your strings will look weird when you print them. Sometimes just one or two characters might be messed up; other times you’ll get complete gibberish. For example:

```{r 11.3.2 Strings 2}
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
x1
x2
```

To fix the problem you need to specify the encoding in `parse_character()`:
2
```{r 11.3.2 Strings 3}
parse_character(x1, locale=locale(encoding="Latin1"))
parse_character(x2,locale=locale(encoding = "Shift-JIS"))
```

How do you find the correct encoding? If you’re lucky, it’ll be included somewhere in the data documentation. Unfortunately, that’s rarely the case, so readr provides `guess_encoding()` to help you figure it out. It’s not foolproof, and it works better when you have lots of text (unlike here), but it’s a reasonable place to start. Expect to try a few different encodings before you find the right one.
```{r 11.3.2 Strings 4}
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))
```

The first argument to `guess_encoding()` can either be a path to a file, or, as in this case, a raw vector (useful if the strings are already in R).

Encodings are a rich and complex topic, and I’ve only scratched the surface here. If you’d like to learn more I’d recommend reading the detailed explanation at http://kunststube.net/encoding/.

### 11.3.3 Factors
R uses factors to represent categorical variables that have a known set of possible values. Give `parse_factor()` a vector of known `levels` to generate a warning whenever an unexpected value is present:
```{r 11.3.3 Factors 1}
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)
```

But if you have many problematic entries, it’s often easier to leave as character vectors and then use the tools you’ll learn about in strings and factors to clean them up.

### 11.3.4 Dates, date-times, and times
You pick between three parsers depending on whether you want a date (the number of days since 1970-01-01), a date-time (the number of seconds since midnight 1970-01-01), or a time (the number of seconds since midnight). When called without any additional arguments:

- `parse_datetime()` expects an ISO8601 date-time. ISO8601 is an international standard in which the components of a date are organised from biggest to smallest: year, month, day, hour, minute, second.
```{r 11.3.4 Dates date-times and times 1}
parse_datetime("2010-10-01T2010")
parse_datetime(20101010)
```

This is the most important date/time standard, and if you work with dates and times frequently, I recommend reading https://en.wikipedia.org/wiki/ISO_8601

- `parse_date()` expects a four digit year, a `-` or `/`, the month, a `-` or `/`, then the day:
```{r 11.3.4 Dates date-times and times 2}
parse_date("2010-10-01")
c <- c("2010-10-01")
parse_date(c)
```

- `parse_time()` expects the hour, `:`, minutes, optionally `:` and seconds, and an optional am/pm specifier:
```{r 11.3.4 Dates date-times and times 3}
library(hms)
parse_time("01:10 am")
parse_time("20:10:01")
```

Base R doesn’t have a great built in class for time data, so we use the one provided in the hms package.

If these defaults don't work for the data, we can suply our onw date-time `format`, built up of the following pieces: 

- Year
  + `%Y` (4 digits)
  + `%y` (2 digits) 00-69 -> 2000-2069, 70-99 -> 1970-1999.
- Month:
  +`%d` (2 digits).
  +`%e` (optional leading space).
- Time 
  +`%H` 0-23 hour.
  +`%I` 0-12, must be used with %p.
  +`%p` AM/PM indicator.
  +`%M` minutes.
  +`%S` integer seconds.
  +`%OS` real seconds.
  +`%Z` Time zone (as name, e.g. America/Chicago). Beware of abbreviations: if you’re American, note that “EST” is a Canadian time zone that does not have daylight savings time. It is not Eastern Standard Time! We’ll come back to this time zones.
%z (as offset from UTC, e.g. +0800).
Non-digits
  + `%.` skips one non-digit character.
  +`%*`skips any number of non-digits.
  
The best way to figure out the correct format is to create a few examples in a character vector, and test with one of the parsing functions. For example: 
```{r 11.3.4 Dates date-times and times 4}
parse_date("01/02/15","%m/%d/%y")
parse_date("01/02/15","%d/%m/%y")
parse_date("01/02/15","%y/%m/%d")
```

If you’re using `%b` or `%B` with non-English month names, you’ll need to set the lang argument to `locale()`. See the list of built-in languages in `date_names_langs()`, or if your language is not already included, create your own with `date_names()`.
```{r 11.3.4 Dates date-times and times 5}
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
#> [1] "2015-01-01"
```


### 11.3.5 Exercises
1. What are the most important arguments to locale()?
This is a bitr tricky because they're all imporant. `encoding` is usually one that brings about a lot of problems if you're working with international data. However, all others are also important except for `asciify` which is hardly used.

2. What happens if you try and set `decimal_mark` and `grouping_mark` to the same character? What happens to the default value of `grouping_mark` when you set decimal_mark to “,”? What happens to the default value of decimal_mark when you set the grouping_mark to “.”?

If you set both to the same character it throws an error. Why? It makes sense. How can you distingush cents from thousands here? `123,456,789`? Is this 123 million or 123 thousand?
```{r}
locale(decimal_mark = ".")
locale(decimal_mark = ",")
```


3. I didn’t discuss the date_format and time_format options to locale(). What do they do? Construct an example that shows when they might be useful.

They set the defaul date/time formats, which are based on the `IS08601` format of yyyy-mm-hh--mm--ss. You can override the default by specifying the `locale` argument with new defaults.

For date:
```{r}
read_csv("a\n05-02-00")
read_csv("a\n05-02-00", locale = locale(date_format = "%d-%m-%y"))
```

For time:
```{r}
read_csv("a\n02-00-08 am")
# read_csv("a\n02-00-08 am",locale=locale(title_format="%M-%S-%I %p"))
```

4. If you live outside the US, create a new locale object that encapsulates the settings for the types of file you read most commonly.
```{r}
locale(date_names="es",
       date_format = "%Y/%m/%d",
       time_format = "%H/%M/%S",
       grouping_mark = ".")
```

5. What’s the difference between read_csv() and read_csv2()?
They perform exactly the same operation but the first reads comma-delimited files and the second one read semi-colon delimited files.


6. What are the most common encodings used in Europe? What are the most common encodings used in Asia? Do some googling to find out.

7. Generate the correct format string to parse each of the following dates and times:
```{r}
d1 <- "January 1, 2010"
parse_date(d1,format = "%B %d, %Y")

# capitalized if its four digits

d2 <- "2015-Mar-07"
parse_date(d2,"%Y-%b-%d")

d3 <- "06-Jun-2016"
parse_date(d3,"%d-%b-%Y")

d4 <- c("August 19 (2015)","July 1 (2015)")
parse_date(d4,"%B %d (%Y)")

d5 <- "12/30/14"
parse_date(d5,"%m/%d/%y")

t1 <- "1705"
parse_time(t1,"%H%M")

t2 <- "11:15:10.12 PM"
parse_time(t2, "%I:%M:%OS %p")

```

## 11.4 Parsing a file
Now that you’ve learned how to parse an individual vector, it’s time to return to the beginning and explore how readr parses a file. There are two new things that you’ll learn about in this section:

1. How readr automatically guesses the type of each column.
2. How to override the default specification.

### 11.4.1 Strategy
Readr uses a heuristic to figure out the type of each column: it reads the first 1000 rows and uses some (moderately conservative) heuristics to figure out the type of each column. You can emulate this process with a character vector using `guess_parser()`, which returns readr’s best guess, and `parse_guess()` which uses that guess to parse the column:
```{r}
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("T","FALSE"))
guess_parser(c("1","5","9"))
guess_parser(c("12,352,561"))
```

The heuristic tries each of the following types, stopping when it finds a match:
- logical: contains only “F”, “T”, “FALSE”, or “TRUE”.
- integer: contains only numeric characters (and -).
- double: contains only valid doubles (including numbers like 4.5e-5).
- number: contains valid doubles with the grouping mark inside.
- time: matches the default time_format.
- date: matches the default date_format.
- date-time: any ISO8601 date.

### 11.4.2 Problems
These defaults don’t always work for larger files. There are two basic problems:

1. The first thousand rows might be a special case, and readr guesses a type that is not sufficiently general. For example, you might have a column of doubles that only contains integers in the first 1000 rows.

2. The column might contain a lot of missing values. If the first 1000 rows contain only NAs, readr will guess that it’s a character vector, whereas you probably want to parse it as something more specific.

readr contains a challenging CSV that illustrates both of these problems:
```{r}
challenge <- read_csv(readr_example("challenge.csv"))
```

(Note the use of `readr_example()` which finds the path to one of the files included with the package)

There are two printed outputs: the column specification generated by looking at the first 1000 rows, and the first five parsing failures. It’s always a good idea to explicitly pull out the `problems()`, so you can explore them in more depth:
```{r}
problems(challenge)
```

A good strategy is to work column by column until there are no problems remaining. Here we can see that there are a lot of parsing problems with the `x` column - there are trailing characters after the integer value. That suggests we need to use a double parser instead.

To fix the call, start by copying and pasting the column speficifcation into your original call:
```{r}
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x=col_integer(),
    y=col_character()
  )
)
```

That fixes the first problem, but if we look at the last few rows, you will see that they are dates stored in a character vector:
```{r}
tail(challenge)
```

You can fixe that by specifying that `y` is a date column:
```{r}
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types=cols(
    x=col_double(),
    y=col_date()
  )
)

tail(challenge)
```

Every `parse_xyz()` function has a corresponding `col_xyz()` function. You use `parse_xyz()` when the data is in a character vector in R already; you use `col_xyz()` when you want to tell readr how to load the data.
  
I highly recommend always supplying `col_types`, building up from the print-out provided by readr. This ensures that you have a consistent and reproducible data import script. If you rely on the default guesses and your data changes, readr will continue to read it in. If you want to be really strict, use `stop_for_problems()`: that will throw an error and stop your script if there are any parsing problems.

### 11.4.3 Other strategies
There are a few other general strategies to help you parse files:

- In the previous example, we just got unlucky: if we look at just one more row than the default, we can correctly parse in one shot:
```{r}
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2
```

- Sometimes it’s easier to diagnose problems if you just read in all the columns as character vectors:
```{r}
challenge2 <- read_csv(readr_example("challenge.csv"), 
  col_types = cols(.default = col_character()))
challenge2
```

This is particularly useful in conjunction with `type_convert()`, which applies the parsing heuristics to the character columns in a data frame.
```{r}
df <- tribble(
  ~x, ~y,
  1,"1.21",
  2,"2.32",
  3,"4.56"
)
df
type_convert(df)
```

- If you’re reading a very large file, you might want to set `n_max` to a smallish number like 10,000 or 100,000. That will accelerate your iterations while you eliminate common problems.

- If you’re having major parsing problems, sometimes it’s easier to just read into a character vector of lines with `read_lines()`, or even a character vector of length 1 with `read_file()`. Then you can use the string parsing skills you’ll learn later to parse more exotic formats.

## 11.5 Writing to a file
eadr also comes with two useful functions for writing data back to disk: `write_csv()` and `write_tsv()`. Both functions increase the chances of the output file being read back in correctly by:

- Always encoding strings in UTF-8.
- Saving dates and date-times in ISO8601 format so they are easily parsed elsewhere.

If you want to export a csv file to Excel, use `write_excel_csv()` — this writes a special character (a “byte order mark”) at the start of the file which tells Excel that you’re using the UTF-8 encoding.

The most important arguments are `x` (the data frame to save), and `path` (the location to save it). You can also specify how missing values are written with na, and if you want to append to an existing file.
```{r}
write_csv(challenge,"challenge.csv")
```

Note that the tyoe information is lost when we save to csv
```{r}
challenge
write_csv(challenge,"challenge-2.csv")
read_csv("challenge-2.csv")
```

This makes CSVs a little unreliable for caching interim results- you need to recreate the column specification every time you load in. There are two alternatives:

1. `write_rds()` and `read_rds()` are uniform wrappers around the base functions `readRDS()` and `saveRDS()`. These store data in R’s custom binary format called RDS:
```{r}
library(tidyverse)
write_rds(challenge,"challenge.rds")
read_rds("challenge.rds")
```

2. The feather package impelments a fast binary file format that can be shared across programming languages:
```{r}
# install.packages("feather")
# library(feather)
# write_feather(challenge,"challenge.feather")
# read_feather("challenge.feather")
```

Feather tends to be faster than RDS and is usable outside of R. RDS supports list-columns (which you’ll learn about in many models); feather currently does not.

## 11.6 Other types of data
To get other types of data into R, we recommend starting with the tidyverse packages listed below. They’re certainly not perfect, but they are a good place to start. For rectangular data:

- haven reads SPSS, Stata, and SAS files.
- readxl reads excel files (both .xls and .xlsx).
- DBI, along with a database specific backend (e.g. RMySQL, RSQLite, RPostgreSQL etc) allows you to run SQL queries against a database and return a data frame.

For hierarchical data: use jsonlite (by Jeroen Ooms) for json, and xml2 for XML. Jenny Bryan has some excellent worked examples at https://jennybc.github.io/purrr-tutorial/.

For other file types, try the R data import/export manual and the rio package.

# Chapter 12: Tidy data
## 12.1 Introduction
“Happy families are all alike; every unhappy family is unhappy in its own way.” –– Leo Tolstoy

“Tidy datasets are all alike, but every messy dataset is messy in its own way.” –– Hadley Wickham

In this chapter, you will learn a consistent way to organise your data in R, an organisation called tidy data. Getting your data into this format requires some upfront work, but that work pays off in the long term. Once you have tidy data and the tidy tools provided by packages in the tidyverse, you will spend much less time munging data from one representation to another, allowing you to spend more time on the analytic questions at hand.

This chapter will give you a practical introduction to tidy data and the accompanying tools in the tidyr package. If you’d like to learn more about the underlying theory, you might enjoy the Tidy Data paper published in the Journal of Statistical Software, http://www.jstatsoft.org/v59/i10/paper.

### 12.1.1 Prerequisites
```{r 12.1 Introduction}
library(tidyverse)
```

## 12.1 Tidy data
You can represent the same underlying data in multiple ways. The example below shows the same data organised in four different ways. Each dataset shows the same values of four variables country, year, population, and cases, but each dataset organises the values in a different way.

```{r 12.2 Tidy data 1}
table1 %>% head(15)
table2
table3
table4a
table4b
```

These are all representations of the same underlying data, but they are not equally easy to use. One dataset, the tidy dataset, will be much easier to work with inside the tidyverse.

These are three interrelated rules which make a dataset tidy:
1. Each variable must have its own row
2. Each observation must have its own row.
3. Each value must have its own cell.

These three rules are interrelated because it’s impossible to only satisfy two of the three. That interrelationship leads to an even simpler set of practical instructions:
1. Put each dataset in a tibble
2. Put each variable in a column

In this example, only table1 is `tidy`. It’s the only representation where each column is a variable.

Why ensure that your data is tidy? There are two main advantages:
1. There’s a general advantage to picking one consistent way of storing data. If you have a consistent data structure, it’s easier to learn the tools that work with it because they have an underlying uniformity.
2. There’s a specific advantage to placing variables in columns because it allows R’s vectorised nature to shine. As you learned in mutate and summary functions, most built-in R functions work with vectors of values. That makes transforming tidy data feel particularly natural.

dplyr, ggplot2, and all the other packages in the tidyverse are designed to work with tidy data. Here are a couple of small examples showing how you might work with `table1`.

```{r 12.2 Tidy data 2}
# compute rate per 10,000
table1 %>%
  mutate(rate=cases/population*10000)

# compute cases per hear
table1 %>% 
  count(year,wt=cases)

# visualize changes over time
library(ggplot2)
table1
ggplot(table1,aes(year,cases))+
  geom_line(aes(group=country),color="grey50")+
  geom_point(aes(color=country))
```

#### 12.2.1 Exercises
1. Using prose, describe how the variables and observations are organised in each of the sample tables.
```{r 12.2.1 Exercises 1-1}
table1
```
All columns are it's own variables because for example `year` contains only years and `country` contains only countries. For example, if we got a data set where we have `country` and then two columns for `males` and `females` that would be untidy. At least in principle. Because both columns should be a column called gender. This is a bit tricky because nothing says that you can't use the gender columns that way. But for easy use in R it's usually better to work with `tidy` data. But who says that `cases` and `populations` are not the same thing? We could argue that they should be in the same column as it is now.

```{r 12.2.1 Exercises 1-2}
table2
```

In principle, this is also not a `tidy` dataset, although that's debatable. If we had something like different sicknesses in the type column then this would be tidy. But type in this case should be different columns because they measure different things. This dataset is organized in a way that years are nested within countries and then each type is instead within years.

```{r 12.2.1 Exercises 1-3}
table3
```
This is clearly non-tidy because we can't work with values such as the rate column. If this column would be the result of the operation, then this would be a tidy dataset. 

```{r 12.2.1 Exercises 1-4}
table4a
```
This is clearly not a tidy dataset because years, which are the same thing, are in different columns. This is the same case as the gender example I outlined above. These two columns should be one variable and the values should be a separate column. This dataset is only for the type `cases`.

```{r 12.2.1 Exercises 1-5}
table4b
```
Same as above, but this is for `population`.


2. Compute the `rate` for `table2`, and `table4a` + `table4b`. 
You will need to perform four operations:
- Extract the number of TB cases per country per year.
- Extract the matching population per country per year.
- Divide cases by population, and multiply by 10000.
- Store back in the appropriate place.
Which representation is easiest to work with? Which is hardest? Why?
```{r 12.2.1 Exercises 2}
fyear_cases <- 
  table2 %>% 
  filter(year==1999,
         type=="cases")
fyear_pop <- 
  table2 %>% 
  filter(year==1999,
         type=="population")
rate_99 <- 
  bind_cols(fyear_cases,fyear_pop) %>% 
  mutate(rate=count/count1) %>% 
  select(-ends_with("1"),-type)

fyear_cases <-
  table2 %>%
  filter(year == 2000,
         type == "cases")

fyear_pop <- 
  table2 %>% 
  filter(year==2000,
         type=="population")

rate_00 <- 
  bind_cols(fyear_cases,fyear_pop) %>% 
  mutate(rate=count/count1) %>% 
  select(-ends_with("1"),-type)

rate_99
rate_00

## For table 4a and 4b


```

In a way, it's more intuitive to work with `table2` because we use the filtering techniques to understand the operations. But with table4a is more succient. However, if we wanted to turn the `table4a/4b` result to a tidier version, it would be more tedious (withut `gather` and such functions.)

3. Recreate the plot showing change in cases over time using `table2` instead of `table1`. What do you need to do first?
```{r 12.2.1 Exercises 3}
table2 %>% 
  filter(type=="cases") %>% 
  ggplot(aes(year,count,group=country,color=country))+
  geom_point()+
  geom_line()
```

## 12.3 Spreading and gathering 
The principles of tidy data seem so obvious that you might wonder if you’ll ever encounter a dataset that isn’t tidy. Unfortunately, however, most data that you will encounter will be untidy. There are two main reasons:
1. Most people aren’t familiar with the principles of tidy data, and it’s hard to derive them yourself unless you spend a lot of time working with data.
2. Data is often organised to facilitate some use other than analysis. For example, data is often organised to make entry as easy as possible.

This means for most real analyses, you’ll need to do some tidying. The first step is always to figure out what the variables and observations are. Sometimes this is easy; other times you’ll need to consult with the people who originally generated the data. The second step is to resolve one of two common problems:
1. One variable might be spread across multiple columns.
2. One observation might be scattered across multiple rows.

Typically a dataset will only suffer from one of these problems; it’ll only suffer from both if you’re really unlucky! To fix these problems, you’ll need the two most important functions in tidyr: `gather()` and `spread()`.

### 12.3.1 Gathering
A common problem is a dataset where some of the column names are not names of variables, but values of a variable. Take `table4a`: the column names `1999` and `2000` represent values of the `year` variable, and each row represents two observations, not one.
```{r 12.3.1 Gathering 1}
table4a
```

To tidy a dataset like this, we need to gather those columns into a new pair of variables. To describe that operation we need three parameters:
- The set of columns that represent values, not variables. In this example, those are the columns `1999` and `2000`.
- The name of the variable whose values form the column names. I call that the `key`, and here it is `year`.
- The name of the variable whose values are spread over the cells. I call that value, and here it’s the number of cases.

Together those parameters generate the call to `gather()`:
```{r 12.3.1 Gathering 2}
table4a %>% 
  gather(`1999`, `2000`, key="year",value=cases)
```

The columns to gather are specified with `dplyr::select()` style notation. Here there are only two columns, so we list them individually. Note that “1999” and “2000” are non-syntactic names (because they don’t start with a letter) so we have to surround them in backticks. To refresh your memory of the other ways to select columns, see select.

In the final result, the gathered columns are dropped, and we get new `key` and `value` columns. Otherwise, the relationships between the original variables are preserved.. The only difference is the variable stored in the cell values. 
```{r 12.3.1 Gathering 3}
table4b
table4b %>% 
  gather(`1999`,`2000`,key="year",value="population")
```

To combine the tidied versions of `table4a` and `table4b` into a single tibble, we need to use `dplyr::left_join()`, which you'll learn about in relational data.
```{r 12.3.1 Gathering 4}
tidy4a <- table4a %>% 
  gather(`1999`,`2000`,key="year",value="cases")
tidy4b <- table4b %>% 
  gather(`1999`,`2000`,key="year",value="population")
left_join(tidy4a, tidy4b)
```

### 12.3.2 Spreading
Spreading is the opposite of gathering. You use it when an observation is scattered across multiple rows. For example, take `table2`: an observation is a country in a year, but each observation is spread across two rows.
```{r 12.3.2 Spreading 1}
table2
```

To tidy this up, we first analyse the representation in similar way to `gather()`. This time, however, we only need two parameters:

- The column that contains variable names, the `key` column. Here, it’s `type`.
- The column that contains values forms multiple variables, the `value` column. Here it’s `count`.

Once we’ve figured that out, we can use `spread()`, as shown programmatically below, and visually in Figure 12.3.
```{r 12.3.2 Spreading 2}
spread(table2, key=type,value=count)
```

As you might have guessed from the common `key` and `value` arguments, `spread()` and `gather()` are complements. `gather()` makes wide tables narrower and longer; `spread()` makes long tables shorter and wider.

### 12.3.3 Exercises
1. Why are gather() and spread() not perfectly symmetrical?
Carefully consider the following example:
```{r 12.3.3 Exercises 1}
# tribble
stocks <- tribble(
  ~year,~half,~return,
    2015,1,1.88,
    2015,2,0.59,
    2016,1,0.92,
    2016,2,0.17)

# tibble
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>% 
  spread(year,return) %>% 
  gather("year","return",`2015`:`2016`)
```

(Hint: look at the variable types and think about column names.)
Both `spread()` and `gather()` have a convert argument. What does it do?

Both `spread()` and `gather()` have a convert argument. What does it do?

Because the `key` variable is actually the column names, and is thus moved as character column. It would be unwise for gather to treat column names as numerics, logicals, or something else. However, you can find a workaround by specifying `convert = TRUE` which will try to convert the `key` columns to it's correct class.

2. Why does this code fail?
```{r 12.3.3 Exercises 2}
# fixed
head(table4a)
table4a %>% 
  gather(`1999`,`2000`,key=year,value=cases)
```

Because `gather` can't find the columns names. You can't name columns w/ numbers in R without quoting them with tick marks.

3. Why does spreading this tibble fail? How could you add a news column to fixed the problem?
```{r 12.3.3 Exercises 3-1}
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people
# people %>% 
  # spread(key=key,value=value)
```

Because Phillip Woods has two values of age. Think about it.. Phillip woods then would have TWO columns of age. That doesn't make sense! We need to add a unique column id specifying the third or first age as a unique person.
```{r 12.3.3 Exercises 3.2}
people %>%
  mutate(unique_id = c(1, 2, 2, 3, 3)) %>%
  select(unique_id, everything()) %>%
  spread(key, value)
```

4. Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?
```{r 12.3.3 Exercises 4}
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12)
preg
```

The main objective of analysis here is whether pregnant or not (bc males can not be pregnant), so I would go for gathering the gender column rather than spreading the pregnant column.
```{r 12.3. Exercise 4-2}
preg %>% 
  gather(gender,values,pregnant)
```

## 12.4 Separating and uniting
So far you’ve learned how to tidy `table2` and `table4`, but not `table3`. `table3` has a different problem: we have one column (`rate`) that contains two variables (`cases` and `population`). To fix this problem, we’ll need the `separate()` function. You’ll also learn about the complement of `separate()`: `unite()`, which you use if a single variable is spread across multiple columns.

### 12.4.1 Separate
`separate()` pulls apart one column into multiple columns, by splitting wherever a separator character appears. Take `table3`:
```{r 12.4.1 Separate 1}
library(tidyverse)
table3
```

The `rate` column contains both `cases` and `population` variables, and we need to split it into two variables. `separate()` takes the name of the column to separate, and the names of the columns to separate into, as shown in Figure 12.4 and the code below.
```{r 12.4.1 Separate 2}
table3 %>% 
  separate(rate,into=c("cases","population"))
```

By default, `separate()` will split values wherever it sees a non-alphanumeric character (i.e. a character that isn’t a number or letter). For example, in the code above, `separate()` split the values of rate at the forward slash characters. If you wish to use a specific character to separate a column, you can pass the character to the sep argument of `separate()`. For example, we could rewrite the code above as:
```{r 12.4.1 Separate3}
table3 %>% 
  separate(rate,into=c("cases","population"),sep="/")
```

Formally, `sep` is a regular expression, which we will learn more about in strings.

Look carefully at the column types: you’ll notice that `case` and `population` are character columns. This is the default behaviour in `separate()`: it leaves the type of the column as is. Here, however, it’s not very useful as those really are numbers. We can ask `separate()` to try and convert to better types using `convert = TRUE`:
```{r 12.4.1 Separate 4}
table3 %>% 
  separate(rate,into=c("cases","population"),convert=T)
```

You can also pass a vector of integers to `sep`. `separate()` will interpret the integers as positions to split at. Positive values start at 1 on the far-left of the strings; negative value start at -1 on the far-right of the strings. When using integers to separate strings, the length of `sep` should be one less than the number of names in `into`.

You can use this arrangement to separate the last two digits of each year. This make this data less tidy, but is useful in other cases, as you’ll see in a little bit.
```{r 12.4.1 Separate 5}
table3
table3 %>% 
  separate(year,into=c("century","year"),sep=2)
```

### 12.4.2 Unite
`unite()` is the inverse of `separate()`: it combines multiple columns into a single column. You’ll need it much less frequently than `separate()`, but it’s still a useful tool to have in your back pocket.

We can use `unite()` to rejoin the century and year columns that we created in the last example. That data is saved as `tidyr::table5`. `unite()` takes a data frame, the name of the new variable to create, and a set of columns to combine, again specified in `dplyr::select()` style:
```{r 12.4.2 Unite1}
table5
table5 %>% 
  unite(new,century,year)
```

In this case we also need to use the `sep` argument. The default will place an underscore (`_`) between the values from different columns. Here we don’t want any separator so we use `""`:
```{r 12.4.2 Unite2}
table5 %>% 
  unite(new,century,year,sep="")
```

### 12.4.3 Exercises
1. What do the `extra` and `fill` arguments do in `separate()`? Experiment with the various options for the following two toy datasets.
```{r 12.4.3 Exercises 1}
tibble(x=c("a,b,c","d,e,f,g","h,i,j")) %>% 
  separate(x,c("one","two","three"))

tibble(x=c("a,b,c","d,e","f,g,i")) %>% 
  separate(x,c("one","two","three"))
```

It's simple. x has vectors with 3 and 4 characters but we specify 3 columns. `fill` has three values: `warn`, `right` and `left`. Here I specify a fourth column to place the extra letter. The first fills the missing values with the extra character using the right most match. left does the same thing but without a warning. and left places the extra character empty in the first column.
```{r  12.4.3 Exercises 1 solution}
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three", "four"))

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three", "four"), fill = "right")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three", "four"), fill = "left")
```

I've deleted the fourth column to see how this works. extra on the other hand, deals with either droping or merging the extra characters. warn drops the extra character and emits a warning messge. drop does the same thing but without a warning and merge merges the extra character to it's closest end. No aparent option to merge with the first column rather than the last.
```{r  12.4.3 Exercises 1 solution 2}
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "warn")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")
```


2. Both `unite()` and `separate()` have a remove argument. What does it do? Why would you set it to FALSE?

Because `unite` and `separate` receive columns and create new ones, `remove` allows you to remove the original columns that you unite/separate on. You might want to leave them as they are if you're checking whether the transformation was done correctly.

3. Compare and contrast separate() and extract(). Why are there three variations of separation (by position, by separator, and with groups), but only one unite?

Because you can separate differently. Examples below:
```{r 12.4.3 Exercises 3}
df_sep <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df_extract <- data.frame(x = c(NA, "ap.b", "aa/d", "b.c", "d-ee"))

# This is easy with separate
df_sep %>% separate(x, c("new", "old"), sep = 1)
df_sep %>% separate(x, c("new", "old"), sep = "-")

# Here we can define 2 or more groups to separate the more complex string
df_extract %>% extract(x, c("new", "old"), regex = "(.*)[:punct:](.*)")
```

## 12.5 Missing values 
Changing the representation of a dataset brings up an important subtlety of missing values. Surprisingly, a value can be missing in one of two possible ways:

- Explicitly, i.e. flagged with NA.
- Implicitly, i.e. simply not present in the data.

Let’s illustrate this idea with a very simple data set:
```{r ## 12.5 Missing values 1}
stocks <- tibble(
  year= c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr=c(   1,    2,    3,    4,    2,    3,    4),
  return=c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
```

There are two missing values in this dataset:
1. The return for the fourth quarter of 2015 is explicitly missing, because the cell where its value should be instead contains NA.
2. The return for the first quarter of 2016 is implicitly missing, because it simply does not appear in the dataset.

One way to think about the difference is with this Zen-like koan: An explicit missing value is the presence of an absence; an implicit missing value is the absence of a presence.

The way that a dataset is represented can make implicit values explicit. For example, we can make the implicit missing value explicit by putting years in the columns:
```{r 12.5 Missing values 2}
stocks %>% 
  spread(year,return)
```

Because these explicit missing values may not be important in other representations of the data, you can set `na.rm = TRUE` in `gather()` to turn explicit missing values implicit:
```{r 12.5 Missing values 3}
stocks %>% 
  spread(year,return) %>% 
  gather(`2015`,`2016`,key=year,value=return,na.rm = T)

stocks %>% 
  spread(year,return) %>% 
  gather(year,return,`2015`:`2016`,na.rm=T)
```

Another important tool for making missing values explicit in tidy data is `complete()`:
```{r 12.5 Missing values 4}
stocks
stocks %>% 
  complete(year,qtr)
```

`complete()` takes a set of columns, and finds all unique combinations. It then ensures the original dataset contains all those values, filling in explicit NAs where necessary.

There’s one other important tool that you should know for working with missing values. Sometimes when a data source has primarily been used for data entry, missing values indicate that the previous value should be carried forward:

```{r 12.5 Missing values 5}}
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
```

You can fill in these missing values with `fill()`g. It takes a set of columns where you want missing values to be replaced by the most recent non-missing value (sometimes called last observation carried forward).
```{r 12.5 Missing values 6}
treatment %>% 
  fill(person)
```

### 12.5.1 Exercises
1. Compare and contrast the fill arguments to `spread()` and `complete()`.

The `fill` argument in `spread()` will replace ALL missing values regardless of columns with the same value. The fill argument of `complete()` accepts a list where each slot is the missing value for each column. So missing values per column are customizable to any chosen missing.

2. What does the direction argument to `fill()` do?
If we have this dataset
```{r 12.5.1 Exercise 2}
library(tidyverse)

treatment <- tribble(
  ~person,~treatment,~response,
  "Derrick Whitmore",1,7,
  NA,2,10,
  NA,3,9,
  "Katherine Burke",1,4
)
treatment
```

We have two missing values in column `person`. We can carry over the value `Katherine` to replace the missing values or we could take `Derrick` to replace the missing values. `.direction` does exactly that by specifying either down or up.
```{r Ex 1.}
library(tidyr) #fill()
fill(treatment,person, .direction="up")
```

```{r Ex.2}
fill(treatment, person,.direction = "down")
```

## 12.6 Case Study

To finish off the chapter, let’s pull together everything you’ve learned to tackle a realistic data tidying problem. The tidyr::who dataset contains tuberculosis (TB) cases broken down by year, country, age, gender, and diagnosis method. The data comes from the 2014 World Health Organization Global Tuberculosis Report, available at http://www.who.int/tb/country/data/download/en/.

There’s a wealth of epidemiological information in this dataset, but it’s challenging to work with the data in the form that it’s provided:

```{r 12.6 case study 1: data overview}
library(tidyr)
head(who,20)
names(who)

library(Amelia)
missmap(who)
```

This is a very typical real-life example dataset. It contains redundant columns, odd variable codes, and many missing values. In short, `who` is messy, and we’ll need multiple steps to tidy it. Like dplyr, tidyr is designed so that each function does one thing well. That means in real-life situations you’ll usually need to string together multiple verbs into a pipeline.

The best place to start is almost always to gather together the columns that are not variables. Let’s have a look at what we’ve got:
- It looks like `country`, `iso2`, and `iso3` are three variables that redundantly specify the country.
- `year` is clearly also a variable.
- We don’t know what all the other columns are yet, but given the structure in the variable names (e.g. `new_sp_m014`, `new_ep_m014`, `new_ep_f014`) these are likely to be values, not variables.

So we need to gather together all the columns from `new_sp_m014` to `newrel_f65`. We don’t know what those values represent yet, so we’ll give them the generic name "key". We know the cells represent the count of cases, so we’ll use the variable cases. There are a lot of missing values in the current representation, so for now we’ll use `na.rm` just so we can focus on the values that are present.
```{r 12.6 Case study 2}
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65,key="key",value="cases",na.rm=T)
head(who1,10)
```

We can get some hint of the structure of the values in the new `key` column by counting them:
```{r 12.6 case study 3}
who1 %>% 
  count(key)
```

You might be able to parse this out by yourself with a little thought and some experimentation, but luckily we have the data dictionary handy. It tells us:
1. The first three letters of each column denote whether the column contains new or old cases of TB. In this dataset, each column contains new cases.
2. The next two letters describe the type of TB:
  - rel stands for cases of relapse
  - ep stands for cases of extrapulmonary TB
  - sn stands for cases of pulmonary TB that could not be   -     - diagnosed by a pulmonary smear (smear negative)
  - sp stands for cases of pulmonary TB that could be diagnosed be a pulmonary smear (smear positive)
3. The sixth letter gives the sex of TB patients. The dataset groups cases by males (`m`) and females (`f`).
4. The remaining numbers gives the age group. The dataset groups cases into seven age groups:
  - `014` = 0 – 14 years old
  - `1524` = 15 – 24 years old
  - `2534` = 25 – 34 years old
  - `3544` = 35 – 44 years old
  - `4554` = 45 – 54 years old
  - `5564` = 55 – 64 years old
  - `65` = 65 or older

We need to make a minor fix to the format of the column names: unfortunately the names are slightly inconsistent because instead of `new_rel` we have `newrel` (it’s hard to spot this here but if you don’t fix it we’ll get errors in subsequent steps). You’ll learn about str_replace() in strings, but the basic idea is pretty simple: replace the characters “newrel” with “`new_rel`”. This makes all variable names consistent.
```{r 12.6 case study 4}
who2 <- who1 %>% 
  mutate(key=stringr::str_replace(key,"newrel","new_rel"))
tail(who2)
```

We can separate the values in each code with two passes of `separate()`. The first pass will split the codes at each underscore.
```{r 12.6 case study 5}
who3 <- who2 %>% 
  separate(key, c("new","type","sexage"),sep="_")
head(who3)
```

Then we might as well drop the `new` column because it’s constant in this dataset. While we’re dropping columns, let’s also drop `iso2` and `iso3` since they’re redundant.
```{r 12.6 case study 6}
who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new,-iso2,-iso3)
```

Next we’ll separate `sexage` into `sex` and `age` by splitting after the first character:
```{r 12.6 case study 7}
who5 <- who4 %>% 
  separate(sexage,c("sex","age"),sep=1)
```

The `who` dataset is now tidy!

I’ve shown you the code a piece at a time, assigning each interim result to a new variable. This typically isn’t how you’d work interactively. Instead, you’d gradually build up a complex pipe:
```{r}
who %>% 
  gather(code,value,new_sp_m014:newrel_f65,na.rm = T) %>% 
   mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
```

### 12.6.1 Exercises
1. In this case study I set `na.rm = TRUE` just to make it easier to check that we had the correct values. Is this reasonable? Think about how missing values are represented in this dataset. Are there implicit missing values? What’s the difference between an NA and zero?

A proper analysis would not exclude the missing values because that's information! It is the presence of an absence. So for our purposes it is reasonable, but for appropriate descriptive statistics it is important to report the number of missing values.

How many implicit missing values are there? That's easy! We use complete with the gathered dataset.
```{r 12.6.1 1}
first <- 
  who %>%
  gather(
    new_sp_m014:newrel_f65,
    key="key",
    value="cases"
  )

second <- first %>% 
  complete(country,year,key)

# we merge both dataset where there are no matching values (so left over rows)
first %>% 
  anti_join(second,by=c("country","year","key"))
```

So no implicit missing values. And the difference between an NA and a 0 is that 0 means there's 0 cases in that cell but NA could mean that there's 20 cases but weren't reported.

2. What happens if you neglect the `mutate()` step? (`mutate(key = stringr::str_replace(key, "newrel", "new_rel"))`)

Well, if we have `new_sp` and `newrel` and we separate on `_` we would get a column where there's `new` and `newrel` together and in the other column there would only be `sp`. If we replace `newrel` with `new_rel` then the same pattern is constant in the same column.

3. I claimed that `iso2` and `iso3` were redundant with `country`. Confirm this claim.
```{r 12.6 Exercises 3}
who %>%
  count(country,iso2,iso3) %>% 
  count(country) %>% 
  filter(nn>1)
```

If there would be repetitions of country, then this would equal more than 1.

4. For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.
```{r 12.6 Exercises 4}
who1 <- 
  who %>%
  gather(
    new_sp_m014:newrel_f65,
    key = "key",
    value = "cases",
    na.rm = TRUE
  ) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"), sep = "_") %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

who1 %>%
  group_by(country, year, sex) %>%
  summarize(n = sum(cases)) %>%
  ggplot(aes(year, n, group = country)) +
  geom_line(alpha = 2/4) +
  facet_wrap(~ sex)
```


## 12.7 Non-tidy data
Before we continue on to other topics, it’s worth talking briefly about non-tidy data. Earlier in the chapter, I used the pejorative term “messy” to refer to non-tidy data. That’s an oversimplification: there are lots of useful and well-founded data structures that are not tidy data. There are two main reasons to use other data structures:

Alternative representations may have substantial performance or space advantages.

Specialised fields have evolved their own conventions for storing data that may be quite different to the conventions of tidy data.

Either of these reasons means you’ll need something other than a tibble (or data frame). If your data does fit naturally into a rectangular structure composed of observations and variables, I think tidy data should be your default choice. But there are good reasons to use other structures; tidy data is not the only way.

If you’d like to learn more about non-tidy data, I’d highly recommend this thoughtful blog post by Jeff Leek: http://simplystatistics.org/2016/02/17/non-tidy-data/

# Chapter 13: Relational Data
## 13.1 Introduction
It’s rare that a data analysis involves only a single table of data. Typically you have many tables of data, and you must combine them to answer the questions that you’re interested in. Collectively, multiple tables of data are called relational data because it is the relations, not just the individual datasets, that are important.

Relations are always defined between a pair of tables. All other relations are built up from this simple idea: the relations of three or more tables are always a property of the relations between each pair. Sometimes both elements of a pair can be the same table! This is needed if, for example, you have a table of people, and each person has a reference to their parents.

To work with relational data you need verbs that work with pairs of tables. There are three families of verbs designed to work with relational data:

- Mutating joins, which add new variables to one data frame from matching observations in another.
- Filtering joins, which filter observations from one data frame based on whether or not they match an observation in the other table.
- Set operations, which treat observations as if they were set elements.

The most common place to find relational data is in a relational database management system (or RDBMS), a term that encompasses almost all modern databases. If you’ve used a database before, you’ve almost certainly used SQL. If so, you should find the concepts in this chapter familiar, although their expression in dplyr is a little different. Generally, dplyr is a little easier to use than SQL because dplyr is specialised to do data analysis: it makes common data analysis operations easier, at the expense of making it more difficult to do other things that aren’t commonly needed for data analysis.

### 13.1.1 Prerequisites
We wil lexplore relational data from `nycflights13` using the two-table verbs from dplyr.
```{r 13.1.1 Prerequisites}
library(tidyverse)
library(nycflights13)
```

## 13.2 nycflights13 
We will use the `nycflights13` package to learn about relational data. nycflights13 contains four tibbles that are related to the `flights` table that you used in data transformation:

- `airlines` lets you look up the full carrier name from its abbreviated code:
```{r 13.2 airlines data}
airlines
```

- `airports` gives information about each airport, identified by the faa airport code:
```{r 13.2 airports data}
airports
```

- `planes` gives information about each plane, identified by its `tailnum`:
```{r 13.2 planes data}
planes
```

- `weather` gives the weather at each NYC airport for each hour:
```{r 13.2 weather data}
weather
```

One way to show the relationships between the different tables is with a drawing:

This diagram is a little overwhelming, but it’s simple compared to some you’ll see in the wild! The key to understanding diagrams like this is to remember each relation always concerns a pair of tables. You don’t need to understand the whole thing; you just need to understand the chain of relations between the tables that you are interested in.

For nycflights13:
- `flights` connects to planes via a single variable, `tailnum`.
- `flights` connects to `airlines` through the `carrier` variable.
- `flights` connects to `airports` in two ways: via the `origin` and `dest` variables.
- `flights` connects to `weather` via `origin` (the location), and `year`, `month`, `day` and `hour` (the time).

### 13.2.1 Exercises
1. Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. What variables would you need? What tables would you need to combine?

You would need to combine `airports` with `flights` because the airports dataset the the coordinates of the airport. You could match them by the `faa` variable in `airports` and the `origin` and `dest` from `flights`

```{r 13.2.1 Exercises 1}
head(flights)
head(airports)
flights %>% 
  left_join(airports,by=c("origin"="faa","dest"="faa"))
```

2. I forgot to draw the relationship between `weather` and `airports`. What is the relationship and how should it appear in the diagram?

Similarly as the above exercise, you can match them with `faa` in `airports` and `origin` in `weather`.
```{r 13.2.1 Exercises 2}
head(airports)
head(weather)
airports %>% 
  left_join(weather,by=c("faa"="origin"))
```


3. weather only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with flights?

You could also connect it with flights through the `dest` and have the weather of every single airport in the U.S that are present in the `flights` dataset.

4. We know that some days of the year are “special”, and fewer people than usual fly on them. How might you represent that data as a data frame? What would be the primary keys of that table? How would it connect to the existing tables?

You could have a separate dataset with the festivities in the U.S by day and month. With this information you can match it with each flight in the flights data set and subsequently with the weather dataset

## 13.3 Keys
The variables used to connect each pair of tables are called keys. A key is a variable (or set of variables) that uniquely identifies an observation. In simple cases, a single variable is sufficient to identify an observation. For example, each plane is uniquely identified by its `tailnum`. In other cases, multiple variables may be needed. For example, to identify an observation in weather you need five variables: `year`, `month`, `day`, `hour`, and `origin`.

There are two types of keys:
- A primary key uniquely identifies an observation in its own table. For example, `planes$tailnum` is a primary key because it uniquely identifies each plane in the `planes` table.
- A foreign key uniquely identifies an observation in another table. For example, the `flights$tailnum` is a foreign key because it appears in the `flights` table where it matches each flight to a unique plane.

A variable can be both a primary key and a foreign key. For example, origin is part of the `weather` primary key, and is also a foreign key for the `airport` table.

Once you’ve identified the primary keys in your tables, it’s good practice to verify that they do indeed uniquely identify each observation. One way to do that is to `count()` the primary keys and look for entries where `n` is greater than one:

```{r 13.3 Keys 1}
planes %>% 
  count(tailnum) %>% 
  filter(n>1)

weather %>% 
  count(year,month,day,hour,origin) %>% 
  filter(n>1)
```

Sometimes a table doesn’t have an explicit primary key: each row is an observation, but no combination of variables reliably identifies it. For example, what’s the primary key in the `flights` table? You might think it would be the date plus the flight or tail number, but neither of those are unique:
```{r 13.3 Keys 2}
flights %>% 
  count(year,month,day,flight) %>% 
  filter(n>1)

flights %>% 
  count(year,month,day,tailnum) %>% 
  filter(n>1)
```

When starting to work with this data, I had naively assumed that each flight number would be only used once per day: that would make it much easier to communicate problems with a specific flight. Unfortunately that is not the case! If a table lacks a primary key, it’s sometimes useful to add one with `mutate()` and `row_number()`. That makes it easier to match observations if you’ve done some filtering and want to check back in with the original data. This is called a surrogate key.

A primary key and the corresponding foreign key in another table form a relation. Relations are typically one-to-many. For example, each flight has one plane, but each plane has many flights. In other data, you’ll occasionally see a 1-to-1 relationship. You can think of this as a special case of 1-to-many. You can model many-to-many relations with a many-to-1 relation plus a 1-to-many relation. For example, in this data there’s a many-to-many relationship between airlines and airports: each airline flies to many airports; each airport hosts many airlines.

### 13.3.1 Exercises

1. Add a surrogate key to flights.
```{r}
head(flights,10)
flights %>% 
  mutate(id=row_number(year)) %>% 
  select(id,everything())
```


2. Identify the keys in the following datasets
- Lahman::Batting,
- babynames::babynames
- nasaweather::atmos
- fueleconomy::vehicles
- ggplot2::diamonds
(You might need to install some packages and read some documentation.)

```{r}
# - Lahman::Batting,
as_tibble(Lahman::Batting)
# It's playerID

# - babynames::babynames
# I think it might be the combination of year and name. That identifies each name-year pair for matching with other tables.
# install.packages("babynames")
as_tibble(babynames::babynames)

# - nasaweather::atmos
# install.packages("nasaweather")
library(nasaweather)
as_tibble(nasaweather::atmos)
# Here it's most likely lat, long, year and month, which locate a specific place in a month/year.

# - fueleconomy::vehicles
# install.packages("fueleconomy")
as_tibble(fueleconomy:vehicles)
# id is the simple key

# - ggplot2::diamonds
as_tibble(diamonds)
# There is not key because there are not other datasets! The concept of key only makes sense when there are other relational datasets.

```


3. Draw a diagram illustrating the connections between the `Batting`, `Master`, and `Salaries` tables in the Lahman package. Draw another diagram that shows the relationship between `Master`, `Managers`, `AwardsManagers`.

How would you characterise the relationship between the `Batting`, `Pitching`, and `Fielding` tables?

It's actualy very straight forward: all three tables have the same playerID and yearID and each table has the information that the other doesn't have, so they complement each other. I think it is one-to-one relationships but that needs to have inspected further.

## 13.4 Mutating joins
The first tool we’ll look at for combining a pair of tables is the mutating join. A mutating join allows you to combine variables from two tables. It first matches observations by their keys, then copies across variables from one table to the other.

Like `mutate()`, the join functions add variables to the right, so if you have a lot of variables already, the new variables won’t get printed out. For these examples, we’ll make it easier to see what’s going on in the examples by creating a narrower dataset:

```{r 13.4 Mutating joins 1}
flights2 <- flights %>% 
  select(year:day,hour,origin, dest,tailnum,carrier)
flights2
```

(Remember, when you’re in RStudio, you can also use `View()` to avoid this problem.)

magine you want to add the full airline name to the `flights2` data. You can combine the `airlines` and `flights2` data frames with `left_join()`:
```{r 13.4 Mutating joins 2}
# airlines data
airlines

# left_join()
flights2 %>% 
  select(-origin,-dest) %>% 
  left_join(airlines,by="carrier")
```

The result of joining airlines to flights2 is an additional variable: `name`. This is why I call this type of join a mutating join. In this case, you could have got to the same place using `mutate()` and R’s base subsetting:
```{r 13.4 Mutating joins 3}
flights2 %>% 
  mutate(name=airlines$name[match(carrier,airlines$carrier)])
```

But this is hard to generalise when you need to match multiple variables, and takes close reading to figure out the overall intent.

The following sections explain, in detail, how mutating joins work. You’ll start by learning a useful visual representation of joins. We’ll then use that to explain the four mutating join functions: the inner join, and the three outer joins. When working with real data, keys don’t always uniquely identify observations, so next we’ll talk about what happens when there isn’t a unique match. Finally, you’ll learn how to tell dplyr which variables are the keys for a given join

### 13.4.1 Understanding joins
To help you learn how joins work, I’m going to use a visual representation:
```{r 13.4.1 Understanding joins 1}
x <- tribble(
  ~key,~val_x,
  1,"x1",
  2,"x2",
  3,"x3"
)

y <- tribble(
  ~key,~val_y,
  1,"y1",
  2,"y2",
  4,"y3"
)
```

The coloured column represents the “key” variable: these are used to match the rows between the tables. The grey column represents the “value” column that is carried along for the ride. In these examples I’ll show a single key variable, but the idea generalises in a straightforward way to multiple keys and multiple values.

A join is a way of connecting each row in `x` to zero, one, or more rows in `y`. The following diagram shows each potential match as an intersection of a pair of lines.

(If you look closely, you might notice that we’ve switched the order of the key and value columns in `x`. This is to emphasise that joins match based on the key; the value is just carried along for the ride.)

In an actual join, matches will be indicated with dots. The number of dots = the number of matches = the number of rows in the output.

### 13.4.2 Inner join
The simplest type of join is the inner join. An inner join matches pairs of observations whenever their keys are equal:

(To be precise, this is an inner equijoin because the keys are matched using the equality operator. Since most joins are equijoins we usually drop that specification.)

The output of an inner join is a new data frame that contains the key, the x values, and the y values. We use `by` to tell dplyr which variable is the key:
```{r 13.4.2 Inner join 1}
x %>% 
  inner_join(y,by="key")
```

The most important property of an inner join is that unmatched rows are not included in the result. This means that generally inner joins are usually not appropriate for use in analysis because it’s too easy to lose observations.

### 13.4.3 Outer joins
An inner join keeps observations that appear in both tables. An outer join keeps observations that appear in at least one of the tables. There are three types of outer joins:

- A left join keeps all observations in `x`.
- A right join keeps all observations in `y`.
- A full join keeps all observations in `x` and `y`.

These joins work by adding an additional “virtual” observation to each table. This observation has a key that always matches (if no other key matches), and a value filled with `NA`.

The most commonly used join is the left join: you use this whenever you look up additional data from another table, because it preserves the original observations even when there isn’t a match. The left join should be your default join: use it unless you have a strong reason to prefer one of the others.

Another way to depict the different types of joins is with a Venn diagram:

However, this is not a great representation. It might jog your memory about which join preserves the observations in which table, but it suffers from a major limitation: a Venn diagram can’t show what happens when keys don’t uniquely identify an observation.

### 13.4.4 Duplicate keys
So far all the diagrams have assumed that the keys are unique. But that’s not always the case. This section explains what happens when the keys are not unique. There are two possibilities:

1. One table has duplicate keys. This is useful when you want to add in additional information as there is typically a one-to-many relationship.

Note that I’ve put the key column in a slightly different position in the output. This reflects that the key is a primary key in `y` and a foreign key in `x`.
```{r 13.4.4 Duplicate keys 1}
x <- tribble(
  ~key, ~val_x,
     1, "x1",
     2, "x2",
     2, "x3",
     1, "x4"
)
y <- tribble(
  ~key, ~val_y,
     1, "y1",
     2, "y2"
  )

left_join(x,y,by="key")
```

2. Both tables have duplicate keys. This is usually an error because in neither table do the keys uniquely identify an observation. When you join duplicated keys, you get all possible combinations, the Cartesian product:
```{r 13.4.4 Duplicate keys 2}
x <- tribble(
  ~key, ~val_x,
     1, "x1",
     2, "x2",
     2, "x3",
     3, "x4"
)
y <- tribble(
  ~key, ~val_y,
     1, "y1",
     2, "y2",
     2, "y3",
     3, "y4"
)
x
y
left_join(x, y, by = "key")
```

### 13.5.6 Defining the key columns
So far, the pairs of tables have always been joined by a single variable, and that variable has the same name in both tables. That constraint was encoded by `by = "key"`. You can use other values for `by` to connect the tables in other ways:

- The default, `by = NULL`, uses all variables that appear in both tables, the so called natural join. For example, the flights and weather tables match on their common variables: `year`, `month`, `day`, `hour` and `origin`.
```{r 13.5.6 Defining the key columns 1}
flights2 %>% 
  left_join(weather)
```

- A character vector, `by = "x"`. This is like a natural join, but uses only some of the common variables. For example, flights and planes have year variables, but they mean different things so we only want to join by `tailnum`.
```{r 13.5.6 Defining the key columns 2}
flights2
planes
flights2 %>% 
  left_join(planes,by="tailnum")
```

Note that the `year` variables (which appear in both input data frames, but are not constrained to be equal) are disambiguated in the output with a suffix.

A named character vector: `by = c("a" = "b")`. This will match variable `a` in table `x` to variable `b` in table `y`. The variables from x will be used in the output.

For example,if we want to draw a map we need to combine the flights data with the airports data which contains the location (`lat` and `long`) of each airport. Each flight has an origin and destination airport, so we need to specify which one we want to join to:
```{r 13.5.6 Defining the key columns 3}
flights
airports
flights %>% 
  left_join(airports,by=c("dest"="faa"))

flights %>% 
  left_join(airports,by=c("origin"="faa"))
```

### 13.4.6 Exercises
1. Compute the average delay by destination, then join on the `airports` data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
```{r}
# install.packages("maps")
airports %>% 
  semi_join(flights,c("faa"="dest")) %>% 
ggplot(aes(lon,lat))+
  borders("state")+
  geom_point()+
  coord_quickmap()
```

(Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)

You might want to use the size or colour of the points to display the average delay for each airport.
```{r}
library(tidyverse)
library(nycflights13)
library(maps)

flights %>%
  mutate(tot_delay = arr_delay + dep_delay) %>%
  group_by(dest) %>%
  summarize(avg_delay = mean(tot_delay, na.rm = TRUE)) %>%
  left_join(select(airports, faa, lon, lat), c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, colour = avg_delay)) +
  borders("state") +
  geom_point(size = 2, alpha = 0.8) +
  xlim(c(-130, -65)) +
  ylim(c(20, 50)) +
  coord_quickmap() +
  viridis::scale_color_viridis()
```


2. Add the location of the origin and destination (i.e. the lat and lon) to flights.
```{r}
flights %>% 
  left_join(select(airports,faa,lat,lon),by=c("origin"="faa")) %>% 
  rename(lat_origin=lat,
         lon_origin=lon) %>% 
  left_join(select(airports,faa,lat,lon),by=c("dest"="faa")) %>% 
  rename(lat_dest=lat,
         lon_dest=lon) %>% 
  select(origin,dest,matches("lat|lon"))
```

3. Is there a relationship between the age of a plane and its delays?
```{r 13.4.6-3}
flights %>% 
  mutate(tot_delay=arr_delay+dep_delay) %>% 
  group_by(tailnum) %>% 
  summarize(avg_delay=mean(tot_delay,na.rm = T)) %>% 
  left_join(select(planes,tailnum,year),by="tailnum") %>% 
  mutate(year=2013-year) %>% 
  ggplot(aes(avg_delay,year))+
  geom_point()+
  geom_smooth()
```

From a very preliminary view, there doesn't seem to be, although the some more older planes have very short delays and some younger planes have very high delays. This pattern however could be due to other things such as the origin/destionation.

4. What weather conditions make it more likely to see a delay?
```{r 13.4.6-4}
avg_del <- 
  flights %>% 
  mutate(tot_delay=arr_delay+dep_delay) %>% 
  group_by(month,day) %>% 
  summarize(avg_delay=mean(tot_delay,na.rm=T))

avg_weather <- 
  weather %>% 
  group_by(month,day) %>% 
  select(-hour) %>% 
  summarize_at(vars(temp,humid,wind_speed,precip),mean,na.rm=T)

avg_del %>%
  left_join(avg_weather) %>%
  ungroup() %>% 
  mutate(avg_delay = cut_width(tot_delay, 35)) %>%
  gather(weather, metrics, -(month:avg_delay)) %>%
  ggplot(aes(avg_delay, metrics)) +
  geom_boxplot() +
  facet_wrap(~ weather, scales = "free_y")
```

Humidity seems to be the one more related to delays, although this is in isolation. A more thorough approach would be to create a grid of all possible combinations of weather conditions and match them up with the delay and then compare the combination of weather conditions with the highest delays.

5. What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.
```{r 13.4.6-5}
flights %>% 
  mutate(tot_delay=arr_delay+dep_delay) %>% 
  group_by(month,day,dest) %>% 
  summarize(avg_delay=mean(tot_delay,na.rm=T)) %>% 
  filter(month==6,day==13) %>% 
  left_join(select(airports,faa,lat,lon),by=c("dest"="faa")) %>% ggplot(aes(lon,lat,color=avg_delay))+
  borders("state")+
  geom_point(size=2,alpha=0.8)+
  xlim(c(-130,-65))+
  ylim(c(20,50))+
  coord_quickmap()+
  viridis::scale_color_viridis()
  
```


### 13.4.7 Other implementations
`base::merge()` can perform all four types of mutating join:

dply | merge
inner_join(x,y) | merge(x,y)
left_join(x,y) | merge(x,y,all.x=T)
right_join(x,y) | merge(x,y,all.y=T)
full_join(x,y) | merge(x,y,all.x=T,all.y=T)

## 13.5 Filtering joins
Filtering joins match observations in the same way as mutating joins, but affect the observations, not the variables. There are two types:

- `semi_join(x, y)` keeps all observations in `x` that have a match in `y`.
- `anti_join(x, y)` drops all observations in `x` that have a match in `y`.

Semi-joins are useful for matching filtered summary tables back to the original rows. For example, imagine you’ve found the top ten most popular destinations:
```{r 13.5 Filtering joins 1}
top_dest <- flights %>% 
  count(dest,sort=T) %>% 
  head(10)
top_dest
```

Now you want to find each flight that went to one of those destinations. You could construct a filter yourself:
```{r 13.5 Filtering joins 2}
flights %>% 
  filter(dest %in% top_dest$dest) %>% 
  head(10)
```

But it’s difficult to extend that approach to multiple variables. For example, imagine that you’d found the 10 days with highest average delays. How would you construct the filter statement that used `year`, `month`, and `day` to match it back to `flights`?

Instead you can use a semi-join, which connects the two tables like a mutating join, but instead of adding new columns, only keeps the rows in `x` that have a match in `y`:
```{r 13.5 Flitering joins 3}
flights %>% 
  semi_join(top_dest)
```

Graphically, a semi-join looks like this:
Only the existence of a match is important; it doesn’t matter which observation is matched. This means that filtering joins never duplicate rows like mutating joins do:

Anti-joins are useful for diagnosing join mismatches. For example, when connecting `flights` and `planes`, you might be interested to know that there are many `flights` that don’t have a match in planes:
```{r 13.5 Filtering joins 4}
flights %>% 
  anti_join(planes,by=c("tailnum")) %>% 
  count(tailnum,sort=T)

```

### 13.5.1 Exercises
1. What does it mean for a flight to have a missing tailnum? What do the tail numbers that don’t have a matching record in planes have in common? (Hint: one variable explains ~90% of the problems.)
```{r 13.5.1-1}
flights %>% 
  anti_join(planes,by=c("tailnum")) %>% 
  count(carrier,sort=T)
```

AA and MQ don't seem to report tail numbers.

2. Filter flights to only show flights with planes that have flown at least 100 flights.
```{r 13.5.1-2}
flights %>% 
  semi_join(count(flights,tailnum)%>% 
              filter(n>=100))
```

3. Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.
```{r 13.5.1-1}
fueleconomy::common
fueleconomy::vehicles

ten_common <- 
  fueleconomy::common %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  top_n(10,n)

fueleconomy::vehicles %>% 
  semi_join(ten_common)
```

4. Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?
```{r 13.5.1-4}
fn <-
  flights %>%
  group_by(month, day) %>%
  summarize(avg_delay = sum(arr_delay + dep_delay, na.rm = TRUE)) %>%
  mutate(twoday_delay = avg_delay + lag(avg_delay)) %>%
  arrange(-twoday_delay)

wea <-
  weather %>%
  group_by(month, day) %>%
  summarize_at(vars(humid, precip, temp), mean, na.rm = TRUE)

fn %>%
  left_join(wea) %>%
  arrange(twoday_delay)
```

5. What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?
```{r 13.5.1-5}
anti_join(flights, airports, by = c("dest" = "faa"))
# Give me the flights from the destionations that that are not present in the `airports` dataset.


anti_join(airports, flights, by = c("faa" = "dest"))
# Give me the airports that are not present as destinations in the `flights` dataset.
```

6. You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned above.
```{r 13.5.1-6}
flights %>%
  group_by(tailnum, carrier) %>%
  count() %>%
  filter(n() > 1) %>%
  select(tailnum) %>%
  distinct(tailnum)
```

## 13.6 Join problems
The data you’ve been working with in this chapter has been cleaned up so that you’ll have as few problems as possible. Your own data is unlikely to be so nice, so there are a few things that you should do with your own data to make your joins go smoothly.

1. Start by identifying the variables that form the primary key in each table. You should usually do this based on your understanding of the data, not empirically by looking for a combination of variables that give a unique identifier. If you just look for variables without thinking about what they mean, you might get (un)lucky and find a combination that’s unique in your current data but the relationship might not be true in general.

For example, the altitude and longitude uniquely identify each airport, but they are not good identifiers!
```{r}
airports %>% 
  count(alt,lon) %>% 
  filter(n>1)
```

2. Check that none of the variables in the primary key are missing. If a value is missing then it can’t identify an observation!
3. Check that your foreign keys match primary keys in another table. The best way to do this is with an `anti_join()`. It’s common for keys not to match because of data entry errors. Fixing these is often a lot of work.

If you do have missing keys, you’ll need to be thoughtful about your use of inner vs. outer joins, carefully considering whether or not you want to drop rows that don’t have a match.

Be aware that simply checking the number of rows before and after the join is not sufficient to ensure that your join has gone smoothly. If you have an inner join with duplicate keys in both tables, you might get unlucky as the number of dropped rows might exactly equal the number of duplicated rows!

## 13.7 Set operations
The final type of two-table verb are the set operations. Generally, I use these the least frequently, but they are occasionally useful when you want to break a single complex filter into simpler pieces. All these operations work with a complete row, comparing the values of every variable. These expect the `x` and `y` inputs to have the same variables, and treat the observations like sets:

- `intersect(x, y)`: return only observations in both x and y.
- `union(x, y)`: return unique observations in x and y.
- `setdiff(x, y)`: return observations in x, but not in y.

Given the sample data:
```{r}
df1 <- tribble(
  ~x, ~y,
   1,  1,
   2,  1
)
df2 <- tribble(
  ~x, ~y,
   1,  1,
   1,  2
)
```

The four possibilities are: 
```{r}
intersect(df1,df2)
union(df1,df2)
setdiff(df1,df2)
setdiff(df2,df1)
```

# Chapter 14: Strings
## 14.1 Introduction
This chapter introduces you to string manipulation in R. You’ll learn the basics of how strings work and how to create them by hand, but the focus of this chapter will be on regular expressions, or regexps for short. Regular expressions are useful because strings usually contain unstructured or semi-structured data, and regexps are a concise language for describing patterns in strings. When you first look at a regexp, you’ll think a cat walked across your keyboard, but as your understanding improves they will soon start to make sense

### 14.1.1 Prerequisites
This chapter will focus on the `stringr` package for string manipulation. stringr is not part of the core tidyverse, because you don't always have textual data, so we need to load it explicitly.
```{r 14.1.1}
library(tidyverse)
library(stringr)
```

## 14.2 String basics
You can create strings with either single quotes or double quotes. Unlike other languages, there is no difference in behaviour. I recommend always using `"`, unless you want to create a string that contains multiple `"`.
```{r 14.2-1}
string1 <- "This is a string"
string2 <- `IF I want to include "quote" inside a string, I use single quotes`
```

If you forget to close a quote, you’ll see `+`, the continuation character:
```{r 14.2-2}
"This is a string without a closing quote
```

If this happen to you, press Escape and try again!

To include a literal single or double quote in a string you can use `\` to “escape” it:
```{r 14.2-3}
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
```

That means if you want to include a literal backslash, you’ll need to double it up: `"\\"`.

Beware that the printed representation of a string is not the same as string itself, because the printed representation shows the escapes. To see the raw contents of the string, use `writeLines()`:

There are a handful of other special characters. The most common are `"\n"`, newline, and `"\t"`, tab, but you can see the complete list by requesting help on `": ?'"'`, or `?"'"`. You’ll also sometimes see strings like `"\u00b5"`, this is a way of writing non-English characters that works on all platforms:
```{r 14.2-4}
x <- "\u00b5"
x
```

Multiple strings are often stored in a character vector, which you can create with `c()`:
```{r 14.2-5}
c("one","two","three")
```

### 14.2.1 String length
Base R contains many functions to work with strings but we’ll avoid them because they can be inconsistent, which makes them hard to remember. Instead we’ll use functions from stringr. These have more intuitive names, and all start with `str_`. For example, `str_length()` tells you the number of characters in a string:
```{r 14.2.1-1}
str_length(c("a","R for data science",NA))
```

The common `str_ prefix` is particularly useful if you use RStudio, because typing `str_` will trigger autocomplete, allowing you to see all stringr functions:

### 14.2.2 Combining strings
To combine two or more strings, use `str_c()`:
```{r 14.2.2-1}
str_c("x","y")
str_c("x","y","z")
```

Use the `sep` argument to control how they're separated:
```{r 14.2.2-2}
str_c("x","y",sep=",")
```

Like most other functions in R, missing values are contagious. If you want them to print as `"NA"`, use `str_replace_na()`:
```{r 14.2.2-3}
x <- c("abc",NA)
str_c("|-",x,"-|")
str_c("|-",str_replace_na(x),"-|")
```

As shown above, `str_c()` is vectorised, and it automatically recycles shorter vectors to the same length as the longest:
```{r 14.2.2-4}
t <- str_c("prefix-",c("a","b","c"),"-suffix")
class(t)
```

Objects of length 0 are silently dropped. This is particularly useful in conjunction with `if`:
```{r 14.2.2-5}
name <- "Hadley"
time_of_day <- "Morning"
birthday <- FALSE

str_c(
  "Good",time_of_day,"",name,
  if(birthday) "and HAPPY BIRTHDAY","."
)

```

To collapse a vector of strings into a single string, use collapse:
```{r 14.2.2-6}
str_c(c("x","y","z"),collapse=",")
```

### 14.2.3 Subsetting strings
You can extract parts of a string using `str_sub()`. As well as the string, `str_sub()` takes `start` and `end` arguments which give the (inclusive) position of the substring:
```{r 14.2.3-1}
x <- c("apple","Banana","Pear")
str_sub(x,1,3)

# negative numvers ccount backwards from end
str_sub(x,-3,-1)
```

Note that `str_sub()` won’t fail if the string is too short: it will just return as much as possible:
```{r 14.2.3-2}
str_sub("a",1,5)
```

You can also use the assigment form of `str_sub()` to modify strings:
```{r 14.2.3-3}
str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))
x
```

### 14.2.4 Locales
Above I used `str_to_lower()` to change the text to lower case. You can also use `str_to_upper()` or `str_to_title()`. However, changing case is more complicated than it might at first appear because different languages have different rules for changing case. You can pick which set of rules to use by specifying a locale:
```{r 14.2.4-1}
# Turkish has two i's: with and without a dot, and it
# has a different rule for capitalising them:
str_to_upper(c("i", "ı"))
#> [1] "I" "I"
str_to_upper(c("i", "ı"), locale = "tr")
#> [1] "İ" "I"
```

The locale is specified as a ISO 639 language code, which is a two or three letter abbreviation. If you don’t already know the code for your language, Wikipedia has a good list. If you leave the locale blank, it will use the current locale, as provided by your operating system.

Another important operation that’s affected by the locale is sorting. The base R `order()` and `sort()` functions sort strings using the current locale. If you want robust behaviour across different computers, you may want to use `str_sort()` and `str_order()` which take an additional locale argument:
```{r 14.2.4-2}
x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English
#> [1] "apple"    "banana"   "eggplant"

str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple"    "eggplant" "banana"
```

### 14.2.5 Exercises
1. In code that doesn’t use stringr, you’ll often see `paste()` and `paste0()`. What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of `NA`?

`paste` and `paste0` are the same but paste0 has `sep = ""` by default and paste has `sep = " "` by default.

`str_c` is the equivalent stringr function.
```{r 14.2.5-1}
str_c(c("a", "b"), NA)
# In `str_c` everything that is pasted with an NA is an NA

paste0(c("a", "b"), NA)
# But in paste0 NA gets converted to a character string a pasted together. To mimic the same behaviour, replace the NA to a string with:
str_c(c("a", "b"), str_replace_na(NA))
```

2. In your own words, describe the difference between the sep and collapse arguments to str_c().

`sep` is what divides what you paste together within a vector of strings. collapse is the divider of a single pasted vector of strings.

3. Use `str_length()` and `str_sub()` to extract the middle character from a string. What will you do if the string has an even number of characters?
```{r}
library(tidyverse)
library(stringr)
uneven <- "one"
even <- "thre"

str_sub(even,str_length(uneven)/2,str_length(uneven)/2)

# Automatically rounds up the lower digit
str_sub(uneven, str_length(uneven) / 2, str_length(uneven) / 2)
```

4. What does str_wrap() do? When might you want to use it?
```{r}
str_wrap(
  "Hey,so this is one paragraph
  I'm interested in writing but I 
  think it might be too long. I just
  want to make sure this is in the right format",
  width = 60,indent=2,exdent=1
) %>% cat()
```

5. What does str_trim() do? What’s the opposite of str_trim()?
6. Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.
```{r}
str_paster <- function(x, collapse = ", ") {
  str_c(x, collapse = collapse)
}

tr <- letters[1:3]
str_paster(tr)

tr <- letters[1:2]
str_paster(tr)

tr <- letters[1]
str_paster(tr)

tr <- letters[0]
str_paster(tr)
```

## 14.3 Matching patterns with regular expressions
Regexps are a very terse language that allow you to describe patterns in strings. They take a little while to get your head around, but once you understand them, you’ll find them extremely useful.

To learn regular expressions, we’ll use `str_view()` and `str_view_all()`. These functions take a character vector and a regular expression, and show you how they match. We’ll start with very simple regular expressions and then gradually get more and more complicated. Once you’ve mastered pattern matching, you’ll learn how to apply those ideas with various stringr functions.

### 14.3.1 Basic matches
The simplest patterns match exact strings:
```{r 14.3.1-1}
x <- c("apple","banana","pear")
str_view(x,"an")
```

The next step up in complexity is `.`, which matches any character (except a newline):
```{r 14.3.1-2}
str_view(x,".a.")
```

But if “`.`” matches any character, how do you match the character “`.`”? You need to use an “escape” to tell the regular expression you want to match it exactly, not use its special behaviour. Like strings, regexps use the backslash, `\`, to escape special behaviour. So to match an `.`, you need the regexp `\.`. Unfortunately this creates a problem. We use strings to represent regular expressions, and `\` is also used as an escape symbol in strings. So to create the regular expression `\.` we need the string "`\\.`".

```{r 14.3.1-3}
# to create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)

# tells R to look for an explicit
str_view(c("abc","a.c","bef"),"a\\.c")
```

If `\` is used as an escape character in regular expressions, how do you match a literal `\`? Well you need to escape it, creating the regular expression `\\`. To create that regular expression, you need to use a string, which also needs to escape `\`. That means to match a literal `\` you need to write "`\\\\`" — you need four backslashes to match one!
```{r 14.3.1-4}
x <- "a\\b"
writeLines(x)

str_view(x,"\\\\")
```

In this book, I’ll write regular expression as `\`. and strings that represent the regular expression as "`\\.`".

#### 14.3.1.1 Exercises
1. Explain why each of these strings don’t match a \: "\", "\\", "\\\".

"" won't match anything because "" needs to be accompanied by two "`\`" to escape "" "" won't match "`\`" because because "" is actualy "`\`" and needs double escaping so "`\\`" will match it.

Same for "`\`".

2. How would you match the sequence "`'\`?
str_view("`"'\`", ""`'\\`")

3. What patterns will the regular expression `\..\..\..` match? How would you represent it as a string?

It mataches a string similar to .a.b.c. So every `.` matches a literal dot and . mataches any character except a new line.
```{r}
str_view(".a.b.c","\\..\\..\\..")
```

### 14.3.2 Anchors
By default, regular expressions will match any part of a string. It’s often useful to anchor the regular expression so that it matches from the start or end of the string. You can use:
 - `^` to match the start of the string
 - `$` to match the end of the string
```{r 14.3.2-1}
x <- c("apple","banana","pear")

# ^ for start of the string
str_view(x,"^a")

# $ for the end of the string
str_view(x,"a$")
```

To remember which is which, try this mnemonic which I learned from Evan Misshula: if you begin with power (`^`), you end up with money (`$`).

To force a regular expression to only match a complete string, anchor it with both `^` and `$`:
```{r 14.3.2-2}
x <- c("apple pie","apple","apple cake")
str_view(x,"apple")

str_view(x,"^apple$")
```

You can also match the boundary between words with `\b`. I don’t often use this in R, but I will sometimes use it when I’m doing a search in RStudio when I want to find the name of a function that’s a component of other functions. For example, I’ll search for `\bsum\b` to avoid matching summarise, summary, rowsum and so 


#### 14.3.2.1 Exercies

1. How would you match the literal string "`$^$`"?

Given the corpus of common words in stringr::words, create regular expressions that find all words that:

2. Given the corpus of common words in `stringr::words`, create regular expressions that find all words that:

- Start with “y”.
```{r}
str_bring <- function(string, pattern) {
  string[str_detect(string, pattern)]
}
str_bring(words,"^y")
```

- End with “x”
```{r}
words[str_detect(words,"x$")]
str_bring(words,"x$")
```

- Are exactly three letters long. (Don’t cheat by using str_length()!)
```{r}
str_bring(words,"^.{3}$")

str_length <- str_length(words)
words[str_length==3]
```

- Have seven letters or more.
```{r}
str_bring(words,"^.{7,}$")
```

Since this list is long, you might want to use the`match` argument to `str_view()` to show only the matching or non-matching words.

### 14.3.3 Character classes and alternatives
There are a number of special patterns that match more than one character. You’ve already seen ., which matches any character apart from a newline. There are four other useful tools:

- `\d`: matches any digit.
- `\s`: matches any whitespace (e.g. space, tab, newline).
- `[abc]`: matches a, b, or c.
- `[^abc]`: matches anything except a, b, or c.

Remember, to create a regular expression containing `\d` or `\s`, you’ll need to escape the `\` for the string, so you’ll type "`\\d`" or "`\\s`".

You can use alternation to pick between one or more alternative patterns. For example, `abc|d..f` will match either ‘“abc”’, or "deaf". Note that the precedence for `|` is low, so that `abc|xyz` matches `abc` or `xyz` not `abcyz` or `abxyz`. Like with mathematical expressions, if precedence ever gets confusing, use parentheses to make it clear what you want:
```{r 14.3.4-1}
str_view(c("grey","gray"),"gr(e|a)y")
```

#### 14.3.3.1 Exercies
1. Create regular expressions to find all words that:
- Start with a vowel.
- That only contain consonants. (Hint: thinking about matching “not”-vowels.)
- End with ed, but not with eed.
- End with ing or ise.
```{r}
str_bring(words,"^[aeiou]")
str_bring(words,"^[^aeiou]")
str_bring(words,"[^e]ed$")
str_bring(words,"(ing|ise)$")
```

2. Empirically verify the rule “i before e except after c”.
```{r}
str_bring(words,"ie[^c]ie")
```

3. Is “q” always followed by a “u”?
```{r}
str_bring(words,"q[^u]")
```

4. Write a regular expression that matches a word if it’s probably written in British English, not American English.
```{r}
str_bring(words,"ou|ise^|ae|oe|yse^")
```

5. Create a regular expression that will match telephone numbers as commonly written in your country.
```{r}
x <- c("34697382009", "18093438932", "18098462020")
str_bring(x,"^34.{9}$")
```

### 14.3.4 Repetition
The next step up in power involves controlling how many times a pattern matches:

- `?`: 0 or 1
- `+`: 1 or more
- `*`0 or more
```{r 14.3.4-1}
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x,"CC?")
```

1888 is the longest year in Roman numerals: MD`CC`CLXXXVIII
```{r}
str_view(x,"CC+")
```

1888 is the longest year in Roman numerials MD`CCC`LXXXVIII
```{r}
str_view(x,'C[LX]+')
```

Note that the precedence of these operators is high, so you can write: `colou?r` to match either American or British spellings. That means most uses will need parentheses, like bana(na)+

You can also specify the number of matches precisely:
- `{n}`: exactly n
- `{n,}`: n or more
- `{,m}`: at most m
- `{n,m}`: between n and m
```{r}
str_view(x,"C{2,}")
```

```{r}
str_view(x,"C{2,3}")
```

By default these matches are “greedy”: they will match the longest string possible. You can make them “lazy”, matching the shortest string possible by putting a `?` after them. This is an advanced feature of regular expressions, but it’s useful to know that it exists:
```{r}
str_view(x,'C[LX]+?')
```

#### 13.3.4.1 Exercises
1. Describe the equivalents of ?, +, * in {m,n} form.
? is {,1}
is {1,}
has no equivalent

2. Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
- ^.*$
- "\\{.+\\}"
- \d{4}-\d{2}-\d{2}
- "\\\\{4}"

^.*$

Matches any string

"\{.+\}"

Matches any string with curly braces.

\d{4}-\d{2}-\d{2}

Matches a set of numbers in this format dddd-dd-dd

"\\{4}"

It matches four back slashes.

3. Create regular expressions to find all words that:
- Start with three consonants.
- Have three or more vowels in a row.
- Have two or more vowel-consonant pairs in a row.
```{r}
str_bring(words,"^[^aeiou]{3}")
str_bring(words,"[aeiou]{3,}")
str_bring(words,"[^aeiou][aeiou]{2,}")
```

4. Solve the beginner regexp crosswords at https://regexcrossword.com/challenges/beginner.

### 14.3.5 Grouping and backreference
Earlier, you learned about parentheses as a way to disambiguate complex expressions. They also define “groups” that you can refer to with backreferences, like `\1`, `\2` etc. For example, the following regular expression finds all fruits that have a repeated pair of letters
```{r}
str_view(fruit,"(..)\\1",match=T)
```

#### 14.3.5.1 Exercises
1. Describe in words, what these expressions will match:
2. Construct regular expressions to match words that
- Start and end with the same character.
- Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)
- Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)


## 14.4 Tools
Now that you’ve learned the basics of regular expressions, it’s time to learn how to apply them to real problems. In this section you’ll learn a wide array of stringr functions that let you:
- Determine which strings match a pattern.
- Find the positions of matches.
- Extract the content of matches.
- Replace matches with new values.
- Split a string based on a match.

A word of caution before we continue: because regular expressions are so powerful, it’s easy to try and solve every problem with a single regular expression. In the words of Jamie Zawinski:
> Some people, when confronted with a problem, think “I know, I’ll use regular expressions.” Now they have two problems.

As a cautionary tale, check out this regular expression that checks if a email address is valid:

This is a somewhat pathological example (because email addresses are actually suprisingly complex), but is used in real code. See the stackoverflow discussion at http://stackoverflow.com/a/201378 for more details.

Don’t forget that you’re in a programming language and you have other tools at your disposal. Instead of creating one complex regular expression, it’s often easier to write a series of simpler regexps. If you get stuck trying to create a single regexp that solves your problem, take a step back and think if you could break the problem down into smaller pieces, solving each challenge before moving onto the next one.

### 14.4.1 Detect matches
To determine if a character vector matches a pattern, use `str_detect()`. It returns a logical vector the same length as the input:
```{r 14.4.1-1}
x <- c("apple","banana","pear")
str_detect(x,"e")
```

Remember that when you use a logical vector in a numeric context, FALSE becomes `0` and TRUE becomes `1`. That makes `sum()` and `mean()` useful if you want to answer questions about matches across a larger vector:
```{r}
str_bring(words,"^t")
# how many common words start with t?
sum(str_detect(words,"^t"))
# what proportion of common words end with a vowel?
mean(str_detect(words,"[aeiou]$"))
```

When you have complex logical conditions (e.g. match a or b but not c unless d) it’s often easier to combine multiple` str_detect()` calls with logical operators, rather than trying to create a single regular expression. For example, here are two ways to find all words that don’t contain any vowels:
```{r}
# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)
#> [1] TRUE

identic <- function(x,y){
  if (x==y){
    return("TRUE")
  }else{
    return("FALSE")}
}
identic(no_vowels_1,no_vowels_2)
```

The results are identical, but I think the first approach is significantly easier to understand. If your regular expression gets overly complicated, try breaking it up into smaller pieces, giving each piece a name, and then combining the pieces with logical operations.

A common use of `str_detect()` is to select the elements that match a pattern. You can do this with logical subsetting, or the convenient `str_subset()` wrapper:
```{r}
words[str_detect(words,"x$")]
str_subset(words,"x$")
```

Typically, however, your strings will be one common of a data frame, and you wll want to use filter instead:
```{r}
df <- tibble(
  word=words,
  i=seq_along(word)
)
df %>% 
  filter(str_detect(words,"x$"))
```

A variation on `str_detect()` is `str_count()`: rather than a simple yes or no, it tells you how many matches there are in a string:
```{r}
x <- c("apple","banana","pear")
str_count(x,"a")

# on average, how may vowels per word?
mean(str_count(words,"[aeiou]"))
```

It’s natural to use `str_count()` with `mutate()`:
```{r}
df %>% 
  mutate(
    vowels =str_count(word,"[aeiou]"),
    consonants=str_count(word,"[^aeiou]"))
```

Note that matches never overlap. For example, in "`abababa`", how many times will the pattern "aba" match? Regular expressions say two, not three:
```{r}
library(stringr)
str_count("abababa","aba")
str_view_all("abababa","aba")
```

Note the use of `str_view_all()`. As you’ll shortly learn, many stringr functions come in pairs: one function works with a single match, and the other works with all matches. The second function will have the suffix `_all`.

### 14.4.2 Exercises
1. For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.
- Find all words that start or end with x.
```{r}
str_bring(words,"^x|x$")

# OR
start_r <- str_detect(words,"^x")
end_r <- str_detect(words,"x$")
words[start_r|end_r]
```

- Find all words that start with a vowel and end with a consonant.
```{r}
str_bring(words,"^[aeiou].*[^aeiou]$")
```

- Are there any words that contain at least one of each different vowel?
```{r}
vowels <-
  str_detect(words, "a") & str_detect(words, "e") & str_detect(words, "i") &
  str_detect(words, "o") & str_detect(words, "u")

words[vowels]
```

2. What word has the highest number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)
```{r}
words
vowels <- str_count(words,"[aeiou]")
words[which.max(vowels)]
```

### 14.4.3 Extract matches 
To extract the actual text of a match, use `str_extract()`. To show that off, we’re going to need a more complicated example. I’m going to use the Harvard sentences, which were designed to test VOIP systems, but are also useful for practicing regexps. These are provided in `stringr::sentences`:
```{r 14.4.3-1}
length(sentences)
head(sentences)
```

Imagine we want to find all sentences that contain a colour. We first create a vector of colour names, and then turn it into a single regular expression:
```{r 14.4.3-2}
color <- c("red","orange","yellow","green","blue","purple")
color_match <- str_c(color,collapse = "|")
color_match
```

Now we can select the sentences that contain a colour, and then extract the colour to figure out which one it is:
```{r 14.4.3-3}
has_color <- str_subset(sentences,color_match)
matches <- str_extract(has_color,color_match)
head(matches)
```

Note that `str_extract()` only extracts the first match. We can see that most easily by first selecting all the sentences that have more than 1 match:
```{r 14.4.3-4}
more <- sentences[str_count(sentences,color_match)>1]
str_view_all(more,color_match)
```

```{r 14.4.3-5}
str_extract(more,color_match)
```

This is a common pattern for stringr functions, because working with a single match allows you to use much simpler data structures. To get all matches, use `str_extract_all()`. It returns a list:
```{r}
str_extract_all(more,color_match)
```

You’ll learn more about lists in lists and iteration.

If you use `simplify = TRUE`, `str_extract_all()` will return a matrix with short matches expanded to the same length as the longest:
```{r}
str_extract_all(more,color_match,simplify = T)
x <- c("a","a b","a b c")
str_extract_all(x,"[a-z]", simplify = T)
```

#### 14.4.3.1 Exercises
1. In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.

2. From the Harvard sentences data, extract:
- The first word from each sentence.
- All words ending in ing.
- All plurals.

### 14.4.4 Grouped matches
Earlier in this chapter we talked about the use of parentheses for clarifying precedence and for backreferences when matching. You can also use parentheses to extract parts of a complex match. For example, imagine we want to extract nouns from the sentences. As a heuristic, we’ll look for any word that comes after “a” or “the”. Defining a “word” in a regular expression is a little tricky, so here I use a simple approximation: a sequence of at least one character that isn’t a space.
```{r}
noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)
```

str_extract() gives us the complete match; str_match() gives each individual component. Instead of a character vector, it returns a matrix, with one column for the complete match followed by one column for each group:
```{r}
has_noun %>% 
  str_match(noun)
```

(Unsurprisingly, our heuristic for detecting nouns is poor, and also picks up adjectives like smooth and parked.)

If your data is in a tibble, it’s often easier to use `tidyr::extract()`. It works like `str_match()` but requires you to name the matches, which are then placed in new columns:
```{r}
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )
```

Like `str_extract()`, if you want all matches for each string, you’ll need `str_match_all()`.

#### 14.4.4.1
1. Find all words that come after a “number” like “one”, “two”, “three” etc. Pull out both the number and the word.
```{r}
numbers <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
)

number_regexp <- str_c("(", str_c(numbers, collapse = "|"), ")")

regexp <- str_c(number_regexp, " ([^ ]+)")
all_match <- str_match(sentences, regexp)

all_match[complete.cases(all_match), ] %>% head()
```

2. Find all contractions. Separate out the pieces before and after the apostrophe.
```{r}
contract_re <- "([a-zA-Z]+)'([a-zA-Z]+)"
contract <- sentences[str_detect(sentences, contract_re)]

str_match(contract, contract_re)
```

### 14.4.5 Replacing matches
`str_replace()` and `str_replace_all()` allow you to replace matches with new strings. The simplest use is to replace a pattern with a fixed string:
```{r 14.4.5-1}
x <- c("apple","pear","banana")
str_replace(x,"[aeiou]","-")
str_replace_all(x,"[aeiou]","-")
```

With `str_replace_all()` you can perform multiple replacements by supplying a named vector:
```{r 14.4.5-2}
x <- c("1 house", "2 cars","3 people")
str_replace_all(x,c("1"="one","2"="two","3"="three"))
as_tibble(x)
```

Instead of replacing with a fixed string you can use backreferences to insert components of the match. In the following code, I flip the order of the second and third words.
```{r}
sentences %>% 
 str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)
```

#### 14.4.5.1 Exercises
1. Replace all forward slashes in a string with backslashes.
```{r}
str_replace_all(c("hey this is a /", "and another / in the pic"),
                "/", "\\\\")
```

2. Implement a simple version of `str_to_lower()` using `replace_all()`.
```{r}
my_str_lower <- function(x) {
  lower_let <- letters
  names(lower_let) <- LETTERS
  str_replace_all(x, lower_let)
}

identical(my_str_lower(sentences), str_to_lower(sentences))
```

3. Switch the first and last letters in words. Which of those strings are still words?
```{r}
str_replace_all(words, "^([a-z])(.*)([a-z])$", c("\\3\\2\\1"))
```


### 14.4.6 Splitting
Use str_split() to split a string up into pieces. For example, we could split sentences into words:
```{r 14.4.6-1}
sentences %>% 
  head(5) %>% 
  str_split(" ")
```

Because each component might contain a different number of pieces, this returns a list. If you’re working with a length-1 vector, the easiest thing is to just extract the first element of the list:
```{r 14.4.6-2}
"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

```

Otherwise, like the other stringr functions that return a list, you can use `simplify = TRUE` to return a matrix:
```{r 14.4.6-3}
sentences %>% 
  head(5) %>% 
  str_split(" ",simplify = T)
```

You can also request a maximum number of pieces:
```{r 14.4.6-4}
fields <- c("Name:Hadley","Country:NZ","Age:35")
fields %>% str_split(":",n=2,simplify=T)
```

Instead of splitting up strings by patterns, you can also split up by character, line, sentence and word boundary()s:
```{r 14.4.6-5}
x <- "This is a sentence. This is another sentence."
str_view_all(x,boundary("word"))
```

```{r}
str_split(x," ")[[1]]
str_split(x,boundary("word"))[[1]]
```

### 14.4.7 Find matches
`str_locate()` and `str_locate_all()` give you the starting and ending positions of each match. These are particularly useful when none of the other functions does exactly what you want. You can use `str_locate()` to find the matching pattern, `str_sub()` to extract and/or modify them.

## 14.5 Other types of pattern
When you use a pattern that’s a string, it’s automatically wrapped into a call to `regex()`:
```{r}
# regular call
str_view(fruit,"nana")
# short hand
str_view(fruit,regex("nana"))
```

You can use the other arguments of `regex()` to control details of the match:

- `ignore_case = TRUE` allows characters to match either their uppercase or lowercase forms. This always uses the current locale.
```{r}
bananas <- c("banana","banana","BANANA")
str_view(bananas,"banana")

str_view(bananas, regex("banana",ignore_case = T))
```

- `multiline = TRUE` allows `^ `and `$` to match the start and end of each line rather than the start and end of the complete string.
```{r}
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x,"^Line")[[1]]
str_extract_all(x,regex("^Line",multiline = T))
```

- `comments = TRUE` allows you to use comments and white space to make complex regular expressions more understandable. Spaces are ignored, as is everything after `#`. To match a literal space, you’ll need to escape it: `"\\ "`.
```{r}
phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [)- ]?   # optional closing parens, dash, or space
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)
```

- `dotall = TRUE` allows . to match everything, including `\n`.

There are three other functions you can use instead of `regex()`:
- `fixed()`: matches exactly the specified sequence of bytes. It ignores all special regular expressions and operates at a very low level. This allows you to avoid complex escaping and can be much faster than regular expressions. The following microbenchmark shows that it’s about 3x faster for a simple example.
```{r}
# install.packages("microbenchmark")
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)
```

Beware using `fixed()` with non-English data. It is problematic because there are often multiple ways of representing the same character. For example, there are two ways to define “á”: either as a single character or as an “a” plus an accent:
```{r}
a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
#> [1] "á" "á"
a1 == a2
#> [1] FALSE
```

They render identically, but because they’re defined differently, `fixed()` doesn’t find a match. Instead, you can use `coll()`, defined next, to respect human character comparison rules:
```{r}
str_detect(a1,fixed(a2))
str_detect(a1,coll(a2))
```

- `coll()`: compare strings using standard collation rules. This is useful for doing case insensitive matching. Note that `coll()` takes a locale parameter that controls which rules are used for comparing characters. Unfortunately different parts of the world use different rules!
```{r}
# That means you also need to be aware of the difference
# when doing case insensitive matches:
i <- c("I", "İ", "i", "ı")
i
#> [1] "I" "İ" "i" "ı"

str_subset(i, coll("i", ignore_case = TRUE))
#> [1] "I" "i"
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))
#> [1] "İ" "i"
```

Both `fixed()` and `regex()` have ignore_case arguments, but they do not allow you to pick the locale: they always use the default locale. You can see what that is with the following code; more on stringi later.
```{r}
stringi::stri_locale_info()
```

The downside of `coll()` is speed; because the rules for recognising which characters are the same are complicated, `coll()` is relatively slow compared to `regex()` and `fixed()`.

As you saw with `str_split()` you can use `boundary()` to match boundaries. You can also use it with the other functions:
```{r}
x <- "This is a sentence."
str_view_all(x,boundary("word"))
str_extract_all(x,boundary("word"))
```

## 14.6 Other uses of regular expressions
There are two useful function in base R that also use regular expressions:
- `apropos()` searches all objects available from the global environment. This is useful if you can’t quite remember the name of the function.
```{r}
apropos("replace")
```

- `dir()` lists all the files in a directory. The pattern argument takes a regular expression and only returns file names that match the pattern. For example, you can find all the R Markdown files in the current directory with:
```{r}
head(dir(pattern="\\.Rmd$"))
```


## 14.7 Stringi
stringr is built on top of the stringi package. stringr is useful when you’re learning because it exposes a minimal set of functions, which have been carefully picked to handle the most common string manipulation functions. stringi, on the other hand, is designed to be comprehensive. It contains almost every function you might ever need: stringi has 232 functions to stringr’s 43.

If you find yourself struggling to do something in stringr, it’s worth taking a look at stringi. The packages work very similarly, so you should be able to translate your stringr knowledge in a natural way. The main difference is the prefix: `str_ vs`. `stri_`.

# Chapter 15: Factors
## 15.1 Introduction
In R, factors are used to work with categorical variables, variables that have a fixed and known set of possible values. They are also useful when you want to display character vectors in a non-alphabetical order.

Historically, factors were much easier to work with than characters. As a result, many of the functions in base R automatically convert characters to factors. This means that factors often crop up in places where they’re not actually helpful. Fortunately, you don’t need to worry about that in the tidyverse, and can focus on situations where factors are genuinely useful.

For more historical context on factors, I recommend stringsAsFactors: An unauthorized biography by Roger Peng, and stringsAsFactors = <sigh> by Thomas Lumley.

## 15.1.1. Prerequisites
To work with factors, we’ll use the forcats package, which provides tools for dealing with categorical variables (and it’s an anagram of factors!). It provides a wide range of helpers for working with factors. forcats is not part of the core tidyverse, so we need to load it explicitly.
```{r}
library(tidyverse)
library(forcats)
```

## 15.2 Creating factors
Imagine that you have a variable that records month:
```{r 15.2-1}
x1 <- c("Dec","Apr","Jan","Mar")
```

Using a string to record this variable has two problems:

1. There are only twelve possible months, and there’s nothing saving you from typos:
```{r 15.2-2}
x2 <- c("Dec","Apr","Jan","Mar")
```

2. It doesn't sort in a useful way:
```{r 15.2-3}
sort(x1)
```

You can fix both of these problems with a factor. To create a factor you must start by creating a list of the valid levels:
```{r 15.2-4}
month_levels <- c(
  "Jan","Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)
```

Now you can create a factor:
```{r 15.2-5}
y1 <- factor(x1, levels=month_levels)
y1
sort(y1)
```

And any values not in the set will be silently coverted to NA:
```{r 15.2-6}
y2 <- factor(x2,levels=month_levels)
y2
```

If you want a warning, you can use `readr::parse_factor()`:
```{r 15.2-7}
y2 <- parse_factor(x2,levels=month_levels)
```

IF you omit the levels, they'll be taken from the data in alphabetical order:
```{r 15.2-8}
factor(x1)
```

Sometimes you’d prefer that the order of the levels match the order of the first appearance in the data. You can do that when creating the factor by setting levels to unique(x), or after the fact, with `fct_inorder()`:
```{r 15.2-8}
f1 <- factor(x1,levels=unique(x1))
f1

f2 <- f1 %>% factor() %>%  fct_inorder()
f2
```

## 15.3 General Social Survey
For the rest of this chapter, we’re going to focus on `forcats::gss_cat`. It’s a sample of data from the General Social Survey, which is a long-running US survey conducted by the independent research organization NORC at the University of Chicago. The survey has thousands of questions, so in `gss_cat` I’ve selected a handful that will illustrate some common challenges you’ll encounter when working with factors.
```{r 15.3-1}
gss_cat
```

(Remember, since this dataset is provided by a package, you can get more information about the variables with `?gss_cat`.)

When factors are stored in a tibble, you can’t see their levels so easily. One way to see them is with `count()`:
```{r 15.3-2}
gss_cat %>% 
  count(race)
```

or with a bar chart:
```{r 15.3-3}
library(ggplot2)
ggplot(gss_cat,aes(race))+geom_bar()
```

By default, ggplot2 will drop levels that don’t have any values. You can force them to display with:
```{r 15.3-4}
ggplot(gss_cat,aes(race))+
  geom_bar()+
  scale_x_discrete(drop=F)
```

These levels represent valid values that simply did not occur in this dataset. Unfortunately, dplyr doesn’t yet have a `drop` option, but it will in the future.

When working with factors, the two most common operations are changing the order of the levels, and changing the values of the levels. Those operations are described in the sections below.

### 15.3.1 Exercise
1. Explore the distribution of rincome (reported income). What makes the default bar chart hard to understand? How could you improve the plot?
```{r}
head(gss_cat)
gss_cat %>% 
  ggplot(aes(rincome))+
  geom_bar()

gss_cat %>% 
  mutate(rincome=
           fct_relevel(rincome,
                       c("No answer","Don't know","Refused","Not applicable"))) %>% 
  ggplot(aes(rincome))+
  geom_bar()+
  coord_flip()
  
```


2. What is the most common relig in this survey? What’s the most common partyid?
```{r}
gss_cat %>% 
  count(relig) %>% 
  arrange(-n)
```

3. Which relig does denom (denomination) apply to? How can you find out with a table? How can you find out with a visualisation?
```{r}
gss_cat %>% 
  count(relig,denom) %>% 
  filter(denom=="No denomination")
```

## 15.4 Modfying factor order
It’s often useful to change the order of the factor levels in a visualisation. For example, imagine you want to explore the average number of hours spent watching TV per day across religions:
```{r 15.4-1}
relig_summary <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age=mean(age,na.rm=TRUE),
    tvhours = mean(tvhours,na.rm=T),
    n=n()
  )

ggplot(relig_summary,aes(tvhours,relig))+geom_point()
ggplot(relig_summary,aes(age,relig))+geom_point()
ggplot(relig_summary,aes(tvhours,age,label=relig))+geom_point()+geom_abline()+geom_label()
```

It is difficult to interpret this plot because there’s no overall pattern. We can improve it by reordering the levels of `relig` using `fct_reorder()`. `fct_reorder()` takes three arguments:

- `f`, the factor whose levels you want to modify.
- `x`, a numeric vector that you want to use to reorder the levels.
- Optionally, `fun`, a function that’s used if there are multiple values of `x` for each value of `f`. The default value is `median`.
```{r 15.4-2}
ggplot(relig_summary,aes(tvhours,fct_reorder(relig,tvhours)))+
  geom_point()
```

Reordering religion makes it much easier to see that people in the “Don’t know” category watch much more TV, and Hinduism & Other Eastern religions watch much less.

As you start making more complicated transformations, I’d recommend moving them out of `aes()` and into a separate `mutate()` step. For example, you could rewrite the plot above as:
```{r 15.4-3}
library(tidyverse)
relig_summary %>% 
  mutate(relig=fct_reorder(relig,tvhours)) %>% 
  ggplot(aes(tvhours,relig))+
  geom_point()
```

What if we create a similar plot looking at how average age varies across reported income level?
```{r 14.5-4}
rincome_summary <- gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    age=mean(age,na.rm=T),
    tvhours=mean(tvhours,na.rm=T),
    n=n()
  )

ggplot(rincome_summary,aes(age,fct_reorder(rincome,age)))+geom_point()
```

Here, arbitrarily reordering the levels isn’t a good idea! That’s because `rincome` already has a principled order that we shouldn’t mess with. Reserve `fct_reorder()` for factors whose levels are arbitrarily ordered.

However, it does make sense to pull “Not applicable” to the front with the other special levels. You can use `fct_relevel()`. It takes a factor, `f`, and then any number of levels that you want to move to the front of the line.
```{r 14.5-5}
ggplot(rincome_summary,aes(age,fct_relevel(rincome,"Not applicable")))+geom_point()
```

Why do you think the average age for “Not applicable” is so high?

Another type of reordering is useful when you are colouring the lines on a plot. `fct_reorder2()` reorders the factor by the y values associated with the largest x values. This makes the plot easier to read because the line colours line up with the legend.
```{r 14.5-6}
by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")
```

Finally, for bar plots, you can use `fct_infreq()` to order levels in increasing frequency: this is the simplest type of reordering because it doesn’t need any extra variables. You may want to combine with `fct_rev()`.
```{r}
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
    geom_bar()
```

### 15.4.1 Exercises

1. There are some suspiciously high numbers in tvhours. Is the mean a good summary?
```{r}
gss_cat%>% 
  ggplot(aes(tvhours))+
  geom_histogram()+
  geom_vline(xintercept=mean(gss_cat$tvhours,na.rm=T),colour="red")+
  geom_vline(xintercept=median(gss_cat$tvhours,na.rm = T),colour="blue")
```
Nope, there's a reasonable differnece between the mean and the median.

2. For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
```{r}
fct_gss <- gss_cat[sapply(gss_cat,is.factor)]
lapply(fct_gss,levels)
```
For all variables except rincome the levels are arbitrary. rincome is the only one which has a principled order.


3. Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
```{r}
# original
gss_cat %>%
  mutate(rincome = rincome %>% fct_relevel("Not applicable")) %>%
  ggplot(aes(rincome)) +
  geom_bar()

# revised
# change the corrdinates (the plot is in the same order)
gss_cat %>% 
  mutate(rincome=rincome %>% fct_relevel("Not applicable")) %>% 
  ggplot(aes(rincome))+
  geom_bar()+
  coord_flip()
```

## 15.5 Modifying factor levels 
More powerful than changing the orders of the levels is changing their values. This allows you to clarify labels for publication, and collapse levels for high-level displays. The most general and powerful tool is `fct_recode()`. It allows you to recode, or change, the value of each level. For example, take the `gss_cat$partyid`:
```{r 15.5-1}
gss_cat %>% count(partyid)
```

The levels are terse and inconsistent. Let’s tweak them to be longer and use a parallel construction.
```{r 15.5-2}
gss_cat %>% 
  mutate(partyid=fct_recode(partyid,
                            "Republican, strong"    = "Strong republican",
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak"        = "Not str democrat",
    "Democrat, strong"      = "Strong democrat")) %>% 
  count(partyid)
```

`fct_recode()` will leave levels that aren’t explicitly mentioned as is, and will warn you if you accidentally refer to a level that doesn’t exist.

To combine groups, you can assign multiple old levels to the same new level:
```{r 15.5-3}
gss_cat %>% 
  mutate(partyid=fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak"        = "Not str democrat",
    "Democrat, strong"      = "Strong democrat",
    "Other"                 = "No answer",
    "Other"                 = "Don't know",
    "Other"                 = "Other party"
                            )) %>% 
  count(partyid)
```

You must use this technique with care: if you group together categories that are truly different you will end up with misleading results.

If you want to collapse a lot of levels, `fct_collapse()` is a useful variant of `fct_recode()`. For each new variable, you can provide a vector of old levels:
```{r 15.5-4}
gss_cat %>% 
  mutate(partyid=fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"),
    rep = c("Strong republican", "Not str republican"),
    ind = c("Ind,near rep", "Independent", "Ind,near dem"),
    dem = c("Not str democrat", "Strong democrat"))) %>% 
  count(partyid)
```

Sometimes you just want to lump together all the small groups to make a plot or table simpler. That’s the job of `fct_lump()`:
```{r 15.5-5}
gss_cat %>% 
  mutate(relig=fct_lump(relig)) %>% 
  count(relig)
```

The default behaviour is to progressively lump together the smallest groups, ensuring that the aggregate is still the smallest group. In this case it’s not very helpful: it is true that the majority of Americans in this survey are Protestant, but we’ve probably over collapsed.

Instead, we can use the `n` parameter to specify how many groups (excluding other) we want to keep:
```{r}
gss_cat %>% 
  mutate(relig=fct_lump(relig,n=10)) %>% 
  count(relig,sort=T) %>% 
  print(n=Inf)
```

### 15.5.1 Exercises
1. How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?
```{r}
all_level <- levels(gss_cat$partyid)
gss_cat %>% 
  mutate(partyid=fct_collapse(partyid,
                               Democract = c('Not str democrat', 'Strong democrat'),
                              Republican = c('Strong republican', 'Not str republican'),
                              Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
                              Others = c("No answer", "Don't know", "Other party")
                              )) %>% 
  count(year,partyid) %>% 
  group_by(year) %>% 
  mutate(perc=n/sum(n)) %>% 
  ggplot(aes(year,perc,group=partyid,colour=partyid))+
  geom_line()+
  theme_bw()
```

It looks like Independents are growing whereas both other groups are shrinking, with Republicans shrinking much faster.

2. How could you collapse rincome into a small set of categories?
A very quick but perhaps not so advisable way (because you might lose important information and substantive meaning) is to use `fct_lump`.
```{r}
gss_cat %>%
  mutate(rincome = fct_lump(rincome, n = 6)) %>%
  count(rincome)

gss_cat %>%
  mutate(rincome =
           fct_collapse(
             rincome,
             `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
             `Lt $5000` = c("Lt $1000", str_c("$", c("1000", "3000", "4000"),
                                              " to ", c("2999", "3999", "4999"))),
             `$5000 to 10000` = str_c("$", c("5000", "6000", "7000", "8000"),
                                      " to ", c("5999", "6999", "7999", "9999"))
           )) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()
```

# Chapter 16: Dates and times 
## 16.1 Introduction
This chapter will show you how to work with dates and times in R. At first glance, dates and times seem simple. You use them all the time in your regular life, and they don’t seem to cause much confusion. However, the more you learn about dates and times, the more complicated they seem to get. To warm up, try these three seemingly simple questions:
- Does every hear have 365 days? 
- Does every day have 24 hours?
- Does every minute have 60 seconds

I’m sure you know that not every year has 365 days, but do you know the full rule for determining if a year is a leap year? (It has three parts.) You might have remembered that many parts of the world use daylight savings time (DST), so that some days have 23 hours, and others have 25. You might not have known that some minutes have 61 seconds because every now and then leap seconds are added because the Earth’s rotation is gradually slowing down.

Dates and times are hard because they have to reconcile two physical phenomena (the rotation of the Earth and its orbit around the sun) with a whole raft of geopolitical phenomena including months, time zones, and DST. This chapter won’t teach you every last detail about dates and times, but it will give you a solid grounding of practical skills that will help you with common data analysis challenges.

### 16.1.1 Prerequisites
This chapter will focus on the lubridate package, which makes it easier to work with dates and times in R. lubridate is not part of core tidyverse because you only need it when you’re working with dates/times. We will also need nycflights13 for practice data.
```{r 16.1.1}
library(tidyverse)
library(lubridate)
library(nycflights13)
```

## 16.2 Creating date/times











