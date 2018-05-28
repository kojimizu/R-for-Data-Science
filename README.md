# Udemy Courses

R Level 1 - EY Badge Blonze
From 2018 January

Data Science and Machine Learning Bootcamp in R
From 2018 February to April

R for Data Science
From 2018 April to Current


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
```{r Tibbles vs. data.frame 1}
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
```{r Tibbles vs. data.frame 4}
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
annoying
annoying %>% 
  rename(one=`1`,
         two=`2`,
         three=`3`)
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
```{r data import 4}
read_csv("1,2,3\n4,5,6",col_names = F)
```

("\n" is a convenient shortcut for adding a new line. You’ll learn more about it and other types of string escape in string basics.)

Alternatively, you can pass col_names a characer vector which will be used as the column names:
```{r data import 5}
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
```{r}

```


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

```{r 12.2 Tidy data 1}
# compute rate per 10,000
table1 %>%
  mutate(rate=cases/population*10000)

# compute cases per hear
table1 %>% 
  count(year,wt=cases)

# visualize changes over time
library(ggplot2)
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
```{r 12.3.1 Gathering 1}
table4a %>% 
  gather(`1999`, `2000`, key="year",value=cases)
```

The columns to gather are specified with `dplyr::select()` style notation. Here there are only two columns, so we list them individually. Note that “1999” and “2000” are non-syntactic names (because they don’t start with a letter) so we have to surround them in backticks. To refresh your memory of the other ways to select columns, see select.

In the final result, the gathered columns are dropped, and we get new `key` and `value` columns. Otherwise, the relationships between the original variables are preserved.. The only difference is the variable stored in the cell values. 
```{r 12.3.1 Gathering 1}
table4b
table4b %>% 
  gather(`1999`,`2000`,key="year",value="population")
```

To combine the tidied versions of `table4a` and `table4b` into a single tibble, we need to use `dplyr::left_join()`, which you'll learn about in relational data.
```{r 12.3.1 Gathering 2}
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
```

The main objective of analysis here is whether pregnant or not (bc males can not be pregnant), so I would go for gathering the gender column rather than spreading the pregnant column.
```{r 12.3. Exercise 4-2}
preg
preg %>% 
  gather(gender,values,preginant)
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
```{r  12.4.3 Exercises 1 solution}
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

- > So no implicit missing values. And the difference between an NA and a 0 is that 0 means there's 0 cases in that cell but NA could mean that there's 20 cases but weren't reported.

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






