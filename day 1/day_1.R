library(tidyverse)

example_data <- c(199,200,208,210,200,207,240,269,260,263)

# data
input <- read_file("day 1/day_1_input.txt") %>%
    str_split(pattern = "\n") %>%
    unlist() %>%
    as.numeric() %>%
    na.omit()

# part 1
increase_count <- function(dat) sum((dat[-1]-dat[-length(dat)])>0)

increase_count(example_data)

increase_count(input)



# part 2

# 199  A
# 200  A B
# 208  A B C
# 210    B C D
# 200  E   C D
# 207  E F   D
# 240  E F G
# 269    F G H
# 260      G H
# 263        H

# A: 607 (N/A - no previous sum)
# B: 618 (increased)
# C: 618 (no change)
# D: 617 (decreased)
# E: 647 (increased)
# F: 716 (increased)
# G: 769 (increased)
# H: 792 (increased)

window_sum <- function(d, w){
    roll <- c()
    for(i in 0:(length(d)-w)){
        roll <- c(roll, sum(d[i+1:w]))
    }
    roll
}

increase_count(window_sum(example_data, 3))

increase_count(window_sum(input, 3))
