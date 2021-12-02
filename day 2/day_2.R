library(tidyverse)

input_data <- read_file("day 2/input")

example_data <- c(
"forward 5
down 5
forward 8
up 3
down 8
forward 2"
)


cordinate <- function(string) {
    string_split <- unlist(strsplit(x = string, split = "\\n"))

    hor <- grep(pattern = "forward", string_split, value = TRUE)
    hor <- gsub(pattern = "forward ", replacement = "", x = hor)
    hor <- sum(as.integer(hor))

    depth <- grep(pattern = "up|down", string_split, value = TRUE)
    depth <- gsub(pattern = "up ", replacement = "-", x = depth)
    depth <- gsub(pattern = "down ", replacement = "", x = depth)
    depth <- sum(as.integer(depth))

    hor*depth
}

cordinate(example_data)
cordinate(input_data)

# part 2

step_f <- function(string){
    step <- unlist(strsplit(x = string, split = "\n"))
    step <- gsub(pattern = "forward", replacement = "f", step)
    step <- gsub(pattern = "up", replacement = "u", step)
    step <- gsub(pattern = "down", replacement = "d", step)

    hor <- hor_i <- depth <- aim <- 0

    for(i in 1:length(step)){
        if(grepl("f", step[i])){
            hor_i <- as.integer(gsub("f ", "", x = step[i]))

            hor <- hor + hor_i
            depth <- depth + hor_i*aim

        } else if(grepl("u", step[i])){
            aim <- aim + as.integer(gsub("u ", "-", x = step[i]))
        } else if(grepl("d", step[i])){
            aim <- aim + as.integer(gsub("d ", "", x = step[i], ))
        }
    }

    hor*depth
}


step_f(example_data)
step_f(input_data)
