library(dplyr)
library(tidyr)
library(stringr)

dat <- read.csv("C:/Users/s1679680/OneDrive - University of Edinburgh/OneDrive Documents/R/Advent_of_code/1.csv")

# Day 1
## Puzzle 1

dat$status <- dat$depth - (lag(dat$depth)) 

dat$result <- replace(dat$status, which(dat$status < 0), NA)

sum(!is.na(dat$result))

## Puzzle 2

dat$window <- dat$depth + lag(dat$depth) + lead(dat$depth)

dat$status <- dat$window - (lag(dat$window))

dat$result <- replace(dat$status, which(dat$status <= 0), NA)

sum(!is.na(dat$result))

# Day 2
## Puzzle 1

dat2 <- read.csv("C:/Users/s1679680/OneDrive - University of Edinburgh/OneDrive Documents/R/Advent_of_code/2.csv")

### Calculate forward movement

dat2$numeric <- str_extract(dat2$command, "\\d")

dat2$numeric <- as.numeric(dat2$numeric)

dat2$forward <- str_extract(dat2$command, '\\b(forward)\\b')

forward <- dat2 %>% filter(forward=="forward")

### Calculate up

dat2$up <- str_extract(dat2$command, '\\b(up)\\b')

up <- dat2 %>% filter(up=="up")

### Calculate down

dat2$down <- str_extract(dat2$command, '\\b(down)\\b')

down <- dat2 %>% filter(down=="down")

### Calculate answer

sum(forward$numeric) * (sum(down$numeric) - sum(up$numeric))

## Puzzle 2

# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#     It increases your horizontal position by X units.
#     It increases your depth by your aim multiplied by X.

dat2 <- read.csv("C:/Users/s1679680/OneDrive - University of Edinburgh/OneDrive Documents/R/Github/Adventofcode/2.csv")

dat2$numeric <- str_extract(dat2$command, "\\d")

dat2$numeric <- as.numeric(dat2$numeric)

dat2$direct <- str_extract(dat2$command, "[a-z]+")

dat2['aim'] <- 0
dat2['horizontal'] <- 0
dat2['depth'] <- 0

aim <- 0
horizontal <- 0
depth <- 0

for(i in 1:length(dat2$direct)) {
  if (dat2$direct[i] == "down")
    aim <- aim + dat2$numeric[i]
  if (dat2$direct[i] == "up")
    aim <- aim - dat2$numeric[i]
  if (dat2$direct[i] == "forward")
    horizontal <- horizontal + dat2$numeric[i]
    depth <- depth + (aim * dat2$numeric[i])  
}

print(horizontal * depth)

# Day 3

## Puzzle 1

dat3 <- read.csv("C:/Users/s1679680/OneDrive - University of Edinburgh/OneDrive Documents/R/Advent_of_code/3.csv", colClasses = c(code = "character"))

dat3$code <- gsub("(?<=.)(?=.)", ",", dat3$code, perl = TRUE)

dat3 <- dat3 %>%  separate(code, c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"))
