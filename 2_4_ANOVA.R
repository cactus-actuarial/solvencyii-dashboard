library(ggplot2)

set.seed(1)

# d - dwarves
# h - hobbits
# m - all men

m_d <- 202
m_h <- 103
m_m <- 163

sd_d <- 50
sd_h <- 30
sd_m <- 40

# ls - life span

sample_size = 5000

ls_d <- round(rnorm(n = sample_size, mean = m_d, sd = sd_d), 0)
ls_h <- round(rnorm(n = sample_size, mean = m_h, sd = sd_h), 0)
ls_m <- round(rnorm(n = sample_size, mean = m_m, sd = sd_m), 0)

# combine data

life_span <- c(ls_d, ls_h, ls_m)
race <- c(rep("dwarf", sample_size), rep("hobbit", sample_size), rep("all_men", sample_size))

creatures <- data.frame(life_span, race)

head(creatures)
summary(creatures)

ggplot(creatures, aes(x = life_span, y = ..density..)) +
    geom_histogram() +
    facet_grid(race ~ .)

# check normality

shapiro.test(as.matrix(subset(creatures, race == "all_men", select = "life_span")))
shapiro.test(as.matrix(subset(creatures, race == "dwarf", select = "life_span")))
shapiro.test(as.matrix(subset(creatures, race == "hobbit", select = "life_span")))

# one