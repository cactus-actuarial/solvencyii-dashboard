library(ggplot2)

set.seed(1)

# d - dwarves
# o - ogres
# g - goblins

m_d <- 250
m_o <- 83.9
m_g <- 84.1

sd_d <- 10
sd_o <- 4
sd_g <- 5

# ls - life span

sample_size = 1000

ls_d <- round(rnorm(n = sample_size, mean = m_d, sd = sd_d), 0)
ls_o <- round(rnorm(n = sample_size, mean = m_o, sd = sd_o), 0)
ls_g <- round(rnorm(n = sample_size, mean = m_g, sd = sd_g), 0)

# combine data
lifespan <- c(ls_d, ls_o, ls_g)
race <- c(rep("dwarf", sample_size), rep("ogre", sample_size), rep("goblin", sample_size))

creatures_data <- data.frame(lifespan, race)

# shuffle rows
creatures <- creatures_data[sample(1:dim(creatures_data)[1]), ]
row.names(creatures) <- NULL

# sneak peek
head(creatures)

dplyr::glimpse(creatures)

ggplot(creatures, aes(x = lifespan, y = ..density..)) +
    geom_histogram(binwidth = 1) +
    facet_grid(race ~ .) +
    scale_x_continuous(breaks = seq(from = 50, to = 290, by = 20))

boxplot(lifespan ~ race, data = creatures)

# check normality

shapiro.test(as.matrix(subset(creatures, race == "dwarf", select = "life_span")))
shapiro.test(as.matrix(subset(creatures, race == "ogre", select = "life_span")))
shapiro.test(as.matrix(subset(creatures, race == "goblin", select = "life_span")))

# one-way ANOVA

oneway.test(life_span ~ race, data = creatures)

oneway.test(life_span ~ race, data = subset(creatures, race != "dwarf"))

# We can conclude that life expectancy varies significantly across races.

# Kruskal-Wallis test
kruskal.test(life_span ~ race, data = creatures)
