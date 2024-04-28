library(dplyr) # A Grammar of Data Manipulation CRAN v1.1.4
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.0
library(lattice) # Trellis Graphics for R CRAN v0.21-9
library(effsize) # Efficient Effect Size Computation CRAN v0.8.1

# Exploring data ----

# Charge our data
dataFire <- read.csv("Data.csv" , sep = ";")

View(dataFire)
names(dataFire)

# Calculate difference between sample means
Bc_Fire <- dataFire  |> 
  filter(S == "Baccharis conferta")  |> 
  pull(PBC)

Pp_Fire <- dataFire  |> 
  filter(S == "Pinus patula")  |> 
  pull(PBC)

mean(Bc_Fire)
mean(Pp_Fire)
sd(Bc_Fire)
sd(Pp_Fire)

data <- dataFire  |> 
  group_by(S)  |> 
  mutate(mean = mean(PBC))

# Histogram plot 
ggplot(data,aes(x = PBC)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = mean, colour = S), 
             colour = 'springgreen4', size = 1.5)+
  facet_grid(S ~.) +
  theme_bw() + 
  theme(legend.position = "none")+
  theme(strip.text = element_text(face = "italic"))

# Evaluating Model Assumptions ----

# Plot 
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(Bc_Fire, xlab = "", ylab = "",
       main = "Baccharis conferta", col = "firebrick")
qqline(Bc_Fire)
qqnorm(Pp_Fire, xlab = "", ylab = "",
       main = "Pinus patula", col = "springgreen4")
qqline(Pp_Fire)

# Calculate normality test
shapiro.test(Bc_Fire)
shapiro.test(Pp_Fire)

# Run the model to get residuals
fit <- lm(PBC ~ S, data = dataFire)

# Plot residuals by group
densityplot(~ residuals(fit), group = S, data = dataFire, auto.key = TRUE)

# Applied test ----

# Calculate t test
t.test(PBC ~ S, data = dataFire, var.equal = TRUE)

# Calculate effect sizes
cohen.d(formula = PBC ~ S, data = dataFire, paired = FALSE)

# Boxplot 
ggplot(dataFire, aes(x = S, y = PBC, 
                     colour = S)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18, face = "italic"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, face = "italic"))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", linewidth = 1),
        panel.background = element_blank())+
  theme(legend.position = "none")+
  xlab("Species") + 
  ylab("Biomass consumed by Fire (%)")+
  annotate("text", x = 1, y = 98, label = "A") + 
  annotate("text", x = 2, y = 83, label = "B")
