library(corrplot)
library(skimr)
library(lmtest)
library(rpart)
library(nortest)

### Leer la base de datos ###
db <- read.csv(file = 'database.csv')

# Equipos de la fase de grupos de la 
# EUFA Champions League de la temporada 2014-2015
uefa_teams = c(
    'AtlÃ©tico Madrid',
    'Juventus',
    'Olympiacos CFP',
    'MalmÃ¶ FF',
    'Real Madrid',
    'FC Basel 1893',
    'Liverpool',
    'AS Monaco',
    'Bayer 04 Leverkusen',
    'Zenit St. Petersburg',
    'SL Benfica',
    'Borussia Dortmund',
    'Arsenal',
    'RSC Anderlecht',
    'Galatasaray SK',
    'FC Bayern MÃ¼nchen',
    'Manchester City',
    'Roma',
    'PFC CSKA Moscow',
    'FC Barcelona',
    'Paris Saint-Germain',
    'Ajax',
    'Chelsea',
    'FC Schalke 04',
    'Sporting CP',
    'FC Porto',
    'Shakhtar Donetsk',
    'Athletic Club de Bilbao'
)

# Database con solo los equipos de la champions
uefa_db <- data.frame(db[db$club %in% uefa_teams, ])

# Seleccionar las variables que interesan
# de todas las disponibles
short_db <- data.frame(
    uefa_db$overall,
    uefa_db$potential,
    uefa_db$attacking_finishing,
    uefa_db$attacking_short_passing,
    uefa_db$skill_ball_control,
    uefa_db$skill_fk_accuracy,
    uefa_db$skill_long_passing,
    uefa_db$movement_agility,
    uefa_db$movement_balance,
    uefa_db$movement_sprint_speed,
    uefa_db$power_shot_power,
    uefa_db$power_stamina,
    uefa_db$power_long_shots,
    uefa_db$mentality_interceptions,
    uefa_db$mentality_vision,
    uefa_db$defending_marking,
    uefa_db$defending_sliding_tackle
)

# Cambiar el nombre de las variables
# seleccionadas para que sean mas cortos
colnames(short_db) <- c(
    "overall",
    "potential",
    "attacking_finishing",
    "attacking_short_passing",
    "skill_ball_control",
    "skill_fk_accuracy",
    "skill_long_passing",
    "movement_agility",
    "movement_balance",
    "movement_sprint_speed",
    "power_shot_power",
    "power_stamina",
    "power_long_shots",
    "mentality_interceptions",
    "mentality_vision",
    "defending_marking",
    "defending_sliding_tackle"
)

# Ver una descripción general de las variables
data_sum <- skim(short_db)
data_sum

# Conclusion, se

# Analisis de correlacion general
cor_short_db <- cor(short_db)
png(filename = "images/cor_short_db.png")
corrplot(cor_short_db, method = "number", number.digits=1)
dev.off()
symnum(cor_short_db)

# Seleccion de variables 'DL ~ F + PD'
selected_data <-
    data.frame(PC = short_db$PC,
               PL = short_db$PL,
               Vi = short_db$Vi)
cor_selected_data <- cor(selected_data)
write.csv(cor_selected_data, file = "images/cor_selected_data.csv")
png(filename = "images/cor_selected_data.png")
corrplot(cor_selected_data, method = "number")
dev.off()
symnum(cor_selected_data)

### Regresion y ACP #########

cor_short_database <- cor(short_db)
write.csv(cor_short_database, file = "images/correlation.csv")
png(filename = "images/cor_short_database.png")
corrplot(cor_short_database, method = "number")
dev.off()
symnum(cor_short_database)

acp <- prcomp(short_db[-c(3)], scale. = TRUE)
biplot(acp)
plot(acp)
summary(acp)

index <- 14
regression_data <-
    as.data.frame(cbind(
        scale(short_db[c(index)]),
        prcomp(short_db[-c(index)], scale. = TRUE)$x[, 1:3]
    )) # With the first three PC

png(filename = "images/cor_regression_data.png")
cor_regression_data <- cor(regression_data)
corrplot(cor_regression_data, method = "number")
dev.off()
symnum(cor_regression_data)

png(filename = "images/pairs.png")
pairs(regression_data)
dev.off()

model <- lm("I ~ .", data = regression_data)
summary(model)

acp$rotation[, 1:3]

# Validating Regresion

mean(model$residuals) # Error mean
sum(model$residuals) # Error sum

dwtest(model)

shapiro.test(model$residuals)
ks.test(x = model$residuals, y = 'pnorm')
ad.test(model$residuals)

png(filename = "images/hist.png")
hist(model$residuals)
dev.off()

png(filename = "images/qqgraph.png")
qqnorm(model$residuals)
qqline(model$residuals)
dev.off()

png(filename = "images/homocedasticidad.png")
plot(regression_data$I,
     rstandard(model),
     ylab = "Residuos estandarizados",
     xlab = "Val. General")
abline(h = 0, lty = 2)
dev.off()

bptest(model)

##### ANOVA ###########

MI = c(uefa_db$mentality_interceptions)
Fo = as.numeric(c(uefa_db$preferred_foot == 'Right'))
data_anova = data.frame(MI, Fo)
cor_anova <- cor(data_anova)
corrplot(cor_anova, method = "number")
anova_model = aov(MI ~ Fo, data = data_anova)
summary(anova_model)

shapiro.test(anova_model$residuals)
ks.test(x = anova_model$residuals, y = 'pnorm')
ad.test(anova_model$residuals)

png(filename = "images/hist_anova.png")
hist(anova_model$residuals)
dev.off()

png(filename = "images/qqgraph_anova.png")
qqnorm(anova_model$residuals)

qqline(anova_model$residuals)
dev.off()

bartlett.test(anova_model$residuals, data_anova$Fo)
bptest(anova_model)

png(filename = "images/anova_homocedasticidad.png")
plot(data_anova$VG,
     rstandard(anova_model),
     ylab = "Residuos estandarizados",
     xlab = "Val. General")
abline(h = 0, lty = 2)
dev.off()

plot(anova_model)

dwtest(anova_model)

