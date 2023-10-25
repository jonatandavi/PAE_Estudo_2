group = read.csv('data/DadosAcoesGrupoB.csv',header=FALSE)
  
#calculate returns
returns <- matrix(0, length(group$V1)-1, ncol(group))
for (g in 1:nrow(group)-1) {
  for(i in 1:5){
    returns[g, i] = (group[g, i] - group[g+1, i]) / group[g+1, i]
  }
}
  
returns <-data.frame(returns)
returns <-stack(returns)
  
#Box plot
library(ggplot2)
ggplot(returns, 
       aes(x = ind, y = values, fill = ind)) + 
  geom_boxplot() + geom_point()

#Hipotese nula anova
#Teste do ANOVA
my.model <- aov(values ~ ind, data = returns)
summary.aov(my.model)
  
#Premissa da normalidade comprovada
shapiro.test(my.model$residuals)
library(car)
qqPlot(my.model$residuals,
       pch = 16,
       lwd = 3,
       cex = 2,
       las = 1)

hist(my.model$residuals)
  
#premissa da homoscedasticidade
fligner.test(values ~ ind, data = returns)
plot(x = my.model$fitted.values, y = my.model$residuals)

#Teste de Tukey (todos contra todos) dizendo qual é a melhor ação
library(multcomp)
mc1 <- glht(my.model, linfct = mcp(ind = "Tukey"))
mc1_CI <- confint(mc1, level = 0.95)
mc1_CI

plot(mc1_CI)

#Fazer anova, se tiver diferença ai eu faço comparação um a um. c.c. acabou o problema.