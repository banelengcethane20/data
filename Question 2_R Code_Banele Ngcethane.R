# QUESTION 2
# (I)

paint1=c(128,137,135,124,141)
paint2=c(144,133,142,146,130)
paint3=c(133,143,137,136,131)
paint4=c(150,142,135,140,153)

data=data.frame(Paint=rep(c("Paint1","Paint2","Paint3","Paint4"),each=5),
                Value=c(paint1,paint2,paint3,paint4),
                Block=rep(1:4,each=5))

model=aov(Value~Paint+Block,data=data)

summary(model)

#(II)

anova_result= summary(model)

anova_table= na.omit(anova_result)

#get the p values
p_value = anova_table[[1]]$"Pr(>F)"[1]
p_value

# test the null hypothesis at 5% level of significance
#checking mean drying of 4 paints
if( p_value > 0.05){
  print("We conclude that the mean drying for all diffrent paints is the Same")
}else{
  print("We conclude that the mean drying for the four paints is different")
}
