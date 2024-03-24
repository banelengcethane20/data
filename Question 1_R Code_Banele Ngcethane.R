#QUESTION 1
#(I)
scores=matrix(c(48,55,84,
                73,85,68,
                51,70,95,
                65,69,74,
                87,90,67),nrow=5,ncol=3,byrow=TRUE)

colnames(scores)=c("Method1","Method2","Method3")

df=data.frame(Method=factor(rep(1:3,each=5)),
              Observation=factor(rep(1:3, times=5)),
              Score =as.vector(scores))

model=aov(Score~Method+Observation+Method:Observation, data=df)

anova_results=summary(model)

print(anova_results)

#(II)

anova_results= summary(model)

anova_re = na.omit(anova_results)

#get the p values
p_value_Method = anova_re[[1]]$"Pr(>F)"[1]
p_value_Observation = anova_re[[1]]$"Pr(>F)"[2]
p_value_Interaction = anova_re[[1]]$"Pr(>F)"[3]
p_value_Method
p_value_Observation
p_value_Interaction

# test the null hypothesis at 5% level of significance
#for Method
if( p_value_Method < 0.05){
  print("Method does not have a significant effect.")
}else{
  print("Method has a significant efffect.")
}

#for Observation
if( p_value_Observation < 0.05){
  print("Observation does not have a significant effect.")
}else{
  print("Observation has a significant efffect.")
}

#for interaction
if( p_value_Interaction < 0.05){
  print("Interaction does not have a significant effect.")
}else{
  print("Interaction has a significant efffect.")
}

# that is generally correct if you have a teaching method the students will pass
#else students will fail

#also generally correct if you actually observe your students to check if work is done also observe that all the students work
#else they might fail because you will be assuming that they are studying while not that will be seen on tests
#but if you just observe and do nothing they might possible fail the exam

#But doing both Observing and having teaching method that work exceptionally well and on academic there will be a progress
#else they will fail.
