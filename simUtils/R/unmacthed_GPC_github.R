#' Perform Hypothesis Test
#'
#' unmatched GPC test assuming larger values are preferred
#'
#' @param data data.table with the simulation data
#' @param target name of the target variable
#' @param type the target variable (binary or count)
#' @param alpha type-I error rate
#' @param side 1 or 2 for one- or two-sided test
#'
#' @return the test result (TRUE if H0 is rejected, FALSE otherwise)
test_unmatched_gpc <- function(data,
                               target,
                               type,
                               alpha, 
                               side) {

if(type == binary) {
  data_sum <- data %>% group_by(N, Id, Group) %>% summarise(Sum=sum(target))%>%
  ungroup()
  }
else if(type == count){
  data_count <- data %>% group_by(N,Id) %>% mutate(std_diff=ifelse(Time=="t0" | Time=="t2" | Time=="t4" | Time=="t7", (target[Time=="t0"]-target)/target[Time=="t0"], (target[Time=="t8"]-target)/target[Time=="t8"]))%>%
  ungroup()

  data_sum <- data_count %>% group_by(N, Id, Group) %>% summarise(Sum=sum(std_diff,na.rm = TRUE),.groups = 'drop') %>%
  ungroup()
  }

#define score function for pairwise comparisons (here we assume larger values are preferred
Score_fct <- function(Value_i, Value_j){
  
  if(Value_i > Value_j) {
    Score = 1
  }
  else if(Value_i == Value_j){
    Score = 0
  }
  else if(Value_i < Value_j){
    Score = -1
  }  
  return(Score)
}

#define number of subjects in each treatment arm
Id_v <- as.data.frame(filter(data_sum, Group=="V"))
nTest <- length(Id_v[,2])
Id_p <- as.data.frame(filter(data_sum, Group=="P"))
nControl <- length(Id_p[,2])
npatients <- nTest+nControl

#perform pairwise comparisons
U_Gehan = matrix(NA, nrow = nTest, ncol = nControl)

for (i in 1:nTest) {
  for (j in 1:nControl) {
    U = Score_fct(data_sum$Sum[data_sum$Group == "V"][i], data_sum$Sum[data_sum$Group =="P"][j])
    U_Gehan[i,j] = U
  }
}

Gehan = mean(U_Gehan)

#variance function
U_Gehan_v = matrix(NA, nrow = npatients, ncol = npatients)

for (i in 1:npatients) {
  for (j in 1:npatients) {
    U_Gehan_v[i,j] = Score_fct(data_sum$Sum[i], data_sum$Sum[j])[1]
  }
}

Var_Gehan_P = sum(rowSums(U_Gehan_v)^2)/(nTest*nControl*npatients*(npatients-1))

#Perform two-sided and one-sided test

if(side == 1) {
    p_value = pnorm(-(Gehan/sqrt(Var_Gehan_P)))
  }
  else if(side == 2){
    p_value = 2*pnorm(-abs(Gehan/sqrt(Var_Gehan_P))) 
  }
 
return <- (p_value < alpha)

}

