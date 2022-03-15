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
test_matched_gpc <- function(data,
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

data_sum$Sum <- ifelse(data_sum$Group=="P",0-data_sum$Sum,data_sum$Sum)
data_sum <- data_sum %>% group_by(N, Id) %>% summarise(SumTx=sum(Sum))%>%ungroup()
data_sum$ScoreT <- ifelse(data_sum$SumTx>0,1,0)
data_sum$ScoreC <- ifelse(data_sum$SumTx<0,1,0)

#Perform two-sided and one-sided test
data_sum$Z <- (data_sum$SumT-data_sum$SumC)/sqrt(data_sum$SumT+data_sum$SumC)

if(side == 1) {
    p_value = pnorm(as.numeric(data_sum$Z), lower.tail=F)
  }
  else if(side == 2){
    p_value = ifelse(as.numeric(data_sum$Z)>0,2*pnorm(as.numeric(data_sum$Z), lower.tail=F),2*pnorm(as.numeric(data_sum$Z), lower.tail=T)) 
  }
 
return <- (p_value < alpha)

}

