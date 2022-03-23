library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mlogit)
library(purrr)
library(plm)
options(scipen=999)
indpath = "D:/duke/ECON613/A3/Data"

assign(paste0("datjss"), fread(paste0(indpath,"/","datjss.csv")))
assign(paste0("datsss"), fread(paste0(indpath,"/","datsss.csv")))
assign(paste0("datstu"), fread(paste0(indpath,"/","datstu.csv")))

#Exercise 1
#number of students
nrow(datstu)

#number of schools
school <- datsss %>%
  group_by(schoolcode) %>%
  distinct(schoolcode, .keep_all = TRUE)

nrow(school)

#number of programs
program <- datstu %>%
  select(choicepgm1, choicepgm2, choicepgm3, choicepgm4, choicepgm5, choicepgm6)

pgm_long <- gather(program, pgm, name, choicepgm1:choicepgm6)
pgm_long <- pgm_long %>%
  select(name) %>%
  filter(name != "")

pgm <- unique(pgm_long, by="name")
rownames(pgm) <- NULL 

nrow(pgm)

#number of choice
choice <- datstu %>%
  select(V1, schoolcode1:choicepgm6)
choice1 <- gather(choice, choicepgm, program, choicepgm1:choicepgm6)
choice1 <- choice1 %>%
  select(V1, choicepgm, program) %>%
  mutate(V2 = substring(choicepgm, nchar(choicepgm), nchar(choicepgm)))

choice2 <- gather(choice, schoolcode, school, schoolcode1:schoolcode6)
choice2 <- choice2 %>%
  select(V1, schoolcode, school) %>%
  mutate(V2 = substring(schoolcode, nchar(schoolcode), nchar(schoolcode)))
        
school_pgm <- choice1 %>%
  left_join(choice2, by = c("V1", "V2")) %>%
  distinct(school, program, .keep_all = TRUE) %>%
  filter(program != "") %>%
  filter(school != "") %>%
  select(school, program)%>%
  arrange(school)

nrow(school_pgm)


#number of students applying to at least one senior high in the same district to home

same_district <- datstu %>%
  select(V1, schoolcode1:schoolcode6, jssdistrict)
same_district <- gather(same_district, school, schoolcode, schoolcode1:schoolcode6)

school_district <- datsss %>%
  select(schoolcode, sssdistrict)

district <- merge(x = same_district, y = school_district, by = "schoolcode", all.x = TRUE)
district <- unique(district)
district <- district %>%
  arrange(V1) %>%
  mutate(same = ifelse(jssdistrict == sssdistrict, 1,0)) %>%
  filter(same == 1) %>%
  distinct(V1, .keep_all = TRUE)

nrow(district)


#Number of students each senior high school admitted

admit <- datstu %>%
  filter(rankplace != 99 & !is.na(rankplace)) %>%
  select(V1, schoolcode1:schoolcode6, rankplace)

admit$schooladmit <- admit$schoolcode1
admit$schooladmit[admit$rankplace==2] <- admit$schoolcode2[admit$rankplace==2]
admit$schooladmit[admit$rankplace==3] <- admit$schoolcode3[admit$rankplace==3]
admit$schooladmit[admit$rankplace==4] <- admit$schoolcode4[admit$rankplace==4]
admit$schooladmit[admit$rankplace==5] <- admit$schoolcode5[admit$rankplace==5]
admit$schooladmit[admit$rankplace==6] <- admit$schoolcode6[admit$rankplace==6]

admit <- admit %>%
  select(schooladmit) %>%
  group_by(schooladmit) %>%
  mutate(count = n())%>%
  distinct(schooladmit, .keep_all = TRUE) %>%
  ungroup()%>%
  arrange(schooladmit)


#The cutoff of senior high schools

cutoff <- datstu %>%
  filter(!is.na(score))%>%
  filter(rankplace != 99 & !is.na(rankplace))
  
cutoff$schooladmit <- cutoff$schoolcode1
cutoff$schooladmit[cutoff$rankplace==2] <- cutoff$schoolcode2[cutoff$rankplace==2]
cutoff$schooladmit[cutoff$rankplace==3] <- cutoff$schoolcode3[cutoff$rankplace==3]
cutoff$schooladmit[cutoff$rankplace==4] <- cutoff$schoolcode4[cutoff$rankplace==4]
cutoff$schooladmit[cutoff$rankplace==5] <- cutoff$schoolcode5[cutoff$rankplace==5]
cutoff$schooladmit[cutoff$rankplace==6] <- cutoff$schoolcode6[cutoff$rankplace==6]

cutoff <- cutoff %>%
  select(schooladmit, score)%>%
  group_by(schooladmit) %>%
  mutate(lowest = min(score)) %>%
  distinct(lowest, .keep_all = TRUE) %>%
  ungroup() %>%
  select(schooladmit, lowest) %>%
  arrange(schooladmit)

#The quality of senior high schools

quality <- datstu %>%
  filter(!is.na(score))%>%
  filter(rankplace != 99 & !is.na(rankplace))

quality$schooladmit <- quality$schoolcode1
quality$schooladmit[quality$rankplace==2] <- quality$schoolcode2[quality$rankplace==2]
quality$schooladmit[quality$rankplace==3] <- quality$schoolcode3[quality$rankplace==3]
quality$schooladmit[quality$rankplace==4] <- quality$schoolcode4[quality$rankplace==4]
quality$schooladmit[quality$rankplace==5] <- quality$schoolcode5[quality$rankplace==5]
quality$schooladmit[quality$rankplace==6] <- quality$schoolcode6[quality$rankplace==6]

quality <- quality %>%
  select(schooladmit, score) %>%
  group_by(schooladmit) %>%
  mutate(average = mean(score)) %>%
  distinct(average, .keep_all = TRUE) %>%
  ungroup() %>%
  select(schooladmit, average) %>%
  arrange(schooladmit)

#Exercise 2 Data
location <- datsss %>%
  select(schoolcode, sssdistrict, ssslong, ssslat) %>%
  distinct(schoolcode, sssdistrict, ssslong, ssslat, .keep_all = TRUE) %>%
  filter(!is.na(ssslong)) %>%
  filter(!is.na(ssslat))
names(location)[1] <- "school"
names(cutoff)[1] <- "school"
names(quality)[1] <- "school"
names(admit)[1] <- "school"

school_pgm <- school_pgm %>%
  mutate(choice = paste(school,program,sep=','))

df <- school_pgm %>%
  left_join(location, by = "school") %>%
  left_join(cutoff, by = "school") %>%
  left_join(quality, by = "school") %>%
  left_join(admit, by = "school")

#Exercise3 Distance
student_choice <- choice1 %>%
  left_join(choice2, by = c("V1", "V2")) %>%
  left_join(location, by = "school") %>%
  mutate(choice = paste(school,program,sep=',')) %>%
  select(V1, schoolcode, choicepgm, choice, program, school, sssdistrict, ssslong, ssslat)

jss <- datstu %>%
  select(V1, jssdistrict) %>%
  filter(jssdistrict != "")
jss_location <- datjss %>%
  select(jssdistrict, point_x, point_y) %>%
  filter(jssdistrict != "") %>%
  filter(!is.na(point_x)) %>%
  filter(!is.na(point_y))

jss <- jss %>%
  left_join(jss_location, by = "jssdistrict")

names(jss)[3] <- "jsslong"
names(jss)[4] <- "jsslat"

distance <- student_choice %>%
  left_join(jss, by = "V1") %>%
  mutate(distance = sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2 + (69.172*(ssslat - jsslat))^2)) %>%
  arrange(V1)


#Exercise4 Dimensionality Reduction

df_rev <- datstu %>%
  #filter(!is.na(score))%>%
  #filter(rankplace != 99 & !is.na(rankplace)) %>%
  select(V1, score, agey, male, rankplace) %>%
  left_join(distance, by = "V1")

df_rev <- df_rev %>%
  mutate(scode_rev = substr(school, 1, 3)) %>%
  mutate(pgm_rev = ifelse(program=="General Arts"|program=="Visual Arts","arts",0),
         pgm_rev = ifelse(program=="Home Economics"|program=="Business","economics",pgm_rev),
         pgm_rev = ifelse(program=="General Science","Science",pgm_rev),
         pgm_rev = ifelse(program != "General Arts" & program !="Visual Arts" &
                            program != "Home Economics" & program != "Business" & 
                            program != "General Science", "others", pgm_rev)) %>%
  mutate(choice_rev = paste(scode_rev, pgm_rev, sep=','))

cutoff_quality <- df_rev %>%
  group_by(V1) %>%
  filter(row_number() == rankplace) %>%
  ungroup() %>%
  group_by(choice_rev) %>%
  mutate(lowest = min(score)) %>%
  mutate(average = mean(score))%>%
  distinct(choice_rev, .keep_all = TRUE) %>%
  ungroup() %>%
  select(scode_rev, pgm_rev, choice_rev, lowest, average)

###get 20000 highest scores

subset <- df_rev %>%
  group_by(V1) %>%
  distinct(V1, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(desc(score)) %>%
  filter(score >= score[20000]) %>%
  select(V1)

df_2W <- subset %>%
  left_join(df_rev, by = "V1")

df_2W <- df_2W %>%
  left_join(cutoff_quality, by = c("scode_rev", "pgm_rev", "choice_rev"))


  
#Exercise5 First Model

data <- df_2W %>%
  select(V1, score, rankplace, schoolcode, choicepgm, scode_rev, pgm_rev, choice_rev, lowest, average) %>%
  group_by(V1) %>%
  filter(schoolcode == "schoolcode1") %>%
  ungroup()%>%
  filter(choice_rev != "NA,others")

first_model <- data %>%
  select(score, choice_rev)

num_choice <- first_model %>%
  group_by(choice_rev) %>%
  distinct(choice_rev)

matrix1 = as.matrix(first_model)
matrix2 = as.matrix(num_choice)

ni=nrow(matrix1)
nj=nrow(num_choice)

Y <- matrix(0, ni,nj)
for(i in 1:nj){
  for(j in 1:ni){
    if(matrix1[j,2]==matrix2[i]){
      Y[j,i]=1
    }
  }
}


X=cbind(int = rep(1,nrow(first_model)), first_model$score)

######multinomial logit model and likelihood function#########
logit = function (beta, X) {
  P = as.matrix(rowSums(exp(X %*% beta[,2:246])))
  pr = mat.or.vec(nrow(first_model), 246)
  pr[,1] = 1/(1+P)
  for(i in 1:245){
    pr[,i+1] = exp(X %*% beta[,i+1])/(1+P)
  }
  return(pr)
}


likelihood = function (beta, X, Y) {
  beta = mat.or.vec(2, 246)
  P = as.matrix(rowSums(exp(X %*% beta[,2:246])))
  pr = mat.or.vec(nrow(first_model), 246)
  pr[,1] = 1/(1+P)
  for(i in 1:245){
    pr[,i+1] = exp(X %*% beta[,i+1])/(1+P)
  }
  LH = 0
  for(i in 1:246){
    LH = LH + colSums(as.matrix(Y[,i]*log(pr[,i])))
  }
  return(-LH)
}


res1 = optim(function(beta) likelihood(beta = beta, X = X, Y = Y), par=runif(490), method="BFGS")


#####marginal effect#####
score_m=as.matrix(first_model$score, ncol=1)
beta = mat.or.vec(2,246)
beta[1,2:246] = res1$par[1:245]
beta[2,2:246] = res1$par[246:490]
colnames(beta)=num_choice$choice_rev[1:246]
rownames(beta) <- c("Intercept", "Score")

logit_m=logit(beta, X)
beta_m=c(0,res1$par[246:490])
marginal1=array(0,dim=c(nrow(score_m),246))
for (i in 1:nrow(score_m)) {
  sum=sum(logit_m[i,]*beta_m)
  for (j in 1:246) {
    marginal1[i,j] <- logit_m[i,j]*(beta_m[j]-sum)
  }
}
marginal1=apply(marginal1, 2, mean)
marginal1


#Exercise6 Second Model

second_model <- data %>%
  group_by(choice_rev) %>%
  distinct(average, .keep_all = TRUE) %>%
  ungroup() %>%
  select(choice_rev, average)

X_2=cbind(int = rep(1,nrow(second_model)), second_model$average)
Y_2 = Y

ni = nrow(data)
nj = nrow(second_model)

######condition logit model and likelihood function#########
logit2 = function (beta, X) {
  pr = mat.or.vec(1, 246)
  P = sum(diag(exp(X %*% beta)))
  pr = diag(exp(X %*% beta))/P
  
  return(pr)
}

likelihood2 = function (beta, X, Y) {
  beta = mat.or.vec(1, 247)
  beta[1] = 0
  pr = mat.or.vec(1, 246)
  beta_m = mat.or.vec(2, 246)
  beta_m[1,] = beta[1:246]
  beta_m[2,] = beta[247]
  P = sum(diag(exp(X %*% beta_m)))
  pr = diag(exp(X %*% beta_m))/P
  LH = 0
  for(i in 1:246){
    LH = LH + colSums(as.matrix(Y[,i]*log(pr[i])))
  }
  return(-LH)
}

res2 = optim(function(beta) likelihood2(beta = beta,X = X_2, Y = Y_2),par=runif(247), method="BFGS")

#####marginal effect#####
quality_c=as.matrix(second_model$average, ncol=1)
beta2 = mat.or.vec(2,246)
beta2[1,2:246] = res2$par[2:246]
beta2[2,1:246] = res2$par[247]
colnames(beta2)=num_choice$choice_rev[1:246]
rownames(beta2) <- c("Intercept", "Quality")

logit_c=logit2(beta2, X_2)
v=array(0,dim = c(nrow(quality_c),nj))
for (i in 1:246) {
  v[i,i] <- 1
}

marginal2=array(0,dim=c(nrow(quality_c),nj))
for (i in 1:nrow(quality_c)) {
  for (j in 1:246) {
    marginal2[i,j]=logit_c[i]*(v[i,j]-logit_c[j])*res2$par[247]
  }
}

marginal2


#Exercise7 Counterfactual simulations 
third_model <- data %>%
  filter(pgm_rev != "others") %>%
  select(average, choice_rev)

num_choice_third <- third_model %>%
  group_by(choice_rev) %>%
  distinct(choice_rev)

matrix3_1 = as.matrix(third_model)
matrix3_2 = as.matrix(num_choice_third)

ni_3 <- nrow(third_model)
nj_3 <- nrow(num_choice_third)

Y_3 <- matrix(0, ni_3, nj_3)
for(i in 1:nj_3){
  for(j in 1:ni_3){
    if(matrix3_1[j,2]==matrix3_2[i]){
      Y_3[j,i]=1
    }
  }
}

X_3 <- third_model %>%
  group_by(choice_rev) %>%
  distinct(choice_rev, .keep_all = TRUE) %>%
  ungroup() %>%
  select(average)

X_3 = cbind(int = rep(1,nrow(num_choice_third)), X_3$average)


logit3 = function (beta, X) {
  pr = mat.or.vec(1, 196)
  P = sum(diag(exp(X %*% beta)))
  pr = diag(exp(X %*% beta))/P
  
  return(pr)
}

likelihood3 = function (beta, X, Y) {
  beta = mat.or.vec(1, 197)
  beta[1] = 0
  pr = mat.or.vec(1, 196)
  beta_m = mat.or.vec(2, 196)
  beta_m[1,] = beta[1:196]
  beta_m[2,] = beta[197]
  P = sum(diag(exp(X %*% beta_m)))
  pr = diag(exp(X %*% beta_m))/P
  LH = 0
  for(i in 1:196){
    LH = LH + colSums(as.matrix(Y[,i]*log(pr[i])))
  }
  return(-LH)
}

res3 = optim(function(beta) likelihood3(beta = beta,X = X_3, Y = Y_3),par=runif(197), method="BFGS")

beta3 = mat.or.vec(2,196)
beta3[1,2:196] = res2$par[2:196]
beta3[2,1:196] = res2$par[197]
colnames(beta3)=num_choice_third$choice_rev[1:196]
rownames(beta3) <- c("Intercept", "Quality")

logit_c_3=logit3(beta3, X_3)
