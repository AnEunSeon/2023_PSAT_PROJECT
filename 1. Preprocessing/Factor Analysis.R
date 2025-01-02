library('psych')
library('GPArotation')
library('tidyverse')
library('data.table')
library('dplyr')
library('ggplot2')

#데이터 전처리 
#잘 모르겠음 응답인 행 제거
df<-subset(df,`2-4 집에서 학교로 갈 때 교통사고나 안전사고 위험은 어느 정도라고 생각하나요`!='잘 모르겠음'&`3-5 표시한 길에서 교통사고와 안전사고 위험은 어느 정도라고 생각하나요`!='잘 모르겠음')
df_name<-df %>% select(c(학교,클러스터))
df<-df %>% select(-c(학교,클러스터))
#label encoding
label1<-c("매우 안전","안전한 편","보통","위험한 편","매우 위험")
label2<-c("그런 적이 없다","가끔 그랬다","자주 그랬다")
col.names<-colnames(df)[1:2]
for (col in names(df)){
  if (col %in% col.names){
    df[[col]]<-factor(df[[col]],levels=label1,ordered=TRUE)
    df[[col]]<-as.numeric(df[[col]])
  } else {
    df[[col]]<-factor(df[[col]],levels=label2,ordered=TRUE)
    df[[col]]<-as.numeric(df[[col]])
  }
}

#요인 갯수 확인
#eigenvalue 확인
cor_df<-cor(df)
eigen_val<-eigen(cor_df)
eigen_val$values
#scree plot 확인
VSS.scree(df)

#요인분석/점수 결과 도출
df_factor<-factanal(df,factors=5,rotation='varimax',scores='regression')
df_factor
#요인점수 산출
df_scores<-as.data.frame(df_factor$scores)


