#ANOVA 검정
aov_df<-cbind(df_name$클러스터,df_scores)
colnames(aov_df)<-c('cluster','f1','f2','f3','f4','f5')
head(aov_df)
#factor1에 대한 ANOVA
res1<-aov(f1~cluster,aov_df)
summary(res1)
#factor2에 대한 ANOVA
res2<-aov(f2~cluster,aov_df)
summary(res2)
#factor3에 대한 ANOVA
res3<-aov(f3~cluster,aov_df)
summary(res3)
#factor4에 대한 ANOVA
res4<-aov(f4~cluster,aov_df)
summary(res4)
#factor5에 대한 ANOVA
res5<-aov(f5~cluster,aov_df)
summary(res5)

#사후 검정 - Bonferroni t-test
#factor1
pairwise.t.test(aov_df$f1, aov_df$cluster, p.adjust.method = 'bonferroni')
#factor4
pairwise.t.test(aov_df$f4, aov_df$cluster, p.adjust.method = 'bonferroni')
#factor5
pairwise.t.test(aov_df$f5, aov_df$cluster, p.adjust.method = 'bonferroni')
#위험도 평균값 클러스터별로 확인
aov_df %>% group_by(cluster) %>% summarise(f1danger=mean(f1),f2danger=mean(f2),f3danger=mean(f3),f4danger=mean(f4),f5danger=mean(f5))
