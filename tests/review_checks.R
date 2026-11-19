# Additional checks with fixed scope: theoretical claim and text/output alignment.
library(data.table)
source('R/config.R');source('R/functions.R')
O<-PATHS$output
P<-fread(file.path(O,'af/profiles.csv')); S<-fread(file.path(O,'scores/scores_uf.csv'))
# Counterexample: scalar subgroup consistency does not hold for arbitrary within-UF groups.
Bnorm<-matrix(c(1,1),nrow=1);a<-c(.8,.1);a_new<-c(.1,.9);b<-a
cs<-data.table(tau=.5,subgroup_before=bod_score(a,Bnorm,.5)$score,subgroup_after=bod_score(a_new,Bnorm,.5)$score,territory_before=bod_score((a+b)/2,Bnorm,.5)$score,territory_after=bod_score((a_new+b)/2,Bnorm,.5)$score)
stopifnot(cs$subgroup_after>cs$subgroup_before,cs$territory_after<cs$territory_before)
fwrite(cs,file.path(O,'audit/scope_subgroup_counterexample.csv'))
# Population-standardized comparison and exact accounting decomposition.
ss<-merge(S,P[ano==2010,.(uf,N0=N)],by='uf');res<-list()
for(tt in unique(ss$tau)){
 x<-ss[abs(tau-tt)<1e-12]; a<-x[ano==2010][order(uf)];b<-x[ano==2022][order(uf)]
 pi0<-a$N/sum(a$N);pi1<-b$N/sum(b$N);j0<-sum(pi0*a$score);j1<-sum(pi1*b$score);j1_std<-sum(pi0*b$score)
 score_effect<-sum(pi0*(b$score-a$score));composition<-sum((pi1-pi0)*b$score)
 stopifnot(abs(j1-j0-score_effect-composition)<1e-12)
 res[[length(res)+1]]<-data.table(tau=tt,J2010=j0,J2022_current=j1,J2022_fixed_population=j1_std,change_current_pct=100*(j1/j0-1),change_standardized_pct=100*(j1_std/j0-1),score_effect=score_effect,population_effect=composition)
}
fwrite(rbindlist(res),file.path(O,'audit/population_standardization.csv'))
# AF comparison against independently computed IPM-Brasil aggregates, all four censuses; exact semantic mapping of columns.
src<-'docs'
af<-fread(file.path(src,'af_reference_results.csv'))[bloco=='Unidade da Federação']
ufn<-c('11'='Rondônia','12'='Acre','13'='Amazonas','14'='Roraima','15'='Pará','16'='Amapá','17'='Tocantins','21'='Maranhão','22'='Piauí','23'='Ceará','24'='Rio Grande do Norte','25'='Paraíba','26'='Pernambuco','27'='Alagoas','28'='Sergipe','29'='Bahia','31'='Minas Gerais','32'='Espírito Santo','33'='Rio de Janeiro','35'='São Paulo','41'='Paraná','42'='Santa Catarina','43'='Rio Grande do Sul','50'='Mato Grosso do Sul','51'='Mato Grosso','52'='Goiás','53'='Distrito Federal')
prof<-fread(file.path(O,'series/profiles_1991_2022.csv'));prof[,categoria:=ufn[as.character(uf)]]
chk<-merge(prof,af[,.(ano,categoria,H_ref=H/100,A_ref=A/100,M0_ref=M0/100)],by=c('ano','categoria'))
chk[,`:=`(delta_H=H-H_ref,delta_A=A-A_ref,delta_M0=M0-M0_ref)]
fwrite(chk[,.(ano,uf,categoria,delta_H,delta_A,delta_M0)],file.path(O,'audit/af_reference_comparison.csv'))
heads<-fread(file.path(src,'af_reference_headcounts.csv'))[bloco=='Unidade da Federação'];heads[indicador=='frequenta',indicador:='frequencia']
long<-melt(prof,id.vars=c('ano','uf','categoria'),measure.vars=INDICATORS,variable.name='indicador',value.name='h')
ch<-merge(long,heads[,.(ano,categoria,indicador,h_ref=h_censurada/100)],by=c('ano','categoria','indicador'));ch[,delta:=h-h_ref]
fwrite(ch,file.path(O,'audit/af_reference_headcounts_comparison.csv'))
stopifnot(nrow(chk)==108,nrow(ch)==1296,max(abs(chk$delta_M0))<1e-10,max(abs(ch$delta))<1e-10)
# Score re-estimation from saved profiles, all six restrictions.
errs<-rbindlist(lapply(unique(S$tau),function(tt)rbindlist(lapply(c(2010,2022),function(yr){
 pp<-P[ano==yr][order(uf)];bb<-profile_matrix(P[ano==2010][order(uf)]);new<-bod_scores(profile_matrix(pp),bb,tt)
 old<-S[ano==yr&abs(tau-tt)<1e-12][order(uf),score]
 data.table(ano=yr,tau=tt,max_error=max(abs(old-new)))
}))))
fwrite(errs,file.path(O,'audit/saved_scores_recomputed.csv'));stopifnot(max(errs$max_error)<1e-10)
# True territorial subgroup decomposition with fixed weights.
x<-ss[tau==PARAM$tau_main&ano==2022];total<-sum(x$N0*x$score)/sum(x$N0)
groups<-x[,.(N=sum(N0),J=sum(N0*score)/sum(N0)),by=regiao]
stopifnot(abs(total-sum(groups$N*groups$J)/sum(groups$N))<1e-12)
fwrite(groups,file.path(O,'audit/territorial_subgroup_decomposition.csv'))
cat('PASS | Scope counterexample, population decomposition, 108 AF measures, 1296 headcounts, 324 scores and territorial decomposition\n')
print(cs);print(rbindlist(res)[tau==PARAM$tau_main]);print(chk[,.(max_AF=max(abs(delta_M0))),by=ano])
