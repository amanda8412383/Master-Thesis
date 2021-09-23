library(dplyr)
library(survey)
library(stargazer)
library(AER)
library(margins)
library(jtools)
library(texreg)
library(lodown)
library(xtable)
library(ggplot2)

cpsasec_cat <-
  get_catalog( "cpsasec" ,
               output_dir = file.path( path.expand( "~" ) , "CPSASEC" ) )

# 2020 only
cpsasec_cat <- subset( cpsasec_cat , year == 2020 )
# download the microdata to your local computer
cpsasec_cat <- lodown( "cpsasec" , cpsasec_cat)
cpsasec_df <-
  readRDS( file.path( path.expand( "~" ) , "CPSASEC" , "2020 cps asec.rds" ) )

rm(list=setdiff(ls(), "cpsasec_df"))


#name var to keep
variables_to_keep <-
  c('h_idnum', 'a_lineno',  'peridnum', 
    'pepar1', 'pepar2', 'a_age', 'a_sex', 
    #last week enroll, full time partime, highest edu, state
    'a_enrlw', 'a_ftpt', 'a_hga', 'gestfips',
    #parent type
    'pepar1typ', 'pepar2typ',
    #citizenship
    'prcitshp',
    #disability
    'prdisflg',
    #race, hispanic
    'prdtrace', 'pehspnon', 
    #industry, occupation
    'a_mjind', 'a_mjocc',
    #total earn, salary,  covered by association, association = union
    'pearnval',  'a_uncov', 'a_unmem',
    #earnings business/ employer, farm 2nd, self-em 2nd, salary 2nd
    #pearnval = ern_val+frm_val+se_val+ws_val
    'ern_val', 'frm_val', 'se_val', 'ws_val',
    
    #non income topcoding list
    'ann_val', 'cap_val', 'chsp_val', 'csp_val',  
    'dis_val1', 'dis_val2', 'div_val', 'dst_val1', 'dst_val2',
    'dst_val1_yng', 'dst_val2_yng',
    #edu assis, finance assis, 
    'ed_val', 'fin_val', 
    'trdint_val', 'rint_val1',  'rint_val2', 'oi_val', 'rnt_val',
    'sur_val1', 'sur_val2', 'pen_val1', 'pen_val2',

    #non income$, total person income
    #ptotval almost = pothval+pearnval
    'pothval', 'ptotval',
    #is poverty, poverty level, health insurance, Health status
    'pov_univ', 'perlis', 'cov', 'hea', 'one',
    'marsupwt' , 
    grep( "pwwgt" , names( cpsasec_df ) , value = TRUE )
    ); gc()
#get var
keep_df<- cpsasec_df[ variables_to_keep ] ; gc()


## self checking###
#pearnval = ern_val+frm_val+se_val+ws_val
#pothval != sum without spm & income (36210)
#but it's even worse to include spm


###clean###
#mark topcode in pearnval 
topcode_df <- keep_df %>%
  mutate(topcode = ifelse(ern_val > 360000, 1, 
                   ifelse(frm_val > 50000, 1,
                   ifelse(se_val > 100000, 1, 
                   ifelse(ws_val > 70000, 1,
                   ifelse(ann_val > 60000, 1,
                   ifelse(cap_val > 85000, 1,
                   ifelse(chsp_val > 26000, 1,
                   ifelse(csp_val > 21320, 1,
                   ifelse(dis_val1 > 72000, 1,
                   ifelse(dis_val2 > 72000, 1,
                   ifelse(div_val > 38000, 1,
                   ifelse(dst_val1 > 81250, 1,
                   ifelse(dst_val2 > 81250, 1,
                   ifelse(dst_val1_yng > 80000, 1,
                   ifelse(dst_val2_yng > 80000, 1,
                   ifelse(ed_val > 37650, 1,
                   ifelse(fin_val > 54000, 1,
                   ifelse(trdint_val > 10200, 1,
                   ifelse(rint_val1 > 44000, 1,
                   ifelse(rint_val2 > 44000, 1,
                   ifelse(oi_val > 82992, 1,
                   ifelse(rnt_val > 75000, 1,
                   ifelse(sur_val1 > 100000, 1,
                   ifelse(sur_val2 > 100000, 1,
                   ifelse(pen_val1 > 84000, 1,
                   ifelse(pen_val2 > 84000, 1, 0)
                   ))))))))))))))))))))))))))%>%
  select(-ern_val, -frm_val,-se_val, -ws_val, -ann_val, -cap_val, -chsp_val,
         -csp_val, -dis_val1, -dis_val2, -div_val, -dst_val1, -dst_val2,
         -dst_val1_yng, -dst_val2_yng, -ed_val, -fin_val, -trdint_val,
         -rint_val1, -rint_val2, -oi_val, -rnt_val, -sur_val1, -sur_val2,
         -pen_val1, -pen_val2
         ); gc()

#change vars to dummy
dummy_df <- topcode_df %>%
  mutate(a_female = ifelse(a_sex == 1, 0, 1 ))%>%
  mutate(a_enrlw = replace(a_enrlw, a_enrlw != 1, 0 ))%>%
  mutate(a_ftpt = replace(a_ftpt, a_ftpt != 1, 0 ))%>%
  mutate(a_ftpt = replace(a_ftpt, a_ftpt != 1, 0 ))%>%
  mutate(nativeus = ifelse((prcitshp == 1 | prcitshp == 2 | prcitshp == 3), 1, 0))%>%
  mutate(prdisflg = replace(prdisflg, prdisflg != 1, 0 ))%>%
  mutate(prdtrace = replace(prdtrace, (prdtrace != 1 & prdtrace != 2 & prdtrace != 3 & prdtrace != 4 & prdtrace != 5),  7))%>%
  mutate(prdtrace = replace(prdtrace, pehspnon == 1,  6))%>%
  mutate(union = ifelse((a_uncov==1 | a_unmem == 1), 1, 0))%>%
  mutate(cov = replace(cov, cov != 1, 0 ))%>%
  #clean hga
  mutate(hschool = if_else(a_hga > 38, 1, 0))%>%
  mutate(college = if_else(a_hga > 40, 1, 0))%>%
  mutate(master = if_else((a_hga == 44 | a_hga == 46), 1, 0))%>%
  mutate(doctor = if_else(a_hga == 46, 1, 0))%>%
  mutate(a_hga = replace(a_hga, a_hga == 31, 0 ))%>%
  mutate(a_hga = replace(a_hga, a_hga == 32, 2.5 ))%>%
  mutate(a_hga = replace(a_hga, a_hga == 33, 5.5 ))%>%
  mutate(a_hga = replace(a_hga, a_hga == 34, 7.5 ))%>%
  mutate(a_hga = replace(a_hga, a_hga == 35, 9 ))%>%
  mutate(a_hga = replace(a_hga, a_hga == 36, 10))%>%
  mutate(a_hga = replace(a_hga, a_hga == 37, 11))%>%
  mutate(a_hga = replace(a_hga, a_hga > 37, 12))%>%
  mutate(wcollar = ifelse((a_mjocc== 1 | a_mjocc== 2 | a_mjocc== 5 ), 1, 0))%>%
  mutate(bcollar = ifelse((a_mjocc== 6 | a_mjocc== 7 |
                             a_mjocc== 8 | a_mjocc== 9 |
                             a_mjocc==  10 | a_mjocc==  11), 1, 0))%>%
  mutate(lpearnval = ifelse(pearnval == 0 , 0, log(pearnval)))%>%
  mutate(lpearnval = replace(lpearnval, is.na(lpearnval), 0))%>%
  mutate(lpothval = ifelse(pothval == 0 , 0, log(pothval)))%>%
  mutate(lpothval = replace(lpothval, is.na(lpothval), 0))%>%
  mutate(lptotval = ifelse(ptotval == 0 , 0, log(ptotval)))%>%
  mutate(lptotval = replace(lptotval, is.na(lptotval), 0))%>%  
  mutate(one = one/1000)%>%
  mutate(pov = ifelse(perlis != 4,  1, 0))%>%
  select(-a_sex, -prcitshp, -pehspnon ); gc()

#rm(list=setdiff(setdiff(ls(), "cpsasec_df"), "dummy_df"))

# #plot out high/low pay ind/occ
# test_df<-dummy_df %>%
#   filter(a_mjind != 0)%>%
#   group_by(a_mjind )%>%
#   summarise(avg = mean(pearnval), mid = median(pearnval), n=n()); gc()
# 
# test_df<-dummy_df %>%
#   filter(a_mjocc != 0)%>%
#   group_by(a_mjocc )%>%
#   summarise(avg = mean(pearnval), mid = median(pearnval), n=n()); gc()
# 
# plot(test_df$avg, test_df$mid)
# text(test_df$avg, test_df$mid, labels=test_df$a_mjocc, cex= 1)
# rm(test_df)

#add parents info
join_df <- dummy_df %>%  
  left_join(
  dummy_df %>% setNames(paste0('p1_', names(.))), 
  by = c("h_idnum" = "p1_h_idnum", "pepar1" = "p1_a_lineno"))%>%
  left_join(
    dummy_df %>% setNames(paste0('p2_', names(.))), 
    by = c("h_idnum" = "p2_h_idnum", "pepar2" = "p2_a_lineno"))%>%
  mutate(step = ifelse(((pepar1typ == 2) | (pepar1typ == 2 )), 1, 0))%>%
  mutate(adopt = ifelse(((pepar1typ == 3) | (pepar1typ == 3 )), 1, 0))%>%
  mutate(step_adopt = ifelse((step ==1 | adopt == 1), 1, 0))%>%
  mutate(singlef_fam = ifelse(((pepar1typ == -1) & (pepar2typ != -1 )), 1, 0))%>%
  mutate(singlem_fam = ifelse(((pepar2typ == -1) & (pepar1typ != -1 )), 1, 0))%>%
  mutate(single = ifelse((singlef_fam ==1 | singlem_fam == 1), 1, 0))%>%
  mutate(livealone = ifelse(pepar1 == -1 & pepar2 == -1, 1, 0))%>%
  mutate(both_fam = ifelse(livealone == 0 & single == 0, 1, 0))%>%
  mutate(samesex = ifelse(p1_a_female == 0 | p2_a_female == 1, 1, 0))%>%
  mutate(dropout = if_else((hschool == 1) & (a_enrlw == 0), 1, 0)); gc()

#rm(list=setdiff(setdiff(ls(), "cpsasec_df"), "join_df"))

#4482
#test df for counting
test_df <- join_df %>%
filter(a_age > 15 & a_age <= 18)%>%
filter(samesex == 0)%>%
filter(p1_topcode == 0 & p2_topcode == 0); gc()

# 
# # rm(test_df)








###output1:describe data###
#run the imputation first
# cpsasec_design <-
#   svrepdesign(
#     weights = ~marsupwt ,
#     repweights = "^pwwgt[1-9]" ,
#     type = "Fay" ,
#     rho = ( 1 - 1 / sqrt( 4 ) ) ,
#     data = join_df ,
#     combined.weights = TRUE ,
#     mse = TRUE
#   ); gc()

cpsasec_design <-
  svrepdesign(
    weights = ~pwwgt0 ,
    repweights = "^pwwgt[1-9]" ,
    type = "Fay" ,
    rho = ( 1 - 1 / sqrt( 4 ) ) ,
    data = join_df ,
    combined.weights = TRUE ,
    mse = TRUE
  ); gc()

#rm(list=setdiff(setdiff(ls(), "cpsasec_df"), "cpsasec_design"))

#exclude 27 same sex family
#it also exclude live alone 505
#filter 16 to 18
# n = 6891 (include same sex)
schoolchild_design <- 
  subset( cpsasec_design , a_age %in% 16:18 ); gc()


nontopcode_design <- 
  subset( schoolchild_design , (p1_topcode == 0 & p2_topcode == 0 & samesex == 0) ); gc()

#rm(list=setdiff(setdiff(setdiff(ls(), "cpsasec_df"), "nontopcode_design"), "join_df"));gc()

race_design <- 
  subset( nontopcode_design ,  prdtrace != 1  ); gc()


#table 1 # of state by compulsary schooling age (cut from else where(?))
#table 2 prop table living with parent and family type by age and by dropout
#shows the mean within group
xtable(svyby(formula = ~step_adopt + single + livealone + both_fam, by = ~  dropout + a_age, design = schoolchild_design, FUN = svymean, drop.empty.groups=FALSE))
join_df %>%
  filter(a_age > 15 & a_age <= 18)%>%
  group_by(a_age, dropout)%>%
  summarise(n = n_distinct(peridnum)); gc()

#table 3 nontopcode dropout, p_earn, p_hga by race
xtable(svyby(formula = ~ dropout + p1_lpearnval + p1_a_hga + p2_lpearnval + p2_a_hga, by = ~prdtrace, design = nontopcode_design, FUN = svymean, na.rm=F))

join_df %>%
  filter(a_age > 15 & a_age <= 18)%>%
  filter(topcode == 0)%>%
  group_by(prdtrace)%>%
  summarise(n = n_distinct(peridnum)); gc()

#table 4 (shea 2020 t1) by non-topcoded, topcoded, poverty report father and mother
#not exclude samesex
join_df %>%
  filter(a_age > 15 & a_age <= 18 & samesex == 0)%>%
  filter( p1_topcode == 0 & p2_topcode == 0)%>%
  summarise(n = n_distinct(peridnum)); gc()

svymean( ~ a_female + dropout + a_ftpt + hea + cov + nativeus + factor(prdtrace)
         + step_adopt + pov
         + p1_nativeus + p1_prdisflg 
         + p1_lpearnval  + p1_lpothval  
         + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
         + p1_hea + p1_cov 
         + p1_wcollar + p1_bcollar
         + p2_nativeus + p2_prdisflg
         + p2_lpearnval  + p2_lpothval  
         + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
         + p2_hea + p2_cov 
         + p2_wcollar + p2_bcollar,
          nontopcode_design, na.rm=T)


#not exclude samesex
join_df %>%
  filter(a_age > 15 & a_age <= 18 & samesex == 0)%>%
  filter(prdtrace != 1 & p1_topcode == 0 & p2_topcode == 0)%>%
  summarise(n = n_distinct(peridnum)); gc()

svymean( ~ a_female + dropout + a_ftpt + hea + cov + nativeus + factor(prdtrace) 
         + step_adopt + pov
         + p1_nativeus  + p1_prdisflg 
         + p1_lpearnval  + p1_lpothval  
         + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
         + p1_hea + p1_cov 
         + p1_wcollar + p1_bcollar
         + p2_nativeus + p2_prdisflg
         + p2_lpearnval  + p2_lpothval  
         + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
         + p2_hea + p2_cov 
         + p2_wcollar + p2_bcollar
         , race_design, na.rm=T)


#table 5 (in&edu t2) compare listed var by dropout or not by age
xtable(svyby(formula = ~ p1_lpearnval  + p1_a_hga 
      + p2_lpearnval  + p2_a_hga, 
      by = ~dropout + a_age, design = nontopcode_design, FUN = svymean, na.rm=F))

join_df %>%
  filter(a_age > 15 & a_age <= 18)%>%
  filter(topcode == 0)%>%
  group_by(a_age, dropout)%>%
  summarise(n = n_distinct(peridnum)); gc()

#compare dropout by living alone or not
svyttest(dropout~livealone ,  schoolchild_design)


#plot 1 dropout by son and daughter against parent 1&2 earning
plot_df<-join_df %>%
  filter(a_age > 15 & a_age <= 18)

union_p1_df<-
  plot_df%>%
  group_by(p1_union)%>%
  summarise(p1_lpearnval = mean(p1_lpearnval)); gc()

union_p2_df<-
  plot_df%>%
  group_by(p2_union)%>%
  summarise(p2_lpearnval = mean(p2_lpearnval)); gc()
  
ggplot(plot_df, aes(x=p1_lpearnval, colour=factor(p1_union))) +
  geom_density() +
  geom_vline(data=union_p1_df, aes(xintercept=p1_lpearnval,  colour=factor(p1_union)),
             linetype="dashed", size=1)+ 
  labs(title = "Distribution of Log Mother Earning by Union Status",
       x = "Log Earning",
       y= "Density",
       colour = "Union status")


ggplot(plot_df, aes(x=p2_lpearnval, colour=factor(p2_union))) +
  geom_density() +
  geom_vline(data=union_p2_df, aes(xintercept=p2_lpearnval,  colour=factor(p2_union)),
             linetype="dashed", size=1)+ 
  labs(title = "Distribution of Log Father Earning by Union Status",
       x = "Log Earning",
       y= "Density",
       colour = "Union status")



#industry
p1_ind<-join_df %>%
  filter(a_age > 15 & a_age <= 18)%>%
  filter(p1_a_mjind != 0)%>%
  group_by(p1_a_mjind)%>%
  summarize(p1_lpearnval = mean(p1_lpearnval));gc()

p2_ind<-join_df %>%
  filter(a_age > 15 & a_age <= 18)%>%
  filter(p2_a_mjind != 0)%>%
  group_by(p2_a_mjind)%>%
  summarize(p2_lpearnval = mean(p2_lpearnval));gc()

ggplot(join_df%>%
         filter(a_age > 15 & a_age <= 18)%>%
         filter(p1_lpearnval != 0 & p1_a_mjind != 0), 
       aes(x = p1_a_mjind, y = p1_lpearnval, color = factor(p1_a_mjind))) + 
  geom_point() + 
  geom_point(data = p1_ind, size = 4) + 
  scale_colour_hue() + 
  theme_bw() +
  guides(color = guide_legend("Industry", ncol=2, bycol=TRUE))+
  labs(
    title = "Industry to Log Earning for Mother",
    x = "Industry",
    y = "Log Earning")

ggplot(join_df%>%
         filter(a_age > 15 & a_age <= 18)%>%
         filter(p2_lpearnval != 0 & p2_a_mjind != 0), 
       aes(x = p2_a_mjind, y = p2_lpearnval, color = factor(p2_a_mjind))) + 
  geom_point() + 
  geom_point(data = p2_ind, size = 4) + 
  scale_colour_hue() + 
  theme_bw() +
  guides(color = guide_legend("Industry", ncol=2, bycol=TRUE))+
  labs(
    title = "Industry to Log Earning for Father",
    x = "Industry",
    y = "Log Earning")

###simulation###
#1 earning to dropout
probit1 <- svyglm( dropout ~ p1_lpearnval + p2_lpearnval, 
                      nontopcode_design,  family=binomial(link="probit")); gc() 


#2 control personal trait
probit2 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + p1_lpearnval + p2_lpearnval, 
                   nontopcode_design,  family=binomial(link="probit")); gc()  



#3 control parent trait & family type
probit3 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov
                   + p1_nativeus  + p1_prdisflg 
                   + p1_lpearnval  + p1_lpothval + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lpearnval  + p2_lpothval + p2_wcollar + p2_bcollar   
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   nontopcode_design,  family=binomial(link="probit")); gc()  




# cotrol state effect
probit4 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov + factor(gestfips)
                   + p1_nativeus + p1_prdisflg 
                   + p1_lpearnval + p1_lpothval + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lpearnval + p2_lpothval + p2_wcollar + p2_bcollar    
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   design=nontopcode_design,  family=binomial(link="probit")); gc() 

#p1
probit5 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov 
                   + p1_nativeus  + p1_prdisflg 
                   + p1_lpearnval  + p1_lpothval + p1_wcollar + p1_bcollar  
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov , 
                   design=  subset( schoolchild_design , p1_topcode == 0 ),  
                   family=binomial(link="probit")); gc() 

#p2
probit6 <- svyglm( dropout ~ a_female + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov
                   +  p2_nativeus +  p2_prdisflg
                   + p2_lpearnval  + p2_lpothval+ p2_wcollar + p2_bcollar    
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   design=  subset( schoolchild_design , p2_topcode ==0  ),  
                   family=binomial(link="probit")); gc() 

probitm <- svyglm( dropout ~ a_female  + hea + cov + nativeus 
                   + step_adopt + pov
                   + p1_nativeus + p1_prdisflg 
                   + p1_lpearnval  + p1_lpothval + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lpearnval  + p2_lpothval + p2_wcollar + p2_bcollar 
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   design=nontopcode_design,  family=binomial(link="probit")); gc() 


#solve output prb
stargazer(probit1, probit2, probit3, probit4, probit5, probit6,
          out="C:/Users/test/Dropbox/thesis/asecpub20csv/glm_probit_nontopcode.tex")


stargazer(probit1, probit2, probit3, probit4, probit5, probit6, type = "html",
          out="C:/Users/test/Dropbox/thesis/asecpub20csv/test.html")

#report margin
#average marginal effect
xtable(summary(margins(probitm)))


#overall income
#1 earning to dropout
probit1 <- svyglm( dropout ~ p1_lptotval + p2_lptotval, 
                   nontopcode_design,  family=binomial(link="probit")); gc() 


#2 control personal trait
probit2 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + p1_lptotval + p2_lptotval, 
                   nontopcode_design,  family=binomial(link="probit")); gc()  



#3 control parent trait & family type
probit3 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov
                   + p1_nativeus  + p1_prdisflg 
                   + p1_lptotval + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lptotval  + p2_wcollar + p2_bcollar   
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   nontopcode_design,  family=binomial(link="probit")); gc()  




# cotrol state effect
probit4 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov + factor(gestfips)
                   + p1_nativeus + p1_prdisflg 
                   + p1_lptotval  + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lptotval  + p2_wcollar + p2_bcollar    
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   design=nontopcode_design,  family=binomial(link="probit")); gc() 

#p1
probit5 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov 
                   + p1_nativeus  + p1_prdisflg 
                   + p1_lptotval   + p1_wcollar + p1_bcollar  
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov , 
                   design=  subset( schoolchild_design , p1_topcode == 0 ),  
                   family=binomial(link="probit")); gc() 

#p2
probit6 <- svyglm( dropout ~ a_female + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt + pov
                   +  p2_nativeus +  p2_prdisflg
                   + p2_lptotval  + p2_wcollar + p2_bcollar    
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   design=  subset( schoolchild_design , p2_topcode ==0  ),  
                   family=binomial(link="probit")); gc() 

probitm <- svyglm( dropout ~ a_female  + hea + cov + nativeus 
                   + step_adopt + pov
                   + p1_nativeus + p1_prdisflg 
                   + p1_lptotval   + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lptotval   + p2_wcollar + p2_bcollar 
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov , 
                   design=nontopcode_design,  family=binomial(link="probit")); gc() 


#solve output prb
stargazer(probit1, probit2, probit3, probit4, probit5, probit6,
          out="C:/Users/test/Dropbox/thesis/asecpub20csv/glm_probit_income.tex")


stargazer(probit1, probit2, probit3, probit4, probit5, probit6, type = "html",
          out="C:/Users/test/Dropbox/thesis/asecpub20csv/income.html")

#report margin
#average marginal effect
xtable(summary(margins(probitm)))



#repeat for nonwhite sample
#1 earning to dropout
probit1 <- svyglm( dropout ~ p1_lpearnval + p2_lpearnval, 
                   race_design,  family=binomial(link="probit")); gc() 


#2 control personal trait
probit2 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + p1_lpearnval + p2_lpearnval, 
                   race_design,  family=binomial(link="probit")); gc()  


#3 control parent trait & family type
probit3 <- svyglm( dropout ~ a_female  + hea + cov + nativeus + factor(prdtrace) 
                   + step_adopt+ pov
                   + p1_nativeus + p1_prdisflg 
                   + p1_lpearnval  + p1_lpothval + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov 
                   + p2_nativeus + p2_prdisflg
                   + p2_lpearnval  + p2_lpothval + p2_wcollar + p2_bcollar   
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov ,
                   race_design,  family=binomial(link="probit")); gc()  




probitm <- svyglm( dropout ~ a_female  + hea + cov + nativeus 
                   + step_adopt + pov 
                   + p1_nativeus + p1_prdisflg 
                   + p1_lpearnval  + p1_lpothval + p1_wcollar + p1_bcollar
                   + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                   + p1_hea + p1_cov         
                   + p2_nativeus + p2_prdisflg
                   + p2_lpearnval  + p2_lpothval + p2_wcollar + p2_bcollar 
                   + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                   + p2_hea + p2_cov ,  
                   design=race_design,  family=binomial(link="probit")); gc() 



stargazer(probit1, probit2, probit3,
          out="C:/Users/test/Dropbox/thesis/asecpub20csv/glm_probit_rac.tex")

stargazer(probit1, probit2, probit3, type = "html",
          out="C:/Users/test/Dropbox/thesis/asecpub20csv/glm_probit_rac.html")

xtable(summary(margins(probitm)))


#RESUME in&edu t4 shea t2


iv_p1<- as.data.frame(withReplicates(nontopcode_design, 
               function(.weights, .data){
                 .data$.pweights<-.weights
                 m<-ivreg(dropout ~ p1_lpearnval                           
                          + a_female  + hea + cov + nativeus + factor(prdtrace) 
                          + step_adopt + pov
                          + p1_nativeus + p1_prdisflg
                          + p1_lpothval 
                          + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                          + p1_hea + p1_cov 
                          + p1_wcollar + p1_bcollar
                          + p2_nativeus + p2_prdisflg
                          + p2_lpearnval + p2_lpothval  
                          + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                          + p2_hea + p2_cov 
                          + p2_wcollar + p2_bcollar
                          
                          | p1_union + factor(p1_a_mjind)  
                          + a_female  + hea + cov + nativeus + factor(prdtrace) 
                          + step_adopt + pov
                          + p1_nativeus + p1_prdisflg
                          + p1_lpothval 
                          + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                          + p1_hea + p1_cov 
                          + p1_wcollar + p1_bcollar
                          + p2_nativeus + p2_prdisflg
                          + p2_lpearnval + p2_lpothval  
                          + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                          + p2_hea + p2_cov 
                          + p2_wcollar + p2_bcollar
                          ,data= .data, weights=.pweights)
                 coef(m)
               }));gc()


iv_p2<- as.data.frame(withReplicates(nontopcode_design, 
                       function(.weights, .data){
                         .data$.pweights<-.weights
                         m<-ivreg(dropout ~ p2_lpearnval                           
                                  +  a_female  + hea + cov + nativeus + factor(prdtrace) 
                                  + step_adopt + pov
                                  + p1_nativeus + p1_prdisflg
                                  + p1_lpearnval + p1_lpothval 
                                  + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                                  + p1_hea + p1_cov 
                                  + p1_wcollar + p1_bcollar
                                  + p2_nativeus + p2_prdisflg
                                  + p2_lpothval  
                                  + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                                  + p2_hea + p2_cov 
                                  + p2_wcollar + p2_bcollar
                                  
                                  | p2_union + factor(p2_a_mjind) 
                                  +  a_female  + hea + cov + nativeus + factor(prdtrace) 
                                  + step_adopt + pov
                                  + p1_nativeus + p1_prdisflg
                                  + p1_lpearnval + p1_lpothval 
                                  + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
                                  + p1_hea + p1_cov 
                                  + p1_wcollar + p1_bcollar
                                  + p2_nativeus + p2_prdisflg
                                  + p2_lpothval  
                                  + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
                                  + p2_hea + p2_cov 
                                  + p2_wcollar + p2_bcollar
                                  ,data= .data, weights=.pweights)
                         coef(m)
                       }));gc()



#df = N-p(#of x + intercept)
iv_res1 <-iv_p1%>%
  mutate(t_value = theta/SE)%>%
  mutate(p_value = (2*pt(-abs(t_value), df=4103-55)))%>%
  mutate(star = if_else(p_value <= 0.1, '*', 
                        if_else(p_value <= 0.05, '**',
                                if_else(p_value <= 0.01, '***', ''))));gc()

iv_res2 <-iv_p2%>%
  mutate(t_value = theta/SE)%>%
  mutate(p_value = (2*pt(-abs(t_value), df=4103-55)))%>%
  mutate(star = if_else(p_value <= 0.1, '*', 
                if_else(p_value <= 0.05, '**',
                if_else(p_value <= 0.01, '***', ''))));gc()


xtable(iv_res1)
xtable(iv_res2)

#test

iv_p1<-ivreg(dropout ~ p1_lpearnval                           
             + a_female  + hea + cov + nativeus + factor(prdtrace) 
             + step_adopt + pov
             + p1_nativeus + p1_prdisflg
             + p1_lpothval 
             + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
             + p1_hea + p1_cov 
             + p1_wcollar + p1_bcollar
             + p2_nativeus + p2_prdisflg
             + p2_lpearnval + p2_lpothval  
             + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
             + p2_hea + p2_cov 
             + p2_wcollar + p2_bcollar
             
             | p1_union + factor(p1_a_mjind)  
             + a_female  + hea + cov + nativeus + factor(prdtrace) 
             + step_adopt + pov
             + p1_nativeus + p1_prdisflg
             + p1_lpothval 
             + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
             + p1_hea + p1_cov 
             + p1_wcollar + p1_bcollar
             + p2_nativeus + p2_prdisflg
             + p2_lpearnval + p2_lpothval  
             + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
             + p2_hea + p2_cov 
             + p2_wcollar + p2_bcollar
         ,data=test_df);gc()

iv_p2<-ivreg(dropout ~ p2_lpearnval                           
             +  a_female  + hea + cov + nativeus + factor(prdtrace) 
             + step_adopt + pov
             + p1_nativeus + p1_prdisflg
             + p1_lpearnval + p1_lpothval 
             + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
             + p1_hea + p1_cov 
             + p1_wcollar + p1_bcollar
             + p2_nativeus + p2_prdisflg
             + p2_lpothval  
             + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
             + p2_hea + p2_cov 
             + p2_wcollar + p2_bcollar
             
             |   factor(p2_a_mjind) + p2_union
             +  a_female  + hea + cov + nativeus + factor(prdtrace) 
             + step_adopt + pov
             + p1_nativeus + p1_prdisflg
             + p1_lpearnval + p1_lpothval 
             + p1_a_hga + p1_hschool + p1_college + p1_master + p1_doctor
             + p1_hea + p1_cov 
             + p1_wcollar + p1_bcollar
             + p2_nativeus + p2_prdisflg
             + p2_lpothval  
             + p2_a_hga + p2_hschool + p2_college + p2_master + p2_doctor
             + p2_hea + p2_cov 
             + p2_wcollar + p2_bcollar
         ,data=test_df);gc()

summary(iv_p1, diagnostics = TRUE)
summary(iv_p2, diagnostics = TRUE)


