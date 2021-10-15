library(readxl)
parents <- read_excel("responses.xlsx")
View(parents)  

library(readxl)
children <- read_excel("Kids.xlsx")
View(children) 

install.packages("tidyverse")
install.packages("rafalib")
install.packages("dplyr")
install.packages('plyr')
install.packages("ggplot2")

library(tidyverse)
library(rafalib)
library(dplyr)
library(plyr)
library(ggplot2)

### data cleaning ###

#rename parents column names (survey questions) with the inital (p)
names(parents)<- c("code", "group_code", "child_username", "p_parent_literacy", "p_child_literacy",
                     "p_child_participation_imp", "p_financial_knowledge_imp", "p_want_need_diff", 
                     "p_child_saving_schedule","p_child_decision_think", "p_child_decision_compare","p_child_decision_charity", 
                     "p_child_decision_postpone", "p_child_literacy_excited", "p_child_literacy_interested",
                     "p_disc", "p_disc_imp", "p_child_disc_confidence", "p_disc_freq", "p_child_game_rate", 
                     "p_parent_enjoy_literacy", "p_parent_literacy_rate", "p_interest_q", "p_stock_pfolio_q",
                     "p_interest_inflation_q", "p_start_date", "p_submit_date", "p_network_id")
names(parents)

#rename children column names (survey questions) with the inital (c)
names(children)
colnames(children)[c(1:28)]<- c("code", "group_code", "child_username", "c_gender", "c_age", "c_literacy_enjoy",
                             "c_financial_knowledge_imp", "c_decision_immidiate", "c_monthly_saving",
                             "c_manage_spending_saving", "c_charity_imp", "c_decision_think", 
                             "c_decision_confidence", "c_charity", "c_ask_advice", "c_car_q", "c_saving_q", 
                             "c_necklace_q", "c_plan_q", "c_warranty_q", "c_juice_q", "c_choices_q", "c_priority_q", 
                             "c_ball_q", "c_income_q", "c_start_date", "c_submit_date", "c_network_id")
View(parents)
View(children)
summary(children)
summary(parents)

#merge both dataframes
survey<- merge(parents, children, by.x="child_username", by.y="child_username", all=FALSE)
View(survey)
survey_all<- merge(parents, children, by.x="child_username", by.y="child_username", all=TRUE)

#drop unnecessary #code column
survey = subset(survey, select = -c(code.x, code.y))
survey_all = subset(survey_all, select = -c(code.x, code.y))
survey= subset(survey, select= -c(group_code.y))
#drop dates and unnecessary network_id
survey= subset(survey, select = -c(p_network_id, p_start_date, p_submit_date, c_start_date, c_submit_date, c_network_id))
#drop question regarding having a juice business due to insufficient data
survey= subset(survey, select= -c(c_juice_q))


names(survey)
head(survey)
class(survey$p_child_decision_postpone)

#recode likert scale items from string data to numerical data (categorical-ordinal to numerical-interval)
survey_coded<- survey%>% 
  mutate_at(c("p_child_literacy_excited", "p_child_literacy_interested", 
                                     "p_child_decision_postpone", "p_child_decision_charity", 
                                     "p_child_decision_compare","p_child_decision_think",
                                     "p_child_saving_schedule", "p_disc_imp", "p_disc", 
                                     "p_child_disc_confidence", "p_parent_enjoy_literacy", "p_want_need_diff"), 
                                   funs(recode(.,"لا أوافق بشدة"=1, "لا أوافق"=2, "محايد"=3, "أوافق"=4, "أوافق بشدة"=5, "لا أتفق"=2, "لا أتفق بشدة"=1))) %>%
  mutate_at(c("c_literacy_enjoy","c_financial_knowledge_imp", "c_decision_immidiate", 
                                              "c_monthly_saving", "c_manage_spending_saving", "c_charity_imp", 
                                              "c_decision_think", "c_decision_confidence"), 
                                            funs(recode(.,"لا أتفق أبدا"=1, "لا أتفق"=2,"لا أعلم"=3, "أتفق"=4, "أتفق تماما"=5,"لا أعرف"=3))) %>%
  mutate_at(c("p_parent_literacy", "p_child_literacy"),funs(recode(.,"لا"=1, "لا أعرف"=2, "لا أعرف أو لست متأكدا"=2, "نعم"=3))) %>%
  mutate_at(c("p_financial_knowledge_imp"),funs(recode(.,"غير مجدي إلى حد ما"=2,"محايد"=3,"مفيد إلى حد ما"=4,"مفيد للغاية"=5)))%>%
  mutate_at(c("p_interest_inflation_q", "p_interest_q"), funs(recode(.,"أرفض الإجابة"="prefer_no_answer", "لا أعرف"="dk", "أقل مما تشتريه اليوم"="less", "بالضبط نفس مشترياتك اليوم"= "equal", "أكثر مما تشتريه اليوم"="more", "أكثر من 102 دولار"="more", "أقل من 102 دولار بالضبط"="less","102 دولار بالضبط"="equal")))%>%
  mutate(c_gender= recode(c_gender, "بنت"="f", "ولد"="m"))%>%
  mutate(p_stock_pfolio_q= recode(p_stock_pfolio_q, "لا أعرف"="dk","أرفض الإجابة"="prefer_no_answer","غير صحيح"="wrong", "صحيح"="correct"))%>%
  mutate(c_ask_advice= recode(c_ask_advice, "مستبعد تماما"=1,"نادرا"=2, "قليلا"=3, "محتمل"=4, "محتمل جدا"=5))%>%
  mutate(p_child_participation_imp= recode(p_child_participation_imp, "لا، لا ينبغي أن يشاركوا في القرارات المالية."="no", "نعم، يجب أن يشاركوا في بعض القرارات المالية."="yes_some", "نعم، ينبغي أن يشاركوا في العديد من القرارات المالية."= "yes_many")) %>%
  mutate(c_charity= recode(c_charity, "لم أفكر من قبل بالأعمال الخيرية"=1, "ليس مهما"=2, "مهم قليلا"=3, "مهم"=4, "مهم جدا"=5)) %>% 
  mutate(c_income_q= recode(c_income_q, "ينفق 100 نقطة ويتبرع بـ 100 نقطة"= "50%charity","لا ينفق شيء ويتبرع بالـ 200 نقطة"= "100%charity","ينفق الـ 200 نقطة ولا يتبرع بشيء"= "0%charity","لا أعلم"="dk")) %>% 
  mutate(c_necklace_q= recode(c_necklace_q, "لا أعلم أو لست متأكدا"="dk")) %>% 
  mutate(c_car_q= recode(c_car_q, "السيارة الأولى"="car1","السيارة الثانية"="car2","لا أعلم أو لست متأكدا"="dk","كلاهما نفس التكلفة"="same_cost"))%>%
  mutate(c_plan_q= recode(c_plan_q, "مدخراتها منخفضة للغاية"="vsmall_savings", "إنفاقها ومدخراتها أكبر من دخلها"="spending&saving_morethan_income",
                          "لا أعلم أو لست متأكدا"="dk", "دخلها أكبر من انفاقها"= "income_morethan_spending", "دخلها ومدخراتها أكبر من إنفاقها"="income&saving_morethan_spending"))%>%
  mutate(c_warranty_q= recode(c_warranty_q, "عند شراء سيارة"="car", "عند شراء الحلويات"="candy", "عند شراء كرة سلة جديدة"="basketball", "كل ما سبق"="alltheprevious","لا أعرف أو لست متأكدا"="dk")) %>% 
  mutate(c_choices_q= recode(c_choices_q, "خدمة التوصيل"="delivery","طاولة العصير"="juice","كلاهما يكسب نفس الشيء"="same_gain","لن يكسبني أي منهما المال"="no_gain","لا أعلم أو لست متأكدا"="dk")) %>% 
  mutate(c_priority_q= recode(c_priority_q, "شراء اللوازم المدرسية والزي المدرسي"="uniform","شراء جهاز تلفزيون"="TV", "دفع الإيجار"="rent","مشتريات البقالة لهذا الشهر"="groceries", "لا أعلم أو لست متأكدا"="dk")) %>% 
  mutate(c_ball_q= recode(c_ball_q, "أن يشتري الكتاب المدرسي الآن ويشتري كرة القدم الشهر القادم عندما يحصل على مصروفه الشهري"="uniform now & ball later", "ألا يشتري كرة القدم لأنه مضطر لشراء الكتاب المدرسي"="uniform now & no ball","أن يشتري كرة القدم لأنه يملك ما يكفي من المال" ="ball now & enough money","أنه يستطيع شراء كرة القدم، لكنه لن يتمكن من شراء الكتاب المدرسي"="ball now & no uniform","لا أعلم أو لست متأكدا"="dk")) %>% 
  mutate(c_saving_q= recode(c_saving_q, "اشترى محمد سيارة وأجرها لشخص آخر وهكذا يحصل على دخل إضافي ينفقه مع أسرته."="buy car & rent it for extra income","يوفر محمد جزءا من دخله لأنه يريد شراء سيارة بعد عامين."="save from income to buy car later","يطلب محمد اقتراض المال من عائلته لأنه يريد شراء سيارة بعد ستة أشهر."="borrow money to buy car","لا أعلم أو لست متأكدا"="dk")) %>% 
  mutate(c_age= recode(c_age, "9"=9, "10"=10, "11"=11, "12"=12, "12+"=13)) %>% #(12+ is mutated to 13 for simplicity)  
  mutate(p_child_game_rate= recode(p_child_game_rate, "لا أعرف"= "dk", "طفلي لا يملك خبرة في محتوى وألعاب الوعي المالي"="no exp","قليل جدا"="very low exp","قليل"="low exp", "يوجد محتوى لكن لا يستهدف الأطفال"="content not for children","يوجد الكثير من المحتوى عن السلوك المالي"="lots of content", "يوجد محتوى عن السلوك المالي"="some content"))
#checking out data frame
dim(survey_coded)
names(survey_coded)
str(survey_coded)
levels(survey_coded$gender)
#compare columns
survey_coded[1:10, c("p_child_decision_charity", "c_charity_imp", "c_charity")]


##see the count of correct answers for the 3 questions that test financial knowledge of parents
survey_coded$p_stock_pfolio_q_correct<- ifelse(survey_coded$p_stock_pfolio_q == "correct", 1, 0)
survey_coded$p_interest_q_correct<- ifelse(survey_coded$p_interest_q == "more", 1, 0)
survey_coded$p_interest_inflation_q_correct<- ifelse(survey_coded$p_interest_inflation_q == "less", 1, 0)

parents_test<- c("p_stock_pfolio_q_correct", "p_interest_q_correct", 
                 "p_interest_inflation_q_correct") #index for 3qs test for parents
count(survey_coded$p_stock_pfolio_q_correct)
count(survey_coded$p_interest_q_correct)
count(survey_coded$p_interest_inflation_q_correct)

##calculate total score for 3-questions test for parents
survey_coded$parents_test_total<- apply(survey_coded[,parents_test],1, sum, na.rm=TRUE)
count(survey_coded$parents_test_total)
summary(survey_coded$parents_test_total)
ggplot(survey_coded, aes(x=parents_test_total)) + geom_bar() #most people scored 0/3,following 1/3

#see scores of parents who enjoy learning about financial stuff
ggplot(survey_coded, aes(x=p_parent_enjoy_literacy)) + geom_bar() #most agree about enjoying that
summary(survey_coded$p_parent_enjoy_literacy) #mean=3.8(~agree)
count(survey_coded$p_parent_enjoy_literacy)

## NOTE: as a conclusion: they enjoy learning, 
### however,their level of financial literacy is very low

#convert parent literacy rate column from character to numeric
survey_coded$p_parent_literacy_rate <- as.numeric(survey_coded$p_parent_literacy_rate)
str(survey_coded$p_parent_literacy_rate)
#see the scores of parents rating their financial literacy in a 1-7 scale
count(survey_coded$p_parent_literacy_rate)
median(survey_coded$p_parent_literacy_rate)
ggplot(survey_coded, aes(x=p_parent_literacy_rate)) + geom_bar()

#create a vector for 8 questions asked to parents regarding their 
#child behavior in a likert scale form
child_behavior_monitor<- c("p_want_need_diff","p_child_saving_schedule","p_child_decision_think",
                           "p_child_decision_compare","p_child_decision_charity",
                           "p_child_decision_postpone","p_child_literacy_excited",
                           "p_child_literacy_interested")

#Create index variable to create a score for the 8 questions collectively 
#as an average for each child
survey_coded$child_behavior_monitor_index<- apply(survey_coded[,child_behavior_monitor],1, mean, na.rm=TRUE)
ggplot(survey_coded, aes(x=child_behavior_monitor_index)) + geom_bar()
hist(survey_coded$child_behavior_monitor_index)
mean(survey_coded$child_behavior_monitor_index)
median(survey_coded$child_behavior_monitor_index)
View(survey_coded$child_behavior_monitor_index)
survey_coded[1:5, c("child_behavior_monitor_index","p_want_need_diff","p_child_saving_schedule",
                     "p_child_decision_think","p_child_decision_compare","p_child_decision_charity",
                     "p_child_decision_postpone","p_child_literacy_excited","p_child_literacy_interested")]
summary(survey_coded$child_behavior_monitor_index)
write.csv(survey_coded$child_behavior_monitor_index, "child_behavior_monitor_index")
count(survey_coded$child_behavior_monitor_index)

##children questions analysis (9 questions out of which only one is according to child's preference)
#c_car_q -> car1 is the right answer
ggplot(survey_coded, aes(x=c_car_q)) + geom_bar()

#c_saving_q
ggplot(survey_coded, aes(x=c_saving_q)) + geom_bar()

#c_necklace_q -> 3 is the right answer
ggplot(survey_coded, aes(x=c_necklace_q)) + geom_bar()

#c_plan_q -> spending and saving is greater than income
ggplot(survey_coded, aes(x=c_plan_q)) + geom_bar()

#c_warranty_q -> car
ggplot(survey_coded, aes(x=c_warranty_q)) + geom_bar()
unique(survey_coded$c_warranty_q) #extract categories in column
count(survey_coded$c_warranty_q)

#c_choices_q -> delivery
unique(survey_coded$c_choices_q)

#c_priority_q -> TV
unique(survey_coded$c_priority_q)
ggplot(survey_coded, aes(x=c_priority_q)) + geom_bar()

#c_ball_q -> uniform now, ball next month
unique(survey_coded$c_ball_q)

#c_income_q -> ##according to child's preference##
unique(survey_coded$c_income_q)
ggplot(survey_coded, aes(x=c_income_q)) + geom_bar()
count(survey_coded$c_income_q)

##see the count of correct answers for the 8 questions that test financial knowledge of children
survey_coded$c_car_q_correct<- ifelse(survey_coded$c_car_q == "car1", 1, 0) #54 correct                                
survey_coded$c_saving_q_correct<- ifelse(survey_coded$c_saving_q == "save from income to buy car later", 1, 0) #31 correct                                
survey_coded$c_necklace_q_correct<- ifelse(survey_coded$c_necklace_q == "3", 1, 0) #70 correct                                
survey_coded$c_plan_q_correct<- ifelse(survey_coded$c_plan_q == "spending&saving_morethan_income", 1, 0) #40 correct
survey_coded$c_warranty_q_correct<- ifelse(survey_coded$c_warranty_q == "car", 1, 0) #46 correct           
survey_coded$c_choices_q_correct<- ifelse(survey_coded$c_choices_q == "delivery", 1, 0) #64 correct                        
survey_coded$c_priority_q_correct<- ifelse(survey_coded$c_priority_q == "TV", 1, 0) #28 correct                 
survey_coded$c_ball_q_correct<- ifelse(survey_coded$c_ball_q == "uniform now & ball later", 1, 0) #44 correct

count(survey_coded$c_ball_q_correct)

children_test<- c("c_car_q_correct", "c_saving_q_correct", "c_necklace_q_correct", "c_plan_q_correct", "c_warranty_q_correct", 
                 "c_choices_q_correct", "c_priority_q_correct", "c_ball_q_correct") #index for 8qs test for children
##calculate total score for 8-questions test for children
survey_coded$children_test_total<- apply(survey_coded[,children_test],1, sum, na.rm=TRUE)
count(survey_coded$children_test_total)
summary(survey_coded$children_test_total)
ggplot(survey_coded, aes(x=children_test_total)) + geom_bar() #average score is 3/8
hist(survey_coded$children_test_total, breaks=11)

#check age of children (12+ was mutated to 13 for simplicity)
#survey_coded$c_age= factor(survey_coded$c_age, levels=c("9", "10", "11","12", "12+")) #change to factor
summary(survey_coded$c_age)
ggplot(survey_coded, aes(x=c_age)) + geom_bar()

#check correlation between age and children test score
cor(survey_coded$c_age, survey_coded$children_test_total)
?crosstab
#load crosstab function
#http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(survey_coded, row.vars = "c_age", col.vars = "children_test_total", type = "f") #f:frequency
crosstab(survey_coded, row.vars = "c_age", col.vars = "children_test_total", type = "r") #r:row percentage
survey_coded$c_age2= factor(survey_coded$c_age, levels=c(9, 10, 11,12)) 

#percentage of males and females across different ages
crosstab(survey_coded, row.vars = "c_age", col.vars = "c_gender", type = "r") #r:row percentage

#previous financial education 1:no, 3:yes
count(survey_coded$p_parent_literacy)
count(survey_coded$p_child_literacy)
ggplot(survey_coded, aes(x=p_parent_literacy)) + geom_bar()
ggplot(survey_coded, aes(x=p_child_literacy)) + geom_bar()

#imp of financial literacy
ggplot(survey_coded, aes(x=p_financial_knowledge_imp)) + geom_bar()
count(survey_coded$p_financial_knowledge_imp) #79% see it imp & vimp

ggplot(survey_coded, aes(x=p_child_decision_compare)) + geom_bar()
median(survey_coded$p_child_decision_compare)
count(survey_coded$p_child_decision_compare) 

ggplot(survey_coded, aes(x=p_child_decision_think)) + geom_bar()
ggplot(survey_coded, aes(x=p_disc)) + geom_bar()
ggplot(survey_coded, aes(x=p_disc_imp)) + geom_bar()
count(survey_coded$p_disc) 
count(survey_coded$p_disc_imp) 