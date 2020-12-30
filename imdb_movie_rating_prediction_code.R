raw_films_dataset_2019 = read.csv('C:\\...\\films_dataset_2019.csv')

attach(raw_films_dataset_2019)
raw_data <- raw_films_dataset_2019

###############################################################################################
#######################Step 1: data pre-processing#############################################
###############################################################################################

###############################################################################################
#######################Step 1.1: Film Characteristics pre-processing###########################
###############################################################################################

######Descriptive Stats -- Budget##############################################################
bdgt_stats <- summary(raw_data$budget)

######Missing values for budget################################################################
attach(raw_data)
plot(release_year,budget,col=cm.colors(10))

######Fill missing budget with yearly average. If no yearly average, replace with 0############
library(dplyr)
raw_data1 <- raw_data %>% group_by(release_year) %>% 
    mutate(budget = replace(budget,is.na(budget),mean(budget,na.rm = TRUE))) %>% 
    mutate(budget = replace(budget,is.na(budget),0))

attach(raw_data1)
plot(release_year,budget,col=terrain.colors(10))

######Dummify month variables (11 additional columns created without January)##################
library(fastDummies)
raw_data2 <- raw_data1 %>% arrange(release_month) %>% 
    dummy_cols(select_columns = "release_month", remove_first_dummy = TRUE) %>% 
    select(-release_month)

#####Reseason why dropping columns of release_day##############################################
plot(release_day,imdbRating,xlab='release days',ylab = 'imdbRating',col=topo.colors(2)) 
#####Below code shows that barely have correlation between release_day and imdbRating##########
cor(raw_data2$imdbRating, raw_data2$release_day) 
day_avg_rating <- raw_data2 %>% group_by(release_day) %>% summarise(avg_day = mean(imdbRating)) 
#####The graph shows release_day has no impact on imdbRating##################################
plot(day_avg_rating$release_day,day_avg_rating$avg_day,col=terrain.colors(10)) 
raw_data3 <- raw_data2 %>% select(-release_day)

#####Reason why dropping columns of main_spoken_language######################################
plot(raw_data3$main_spoken_language,col=heat.colors(7))
raw_data3$main_spoken_language <- ifelse(raw_data3$main_spoken_language=="English",1,0)
raw_data3 <- raw_data3 %>% select(-main_spoken_language)

##############################################################################################
#######################Step 1.2: Production Characteristics pre-processing####################
##############################################################################################

#####Replace director_name with director_avg_rating ##########################################
director_reference_table <- raw_data3 %>% 
    group_by(main_director_name) %>% 
    summarise(director_avg_rating = mean(imdbRating))

director_no.of_film_produced <- raw_data3 %>%
     group_by(main_director_name) %>%
     count(main_director_name) %>%
     rename(productive_director=n) %>%
     mutate(productive_director=ifelse(productive_director>1,1,0))
 
director_reference_table <- director_reference_table %>% 
    left_join(director_no.of_film_produced,by="main_director_name") %>% 
    group_by(main_director_name) %>% 
     mutate(director_avg_rating=ifelse(productive_director==1,
                                       director_avg_rating,mean(director_avg_rating)))
raw_data4 <- raw_data3 %>% 
    left_join(director_reference_table,by="main_director_name") %>% 
    select(-main_director_name)

director_yearly_mean <- raw_data3 %>% group_by(release_year) %>% 
    summarise(director_yearly_avg = mean(imdbRating))

raw_data4 <- raw_data4 %>% left_join(director_yearly_mean,by="release_year") %>% 
    mutate(director_avg_rating=ifelse(productive_director==1,
                                      director_avg_rating,
                                      director_yearly_avg)) %>% select(-director_yearly_avg)

#####Replace producer_name with producer_avg_rating##########################################
producer_reference_table <- raw_data4 %>% group_by(main_producer_name) %>% 
    summarise(producer_avg_rating=mean(imdbRating))  
 
producer_no.of_film_produced <- raw_data4 %>% 
     group_by(main_producer_name) %>% 
     count(main_producer_name) %>% 
     rename(productive_producer=n) %>% 
     mutate(productive_producer=ifelse(productive_producer>1,1,0)) 
 
producer_reference_table <- producer_reference_table %>% 
     left_join(producer_no.of_film_produced,by="main_producer_name") %>%
     mutate(producer_avg_rating=ifelse(productive_producer==1,
                                       producer_avg_rating,
                                       mean(producer_avg_rating)))

raw_data5 <- raw_data4 %>% left_join(producer_reference_table,by="main_producer_name") %>% 
    select(-main_producer_name)

producer_yearly_mean <- raw_data4 %>% group_by(release_year) %>% 
    summarise(producer_yearly_avg = mean(imdbRating))

raw_data5 <- raw_data5 %>% left_join(producer_yearly_mean,by="release_year") %>% 
    mutate(producer_avg_rating=ifelse(productive_producer==1,
                                      producer_avg_rating,
                                      producer_yearly_avg)) %>% select(-producer_yearly_avg)

#####Replace production_company with production_company_rating ##############################
production_company_reference_table <- raw_data5 %>% group_by(main_production_company) %>% 
    summarise(company_avg_rating=mean(imdbRating)) 

production_company_no.of_film_produced <- raw_data5 %>%
    group_by(main_production_company) %>%
    count(main_production_company) %>%
    rename(productive_production_company=n) %>%
    mutate(productive_production_company=ifelse(productive_production_company>1,1,0))

production_company_reference_table <- production_company_reference_table %>%
    left_join(production_company_no.of_film_produced,by="main_production_company") %>%
    mutate(company_avg_rating=ifelse(productive_production_company==1,
                                                company_avg_rating,
                                                mean(company_avg_rating)))

raw_data6 <- raw_data5 %>% left_join(production_company_reference_table,
                                     by="main_production_company") %>% 
    select(-main_production_company)

production_company_yearly_mean <- raw_data5 %>% group_by(release_year) %>% 
    summarise(company_yearly_avg = mean(imdbRating))

raw_data6 <- raw_data6 %>% left_join(production_company_yearly_mean,by="release_year") %>% 
    mutate(company_avg_rating=ifelse(productive_production_company==1,
                                      company_avg_rating,
                                      company_yearly_avg)) %>% select(-company_yearly_avg)

#####Replace editor_name with editor_avg_rating##############################################
editor_reference_table <- raw_data6 %>% group_by(editor_name) %>% 
    summarise(editor_avg_rating=mean(imdbRating)) 
   
editor_no.of_film_produced <- raw_data6 %>%
     group_by(editor_name) %>%
     count(editor_name) %>%
     rename(productive_editor=n) %>%
     mutate(productive_editor=ifelse(productive_editor>1,1,0))

editor_reference_table <- editor_reference_table %>%
     left_join(editor_no.of_film_produced,by="editor_name") %>%
     mutate(editor_avg_rating=ifelse(productive_editor==1,
                                      editor_avg_rating,
                                      mean(editor_avg_rating)))

raw_data7 <- raw_data6 %>% left_join(editor_reference_table,by="editor_name") %>% 
    select(-editor_name)

editor_yearly_mean <- raw_data6 %>% group_by(release_year) %>% 
    summarise(editor_yearly_avg = mean(imdbRating))

raw_data7 <- raw_data7 %>% left_join(editor_yearly_mean,by="release_year") %>% 
    mutate(editor_avg_rating=ifelse(productive_editor==1,
                                     editor_avg_rating,
                                     editor_yearly_avg)) %>% select(-editor_yearly_avg)

#####Replace main_production_country with production_country_avg_rating #####################
production_country_reference_table <- raw_data7 %>% group_by(main_production_country) %>% 
    summarise(production_country_avg_rating=mean(imdbRating)) 

production_country_no.of_film_produced <- raw_data7 %>%
    group_by(main_production_country) %>%
    count(main_production_country) %>%
    rename(productive_production_country=n) %>%
    mutate(productive_production_country=ifelse(productive_production_country>1,1,0))

production_country_reference_table <- production_country_reference_table %>%
    left_join(production_country_no.of_film_produced,by="main_production_country") %>%
    mutate(production_country_avg_rating=ifelse(productive_production_country==1,
                                                production_country_avg_rating,
                                                mean(production_country_avg_rating)))

raw_data8 <- raw_data7 %>% left_join(production_country_reference_table,
                                     by="main_production_country") %>% 
    select(-main_production_country)

country_yearly_mean <- raw_data7 %>% group_by(release_year) %>% 
    summarise(country_yearly_avg = mean(imdbRating))

raw_data8 <- raw_data8 %>% left_join(country_yearly_mean,by="release_year") %>% 
    mutate(production_country_avg_rating=ifelse(productive_production_country==1,
                                    production_country_avg_rating,
                                    country_yearly_avg)) %>% select(-country_yearly_avg)

###############################################################################################
#######################Step 1.3: Cast Characteristics pre-processing###########################
###############################################################################################

#####Replace all 3 main actor names and start meter with main_actor_best_star_meter############
#####based on lowest star_meter among the three start_meters ##################################
#####Replace if any of the 3 main_actor is female to main_cast_have female#####################
main_actor_reference_table <- raw_data8 %>% select(imdb_id,imdbRating, main_actor1_name,
                                                   main_actor1_is_female,main_actor1_star_meter,
                                                   main_actor2_name,main_actor2_is_female,
                                                   main_actor2_star_meter,
                                                   main_actor3_name,main_actor3_is_female,
                                                   main_actor3_star_meter) %>% rowwise() %>% 
##########Select the best star_meter among the three actors####################################
    mutate(main_actor_best_star_meter = min(main_actor1_star_meter,
                                            main_actor2_star_meter,
                                            main_actor3_star_meter)) %>% 
#########Replace 3 seperate main_actori_is_female to main_cast_have_female#####################
    mutate(main_cast_have_female = ifelse(sum(main_actor1_is_female,
                                              main_actor2_is_female,
                                              main_actor3_is_female)>=1,1,0)) 

raw_data9 <- raw_data8 %>% left_join(main_actor_reference_table %>% 
                                         select(main_actor_best_star_meter,
                                                main_cast_have_female,imdb_id),by="imdb_id") %>% 
    select(-main_actor1_name,-main_actor1_is_female,
           -main_actor1_star_meter,-main_actor2_name,
           -main_actor2_is_female,-main_actor2_star_meter,
           -main_actor3_name,-main_actor3_is_female,-main_actor3_star_meter)

############# delete 3 columns of main_actor1_known_for########################################
raw_data10 <- raw_data9 %>% select(-main_actor1_known_for,-main_actor2_known_for,-main_actor3_known_for)

############# delete columns of ###############################################################
raw_data_clean <- raw_data10 %>% select(-film_title,-url)


###############################################################################################
####################### Step 2: Model Establishment############################################
###############################################################################################

#####Exclude colinearity between genre#########################################################
final_data <- raw_data_clean %>% select(-total_number_of_genres)
attach(final_data)
names(final_data)

#####Normalizing the continuous variables######################################################
#####Non-normal distribution for budget########################################################
hist(final_data$budget,col='Yellowgreen') 
final_data$budget = log(final_data$budget+1)
#####Normalised################################################################################
hist(final_data$budget,col='Yellowgreen') 
#####normally distrbuted####################################################################### 
hist(final_data$duration_minutes,col='Plum')
#####This dummifies the months by itself#######################################################
mreg_8 <- lm(imdbRating ~ .-imdb_id, data = final_data) 
summary(mreg_8)
plot(mreg_8,col='thistle') ###GIVES plots for Residuals, Heteroskedasticity and Outliers 


####Running Linear Regression using significant predictors##################################### 
mreg_9 = lm(imdbRating~budget+release_year+duration_minutes+genre_action+genre_adventure
            +genre_animation+genre_comedy+genre_documentary+genre_drama+genre_family
            +genre_fantasy+genre_horror+genre_realitytv+genre_scifi+genre_western
            +genre_shortfilm+total_number_of_actors+total_number_of_production_countries
            +director_avg_rating+productive_director+producer_avg_rating 
            +company_avg_rating+editor_avg_rating+production_country_avg_rating
            +main_cast_have_female)
plot(mreg_9,col=terrain.colors(4))

summary(mreg_9)


mreg_10 = lm(imdbRating~budget+release_year+duration_minutes+genre_action+genre_adventure
             +genre_animation+genre_comedy+genre_documentary+genre_drama+genre_family
             +genre_fantasy+genre_horror+genre_realitytv+genre_scifi+genre_western
             +genre_shortfilm+total_number_of_actors
             +director_avg_rating+productive_director+producer_avg_rating 
             +company_avg_rating+editor_avg_rating
             +production_country_avg_rating+main_cast_have_female)

plot(mreg_10,col=terrain.colors(10))
summary(mreg_10)

library(car)
residualPlots(mreg_10)
plot(predict(mreg_10), residuals(mreg_10), col="Lavender")
abline(0,0, lty=2)

#####Studentized Outlier Visual Detection######################################################
qqPlot(mreg_10, col = cm.colors(10))

#####Numerical outlier detection###############################################################
#####p-value < 0.05 are reported as outliers################################################### 
library(car)
outlierTest(mreg_10)

#####Create new dataset without the outliers###################################################
final_data_no_out = final_data[-c(532, 104, 3624, 4916, 834, 106, 4031, 200, 57, 2429),]

detach(final_data)
attach(final_data_no_out)

#####Rerun regression with new dataset without the outliers####################################
mreg_11 = lm(imdbRating~budget+release_year+duration_minutes+genre_action+genre_adventure
             +genre_animation+genre_comedy+genre_documentary+genre_drama+genre_family
             +genre_fantasy+genre_horror+genre_realitytv+genre_scifi+genre_western
             +genre_shortfilm+total_number_of_actors
             +director_avg_rating+productive_director+producer_avg_rating 
             +company_avg_rating+editor_avg_rating
             +production_country_avg_rating+main_cast_have_female)
plot(mreg_11,col=heat.colors(4))
summary(mreg_11)

#####Collinearity detection###################################################################
str(final_data_no_out)
require(psych)
library(ggplot2)
library(GGally)
ggcorr(final_data_no_out)



#####Taking only the quantitative variables from the final_data_no_out dataset################
quantvars = final_data_no_out[,c(3,4,5,7,8,9,11,13,14,15,16,19,23,25,26,30,31,48,49,50,52,54,56,59)]
pairs.panels(quantvars)

#####Rule of thumb: Collinearity is problematic when absolute value of correlation coefficient 
#####is above 0.8 or 0.85####################################################################
#####VIF > 4 is collinearity#################################################################
require(car)
vif(mreg_11)

#####Recheck nonlinearity####################################################################
residualPlots(mreg_11, col = terrain.colors(10))
plot(predict(mreg_11), residuals(mreg_11), col=terrain.colors(10))
abline(0,0, lty=2)


#####Nonlinear degree selection (k-fold with polynomial spline)############################## 
require(caTools)
library(boot)

batman = 5000000 ###Initiate a large number to store the least MSE in the loop############### 
joker = c(0,8) ###Initiate an empty vector to store the best combination of degrees##########

for (a in 2:3){
    for (b in 2:3){
        for (c in 2:3){
            for (d in 2:3){
                for (e in 2:3){
                    for (f in 2:3) {
                        for (g in 2:3) {
                            for (h in 2:3) {
                                lakers_1=glm(imdbRating~poly(budget,a) 
                                             +poly(release_year,b)+poly(duration_minutes,c)
                                             +genre_action+genre_adventure+genre_animation
                                             +genre_comedy+genre_documentary+genre_drama
                                             +genre_family+genre_fantasy+genre_horror
                                             +genre_realitytv+genre_scifi
                                             +genre_western+genre_shortfilm
                                             +poly(total_number_of_actors,d)
                                             +poly(director_avg_rating,e)+productive_director
                                             +poly(producer_avg_rating,f)
                                             +poly(company_avg_rating,g)+poly(editor_avg_rating,h)
                                             +production_country_avg_rating+main_cast_have_female)
                                rocket_1 = cv.glm(final_data_no_out,lakers_1,K=14)$delta[1]
                                if (rocket_1 < batman){
                                    batman=rocket_1
                                    joker[1]=a
                                    joker[2]=b
                                    joker[3]=c
                                    joker[4]=d
                                    joker[5]=e
                                    joker[6]=f
                                    joker[7]=g
                                    joker[8]=h 
                            }
                        }
                    }
                    }
                }
            }
        }
    }
}

batman
joker

###############################################################################################
############################################Final Model########################################
###############################################################################################
final_model <- lm(imdbRating~poly(release_year,3)+poly(total_number_of_actors,3)
                  +poly(duration_minutes,3)+poly(director_avg_rating,3)+poly(company_avg_rating,2)
                  +poly(budget,2)+poly(producer_avg_rating,2)+poly(editor_avg_rating,2)
                  +genre_action+genre_adventure+genre_animation+genre_comedy+genre_documentary
                  +genre_drama+genre_family+genre_fantasy+genre_horror+genre_realitytv+genre_scifi
                  +genre_western+genre_shortfilm+productive_director+production_country_avg_rating
                  +main_cast_have_female)

names(final_model$coefficients) = c('Intercept','release_year','release_year2','release_year3'
                                    ,'total_number_of_actors','total_number_of_actors2'
                                    ,'total_number_of_actors3','duration_minutes','duration_minutes2'
                                    ,'duration_minutes3','director_avg_rating','director_avg_rating2'
                                    ,'director_avg_rating3','company_avg_rating','company_avg_rating2'
                                    ,'budget','budget2','producer_avg_rating','producer_avg_rating2'
                                    ,'editor_avg_rating','editor_avg_rating2','genre_action'
                                    ,'genre_adventure','genre_animation','genre_comedy'
                                    ,'genre_documentary','genre_drama','genre_family','genre_fantasy'
                                    ,'genre_horror','genre_realitytv','genre_scifi','genre_western'
                                    ,'genre_shortfilm','productive_director'
                                    ,'production_country_avg_rating','main_cast_have_female')

summary(final_model)
plot(fitted(final_model),residuals(final_model),col='plum')
abline(0,0,lty=2,col='grey')
plot(final_model, col = 'plum', ps = 300,cex = 2)

#####Heteroskedasty correction#################################################################
ncvTest(final_model) ###p value less than 0.05 therefore our model shows Heteoskedasticity#####

#To correct heteroskedastic errors, we require two packages
require(lmtest)
require(plm)
coeftest(final_model, vcov=vcovHC(final_model, type="HC1")) ###Heteroskedasticity corrected####

###############################################################################################
################################Step 3: Prediction ############################################
###############################################################################################
Prediction <- read.csv("C:\\...\\Test.csv")
detach(final_data_no_out)
attach(Prediction)
#####Normalising the budget in the prediction dataset##########################################
Prediction$budget = log(Prediction$budget+1) 
#####Removing the title which is irrelevant for the prediction################################# 
data_for_prediction <- Prediction %>% select(-Title) 

predicted_imdbrating <- predict(final_model,newdata=data.frame(data_for_prediction))

predicted_imdbrating

write.csv(predicted_imdbrating, "C:\\...\\Prediction.csv")

