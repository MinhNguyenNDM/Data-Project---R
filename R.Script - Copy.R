install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

setwd("C:/Files/Project/Project-OULAD/data1")
options(scipen = 999)
a <- read.csv('studentAssessment.csv')
b <- read.csv('studentInfo.csv')
c <- read.csv('studentVle.csv')
d <- read.csv('assessments.csv')
e <- read.csv('courses.csv')
f <- read.csv('studentRegistration.csv')
g <- read.csv('vle.csv')

###A. Data Wangling
##I. File studentVle.csv
#1.1 Aggregate sum_click by code_module, code_presentation, and id_student
sum_click_studentVle <- aggregate(sum_click ~ code_module + code_presentation + id_student, data = c, FUN = sum)
#1.2 combine 'code_module,'code_presentation','id_student' into 1 column called 'code_combine'
sum_click_studentVle <- sum_click_studentVle %>% mutate(code_combine = paste0(code_module,'-',code_presentation,'-',id_student))

#2.Check the numbers of students in data file
#2.1 Check distinct students in VLE
count_students1 <- sum_click_studentVle %>% summarise(count = n_distinct(id_student))
count_students1
#26074
#2.2.Check distinct of code combine after combination
count_students2 <- sum_click_studentVle %>% summarise(count = n_distinct(code_combine))
count_students2
#29228
#So there are 26,074 distinct students in the file but some of them did/registered more than one code_module & code_presentation
#So the total of row are 29,228.
#2.3 check the specific number of times a student appears in the file
#Group by id_student and count the number of rows for each student
student_appearances <- sum_click_studentVle %>%
  group_by(id_student) %>%
  summarise(appearances = n())
#Determine the maximum number of appearances
max_appearances <- max(student_appearances$appearances)
#Define the thresholds for number of appearances
thresholds <- 1:max_appearances
#Create an empty dataframe to store the results
result_df <- data.frame(num_students = numeric(),
                        threshold = numeric())
#Loop through each threshold
for (threshold in thresholds) {
  #Calculate the number of students with appearances greater than or equal to the threshold
  num_students <- student_appearances %>%
    filter(appearances >= threshold) %>%
    summarise(num_students = n())
  #Add the result to the dataframe
  result_df <- rbind(result_df, data.frame(num_students = num_students$num_students, threshold = threshold))
}
print(result_df)
#2.4 double check by counting the number of distinct code_combine
count_students3 <- sum_click_studentVle %>% summarise(count = n_distinct(code_combine))
count_students3
#29228

##II. Assessment file & studentAssessment & studentInfo
#1. Check weight of assessment
#1.1 Check weight of 'Exam'
weight_100 <- d %>% filter(assessment_type == 'Exam')
weight_100
#=> All the 'Exam' in assessment type are 100% of weight
#1.2 sum the weight base on code module & code presentation to make sure 100% for each
weighting <- d %>%
  filter(assessment_type != 'Exam') %>%
  group_by(code_module, code_presentation) %>%
  summarise(weight_sum = sum(weight))
weighting
#2. merge table 
#2.1 assessments & studentAssessment
As_StAs <- merge(d,a,by='id_assessment')
#write.table(new_table,file = "C:/Files/Project/Project-OULAD/data1/ASS&STASS.csv",row.names = F,sep = ",")
count_students4 <- As_StAs %>% summarise(count = n_distinct(id_student))
count_students4
#23369
#2.2 merge to studentInfo (OPTIONAL-REFERENCE ONLY)
As_StAs_StInf <- merge(As_StAs,b,by=c('code_module','code_presentation','id_student'))
#2.3 OPTION1: merge to studentVle
final_table01 <- merge(As_StAs_StInf,sum_click_studentVle,by=c('code_module','code_presentation','id_student'))
#2.4 merge As_StAs & studentVle (SKIP 2.2,2.3)
final_tablex <- merge(As_StAs,sum_click_studentVle,by=c('code_module','code_presentation','id_student'))
sum(is.na(final_tablex))
#write.table(result00, file="C:/Files/Project/Project-OULAD/test0.csv", row.names = F,sep = ",")

#III. studentInfo file
#3.1 wrangling data
studentInfo <- read.csv('studentInfo.csv')
sum(is.na(studentInfo))
str(studentInfo)
#check distinct imd_bank
imd_band_check <- distinct(select(studentInfo,imd_band))
imd_band_check
#3.1 check the specific number of times a student appears in the file
#Group by id_student and count the number of rows for each student
student_appearances <- studentInfo %>%
  group_by(id_student) %>%
  summarise(appearances = n())
#Determine the maximum number of appearances
max_appearances <- max(student_appearances$appearances)
#Define the thresholds for number of appearances
thresholds <- 1:max_appearances
#Create an empty dataframe to store the results
result_df <- data.frame(num_students = numeric(),
                        threshold = numeric())
#Loop through each threshold
for (threshold in thresholds) {
  #Calculate the number of students with appearances greater than or equal to the threshold
  num_students <- student_appearances %>%
    filter(appearances >= threshold) %>%
    summarise(num_students = n())
  #Add the result to the dataframe
  result_df <- rbind(result_df, data.frame(num_students = num_students$num_students, threshold = threshold))
}
print(result_df)
#2.4 count distinct students and total of result in the file
#count distinct
count_student_infor <- studentInfo %>% summarise(count = n_distinct(id_student))
count_student_infor
#count all
count_infor <- studentInfo %>% summarise(count = n())
count_infor
#2.5 code module - presentaion - type
# count based on module and presentation code
result_code <- studentInfo %>%
  group_by(code_module, code_presentation) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = code_presentation, values_from = count) %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))
result_code
#count the number of assessments
assessment_code <- d %>%
  group_by(code_module, code_presentation) %>%
  summarise(count = n_distinct(id_assessment), .groups = 'drop') %>%
  pivot_wider(names_from = code_presentation, values_from = count) %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))
assessment_code

#
number_of_students_year <- studentInfo %>%
  group_by(code_module, code_presentation) %>%
  summarise(count = n(), .groups = 'drop')

convert_presentation <- function(presentation) {
  year <- str_sub(presentation, 1, 4)
  semester <- str_sub(presentation, 5, 5)
  month <- switch(semester,
                  "B" = "02",
                  "J" = "10",
                  semester)  # default case
  return(paste0(month, "-", year))
}

# Apply the function to code_presentation column
number_of_students_year <- number_of_students_year %>%
  mutate(code_presentation = sapply(code_presentation, convert_presentation))
print(number_of_students_year)
#plot
presentation_order <- c("02-2013", "10-2013", "02-2014", "10-2014")
number_of_students_year <- number_of_students_year %>%
  mutate(code_presentation = factor(code_presentation, levels = presentation_order))

# Create the line plot
ggplot(number_of_students_year, aes(x = code_presentation, y = count, color = code_module, group = code_module)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Student Registrations",
       x = "Course Presentation Start Date",
       y = "Number of Students",
       color = "Code Module") +
  theme_minimal()

#2.6 genders
#a. registration - genders
genders_registration <- studentInfo %>% group_by(gender) %>% summarise(count = n()) 
genders_registration
genders_registration$percentage <- round((genders_registration$count/sum(genders_registration$count) * 100),1)
custom_colors <- c("M" = "#ffefbb", "F" = "#e0eeee")

ggplot(genders_registration, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Genders of students") +
  geom_text(aes(label = paste0(format(count, nsmall=1, big.mark=","),"\n",percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = custom_colors)

#b. results by genders
#Result by gender
result_gender <-studentInfo %>%
  group_by(gender, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count)
result_gender

#visualization
# Define the preferred order of final_result levels
result_order <- c("Distinction", "Pass", "Fail", "Withdrawn")

# Convert final_result to factor with the preferred order
studentInfo$final_result <- factor(studentInfo$final_result, levels = result_order)

gender_plot <- ggplot(studentInfo, aes(x = final_result, fill = gender)) + 
  geom_bar(position = 'dodge') + 
  labs(title = "Comparison of Results Based on Gender", x = "Final Result", y = "") +
  theme_minimal() +
  geom_text(stat='count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 3, show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))
print(gender_plot)

#c.Result by region
# region count only
result_region_count <-studentInfo %>%
  group_by(region) %>%
  summarise(count = n()) %>% arrange(desc(count))
result_region_count

#%
result_region_count <- result_region_count %>%
  mutate(Percentage = round((count/sum(count)*100),1))

result_region <-studentInfo %>%
  group_by(region, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count)
result_region


#visualization
install.packages("sf")
library(sf)
#read shapefile
shapefile <- st_read("C:/Files/Project/Project-OULAD/UK-map.shp")
#merge
merged_data <- merge(shapefile, result_region, by = "region")
#add new row for south region
merged_data01 <- merge(merged_data, result_region_count, by = "region")
colnames(merged_data01)[1] <- 'Regions'

#replace regions name
new_regions_name <- c("East Anglian Region" = "East Anglian",
                      "East Midlands Region" = "East Midlands",
                      "Ireland" = "Ireland",
                      "London Region" = "London",
                      "North Region" = "North Region",
                      "North Western Region" = "North Western",
                      "Scotland" = "Scotland",
                      "South East Region" = "South East",
                      "South West Region" = "South West",
                      "Wales" = "Wales",
                      "West Midlands Region" = "West Midlands",
                      "Yorkshire Region" = "Yorkshire")

merged_data01$Regions <- new_regions_name[merged_data01$Regions]
merged_data01 <- merged_data01 %>%
  mutate(UK_regions = paste(Regions,"-",count,"-",Percentage,"%"))

#plot
#c1.general
ggplot(data = merged_data01) +
  geom_sf(aes(fill = UK_regions)) +
  geom_sf_label(aes(label = count), size = 2.5) +
  labs(title = "The number and percentage of students in each region - UK ") +
  theme_void()

#c2.Withdrawn
merged_data01 <- merged_data01 %>%
  mutate(PercentageOfWithdrawn = round((Withdrawn/count)*100,1))

ggplot(data = merged_data01) +
  geom_sf(aes(fill = PercentageOfWithdrawn)) +
  geom_sf_label(aes(label = Regions), size = 2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Percentage") +
  labs(title = "The percentage of Withdrawn in each region - UK ") +
  theme_void()
#view
merged_data01 %>% select(Regions,PercentageOfWithdrawn) %>% arrange(desc(PercentageOfWithdrawn))

#c3.Failed
merged_data01 <- merged_data01 %>%
  mutate(PercentageOfFail = round((Fail/count)*100,1))

ggplot(data = merged_data01) +
  geom_sf(aes(fill = PercentageOfFail)) +
  geom_sf_label(aes(label = Regions), size = 2) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "Percentage") +
  labs(title = "The percentage of fail in each region - UK ") +
  theme_void()
#view
merged_data01 %>% select(Regions,PercentageOfFail) %>% arrange(desc(PercentageOfFail))

#c4.Pass
merged_data01 <- merged_data01 %>%
  mutate(PercentageOfPass = round((Pass/count)*100,1))

ggplot(data = merged_data01) +
  geom_sf(aes(fill = PercentageOfPass)) +
  geom_sf_label(aes(label = Regions), size = 2) +
  scale_fill_gradient(low = "gray", high = "orange", name = "Percentage") +
  labs(title = "The percentage of pass in each region - UK ") +
  theme_void()

#view
t3 <- merged_data01 %>% select(Regions,PercentageOfPass) %>% arrange(desc(PercentageOfPass))
#c5.Distinction
merged_data01 <- merged_data01 %>%
  mutate(PercentageOfDistinction = round((Distinction/count)*100,1))

ggplot(data = merged_data01) +
  geom_sf(aes(fill = PercentageOfDistinction)) +
  geom_sf_label(aes(label = Regions), size = 2) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Percentage") +
  labs(title = "The percentage of distinction in each region - UK ") +
  theme_void()
view
t4<- merged_data01 %>% select(Regions,PercentageOfDistinction) %>% arrange(desc(PercentageOfDistinction))
#c6.results in all regions
#results
result_region <-studentInfo %>%
  group_by(region, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count)
result_region
#plot
ggplot(studentInfo, aes(x = region, fill = final_result)) +
  geom_bar() +
  labs(title = "Student Outcomes by Region",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Distinction" = "#cdad00", "Pass" = "#9bcd9b", "Fail" = "#cd3333", "Withdrawn" = "#cdc8b1"))

#2.7 Education aspect
check_education <- distinct(select(studentInfo,highest_education))
check_education

# Define the order of levels
order_levels <- c("Post Graduate Qualification", "HE Qualification", 
                  "A Level or Equivalent", "Lower Than A Level", 
                  "No Formal quals")

# Convert code_module and code_presentation to factor with custom levels order
studentInfo <- studentInfo %>%
  mutate(highest_education = factor(highest_education, levels = order_levels))

#get result and arrange level of education
result_education <-studentInfo %>%
  group_by(highest_education, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count) %>%
  arrange(highest_education,decreasing = TRUE)
result_education


#plot
aggregated_data <- studentInfo %>%
  group_by(highest_education, final_result) %>%
  summarise(count = n())

ggplot(aggregated_data, aes(x = highest_education, y= count ,fill = final_result)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Outcomes by Highest Education",
       x = "Highest Education",
       y = "") +
  #scale_fill_discrete(name = "Results") +
  scale_fill_manual(name = "Outcome",values = c("Distinction" = "#cdad00", "Pass" = "#9bcd9b", "Fail" = "#cd3333", "Withdrawn" = "#cdc8b1"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#education data
education_data <- studentInfo %>% group_by(highest_education) %>%
  summarise(count=n())
education_data

#Create the plot
ggplot(studentInfo, aes(x = highest_education, fill = highest_education)) + 
  geom_bar() +
  coord_polar() +
  scale_fill_manual(name = "Education", values = c("Post Graduate Qualification" = "#8b0000", 
                                                   "HE Qualification" = "#2e8b57", 
                                                   "A Level or Equivalent" = "#8b5a00", 
                                                   "Lower Than A Level" = "#8b8b00",
                                                   "No Formal quals" = "#36648b")) +
  labs(y = "") +  
  theme_minimal()

#2.8 Result by imd_band
#get infor
check_imd <- distinct(select(studentInfo,imd_band))
check_imd
# replace "10-20" by "10-20%"
studentInfo$imd_band <- ifelse(studentInfo$imd_band == "10-20", "10-20%", studentInfo$imd_band)

#order
order_imd_levels <- c("90-100%", "80-90%", "70-80%", "60-70%","50-60%",
                      "40-50%","30-40%","20-30%","10-20%","0-10%")

#factor
studentInfo <- studentInfo %>%
  mutate(imd_band = factor(imd_band, levels = order_imd_levels))


#get result and arrange level
result_imd_band <-studentInfo %>%
  group_by(imd_band, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count) %>%
  arrange(imd_band,decreasing = TRUE)
result_imd_band

#plot
student_filtered <- studentInfo %>%
  filter(!is.na(imd_band)) 

# Calculate the count of observations for each combination of IMD band and final result
student_summary <- student_filtered %>%
  count(imd_band, final_result)

# Create the bar plot
ggplot(student_summary, aes(x = imd_band, y = n, fill = final_result)) +
  geom_bar(stat = "identity", position = "fill") +
  xlab("IMD Band") + ylab("") +
  labs(fill = "") +
  theme_minimal()

#imd band (original)
student_summary1 <- studentInfo %>%
  count(imd_band)

#2.9 Result by Age Band
check_age_band <- distinct(select(studentInfo,age_band))
check_age_band

result_age_band <-studentInfo %>%
  group_by(age_band, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count)
result_age_band

student_summary_age <- studentInfo %>%
  count(age_band, final_result)

ggplot(student_summary_age, aes(x = age_band, y = n, fill = final_result)) +
  geom_bar(stat = "identity", position = "fill",width = 0.5) +
  xlab("Age Band") + ylab("") +
  labs(fill = "") +
  theme_minimal()

#2.10# Result by Disability
result_Disability <-studentInfo %>%
  group_by(disability, final_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = final_result,values_from = count)
result_Disability

student_summary_Disability <- studentInfo %>%
  count(disability, final_result)

ggplot(student_summary_Disability, aes(x = disability, y = n, fill = final_result)) +
  geom_bar(stat = "identity", position = "fill",width = 0.3) +
  xlab("Disability status") + ylab("") +
  labs(fill = "") +
  theme_minimal()

#3. Code module + code presentation & Results
studentInfo <- studentInfo %>% mutate(code_combine = paste0(code_presentation,'-',code_module))
code_result <- studentInfo %>%group_by(code_combine) %>% count(final_result)
code_result

# Calculate the percentage for each code_combine group
code_result <- code_result %>%
  group_by(code_combine) %>%
  mutate(percent = n / sum(n))
sum(code_result$n)

# Create the plot with percentage labels
ggplot(code_result, aes(x = code_combine, y = n, fill = final_result)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  xlab("Code-Module") +
  ylab("") +
  labs(fill = "") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(name = "Outcome",values = c("Distinction" = "#cdad00", "Pass" = "#9bcd9b", "Fail" = "#cd3333", "Withdrawn" = "#8c8c8c"))

##II. SCORE & SUM CLICKS FILE
final_tablex <- final_tablex %>% mutate(code = paste0(code_presentation,'-',code_module))
distinct_code <- distinct(select(final_tablex,code))
distinct_code

#check na and handling
sum(is.na(final_tablex$score))
#172

#Note: If the student does not submit the assessment, no result is recorded. The final exam submissions is missing, 
#Replace na by 0
if (any(is.na(final_tablex$score))) {
  final_tablex$score[is.na(final_tablex$score)] <- 0
}
## if the result of the assessments is not stored in the system.

#na in "date" column
sum(is.na(final_tablex$date))
#2864
#Note: date – information about the final submission date of the assessment calculated as the number of days since the start of the module-presentation. 
#The starting date of the presentation has number 0 (zero). If the information about the final exam date is missing, 
#it is at the end of the last presentation week.
max_days <- max(final_tablex$date, na.rm = TRUE)
# replace
if (any(is.na(final_tablex$date))) {
  final_tablex$date[is.na(final_tablex$date)] <- max_days
}
sum(is.na(final_tablex))

#plot distribution of score
ggplot(final_tablex, aes(x=score)) + 
  geom_histogram(binwidth=1, fill="#FF9999", color="#e9ecef", alpha=0.9) +
  labs(title  = "Distribution of Score") + ylab("") + theme_minimal()


# score & code
ggplot(final_tablex, aes(x = score, fill = factor(code))) + 
  geom_histogram(aes(y = after_stat(density)), position = 'dodge', binwidth = 2, alpha = 0.5) +
  geom_density(alpha = 0.25) +
  labs(title = "Distribution of Score based on Code Module and Year",
       x = "Score",
       y = "Density",
       fill = "") +
  facet_wrap(~ code) +
  theme_minimal() +
  theme(legend.position = "none")

# score & assessment_type
ggplot(final_tablex, aes(x = score, fill = factor(assessment_type))) + 
  geom_histogram(aes(y = after_stat(density)), position = 'dodge', binwidth = 2, alpha = 0.5) +
  geom_density(alpha = 0.25) +
  labs(title = "Distribution of Score based on Code Module and Year",
       x = "Score",
       y = "Density",
       fill = "") +
  facet_wrap(~ assessment_type) +
  theme_minimal() +
  theme(legend.position = "none")

# score and assessment type 2
ggplot(final_tablex, aes(x=score,y=after_stat(density),fill=factor(assessment_type))) + 
  geom_histogram(position='dodge', binwidth=2) +
  geom_density(alpha=0.25) +
  labs(title  = "",
       fill = "Assessment types")
 
# sum click and assessment type
ggplot(final_tablex, aes(x=sum_click,y=after_stat(density),fill=factor(assessment_type))) + 
  geom_histogram(position='dodge', binwidth=2) +
  geom_density(alpha=0.25) +
  labs(title  = "",
       fill = "Assessment types", x = "Sum of clicks") +
  facet_wrap(~ assessment_type)

library(ggplot2)

# Create the plot with faceting and set the x-axis range
ggplot(final_tablex, aes(x = sum_click, fill = factor(code))) + 
  geom_histogram(aes(y = after_stat(density)), position = 'dodge', binwidth = 2, alpha = 0.5) +
  geom_density(alpha = 0.25) +
  labs(title = "",
       fill = "",
       x = "Sum of clicks") +
  facet_wrap(~ code) +
  scale_x_continuous(limits = c(0, 10000)) +
  theme_minimal()+
  theme(legend.position = "none")

###

#score and weight
ggplot(final_tablex, aes(x = score , y = factor(weight))) + 
  geom_boxplot(fill = "lightpink", color = "black") + 
  labs(x = "Sale Revenue", y = "Age Category") + 
  ggtitle("The target customer") 

ggplot(final_tablex, aes(x = score, y = factor(weight))) + 
  geom_col(color = "#9bcd9b") + 
  labs(x = "Count", y = "Weight of assessment") + 
  ggtitle("")

ggplot(final_tablex, aes(x = factor(weight), y = score, fill = factor(weight))) + 
  geom_boxplot() + 
  labs(x = "Weight Category", y = "Assessment Score") + 
  ggtitle("Box Plot of Assessment Scores by Weight Category") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(final_tablex, aes(x = factor(weight), y = score, fill = factor(weight))) + 
  geom_violin() + 
  labs(x = "Weight Category", y = "Assessment Score") + 
  ggtitle("Violin Plot of Assessment Scores by Weight Category") +
  theme_minimal() +
  theme(legend.position = "none")
##
##
# Calculate mean scores for each weight category
mean_scores <- final_tablex %>%
  group_by(weight) %>%
  summarise(mean_score = mean(score))

# Calculate counts for each weight category
count_scores <- final_tablex %>%
  group_by(weight) %>%
  summarise(count = n())

# Combine violin plot with mean points and count labels
ggplot(final_tablex, aes(x = factor(weight), y = score, color='blue')) + 
  geom_violin() + 
  geom_point(data = mean_scores, aes(x = factor(weight), y = mean_score), color = "#458B74", size = 3) +
  geom_text(data = mean_scores, aes(x = factor(weight), y = mean_score, label = round(mean_score, 1)), 
            vjust = -0.5, color = "black") +
  geom_text(data = count_scores, aes(x = factor(weight), y = max(final_tablex$score), label = count), 
            vjust = -0.5, color = "black") +
  labs(x = "Weight Category", y = "Assessment Score") + 
  ggtitle("Assessment Scores by Weight Category") +
  theme_minimal() +
  theme(legend.position = "none")

#
filtered_data <- final_tablex %>% filter(score == 100)

# Calculate counts for each weight category where score == 100
count_weights <- filtered_data %>%
  group_by(weight) %>%
  summarise(count = n())

# Bar plot with count labels for score == 100
ggplot(count_weights, aes(x = factor(weight), y = count, fill = factor(weight))) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(x = "Weight Category", y = "") + 
  ggtitle("Count of Scores 100 by Weight Category") +
  theme_minimal() +
  theme(legend.position = "none")


#score and sum_click
aaa <- ggplot(final_tablex, aes(x = score, y = sum_click)) +
  geom_point(color = '#cdad00') + 
  labs(title = "", y = "Sum of clicks") + 
  theme(legend.position = "none")

print(aaa)

# Add a linear model smoothing line and apply the minimal theme
aaa <- aaa + geom_smooth(method = "lm", formula = y ~ x) + theme_minimal()
print(aaa)

