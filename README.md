Description: This project provides an in-depth analysis of the Open University Learning Analytics Dataset (OULAD) with a specific focus on understanding student demographics, their engagement with the courses, and the outcomes of their assessments. By analyzing these aspects, the report seeks to uncover patterns and trends that can provide valuable insights into student behavior and performance.

Data Files here: https://drive.google.com/file/d/1jbQDr6Jhlpap8m3hjwyb94_mkD18b-rG/view?usp=sharing

Project Overview:
This project focuses on analyzing student information to examine factors influencing student outcomes, including education level, income band, age, disability status, and assessment performance. I used R for data wrangling and ggplot2 for visualization, providing insights into student success based on various demographics and metrics.

1. Education Level and Final Results
What I did: I categorized students by their highest education level and calculated the distribution of final results (Distinction, Pass, Fail, Withdrawn). Visualized this data using a bar chart and a polar plot to display the distribution.
Why: The goal was to understand how prior education impacts student performance.
Insight: Students with higher education qualifications tend to perform better, with more distinctions and passes, while those with lower or no formal qualifications have higher failure and withdrawal rates.
2. Income Band and Final Results
What I did: I grouped students based on the Index of Multiple Deprivation (IMD) band and examined the distribution of final results. A bar plot was used to visualize how outcomes vary across income bands.
Why: Socioeconomic status is often a key factor in educational performance, so this analysis aimed to highlight any disparities.
Insight: Lower IMD bands (higher deprivation) showed a trend of higher failure and withdrawal rates, while higher IMD bands had more distinctions and passes.
3. Age Band and Final Results
What I did: I analyzed how different age groups performed in their final results using bar charts to illustrate the proportion of distinctions, passes, failures, and withdrawals by age band.
Why: Age is another demographic factor that can affect learning outcomes.
Insight: Younger students (18-35) had a higher proportion of distinctions, while older students showed a higher withdrawal rate, possibly due to life circumstances or balancing studies with other responsibilities.
4. Disability Status and Final Results
What I did: I compared the final results of students with and without disabilities using a grouped bar chart.
Why: Understanding how students with disabilities perform relative to those without can help highlight support needs.
Insight: Students with disabilities had a higher percentage of withdrawals, which might indicate the need for additional support services.
5. Assessment Scores and Clicks
What I did: I visualized the distribution of assessment scores and the number of clicks (engagement) using histograms, box plots, and violin plots. Additionally, I looked at how assessment type and weight impacted scores.
Why: The goal was to analyze how student engagement (clicks) and assessment type influence performance.
Insight: Higher engagement (clicks) correlated with higher scores. The distribution of scores varied significantly based on the weight of the assessment, with heavier-weighted assessments showing more variation in scores.
6. Code Module and Final Results
What I did: I combined the course module and presentation year, analyzed the percentage of final results by module, and visualized this with stacked bar charts.
Why: The goal was to observe differences in outcomes across different modules and presentations.
Insight: Certain modules had higher failure rates, indicating they may be more challenging or need better instructional design.

Conclusion:
This analysis highlighted key demographic and academic factors that influence student success. Insights gained from this project can guide institutions in identifying areas where students may need more support, such as offering additional resources for students from lower socioeconomic backgrounds, older age groups, or those with disabilities.
