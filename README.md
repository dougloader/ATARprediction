# ATARprediction
This code is written using R. The code will create 3 graphs analysing your past ATAR data. It will then create a linear model for each course, and apply those models to create predictions for student's who have a School Assessment Score. Finally the predictions are exported to 2 x csv files.

# Getting Started
You will need 2 xlsx files placed in the Source File (this file) working directory. Name the files accordingly or adjust the lines 15 and 16.

Expected file columns:

TISCMaster. This is a standard download from the TISC site:

Surname - Given Names	Appnum	Type	Year	Course	Stage	Sch Assess	Sch Assess Prac	Raw Exam	Raw Exam Prac	Mod/Std Sch Assess	Mod/Std Std Assess Prac	Std Exam	Std Exam Prac	Comb Mark	Adj Comb Mark	Final Comb	Final Scaled	ATAR
![image](https://user-images.githubusercontent.com/95998842/145706259-5ee28b2d-27eb-49c9-97b9-451d364d20f5.png)

CurrentData11_12. Generate this report from a SQL query:

Student Name	Year Group Code	Student Government ID	Timetable Period Code	Report Type Label	Subject Name	Course Code	Moderated Score#	Course Unit Code
![image](https://user-images.githubusercontent.com/95998842/145706366-8dcc3543-3b89-4ec9-ad28-4df6990d0e8d.png)

# That is it!
Expect 2 csv documents in the Source File as the output of this program. The csv documents will contain predictions for 'scaled scores' for each course and final ATAR prediction per student. These files are perfect for loading into Power BI for review by teachers and student. The discussion with the student is what is important here. How can you increase their performance, across each course, over the 4 semesters?

# Things to look out for
1. What if you are offering new courses that have no historical data? This should be an easy fix, perhaps an if/else statement in the for loop.
2. The quality of the data that you input in the 'CurrentData11_12' file is very important. Check for duplication, nulls and na's.
