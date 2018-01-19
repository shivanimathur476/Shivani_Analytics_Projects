library(ISLR)
library(dplyr)
#Reading Fall 2016 data
Fall2016=read.csv('C:/Users/shiva/Documents/Quantitative Modelling/ProjectData/MIS Fall 2016 Schedule Sections1.csv')
View(Fall2016)

E=Fall2016%>%filter(Available<=1)%>%group_by(CourseNo,Instructor)%>%summarise(Availability=sum(Available))
View(E)
#Shows how many courses faculty is teaching in fall 2017 
FWC=E%>%group_by(Instructor)%>%summarise(NoOfCourses=n()) 
View(FWC)
#Creating new colunm workload assuming each course is of 3 credits, can be changed later
FWC$Workload=FWC$NoOfCourses*3
View(FWC)
FWC%>%filter(Workload==3)#These people can be given two more courses
FWC%>%filter(Workload==6)#These people can be given one more course
# Read the workload table
Workload=read.csv('C:/Users/shiva/Documents/Quantitative Modelling/ProjectData/TEACHING_LOAD.csv')
# Reading the spring 2017 data
FWCFull=FWC%>%filter(Workload>=9)#These people cannot take more load
View(FWCFull)
FWC2=FWC%>%filter(Workload==3)#These people can be given two more courses
FWC1=FWC%>%filter(Workload==6) #These people can be given one more course
#Find out the subjects which are taught by professors who cannot take extra load
subjects1=Fall2017%>%filter(Instructor=='Patuwo, B. Eddy')
unique(subjects1$CRN) #These CRNs need another teacher
C15816=Fall2017%>%filter(CRN=='15816') #check the another faculties available for the subject from original data
#here for course 15818 only 2 faculties are found, who already have full workload. So search for another faculty in 
#Qualifications table having same area of study so they can teach this subject.
#Here for 15816 only 1 instructor is found, who is full
Spring2017=read.csv('C:/Users/shiva/Documents/Quantitative Modelling/ProjectData/MIS Spring 2017 Schedule Sections.csv')
View(Spring2017)
#installing sql package
install.packages("sqldf")
library(sqldf)
#Comparing same courses in fall and spring semester
CouesesInBothSem <- sqldf('SELECT CRN FROM Fall2017 EXCEPT SELECT CRN FROM Spring2017')
COMMONCOURSES <- sqldf('SELECT COURSENO FROM Fall2017 INTERSECT SELECT COURSENO FROM Spring2017')
View(COMMONCOURSES)
#Fnd the the courses that was in high demand by selecting only those courses that are in not offered in both semesters.
MostDemandedCourse=sqldf('SELECT COURSENO FROM E EXCEPT SELECT COURSENO FROM COMMONCOURSES')
View(MostDemandedCourse)
FWCFull=FWC%>%filter(Workload>=9)#These people cannot take more load
View(E)
FWC2=FWC%>%filter(Workload==3)#These people can be given two more courses
FWC1=FWC%>%filter(Workload==6) #These people can be given one more course
#Find out the subjects which are taught by professors who cannot take extra load
subjects1=Fall2017%>%filter(Instructor=='Patuwo, B. Eddy')
unique(subjects1$CRN) #These CRNs need another teacher
C15816=Fall2017%>%filter(CRN=='15816') #check the another faculties available for the subject from original data
#here for course 15818 only 2 faculties are found, who already have full workload. So search for another faculty in 
#Qualifications table having same area of study so they can teach this subject.
#Here for 15816 only 1 instructor is found, who is full

#Qualification table
QU=read.csv('C:/Users/shiva/Documents/Quantitative Modelling/Qualifications.csv')
View(QU)
#Initially assuming FT and PT can have same workload and every faculty can teach any course of any level of their 
# arranging faculty according to area
result <- aggregate(Faculty ~ Area..I.think.., data = QU, paste, collapse = ",")
View(result)
# courses taught by adjunts
CA=Fall2017 %>% group_by(COURSENO,Title,Instructor,FT.PT)%>% summarise(Availability=sum(Available))
View(Fall2017)
View(CA)
CA=Fall2017 %>% group_by(COURSENO,Instructor)%>% filter(FT.PT='FT')
Fall2016=read.csv("C:/Users/shiva/Documents/Quantitative Modelling/ProjectData/Fall_2016.csv")
#Common courses
COMMONCOURSES16 <- sqldf('SELECT CourseNo FROM Fall2016 INTERSECT SELECT COURSENO FROM Spring2017')
View(COMMONCOURSES16)
#Fnd the the courses that was in high demand by selecting only those courses that are in not offered in both semesters.
MostDemandedCourse=sqldf('SELECT CourseNo FROM E EXCEPT SELECT COURSENO FROM COMMONCOURSES16')
View(MostDemandedCourse)
#courses taught by only 1 faculty
UI=Fall2016 %>% group_by(CourseNo) %>% select(unique('Instructor'))
UIFall=aggregate(Instructor ~ CourseNo, data = Fall2016, paste, collapse = ",")
View(UIFall)
UISpring=aggregate(Instructor ~ COURSENO, data = Spring2017, paste, collapse = ",")
View(UISpring)

