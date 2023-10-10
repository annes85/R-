library(dplyr)
library(ggplot2)
library(lubridate)
getwd()
setwd("D:/r/project1")
getwd()
comcast<-read.csv("Comcast.csv")
head(comcast)
comcast$Date<-dmy(comcast$Date)
head(comcast)
#complaint on daily basis
dailybasis<-comcast %>% group_by(Date) %>% summarize(Numberofcomplaints=n())
ggplot(data=dailybasis,aes(as.POSIXct(Date),Numberofcomplaints))+
  geom_line()+
  geom_point(size =1)+
  scale_x_datetime(breaks = "1 weeks", date_labels = "%d/%m")+
  labs(title="Daily ticketcount",x="days",y="no of tickets")+
  theme(axis.text.x = element_text(angle = 75),plot.title = element_text(hjust = 0.5) )
#complaint on month wise
monthwise<-comcast %>% group_by(month=as.integer(month(Date))) %>% summarize(Numberofcomplaints=n())
ggplot(data = monthwise,aes(month,Numberofcomplaints,label = Numberofcomplaints))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = monthwise$month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))

#ticket count

network_ticket<-contains(comcast$Customer.Complaint,match="network",ignore.case=T)
internet_ticket<-contains(comcast$Customer.Complaint,match="internet",ignore.case=T)
bill_ticket<-contains(comcast$Customer.Complaint,match="bill",ignore.case=T)
email_ticket<-contains(comcast$Customer.Complaint,match="email",ignore.case=T)
charge_ticket<-contains(comcast$Customer.Complaint,match="charge",ignore.case=T)
comcast$complaint.type[internet_ticket]<-"Internet"
comcast$complaint.type[network_ticket]<-"Network"
comcast$complaint.type[bill_ticket]<-"Bill"
comcast$complaint.type[email_ticket]<-"Email"
comcast$complaint.type[charge_ticket]<-"Charge"
comcast$complaint.type[-c(internet_ticket,network_ticket,bill_ticket,email_ticket,charge_ticket)]<-"others"
table(comcast$complaint.type)

# statusof closed and open ticket
open_complaint<-(comcast$Status=='Open' |comcast$Status=='Pending' )
closed_complaint<-(comcast$Status=='Closed' |comcast$Status=='Solved' )
comcast$complaint.status[open_complaint]<-"Open"
comcast$complaint.status[closed_complaint]<-"closed"
ticket.status<-table(comcast$complaint.status,comcast$State)
ticket.status

#find the state wise complaint
comcast<-group_by(comcast,State,complaint.status)
chart<-summarise(comcast,count=n())
ggplot(as.data.frame(chart),mapping = aes(State,count))+
  geom_col(aes(fill=complaint.status),width=.95)+
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        title = element_text(size=15,color="blue"),
        plot.title = element_text(hjust =0.5))+
        labs(title = "Ticket Status",x="States",y="No of Tickets",fill="Status")
#find the state with maximum complaint             
comcast %>% filter(complaint.status=='Open') %>%group_by(State) %>% summarise(numeberofcomplaint=n())
#find the total complaints
total_complaint<-comcast %>% group_by(complaint.status) %>% summarise(numberofcomplaints=n())
total_complaint
#pie chart to show the percentage of complaints
slices<-total_complaint$numberofcomplaints
percentage<-round((slices/sum(slices)*100),2)
lab<-paste(total_complaint$complaint.status,"",percentage,"%",sep="")
pie(slices,labels =lab )

