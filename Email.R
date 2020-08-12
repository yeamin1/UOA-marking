Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-14")
#Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_211")
library(mailR)
path = 'C:/Users/zhiji/OneDrive - The University of Auckland/course/marking/220-2020/'
setwd(path)

emailProcess = function(fileA, grade){
  
  csv = read.csv(fileA, stringsAsFactors = FALSE, check.names=FALSE)
  gradeFile = read.csv(grade, stringsAsFactors = FALSE)
  Order = match(csv$id, gradeFile$ID)

  ## I don't have email in my *.csv file
  upi = gradeFile$SIS.Login.ID[Order]
  emailAddress = paste0(upi, '@aucklanduni.ac.nz')
  
  outcsv = cbind(csv, email = emailAddress, upi = upi)
  ## get the useful info prepare for mesg and comment
  markz = outcsv[, c(1:(ncol(outcsv) - 2), ncol(outcsv))]
  TotalMark = markz[, ncol(markz) - 2]
  
  ## always use the last column as comments in *.csv
  outcsv$Comments = as.character(csv[, ncol(csv)])
  outcsv$Comments = ifelse(outcsv$Comments == '' & TotalMark == max(TotalMark), 'Well Done!', outcsv$Comments)
  
  marks = unlist(lapply(1:nrow(markz), function(x){
    title = gsub('\\{[0-9]}[.]*', '\1 ', colnames(markz))
    title[grep('[0-9]+', title)] = paste0(title[grep('[0-9]+', title)], '')
    out = paste0(title, ': ', markz[x, ])
    out = out[-length(out)]
    paste0(out, collapse = '\n')
  }))

  upi = paste0('upi:', outcsv$upi)
  
  contents = paste(upi, marks, sep = '\n')
  
  list(contents = contents, subject = gsub('-combine|[.]csv', '', fileA), email = outcsv$email)
  
}



SentEmail = function(Econtents, tome = FALSE){
  n = length(Econtents[[1]])
  for(i in 1:n){
    sender <- "jwen246@aucklanduni.ac.nz"
    
    
    para = paste0('Hello students, \nHere is the marking detals for stats 220: \n\n', Econtents$contents[i], '\n\n',
                  'Please do not reply to this email unless you have any issues about the assignment/lab/test (otherwishes I got tons of email ...)', '\n\n',
                  '(If you would like to change the mark, please talk with Earo, since I have no premission on changing the marks, but I am happy to discuss it.)\n',
                  'This email is generated automatically, and there may still some bugs and needed to be improved in the future. \n',
                  'Please let me or Earo knows if there any problems. \n\n',
                  'Best Wishes \nJason')
    
    # para = paste0('Hello students, \nSorry about that some stuents recevied somebody else marks, we will fix this as soon as possible.\n',
    #         'At this time, I would like to sent you this test email, to double check whether you are the correct student to receive the correct email.\n',
    #         'If you are not the incorrect student, please let me know. your detail is: \n\n', Econtents$contents[i],
    #         '\n\nSorry to bother you at this time.\n\nBest Wishes \nJason')
    
    recipients <- as.character(Econtents$email[i])
    #subject = 'Stats 220 email system (Test)'
    subject =  paste0("Stats 220: ", Econtents$subject, ' detail:') 
    
    if(tome){
      n = 1
      recipients = 'zhijianwen1992@gmail.com'
    }

    
    send.mail(from = sender,
              to = recipients,
              subject = subject,
              body = para,
              smtp = list(host.name = "smtp.gmail.com", port = 465, 
                          user.name = 'jwen246@aucklanduni.ac.nz',            
                          passwd = "********", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    print(paste0('Email has been sent to: ', recipients))
    
    if(tome) break 
  }
}

Econtents = emailProcess('A3-marks-updated.csv', '2020-06-11T1023_Grades-STATS_220.csv')
cat(Econtents[[1]][20])
SentEmail(Econtents, FALSE)

#Econtent = list(contents = Econtents$contents[[86]], 
#                subject = Econtents$subject, email = Econtents$email[86])
