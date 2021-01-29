filterData <- list()
filterData[["incPov-1"]] <- filter(EEFdata,Characteristic=="Gender",Measure%in%c("% in relative poverty AHC"))
filterData[["incPov-4"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure%in%c("% in relative poverty AHC","% in relative poverty BHC"))
filterData[["incPov-8"]] <- filter(EEFlatest,Characteristic=="Religion",Measure%in%c("% in relative poverty AHC","% in relative poverty BHC"))
filterData[["incPov-6"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% in relative poverty AHC")
filterData[["sch-Edu-1"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-2"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-3"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-4"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-5"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-6"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-7"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-8"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-21"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-22"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-23"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-24"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-9"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-10"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-11"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-12"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-14"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure=="Pupil ethnicity")
filterData[["sch-Edu-15"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure=="Teacher ethnicity")
filterData[["sch-Edu-41"]] <- filter(EEFlatest,Characteristic=="Gender",Measure%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
filterData[["sch-Edu-13"]] <- filter(EEFdata,Measure=="Teachers (headcount)",Characteristic=="Age")
