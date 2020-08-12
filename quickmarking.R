
path = 'C:/Users/yeamin/OneDrive - The University of Auckland/course/marking/220-2020/'
## generate the name on that file
setwd(path)


cCsv = function(folder)
{
  dir = path
  path = paste(dir, '/', folder, sep = '')
  full_csv = '2020-06-11T1023_Grades-STATS_220.csv'
  full_csv = paste(dir, '/', full_csv, sep = '')

  listfile = list.files(path)
  idLocation = gregexpr('_[0-9]*_', listfile)
  idStart = unlist(lapply(idLocation, function(x) x[1]))

  ## only the first pattern is the id
  mulidLength = lapply(idLocation, attr, 'match.length')
  idLength = unlist(lapply(mulidLength, function(x) x[1]))

  id.prep = substring(listfile, idStart, idStart + idLength)

  id = gsub('_', '', id.prep)
  summit_id <<- unique(as.numeric(substring(id, 1, nchar(id) - 1)))


  idname = read.csv(full_csv)
  full_id = unique(idname[-1,2])
  full_name = unique(as.character(idname[-1,1]))

  summit_name <<- full_name[match(summit_id, full_id)]

  ## output for csv file
  dcsv = data.frame(name = summit_name, id = summit_id)


  out_name = paste(folder, '-', 'marks', '.csv', sep = '')
  out_file = file(out_name)
  write.csv(dcsv, out_file, row.names = FALSE)
}

################################################## start working on chrunk ################################################## 
## divide into chrunk
cutAt = function(csv, tot = 6){
  n = dim(csv)[1]
  remainder = n %% tot
  each = n %/% tot
  cutIndex = seq(1, n, by = each)
  atName = csv$name[cutIndex]
  as.character(atName)[1:tot]
}

divide = function(marks_csv, tot = 6){
  if(!any(list.dirs() %in% './_temp')) dir.create('_temp')
  mfile = read.csv(marks_csv,check.names = FALSE)
  cc = cutAt(mfile, tot = tot)
  z = list()
  llat = lnew = 1
  for(i in 2:length(cc)){
    lnew = which(mfile$name == cc[i])
    z[[i - 1]] = mfile[llat:lnew,]
    llat = which(mfile$name == cc[i]) + 1
  }
  z[[length(cc)]] = mfile[llat:nrow(mfile), ]
  for(i in 1:length(z)){
    bname = strsplit(marks_csv, '[.]')[[1]][1]
    fn = paste0('_temp/', bname, '-c-', i, '.csv')
    out_file = file(fn)
    write.csv(z[[i]], out_file, row.names = FALSE, na = '')
  }
}

combine = function(fromDir = '_temp/', toFile = 'lab01-marks-combine.csv' ){
  tempf = list.files(fromDir, full.names = TRUE)
  csv = numeric(0)
  for(i in 1:length(tempf)){
    csv = rbind(csv, read.csv(tempf[i]))
  }
  apply(is.na((csv[,-1])), 1, all)
  
  write.csv(csv, toFile, row.names = FALSE)
}
################################################## end working on chrunk ################################################## 


## marks.csv to total_marks.csv

ExportMark = function(fileIn, fileOut, weight = 1, totalIn = 8){
  d1 = read.csv(paste0(path, fileIn), sep = ',')[,]
  d2 = read.csv(paste0(path, fileOut))[-1,]
  d2 = d2[!is.na(d2$ID), ]

  full_id = d2$ID
  sub_id <<- d1$id

  sub_id %in% as.numeric(full_id)

  d1.total = d1$Total ## here
  d1.mark = d1.total


  summit_id = sub_id
  mark.order = match(d2$ID,summit_id)
  result = as.numeric(d1.mark[mark.order] * weight / totalIn)
  result[is.na(result)] = 0
  marks = cat(result ,sep = '\n')
  invisible(result)
}

check = function(individual, grade){
  ind = read.csv(individual, stringsAsFactors = FALSE)
  fin = read.csv(grade, stringsAsFactors = FALSE)
  correct = TRUE
  for(i in 1:nrow(ind)){
    nameLoc = grep(ind[i,1], fin[,1])
    if (length(nameLoc) == 0){
      print(i)
      print(paste0("I can't find this name in grade.csv", '\nName = ', ind[i,1]))
      correct = FALSE
      next
    } 
    idLoc = grep(ind[i,2], fin[,2])
    if (length(idLoc) == 0){
      print(paste0("I can't find this ID in grade.csv", '\nID = ', ind[i,2]))
      correct = FALSE
      next
    }
    if(nameLoc != idLoc){
      print(paste0("Students have incorrect id", ind[i,2]))
      correct = FALSE
    }
  }
  correct
}




## before marking
#cCsv('A3')
#divide('A3-marks.csv', 6)





## after marking
combine('_temp', 'A3-marks.csv')
check('A3-marks.csv', 'grades.csv')
ExportMark('A3-marks.csv', '2020-06-11T1023_Grades-STATS_220.csv', totalIn = 16, weight = 10)

all.equal(read.csv('lab03-marks.csv'), read.csv('lab03-marks-combine.csv'))

mfile = read.csv(marks_csv, stringsAsFactors = FALSE)
cc = cutAt(mfile, tot = 6)
z = list()
llat = lnew = 1
for(i in 2:length(cc)){
  lnew = which(mfile$name == cc[i])
  z[[i - 1]] = mfile[llat:lnew,]
  llat = which(mfile$name == cc[i]) + 1
}
z[[length(cc)]] = mfile[llat:nrow(mfile), ]
for(i in 1:length(z)){
  bname = strsplit(marks_csv, '[.]')[[1]][1]
  fn = paste0('_temp/', bname, '-c-n', i, '.csv')
  out_file = file(fn)
  write.csv(z[[i]], out_file, row.names = FALSE, na = '')
}


old = read.csv('_temp - Copy/lab01-marks-c-6.csv', stringsAsFactors = FALSE)
new = read.csv('_temp/lab01-marks-c-n6.csv', stringsAsFactors = FALSE)
old$name == new$name

setwd('C:/Users/zhiji/OneDrive - The University of Auckland/course/marking/220-2020/_temp')

## double check when delete the first row

handin = function(ID)
{
  if(any(ID %in% sub_id))
  {
    TRUE
  }else
  {
      FALSE
    }
}

handin(199936)









###########################################################################################################
###tidy html
cmd = paste0(
  'f="',list.files('A1/'), '"\n',
  'echo working on: lab01error/"$f"', '\n',
  'tidy ', '"', list.files('A1/', full.names = TRUE), '"', '&> lab01error/"$f"', '\n')
cat(cmd, sep = '\n')


## copy cat in console (not srue why .bat got error)
#######################################################
writeChar(cmd, 'autoXml.bat', useBytes = TRUE)
#######################################################


errorfiles = list.files('lab01error/', full.names = TRUE)
out = matrix('', nrow = length(errorfiles), ncol = 2)
for(i in 1:length(errorfiles)){
  ef = readLines(errorfiles[i])
  isPass = any(grepl('No warnings or errors were found', ef))
  out[i,] = c(basename(errorfiles[i]), isPass)
}
cat(ifelse(as.logical(out[,2]) == TRUE, 2, 0.5), sep = '\n')




###########################################################################################################
###tidy xml
cmd = paste0(
  'f="',list.files('A2_temp/'), '"\n',
  'echo working on: lab01error/"$f"', '\n',
  'xmllint --valid ', '"', list.files('A2_temp/', full.names = TRUE), '"', '&> lab01error/"$f"', '\n')
cat(cmd, sep = '\n')


writeChar(cmd, 'autoXml.bat', useBytes = TRUE)



errorfiles = list.files('lab01error/', full.names = TRUE)
out = matrix('', nrow = length(errorfiles), ncol = 2)
for(i in 1:length(errorfiles)){
  ef = readLines(errorfiles[i])
  notPass = any(grepl('validity error', ef))
  out[i,] = c(basename(errorfiles[i]), notPass)
}
score =  ifelse(as.logical(out[,2]) == TRUE, 1, 2)
out = cbind(out, score)


## if the file is missing...
idLocation = gregexpr('_[0-9]*_', out[,1])
idStart = unlist(lapply(idLocation, function(x) x[1]))
mulidLength = lapply(idLocation, attr, 'match.length')
idLength = unlist(lapply(mulidLength, function(x) x[1]))
id.prep = substring(out[,1], idStart, idStart + idLength - 1)
id = gsub('_', '', id.prep)
out[,1] = id

mcsv = read.csv('_temp-a2/A2-marks-c-5.csv')
mid = mcsv$id

finalmat = out[match(mid, as.numeric(id)),]
cat(finalmat[,3], sep = '\n')
###########################################################################################################
  
## unzip file
## two dir: unzip; untar
## cd on lab0*
fname = list.files('./', pattern = 'tar')

for(i in 3:length(fname)){
  cmd = paste0('rm -r ../lab07unzip/* ;', 
               paste0('tar -xf ', fname[i], ' --directory ../lab07unzip/ '),sep = '\n')
  cmd2 = 'rm ../lab07unzip/*html; rm ../lab07unzip/*pdf'
  #cat('rm ../lab05Out/* \n')
  #cat(paste0('tar -xf ', fname[i], ' --directory ../lab05Out/'), sep = '\n')
  system(cmd)
  system(cmd2)
  print(paste0(fname[i], ': has following things'))
  system('ls ../lab07unzip')
  system('cd ../lab07unzip; make')
  system('cd ../lab07')
  #cat(cmd)
  invisible(readline(prompt="Press [enter] to continue"))
}

setwd('lab07')
fname = list.files('./', pattern = 'tar')
for(i in 1:length(fname)){
  cmd = paste0('tar xf ', fname[i], ' --directory ../lab07unzip/')
  
  cmd2 = 'rm ../lab07unzip/*'
  system(cmd, wait = FALSE)
  system(cmd2)
  invisible(readline(prompt="Press [enter] to continue"))
}


## linud cmd
## vm = sc-cer00014-04.its.auckland.ac.nz
cp ../lab07/* .

for i in *.tar*
do
  filename=$i
  file="${filename%.*.*}"
  echo '----------------------------------Working On: ------------------------------------------------'
  echo 'File::::' $file
  dir=../lab09unzip/$file/
  mkdir $dir
  tar zxvf $filename --xform='s#.+/##x' --directory $dir
  cd $dir
  rm *pdf; rm *html
  make --always-make > ../out.txt
  #rm -r *
  cd ../../lab09
  echo '------------------------------------- End ----------------------------------------------------'
  #rm $filename
done



for i in *.tar*
do
  filename=$i
  file="${filename%.*.*}"
  dir=../lab09unzip/$file/
  mkdir $dir
  tar xvf $filename --xform='s#.+/##x' --directory $dir
  cd $dir
  #rm *pdf; rm *html
  cd ../../lab09
done


for i in *
do
  echo '(cd' $i '; make --always-make &> ./out.txt; cd../) & ' >> ../bin
done


## unzip
for i in *.tar*
do
  filename=$i
  file="${filename%.*.*}"
  dir=../lab010unzip/$file/
    mkdir $dir
  tar xvf $filename --xform='s#.+/##x' --directory $dir
done
