fresh_line <- function(output){
  writeLines("",output,sep="\n")
}

insert_conductance <- function(current_name,output){
  writeLines(paste("    insert ",current_name,sep=""),output,sep="\n")
}

set_property <- function(name,value,output){
    writeLines(paste("    ",name," = ",value,sep=""),output,sep="\n")
}

fresh_line <- function(output){
  writeLines("",output,sep="\n")
}

equal <- function(object,value,output){
  writeLines(paste(object," = ",value,sep=""),output,sep="\n")
}

strdefs <- function(names,output){
  writeLines(paste("strdef ",paste(names,collapse=","),sep=""),output,sep="\n")
}

objrefs <- function(names,output){
  writeLines(paste("objref ",paste(names,collapse=","),sep=""),output,sep="\n")
}

Vectors <- function(names,output){
  for(name in names){
    equal(name,"new Vector()",output)
  }
}

Vector_append <- function(name,vect,output){
  writeLines(paste(name,".append(",paste(vect,collapse=","),")",sep=""),output,sep="\n")
}

load_file <- function(filename,output){
  writeLines(paste("load_file(\"",filename,"\")",sep=""),output,sep="\n")
}

topology <- function(output){
  writeLines("topology()",output,sep="\n")
}
  
write_value <- function(value,output){
  writeLines(paste(value,collapse=" "),output,sep="\n")
}

comment <- function(comment,output){
  writeLines(comment,output,sep="\n")
}
