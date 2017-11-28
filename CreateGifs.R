.libPaths("E:/R packages")
require(tcltk)
require(magick)

for (i in 13){
  rm(list=setdiff(ls(), "i"))
  setwd("E:/Kiri's Storage/R mapping")
  treeList <- c("Yc","Lw","Fd","Py","Pl","Sx","Cw","Hw","Bl","Dr","Ep","Pw","Qg")
  n1 <- paste(treeList[i],"Currentsame.png", sep = "")
  n2 <- paste(treeList[i],"2025.png", sep = "")
  n3 <- paste(treeList[i],"2055.png", sep = "")
  n4 <- paste(treeList[i],"2085.png", sep = "")
  i1 <- image_read(n1)
  i1 <- image_scale(i1, "800x800")
  i2 <- image_read(n2)
  i2 <- image_scale(i2, "800x800")
  i3 <- image_read(n3)
  i3 <- image_scale(i3, "800x800")
  i4 <- image_read(n4)
  i4 <- image_scale(i4, "800x800")
  
  i1 <- image_annotate(i1, "Current", gravity = "northeast", size = 36, location = "+350+80", color = "black")
  
  i2 <- image_annotate(i2, "2025", gravity = "northeast", location = "+350+80", size = 36,  color = "black")
  
  i3 <- image_annotate(i3, "2055", gravity = "northeast", location = "+350+80", size = 36,  color = "black")
  
  i4 <- image_annotate(i4, "2085", gravity = "northeast", location = "+350+80", size = 36, color = "black")
  
  gif <- image_animate(c(i1,i2,i3,i4), fps = 1)
  outname <- paste(treeList[i],".gif", sep = "")
  image_write(gif, outname)
}
