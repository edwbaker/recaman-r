library(plotrix)
n <- 25;

i <- 1:n*100;
v <- rep_len(FALSE, n*100)


x <- c()
r <- c()
deg1 <- c()
deg2 <- c()

j <- 1

while(length(x) < n) {
  if (j == 1) {
    old_pos <- 1;
    new_pos <- 2;
    v[[1]] <- TRUE;
  } else {
    if ((old_pos - j) < 1 || v[[old_pos - j]] == TRUE) {
      new_pos <- old_pos + j
    }   else {
      new_pos <- old_pos - j
    }
  }
  
  x <- c(x, (old_pos + new_pos)/2)
  r <- c(r, abs(old_pos-new_pos)/2)
  
  if ((j %% 2) == 0) {
    deg1 <- c(deg1, 0)
    deg2 <- c(deg2, 180)
  } else {
    deg1 <- c(deg1, 180)
    deg2 <- c(deg2, 360)
  }
  
  
  v[[new_pos]] <- TRUE
  old_pos <- new_pos
  
  j <- j+1
}

y <- rep_len(0, length(x))

max_x <- max(x) + max(r)
max_y <- max(r)
plot(c(0, 0, max_x), c(-max_y, 0, max_y), type="n",asp = 1,main="Test draw.arc")
draw.arc(x=x, y=y, radius=r,deg1=deg1,deg2=deg2)