open3d()
bg3d("white")
material3d(col = "black")

zero = matrix(0, 20, 20)
persp3d(x=seq(1,20), y=seq(1,20), z = zero, aspect = c(1, 1, 0.5), col = "red",
        xlab = "X", ylab = "Y", zlab = "z", 
        xlim = c(-4, 4), ylim = c(-1, 1), zlim = c(0, 2),
        polygon_offset = 1)
persp3d(x=seq(1,20), y=seq(1,20), z = zero, front = "lines", back = "lines",
        xlim = c(-4, 4), ylim = c(-1, 1), zlim = c(0, 2),
        lit = FALSE, add = TRUE)
h = 0
b = 2
cg = 0
c = 0.55191502449
for (i in 1:30){
  p1 <- matrix(c(0 + b, 1 + b, 0 + h, 
                 c + cg, 1 + b, 0 + h, 
                 1 + b, c + cg, 0 + h, 
                 1 + b, 0 + b, 0 + h, 
                 1 + b, 0 + b, 0 + h,
                 1 + b, -c - cg, 0 + h,
                 c + cg, -1 - b, 0 + h,
                 0 + b, -1 - b, 0 + h,
                 0 + b, -1 - b, 0 + h,
                 -c - cg, -1 - b, 0 + h,
                 -1 - b, -c - cg, 0 + h,
                 -1 - b, 0 + b, 0 + h,
                 -1 - b, 0 + b, 0 + h,
                 -1 - b, c + cg, 0 + h,
                 -c - cg, 1 + b, 0 + h,
                 0 + b, 1 + b, 0 + h), 
               nrow=16, ncol=3, byrow=TRUE)
  plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ],), col = "#8A0808", size = 1, add = TRUE)
  b = b + 0.1
  h = h + 0.01
}