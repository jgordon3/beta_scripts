library(venneuler)
require(venneuler)
v <- venneuler(c(A=3341, B=6560, C=20715, D=24917, "A&B"=2057, "A&C"=2551, "A&D"=2805, "B&C"=5780, "B&D"=6126, "C&D"=17464,
                  "A&B&C"=2057, "A&C&D"=2015, "A&B&C&D"=2015))
plot(v)