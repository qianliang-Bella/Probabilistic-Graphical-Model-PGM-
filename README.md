# Probabilistic-Graphical-Model-PGM-
Goal of this assignment is to show how with directed acyclic graph we can represent a probability distribution much more compactly. It is based on two key ideas we learned in the lecture: conditional independence and factorization of joint probabilities by the graphical model.
[Assign_PGM1 (1).docx](https://github.com/user-attachments/files/23843496/Assign_PGM1.1.docx)
# IQ
P_i <- c(i0 = 0.7, i1 = 0.3)
# Class difficulty
P_d <- c(d0 = 0.6, d1 = 0.4)
# Grade probabilities P(g | i, d)
P_g_given_i_d <- list(
  i0 = list(d0 = c(g1=0.3, g2=0.4, g3=0.3),
            d1 = c(g1=0.05, g2=0.25, g3=0.7)),
  i1 = list(d0 = c(g1=0.9, g2=0.08, g3=0.02),
            d1 = c(g1=0.5, g2=0.3, g3=0.2))
)

# SAT probabilities P(s | i)
P_s_given_i <- list(
  i0 = c(s0=0.95, s1=0.05),
  i1 = c(s0=0.2,  s1=0.8)
)
# Recommendation probabilities P(l | g)
P_l_given_g <- c(l1_g1 = 0.9, l1_g2 = 0.6, l1_g3 = 0.01)

# Compute P(g3|i) over difficulty
P_g_given_i <- function(i, g) {
  sum(sapply(names(P_d), function(d) P_d[d] * P_g_given_i_d[[i]][[d]][g]))
}

# Compute p(g3)
P_g3 <- sum(sapply(names(P_i), function(i) P_i[i] * P_g_given_i(i, "g3")))
#commputp(i1|g3)
P_i1_given_g3 <- (P_i["i1"] * P_g_given_i("i1", "g3")) / P_g3
P_i1_given_g3
