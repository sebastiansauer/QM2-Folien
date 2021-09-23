library(DiagrammeR)
library(DiagrammeRsvg)
library(nomnoml)

m1 <-
  mermaid("
        graph LR
        A(0)-->|1/2| B(K)
        A-->|1/2| C(Z)
        B-->|1/2| D(K)
        B-->|1/2| E(Z)
        C-->|1/2| F(K)
        C-->|1/2| G(K)
        D --- H(KK = 2K)
        E --- I(KZ = 1K)
        F --- J(ZK = 1K)
        G --- K(KK = 1K)
        ")
m1

plotly::export(m1, file = "muenz1.pdf")





nomnoml("
#direction: down
[<start> start] -> 1/2[K]
[<start> start] -> 1/2[Z]
[K] -> 1/2[KK = 2K]
[K] -> 1/2[KZ = 1K]

[Z] -> 1/2[ZK = 1K]
[Z] -> 1/2[ZZ = 0k]",
png = "img/muenz1.png")




nomnoml("
#direction: down
[<start> start] -> 1/2[K]
[<start> start] -> 1/2[Z]
[K] -> 1/2[KK]
[K] -> 1/2[KZ]

[Z] -> 1/2[ZK]
[Z] -> 1/2[ZZ]

[KK] -> 1/2 [KKK = 3K]
[KK] -> 1/2 [KKZ = 2K]
[KZ] -> 1/2 [KZK = 2K]
[KZ] -> 1/2 [KZZ = 1K]
[ZK] -> 1/2 [ZKK = 2K]
[ZK] -> 1/2 [ZKZ = 1K]
[ZZ] -> 1/2 [ZZK = 1K]
[ZZ] -> 1/2 [ZZZ = 0K]
",
png = "img/muenz2.png")




nomnoml("
#direction: down
[Regression] -> [t-Test]
[Regression] -> [Wilxocon Test]
[Regression] -> [Korrelation]
[Regression] -> [Varianzanalyse]
[Regression] -> [Chi-Quadrat-Test]
[Regression] -> [...]
        ",
png = "img/regression-specialcases.png")



nomnoml("
        [Priori-Vert.] -> [Likelihood]
        [Likelihood] -> [Posteriori-Vert.]
        ",
        png = "img/bayesupdate.png")



nomnoml("
        [Vorab-Wissen] -> [Daten]
        [Daten] -> [Geupdatetes Wissen]
        ",
        png = "img/bayesupdate2.png")
