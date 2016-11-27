# wicedproj --- Wittgenstein Centre Education Projections

The `wicedproj` R package contains code implementing the projection model of educational attainment
of the [Wittgenstein Centre for Demography and Global Human Capital](http://www.wittgensteincentre.org/en/index.htm). 

The projections from this package have been used in the Global Education Monitoring 2016 among others,
and in particular the publication:

Abel G.J., Barakat B., KC S., and Lutz W. 2016. Meeting the Sustainable Development Goals leads to lower world population growth. Proceedings of the National Academy of Science. doi:10.1073/pnas.1611386113 [(code)](http://www.iiasa.ac.at/web/home/research/researchPrograms/WorldPopulation/SDG_Scenarios_2016.html)

In its appendix, Abel et al. (2016) also includes a narrative description of key assumptions and features of the education projection model implemented here.


# Installation

Using the `devtools` package:

```R
install.packages("devtools")
devtools::install_github("hadley/devtools")
```

Note that you require a working development environment, since some code requires compilation at run-time.
Setting this up is somewhat platform-dependent and cannot be covered exhaustively here.
However, helpful starting points are available in the documentation for the `devtools` and `rstan` packages.
