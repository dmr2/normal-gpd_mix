Codes for a Normal-GPD probability mixture model for tide gauge data (R language)

README file last updated by DJ Rasmussen, dmr2-at-princeton-dot-edu, Sat Oct 30 11:36:06 PDT 2021

# Contents

**fit** -- Codes for inferring the parameters of the Normal-Generalized Pareto Distribution fit to the historical tide gauge data

**return_curves** -- Codes for producing extreme sea level return curves (including their uncertainties) from the Normal-Generalized Pareto Distribution fits

# Data Requirements

* Daily maximum extreme sea level data at tide gauges (e.g. from the GESLA2 database or the University of Hawawii Sea Level Center)
* Probability distributions of future relative sea level change (e.g., from Kopp et al., 2014 or Rasmussen et al., 2018 or Bamber et al., 2019)

Examples of these data are provided, including Normal-GPD parameters previously fit to gauge data from several hundred tide gauges

# Package Requirements

* R programming language
* MASS library
* ggPlot2 library
* scales library
* extRemes library

## References

Based off of: 

Ghanbari et al. 2019: A Coherent Statistical Model for Coastal Flood Frequency Analysis Under Nonstationary Sea Level Conditions. Earth's Future. doi: 10.1029/2018EF001089

D. J. Rasmussen et al. 2018: Extreme sea level implications of 1.5 °C, 2.0 °C, and 2.5 °C temperature stabilization targets in the 21st and 22nd centuries. Environmental Research Letters. doi: 10.1088/1748-9326/aaac87.

----

    Copyright (C) 2021 by D.J. Rasmussen

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
