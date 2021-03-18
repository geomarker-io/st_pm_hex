## variable importance for 2017

variable importance in the grf package is:

> A simple weighted sum of how many times feature i was split on at each depth in the forest.

decay exponent controls how much weight split depth has; max depth of 4

2017 MAE before taking variables out: 0.9697 

| var_imp|variable           |
|-------:|:------------------|
|  0.3452|nearby_pm          |
|  0.1784|x                  |
|  0.1105|hpbl               |
|  0.0826|doy                |
|  0.0536|nei_event          |
|  0.0479|vwnd.10m           |
|  0.0428|air.2m             |
|  0.0355|rhum.2m            |
|  0.0172|y                  |
|  0.0156|vis                |
|  0.0132|nei_nonroad        |
|  0.0116|uwnd.10m           |
|  0.0082|pres.sfc           |
|  0.0077|population_density |
|  0.0059|nei_dist           |
|  0.0056|prate              |
|  0.0038|impervious         |
|  0.0024|nonimpervious      |
|  0.0020|nei_point          |
|  0.0020|nonroad_urban      |
|  0.0016|s1100_dist         |
|  0.0015|green              |
|  0.0014|holiday            |
|  0.0012|dow                |
|  0.0010|tertiary_urban     |
|  0.0009|aod                |
|  0.0004|secondary_urban    |
|  0.0002|primary_urban      |
|  0.0000|year               |
|  0.0000|nei_onroad         |
|  0.0000|nei_nonpoint       |
|  0.0000|primary_rural      |
|  0.0000|secondary_rural    |
|  0.0000|tertiary_rural     |
|  0.0000|thinned_urban      |
|  0.0000|thinned_rural      |
|  0.0000|nonroad_rural      |
|  0.0000|energyprod_urban   |
|  0.0000|energyprod_rural   |
|  0.0000|fire_pm25          |
|  0.0000|fire_area          |

MAE after removing `year` and below: 0.9645

| var_imp|variable           |
|-------:|:------------------|
|  0.4918|nearby_pm          |
|  0.1897|x                  |
|  0.0871|hpbl               |
|  0.0577|doy                |
|  0.0429|nei_event          |
|  0.0325|air.2m             |
|  0.0239|vwnd.10m           |
|  0.0191|rhum.2m            |
|  0.0160|y                  |
|  0.0072|vis                |
|  0.0060|uwnd.10m           |
|  0.0060|pres.sfc           |
|  0.0030|population_density |
|  0.0024|nei_nonroad        |
|  0.0022|impervious         |
|  0.0021|nei_dist           |
|  0.0020|prate              |
|  0.0018|holiday            |
|  0.0017|green              |
|  0.0016|nonimpervious      |
|  0.0010|nonroad_urban      |
|  0.0010|aod                |
|  0.0005|nei_point          |
|  0.0004|s1100_dist         |
|  0.0003|tertiary_urban     |
|  0.0002|dow                |
|  0.0001|secondary_urban    |
|  0.0001|primary_urban      |


MAE after removing `nei_point` and below: 0.9566

| var_imp|variable           |
|-------:|:------------------|
|  0.6106|nearby_pm          |
|  0.1631|x                  |
|  0.0687|hpbl               |
|  0.0381|doy                |
|  0.0304|air.2m             |
|  0.0240|nei_event          |
|  0.0152|vwnd.10m           |
|  0.0143|y                  |
|  0.0100|rhum.2m            |
|  0.0054|pres.sfc           |
|  0.0048|vis                |
|  0.0022|population_density |
|  0.0020|nei_nonroad        |
|  0.0018|uwnd.10m           |
|  0.0017|impervious         |
|  0.0015|holiday            |
|  0.0014|nei_dist           |
|  0.0010|green              |
|  0.0010|nonimpervious      |
|  0.0010|aod                |
|  0.0009|nonroad_urban      |
|  0.0008|prate              |
|  0.0001|dow                |

MAE after running above set again: 0.9576

| var_imp|variable           |
|-------:|:------------------|
|  0.6097|nearby_pm          |
|  0.1573|x                  |
|  0.0692|hpbl               |
|  0.0435|doy                |
|  0.0291|air.2m             |
|  0.0273|nei_event          |
|  0.0146|vwnd.10m           |
|  0.0138|y                  |
|  0.0091|rhum.2m            |
|  0.0049|vis                |
|  0.0047|pres.sfc           |
|  0.0030|uwnd.10m           |
|  0.0024|nei_nonroad        |
|  0.0020|population_density |
|  0.0017|impervious         |
|  0.0015|holiday            |
|  0.0015|nei_dist           |
|  0.0010|green              |
|  0.0010|nonimpervious      |
|  0.0009|nonroad_urban      |
|  0.0008|aod                |
|  0.0007|prate              |
|  0.0001|dow                |

MAE after cutting below population density: 0.9257

| var_imp|variable           |
|-------:|:------------------|
|  0.8878|nearby_pm          |
|  0.0584|x                  |
|  0.0144|hpbl               |
|  0.0108|air.2m             |
|  0.0080|nei_event          |
|  0.0063|doy                |
|  0.0049|y                  |
|  0.0033|vwnd.10m           |
|  0.0022|rhum.2m            |
|  0.0013|vis                |
|  0.0013|pres.sfc           |
|  0.0011|population_density |
|  0.0001|uwnd.10m           |
|  0.0001|nei_nonroad        |

MAE after running with only nearby_pm, x, hpbl, doy, air.2m, nei_event, vwnd.10m, y: 0.8989

 var_imp|variable  |
|-------:|:---------|
|  0.9891|nearby_pm |
|  0.0053|x         |
|  0.0019|doy       |
|  0.0013|air.2m    |
|  0.0009|nei_event |
|  0.0008|hpbl      |
|  0.0005|y         |
|  0.0003|vwnd.10m  |

## overall results

| n_var | mae |
|-----:|:-----|
| 41 | 0.970 |
| 28 | 0.965 |
| 14 | 0.926 |
| 8  | 0.899 |  
