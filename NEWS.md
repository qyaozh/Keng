# Keng 2024.11.30

* `power_r()` has been added to conduct post-hoc and prior power analysis, and plan the sample size for Pearson's r.
* `power_lm()` has reorganized its output.
* `power_r()` and `power_lm()` return the `Keng_power` class, which has `print()` and `plot()` methods.

# Keng 2024.11.25

* `test_r()` has added Fisher's z.
* `compare_lm()` has added f_squared and post-hoc power.
* `calc_PRE()` has been added to calculate PRE from partial correlation, Cohen's f, or f_squared.
* `power_lm()` has been added to conduct post-hoc and prior power analysis, and plan the sample size for one or a set of predictors in linear regression.

# Keng 2024.11.17

* Added a `NEWS.md` file to track changes to the package.
* Dataset `depress` is added.
* Format of `compare_lm()`'s output is reorganized.
* Versatile `Scale()` is added. 
