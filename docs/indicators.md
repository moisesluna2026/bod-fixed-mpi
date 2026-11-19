# Indicators and Census variables

Universe: residents of permanent private occupied households (2010: V4001 = 1; 2022: D0130 = 1 and
P0130 = 1). Weights: V0010 (2010 person record) and P0110 (2022). Household indicators are read from
the household record; person indicators are computed for each resident and the household is
deprived when at least one resident is deprived. Missing answers count as not deprived. Age bands
follow the five-year groups of the 2022 public files (P0180), except school delay in 2010, which uses
age in completed years (V6036). The definitions follow the IPM-Brasil specification (12 indicators,
weights 1/12, k = 1/3); the 2010 and 2022 national and UF estimates of H, A and M0 reproduce that
specification exactly.

| Dimension | Indicator | 2010 | 2022 |
|---|---|---|---|
| Sanitation | water | V0208 in {2..10} and V0209 = 3 | D0260 in {2..8} and D0300 = 3 |
| | sewage | V0206 = 2 or V0207 in {3..6} | D0250 in {4..9} |
| | garbage | V0210 in {3..7} | D0310 in {3..6} |
| Housing and goods | density | V6204 > 2 (residents per bedroom, IBGE) | D0240 > 2 |
| | bathroom | V0205 = 0 | D0270 in {5,6,7} |
| | goods | V0215 = 2 (no washing machine) | D0320 = 2 |
| Education | attendance | age 5-14 and V0628 in {3,4} | P0180 in {2,3} and P0650 in {2,3} |
| | delay | V0628 in {1,2}, age 6-19 and level of the course (V0629) below the level adequate to the age | P0180 in {2,3,4}, P0650 = 1 and P0780 = 2 |
| | schooling | age 20+ and V6400 = 1 | P0180 in {5..17} and P0770 = 1 |
| Work and income | work | age 20+ and [V0654 = 1, or V0648 != 6 and V0650 = 3] | P0180 in {5..17} and [P0930 = 1 and P0950 = 0, or P0950 = 1, P0990 != 7 and P0940 = 0] |
| | child labour | age 10-14 and any of V0641..V0644 = 1 | P0180 = 3 and P0960 = 1 |
| | income | V6532 < 0.25 (household income per capita in minimum wages) | D0360 < 303 (one quarter of R$ 1,212) |

Level of the course in 2010 (V0629): 1-4 pre-school and literacy; 5-6 fundamental; 7-8 secondary;
9-12 tertiary. Adequate level: fundamental at 6-14, secondary at 15-17, tertiary at 18-19.

Robustness versions (stage 07):
- *income_line_constant_real*: the 2010 line (R$ 127.50) in July 2022 reais using the IPCA number
  index (IBGE, SIDRA table 1737): 127.50 x 6411.95 / 3111.05 = 262.78; D0360 < 262.78.
- *school_delay_harmonized*: same rule in both years, by age group and course attended:
  2010, V0628 in {1,2} and [age 10-14 and V0629 in {1..4}, or 15-19 and V0629 in {1..6}];
  2022, P0650 = 1 and [P0180 = 3 and P0660 in {1,2,3}, or P0180 = 4 and P0660 in {1..5}].
- *eleven_indicators_without_child_labour*: the occupation questions changed in 2022.
- *2022_without_suppressed_age*: 2022 persons with suppressed age (P0180 = 99) cannot be placed in
  an age band; their households are removed.
- *four_dimensions*, *pooled_reference_2010_2022*, *normative_reference_total_deprivation*,
  leave-one-out of the reference and Poisson bootstrap.

## 1991 and 2000 (stage 09, optional)

Same twelve indicators, with the rules adapted to each questionnaire (variables: 1991 V0205, V0206,
V0214, V2121, V0213, V0226, V0221, V0325, V0326, V3241, V0345, V0349, V0353, V0358, V2012, V0302;
2000 V0207, V0208, V0210, V0211, V0212, V7204, V0209, V0217, V0429, V0430, V4300, V0439-V0443,
V0447, V0450, V0455, V7616, V0402). Universe: permanent private households (V0201 = 1). Minimum wage of
reference: Cr$ 36,161.60 (1991) and R$ 151.00 (2000). In 1991 the goods indicator also counts the lack
of electric lighting, the bathroom question includes the servants' bathroom and occupation refers to
the 12 months before the Census. Exact conditions in `R/09_series_1991_2022.R`.
