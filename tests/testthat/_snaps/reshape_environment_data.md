# reshape_environment_data() works with correct inputs

    Code
      print(object, n = nrow(object))
    Output
      # A tibble: 792 x 8
          index_name  value  year month   lag impacted_group species       group
          <chr>       <dbl> <int> <int> <int> <chr>          <chr>         <chr>
        1 amo        0.679   1984     1     1 menhaden 0     menhaden      0    
        2 amo        0.718   1984     2     1 menhaden 0     menhaden      0    
        3 amo        0.639   1984     3     1 menhaden 0     menhaden      0    
        4 amo        0.571   1984     4     1 menhaden 0     menhaden      0    
        5 amo        0.552   1984     5     1 menhaden 0     menhaden      0    
        6 amo        0.213   1984     6     1 menhaden 0     menhaden      0    
        7 amo        0.339   1984     7     1 menhaden 0     menhaden      0    
        8 amo        0.424   1984     8     1 menhaden 0     menhaden      0    
        9 amo        0.460   1984     9     1 menhaden 0     menhaden      0    
       10 amo        0.173   1984    10     1 menhaden 0     menhaden      0    
       11 amo        0       1984    11     1 menhaden 0     menhaden      0    
       12 amo        0.228   1984    12     1 menhaden 0     menhaden      0    
       13 amo        0.149   1985     1     1 menhaden 0     menhaden      0    
       14 amo        0.202   1985     2     1 menhaden 0     menhaden      0    
       15 amo        0.145   1985     3     1 menhaden 0     menhaden      0    
       16 amo        0.0511  1985     4     1 menhaden 0     menhaden      0    
       17 amo        0.185   1985     5     1 menhaden 0     menhaden      0    
       18 amo        0.633   1985     6     1 menhaden 0     menhaden      0    
       19 amo        0.579   1985     7     1 menhaden 0     menhaden      0    
       20 amo        0.305   1985     8     1 menhaden 0     menhaden      0    
       21 amo        0.377   1985     9     1 menhaden 0     menhaden      0    
       22 amo        0.381   1985    10     1 menhaden 0     menhaden      0    
       23 amo        0.275   1985    11     1 menhaden 0     menhaden      0    
       24 amo        0.187   1985    12     1 menhaden 0     menhaden      0    
       25 amo        0.173   1986     1     1 menhaden 0     menhaden      0    
       26 amo        0.317   1986     2     1 menhaden 0     menhaden      0    
       27 amo        0.281   1986     3     1 menhaden 0     menhaden      0    
       28 amo        0.213   1986     4     1 menhaden 0     menhaden      0    
       29 amo        0.417   1986     5     1 menhaden 0     menhaden      0    
       30 amo        0.347   1986     6     1 menhaden 0     menhaden      0    
       31 amo        0.375   1986     7     1 menhaden 0     menhaden      0    
       32 amo        0.324   1986     8     1 menhaden 0     menhaden      0    
       33 amo        0.437   1986     9     1 menhaden 0     menhaden      0    
       34 amo        0.226   1986    10     1 menhaden 0     menhaden      0    
       35 amo        0.0575  1986    11     1 menhaden 0     menhaden      0    
       36 amo        0.113   1986    12     1 menhaden 0     menhaden      0    
       37 amo        0.311   1987     1     1 menhaden 0     menhaden      0    
       38 amo        0.441   1987     2     1 menhaden 0     menhaden      0    
       39 amo        0.833   1987     3     1 menhaden 0     menhaden      0    
       40 amo        0.965   1987     4     1 menhaden 0     menhaden      0    
       41 amo        1.05    1987     5     1 menhaden 0     menhaden      0    
       42 amo        1.34    1987     6     1 menhaden 0     menhaden      0    
       43 amo        1.50    1987     7     1 menhaden 0     menhaden      0    
       44 amo        1.50    1987     8     1 menhaden 0     menhaden      0    
       45 amo        1.32    1987     9     1 menhaden 0     menhaden      0    
       46 amo        1.00    1987    10     1 menhaden 0     menhaden      0    
       47 amo        0.754   1987    11     1 menhaden 0     menhaden      0    
       48 amo        0.995   1987    12     1 menhaden 0     menhaden      0    
       49 amo        0.814   1988     1     1 menhaden 0     menhaden      0    
       50 amo        0.654   1988     2     1 menhaden 0     menhaden      0    
       51 amo        0.880   1988     3     1 menhaden 0     menhaden      0    
       52 amo        1.01    1988     4     1 menhaden 0     menhaden      0    
       53 amo        1.19    1988     5     1 menhaden 0     menhaden      0    
       54 amo        1.25    1988     6     1 menhaden 0     menhaden      0    
       55 amo        1.13    1988     7     1 menhaden 0     menhaden      0    
       56 amo        0.880   1988     8     1 menhaden 0     menhaden      0    
       57 amo        0.694   1988     9     1 menhaden 0     menhaden      0    
       58 amo        0.532   1988    10     1 menhaden 0     menhaden      0    
       59 amo        0.581   1988    11     1 menhaden 0     menhaden      0    
       60 amo        0.547   1988    12     1 menhaden 0     menhaden      0    
       61 amo        0.424   1989     1     1 menhaden 0     menhaden      0    
       62 amo        0.562   1989     2     1 menhaden 0     menhaden      0    
       63 amo        0.356   1989     3     1 menhaden 0     menhaden      0    
       64 amo        0.279   1989     4     1 menhaden 0     menhaden      0    
       65 amo        0.584   1989     5     1 menhaden 0     menhaden      0    
       66 amo        1.09    1989     6     1 menhaden 0     menhaden      0    
       67 amo        1.27    1989     7     1 menhaden 0     menhaden      0    
       68 amo        1.14    1989     8     1 menhaden 0     menhaden      0    
       69 amo        0.694   1989     9     1 menhaden 0     menhaden      0    
       70 amo        0.633   1989    10     1 menhaden 0     menhaden      0    
       71 amo        0.575   1989    11     1 menhaden 0     menhaden      0    
       72 amo        0.569   1989    12     1 menhaden 0     menhaden      0    
       73 amo        0.258   1990     1     1 menhaden 0     menhaden      0    
       74 amo        0.550   1990     2     1 menhaden 0     menhaden      0    
       75 amo        0.513   1990     3     1 menhaden 0     menhaden      0    
       76 amo        0.645   1990     4     1 menhaden 0     menhaden      0    
       77 amo        0.769   1990     5     1 menhaden 0     menhaden      0    
       78 amo        0.775   1990     6     1 menhaden 0     menhaden      0    
       79 amo        0.848   1990     7     1 menhaden 0     menhaden      0    
       80 amo        1.01    1990     8     1 menhaden 0     menhaden      0    
       81 amo        1.21    1990     9     1 menhaden 0     menhaden      0    
       82 amo        1.14    1990    10     1 menhaden 0     menhaden      0    
       83 amo        0.780   1990    11     1 menhaden 0     menhaden      0    
       84 amo        0.818   1990    12     1 menhaden 0     menhaden      0    
       85 amo        0.479   1991     1     1 menhaden 0     menhaden      0    
       86 amo        0.645   1991     2     1 menhaden 0     menhaden      0    
       87 amo        0.745   1991     3     1 menhaden 0     menhaden      0    
       88 amo        0.611   1991     4     1 menhaden 0     menhaden      0    
       89 amo        0.567   1991     5     1 menhaden 0     menhaden      0    
       90 amo        0.618   1991     6     1 menhaden 0     menhaden      0    
       91 amo        0.665   1991     7     1 menhaden 0     menhaden      0    
       92 amo        0.660   1991     8     1 menhaden 0     menhaden      0    
       93 amo        0.814   1991     9     1 menhaden 0     menhaden      0    
       94 amo        0.334   1991    10     1 menhaden 0     menhaden      0    
       95 amo        0.337   1991    11     1 menhaden 0     menhaden      0    
       96 amo        0.437   1991    12     1 menhaden 0     menhaden      0    
       97 amo        0.486   1992     1     1 menhaden 0     menhaden      0    
       98 amo        0.690   1992     2     1 menhaden 0     menhaden      0    
       99 amo        0.694   1992     3     1 menhaden 0     menhaden      0    
      100 amo        0.498   1992     4     1 menhaden 0     menhaden      0    
      101 amo        0.417   1992     5     1 menhaden 0     menhaden      0    
      102 amo        0.569   1992     6     1 menhaden 0     menhaden      0    
      103 amo        0.424   1992     7     1 menhaden 0     menhaden      0    
      104 amo        0.0788  1992     8     1 menhaden 0     menhaden      0    
      105 amo        0.102   1992     9     1 menhaden 0     menhaden      0    
      106 amo        0.251   1992    10     1 menhaden 0     menhaden      0    
      107 amo        0.177   1992    11     1 menhaden 0     menhaden      0    
      108 amo        0.283   1992    12     1 menhaden 0     menhaden      0    
      109 amo        0.379   1993     1     1 menhaden 0     menhaden      0    
      110 amo        0.492   1993     2     1 menhaden 0     menhaden      0    
      111 amo        0.368   1993     3     1 menhaden 0     menhaden      0    
      112 amo        0.520   1993     4     1 menhaden 0     menhaden      0    
      113 amo        0.541   1993     5     1 menhaden 0     menhaden      0    
      114 amo        0.503   1993     6     1 menhaden 0     menhaden      0    
      115 amo        0.298   1993     7     1 menhaden 0     menhaden      0    
      116 amo        0.403   1993     8     1 menhaden 0     menhaden      0    
      117 amo        0.539   1993     9     1 menhaden 0     menhaden      0    
      118 amo        0.371   1993    10     1 menhaden 0     menhaden      0    
      119 amo        0.211   1993    11     1 menhaden 0     menhaden      0    
      120 amo        0.264   1993    12     1 menhaden 0     menhaden      0    
      121 amo        0.258   1994     1     1 menhaden 0     menhaden      0    
      122 amo        0.230   1994     2     1 menhaden 0     menhaden      0    
      123 amo        0.294   1994     3     1 menhaden 0     menhaden      0    
      124 amo        0.462   1994     4     1 menhaden 0     menhaden      0    
      125 amo        0.447   1994     5     1 menhaden 0     menhaden      0    
      126 amo        0.415   1994     6     1 menhaden 0     menhaden      0    
      127 amo        0.394   1994     7     1 menhaden 0     menhaden      0    
      128 amo        0.386   1994     8     1 menhaden 0     menhaden      0    
      129 amo        0.562   1994     9     1 menhaden 0     menhaden      0    
      130 amo        0.760   1994    10     1 menhaden 0     menhaden      0    
      131 amo        0.846   1994    11     1 menhaden 0     menhaden      0    
      132 amo        0.694   1994    12     1 menhaden 0     menhaden      0    
      133 amo        0.754   1995     1     1 menhaden 0     menhaden      0    
      134 amo        0.801   1995     2     1 menhaden 0     menhaden      0    
      135 amo        0.920   1995     3     1 menhaden 0     menhaden      0    
      136 amo        1.05    1995     4     1 menhaden 0     menhaden      0    
      137 amo        1.47    1995     5     1 menhaden 0     menhaden      0    
      138 amo        1.66    1995     6     1 menhaden 0     menhaden      0    
      139 amo        1.55    1995     7     1 menhaden 0     menhaden      0    
      140 amo        1.28    1995     8     1 menhaden 0     menhaden      0    
      141 amo        1.00    1995     9     1 menhaden 0     menhaden      0    
      142 amo        1.10    1995    10     1 menhaden 0     menhaden      0    
      143 amo        1.16    1995    11     1 menhaden 0     menhaden      0    
      144 amo        0.976   1995    12     1 menhaden 0     menhaden      0    
      145 amo        0.865   1996     1     1 menhaden 0     menhaden      0    
      146 amo        0.818   1996     2     1 menhaden 0     menhaden      0    
      147 amo        0.763   1996     3     1 menhaden 0     menhaden      0    
      148 amo        0.905   1996     4     1 menhaden 0     menhaden      0    
      149 amo        0.748   1996     5     1 menhaden 0     menhaden      0    
      150 amo        0.622   1996     6     1 menhaden 0     menhaden      0    
      151 amo        0.662   1996     7     1 menhaden 0     menhaden      0    
      152 amo        0.867   1996     8     1 menhaden 0     menhaden      0    
      153 amo        0.882   1996     9     1 menhaden 0     menhaden      0    
      154 amo        0.577   1996    10     1 menhaden 0     menhaden      0    
      155 amo        0.515   1996    11     1 menhaden 0     menhaden      0    
      156 amo        0.554   1996    12     1 menhaden 0     menhaden      0    
      157 amo        0.690   1997     1     1 menhaden 0     menhaden      0    
      158 amo        0.809   1997     2     1 menhaden 0     menhaden      0    
      159 amo        0.895   1997     3     1 menhaden 0     menhaden      0    
      160 amo        0.901   1997     4     1 menhaden 0     menhaden      0    
      161 amo        0.969   1997     5     1 menhaden 0     menhaden      0    
      162 amo        0.903   1997     6     1 menhaden 0     menhaden      0    
      163 amo        1.01    1997     7     1 menhaden 0     menhaden      0    
      164 amo        0.916   1997     8     1 menhaden 0     menhaden      0    
      165 amo        1.12    1997     9     1 menhaden 0     menhaden      0    
      166 amo        1.21    1997    10     1 menhaden 0     menhaden      0    
      167 amo        0.993   1997    11     1 menhaden 0     menhaden      0    
      168 amo        1.17    1997    12     1 menhaden 0     menhaden      0    
      169 amo        1.16    1998     1     1 menhaden 0     menhaden      0    
      170 amo        1.51    1998     2     1 menhaden 0     menhaden      0    
      171 amo        1.57    1998     3     1 menhaden 0     menhaden      0    
      172 amo        1.51    1998     4     1 menhaden 0     menhaden      0    
      173 amo        1.70    1998     5     1 menhaden 0     menhaden      0    
      174 amo        1.93    1998     6     1 menhaden 0     menhaden      0    
      175 amo        1.92    1998     7     1 menhaden 0     menhaden      0    
      176 amo        1.98    1998     8     1 menhaden 0     menhaden      0    
      177 amo        1.76    1998     9     1 menhaden 0     menhaden      0    
      178 amo        1.70    1998    10     1 menhaden 0     menhaden      0    
      179 amo        1.56    1998    11     1 menhaden 0     menhaden      0    
      180 amo        1.48    1998    12     1 menhaden 0     menhaden      0    
      181 amo        0.993   1999     1     1 menhaden 0     menhaden      0    
      182 amo        1.01    1999     2     1 menhaden 0     menhaden      0    
      183 amo        1.03    1999     3     1 menhaden 0     menhaden      0    
      184 amo        0.995   1999     4     1 menhaden 0     menhaden      0    
      185 amo        1.23    1999     5     1 menhaden 0     menhaden      0    
      186 amo        1.27    1999     6     1 menhaden 0     menhaden      0    
      187 amo        1.31    1999     7     1 menhaden 0     menhaden      0    
      188 amo        1.54    1999     8     1 menhaden 0     menhaden      0    
      189 amo        1.28    1999     9     1 menhaden 0     menhaden      0    
      190 amo        0.920   1999    10     1 menhaden 0     menhaden      0    
      191 amo        0.780   1999    11     1 menhaden 0     menhaden      0    
      192 amo        0.916   1999    12     1 menhaden 0     menhaden      0    
      193 amo        0.705   2000     1     1 menhaden 0     menhaden      0    
      194 amo        0.803   2000     2     1 menhaden 0     menhaden      0    
      195 amo        1.10    2000     3     1 menhaden 0     menhaden      0    
      196 amo        0.973   2000     4     1 menhaden 0     menhaden      0    
      197 amo        1.11    2000     5     1 menhaden 0     menhaden      0    
      198 amo        0.846   2000     6     1 menhaden 0     menhaden      0    
      199 amo        1.04    2000     7     1 menhaden 0     menhaden      0    
      200 amo        1.10    2000     8     1 menhaden 0     menhaden      0    
      201 amo        1.10    2000     9     1 menhaden 0     menhaden      0    
      202 amo        0.799   2000    10     1 menhaden 0     menhaden      0    
      203 amo        0.771   2000    11     1 menhaden 0     menhaden      0    
      204 amo        0.613   2000    12     1 menhaden 0     menhaden      0    
      205 amo        0.611   2001     1     1 menhaden 0     menhaden      0    
      206 amo        0.826   2001     2     1 menhaden 0     menhaden      0    
      207 amo        0.914   2001     3     1 menhaden 0     menhaden      0    
      208 amo        0.858   2001     4     1 menhaden 0     menhaden      0    
      209 amo        0.865   2001     5     1 menhaden 0     menhaden      0    
      210 amo        1.31    2001     6     1 menhaden 0     menhaden      0    
      211 amo        1.17    2001     7     1 menhaden 0     menhaden      0    
      212 amo        1.26    2001     8     1 menhaden 0     menhaden      0    
      213 amo        1.49    2001     9     1 menhaden 0     menhaden      0    
      214 amo        1.42    2001    10     1 menhaden 0     menhaden      0    
      215 amo        1.22    2001    11     1 menhaden 0     menhaden      0    
      216 amo        1.34    2001    12     1 menhaden 0     menhaden      0    
      217 amo        1.27    2002     1     1 menhaden 0     menhaden      0    
      218 amo        1.24    2002     2     1 menhaden 0     menhaden      0    
      219 amo        1.19    2002     3     1 menhaden 0     menhaden      0    
      220 amo        0.941   2002     4     1 menhaden 0     menhaden      0    
      221 amo        0.780   2002     5     1 menhaden 0     menhaden      0    
      222 amo        0.635   2002     6     1 menhaden 0     menhaden      0    
      223 amo        0.743   2002     7     1 menhaden 0     menhaden      0    
      224 amo        1.11    2002     8     1 menhaden 0     menhaden      0    
      225 amo        1.05    2002     9     1 menhaden 0     menhaden      0    
      226 amo        1.12    2002    10     1 menhaden 0     menhaden      0    
      227 amo        0.924   2002    11     1 menhaden 0     menhaden      0    
      228 amo        0.892   2002    12     1 menhaden 0     menhaden      0    
      229 amo        0.990   2003     1     1 menhaden 0     menhaden      0    
      230 amo        0.856   2003     2     1 menhaden 0     menhaden      0    
      231 amo        1.12    2003     3     1 menhaden 0     menhaden      0    
      232 amo        1.05    2003     4     1 menhaden 0     menhaden      0    
      233 amo        1.21    2003     5     1 menhaden 0     menhaden      0    
      234 amo        1.32    2003     6     1 menhaden 0     menhaden      0    
      235 amo        1.47    2003     7     1 menhaden 0     menhaden      0    
      236 amo        1.77    2003     8     1 menhaden 0     menhaden      0    
      237 amo        1.84    2003     9     1 menhaden 0     menhaden      0    
      238 amo        1.79    2003    10     1 menhaden 0     menhaden      0    
      239 amo        1.36    2003    11     1 menhaden 0     menhaden      0    
      240 amo        1.36    2003    12     1 menhaden 0     menhaden      0    
      241 amo        1.33    2004     1     1 menhaden 0     menhaden      0    
      242 amo        1.33    2004     2     1 menhaden 0     menhaden      0    
      243 amo        1.22    2004     3     1 menhaden 0     menhaden      0    
      244 amo        1.11    2004     4     1 menhaden 0     menhaden      0    
      245 amo        0.895   2004     5     1 menhaden 0     menhaden      0    
      246 amo        1.26    2004     6     1 menhaden 0     menhaden      0    
      247 amo        1.37    2004     7     1 menhaden 0     menhaden      0    
      248 amo        1.56    2004     8     1 menhaden 0     menhaden      0    
      249 amo        1.40    2004     9     1 menhaden 0     menhaden      0    
      250 amo        1.40    2004    10     1 menhaden 0     menhaden      0    
      251 amo        1.35    2004    11     1 menhaden 0     menhaden      0    
      252 amo        1.28    2004    12     1 menhaden 0     menhaden      0    
      253 amo        1.12    2005     1     1 menhaden 0     menhaden      0    
      254 amo        1.15    2005     2     1 menhaden 0     menhaden      0    
      255 amo        1.49    2005     3     1 menhaden 0     menhaden      0    
      256 amo        1.50    2005     4     1 menhaden 0     menhaden      0    
      257 amo        1.51    2005     5     1 menhaden 0     menhaden      0    
      258 amo        1.58    2005     6     1 menhaden 0     menhaden      0    
      259 amo        1.83    2005     7     1 menhaden 0     menhaden      0    
      260 amo        1.82    2005     8     1 menhaden 0     menhaden      0    
      261 amo        1.77    2005     9     1 menhaden 0     menhaden      0    
      262 amo        1.39    2005    10     1 menhaden 0     menhaden      0    
      263 amo        1.18    2005    11     1 menhaden 0     menhaden      0    
      264 amo        1.34    2005    12     1 menhaden 0     menhaden      0    
      265 amo        1.15    2006     1     1 menhaden 0     menhaden      0    
      266 amo        1.04    2006     2     1 menhaden 0     menhaden      0    
      267 amo        1.01    2006     3     1 menhaden 0     menhaden      0    
      268 amo        1.30    2006     4     1 menhaden 0     menhaden      0    
      269 amo        1.54    2006     5     1 menhaden 0     menhaden      0    
      270 amo        1.59    2006     6     1 menhaden 0     menhaden      0    
      271 amo        1.68    2006     7     1 menhaden 0     menhaden      0    
      272 amo        1.74    2006     8     1 menhaden 0     menhaden      0    
      273 amo        1.66    2006     9     1 menhaden 0     menhaden      0    
      274 amo        1.59    2006    10     1 menhaden 0     menhaden      0    
      275 amo        1.50    2006    11     1 menhaden 0     menhaden      0    
      276 amo        1.25    2006    12     1 menhaden 0     menhaden      0    
      277 amo        1.25    2007     1     1 menhaden 0     menhaden      0    
      278 amo        1.34    2007     2     1 menhaden 0     menhaden      0    
      279 amo        1.15    2007     3     1 menhaden 0     menhaden      0    
      280 amo        1.22    2007     4     1 menhaden 0     menhaden      0    
      281 amo        1.12    2007     5     1 menhaden 0     menhaden      0    
      282 amo        1.07    2007     6     1 menhaden 0     menhaden      0    
      283 amo        1.16    2007     7     1 menhaden 0     menhaden      0    
      284 amo        0.999   2007     8     1 menhaden 0     menhaden      0    
      285 amo        1.09    2007     9     1 menhaden 0     menhaden      0    
      286 amo        1.22    2007    10     1 menhaden 0     menhaden      0    
      287 amo        1.26    2007    11     1 menhaden 0     menhaden      0    
      288 amo        1.12    2007    12     1 menhaden 0     menhaden      0    
      289 amo        0.946   2008     1     1 menhaden 0     menhaden      0    
      290 amo        1.15    2008     2     1 menhaden 0     menhaden      0    
      291 amo        1.22    2008     3     1 menhaden 0     menhaden      0    
      292 amo        0.973   2008     4     1 menhaden 0     menhaden      0    
      293 amo        1.25    2008     5     1 menhaden 0     menhaden      0    
      294 amo        1.43    2008     6     1 menhaden 0     menhaden      0    
      295 amo        1.32    2008     7     1 menhaden 0     menhaden      0    
      296 amo        1.25    2008     8     1 menhaden 0     menhaden      0    
      297 amo        1.30    2008     9     1 menhaden 0     menhaden      0    
      298 amo        1.10    2008    10     1 menhaden 0     menhaden      0    
      299 amo        0.884   2008    11     1 menhaden 0     menhaden      0    
      300 amo        0.922   2008    12     1 menhaden 0     menhaden      0    
      301 amo        0.756   2009     1     1 menhaden 0     menhaden      0    
      302 amo        0.532   2009     2     1 menhaden 0     menhaden      0    
      303 amo        0.541   2009     3     1 menhaden 0     menhaden      0    
      304 amo        0.603   2009     4     1 menhaden 0     menhaden      0    
      305 amo        0.752   2009     5     1 menhaden 0     menhaden      0    
      306 amo        1.14    2009     6     1 menhaden 0     menhaden      0    
      307 amo        1.37    2009     7     1 menhaden 0     menhaden      0    
      308 amo        1.21    2009     8     1 menhaden 0     menhaden      0    
      309 amo        1.00    2009     9     1 menhaden 0     menhaden      0    
      310 amo        1.23    2009    10     1 menhaden 0     menhaden      0    
      311 amo        1.03    2009    11     1 menhaden 0     menhaden      0    
      312 amo        1.06    2009    12     1 menhaden 0     menhaden      0    
      313 amo        0.967   2010     1     1 menhaden 0     menhaden      0    
      314 amo        1.26    2010     2     1 menhaden 0     menhaden      0    
      315 amo        1.49    2010     3     1 menhaden 0     menhaden      0    
      316 amo        1.79    2010     4     1 menhaden 0     menhaden      0    
      317 amo        1.86    2010     5     1 menhaden 0     menhaden      0    
      318 amo        1.83    2010     6     1 menhaden 0     menhaden      0    
      319 amo        1.84    2010     7     1 menhaden 0     menhaden      0    
      320 amo        2       2010     8     1 menhaden 0     menhaden      0    
      321 amo        1.84    2010     9     1 menhaden 0     menhaden      0    
      322 amo        1.57    2010    10     1 menhaden 0     menhaden      0    
      323 amo        1.38    2010    11     1 menhaden 0     menhaden      0    
      324 amo        1.32    2010    12     1 menhaden 0     menhaden      0    
      325 amo        1.19    2011     1     1 menhaden 0     menhaden      0    
      326 amo        1.11    2011     2     1 menhaden 0     menhaden      0    
      327 amo        0.999   2011     3     1 menhaden 0     menhaden      0    
      328 amo        1.08    2011     4     1 menhaden 0     menhaden      0    
      329 amo        1.20    2011     5     1 menhaden 0     menhaden      0    
      330 amo        1.26    2011     6     1 menhaden 0     menhaden      0    
      331 amo        1.08    2011     7     1 menhaden 0     menhaden      0    
      332 amo        1.19    2011     8     1 menhaden 0     menhaden      0    
      333 amo        1.19    2011     9     1 menhaden 0     menhaden      0    
      334 amo        1.01    2011    10     1 menhaden 0     menhaden      0    
      335 amo        0.731   2011    11     1 menhaden 0     menhaden      0    
      336 amo        0.786   2011    12     1 menhaden 0     menhaden      0    
      337 amo        0.741   2012     1     1 menhaden 0     menhaden      0    
      338 amo        0.890   2012     2     1 menhaden 0     menhaden      0    
      339 amo        0.935   2012     3     1 menhaden 0     menhaden      0    
      340 amo        1.05    2012     4     1 menhaden 0     menhaden      0    
      341 amo        1.23    2012     5     1 menhaden 0     menhaden      0    
      342 amo        1.52    2012     6     1 menhaden 0     menhaden      0    
      343 amo        1.68    2012     7     1 menhaden 0     menhaden      0    
      344 amo        1.80    2012     8     1 menhaden 0     menhaden      0    
      345 amo        1.84    2012     9     1 menhaden 0     menhaden      0    
      346 amo        1.58    2012    10     1 menhaden 0     menhaden      0    
      347 amo        1.23    2012    11     1 menhaden 0     menhaden      0    
      348 amo        1.18    2012    12     1 menhaden 0     menhaden      0    
      349 amo        1.15    2013     1     1 menhaden 0     menhaden      0    
      350 amo        1.13    2013     2     1 menhaden 0     menhaden      0    
      351 amo        1.22    2013     3     1 menhaden 0     menhaden      0    
      352 amo        1.17    2013     4     1 menhaden 0     menhaden      0    
      353 amo        1.09    2013     5     1 menhaden 0     menhaden      0    
      354 amo        0.978   2013     6     1 menhaden 0     menhaden      0    
      355 amo        1.28    2013     7     1 menhaden 0     menhaden      0    
      356 amo        1.29    2013     8     1 menhaden 0     menhaden      0    
      357 amo        1.42    2013     9     1 menhaden 0     menhaden      0    
      358 amo        1.62    2013    10     1 menhaden 0     menhaden      0    
      359 amo        1.15    2013    11     1 menhaden 0     menhaden      0    
      360 amo        0.956   2013    12     1 menhaden 0     menhaden      0    
      361 amo        0.748   2014     1     1 menhaden 0     menhaden      0    
      362 amo        0.788   2014     2     1 menhaden 0     menhaden      0    
      363 amo        0.707   2014     3     1 menhaden 0     menhaden      0    
      364 amo        0.679   2014     4     1 menhaden 0     menhaden      0    
      365 amo        0.875   2014     5     1 menhaden 0     menhaden      0    
      366 amo        1.01    2014     6     1 menhaden 0     menhaden      0    
      367 amo        1.35    2014     7     1 menhaden 0     menhaden      0    
      368 amo        1.59    2014     8     1 menhaden 0     menhaden      0    
      369 amo        1.54    2014     9     1 menhaden 0     menhaden      0    
      370 amo        1.50    2014    10     1 menhaden 0     menhaden      0    
      371 amo        1.02    2014    11     1 menhaden 0     menhaden      0    
      372 amo        1.00    2014    12     1 menhaden 0     menhaden      0    
      373 amo        0.863   2015     1     1 menhaden 0     menhaden      0    
      374 amo        0.871   2015     2     1 menhaden 0     menhaden      0    
      375 amo        0.605   2015     3     1 menhaden 0     menhaden      0    
      376 amo        0.726   2015     4     1 menhaden 0     menhaden      0    
      377 amo        0.973   2015     5     1 menhaden 0     menhaden      0    
      378 amo        0.941   2015     6     1 menhaden 0     menhaden      0    
      379 amo        1.16    2015     7     1 menhaden 0     menhaden      0    
      380 amo        1.26    2015     8     1 menhaden 0     menhaden      0    
      381 amo        1.52    2015     9     1 menhaden 0     menhaden      0    
      382 amo        1.57    2015    10     1 menhaden 0     menhaden      0    
      383 amo        1.28    2015    11     1 menhaden 0     menhaden      0    
      384 amo        1.37    2015    12     1 menhaden 0     menhaden      0    
      385 amo        1.37    2016     1     1 menhaden 0     menhaden      0    
      386 amo        1.21    2016     2     1 menhaden 0     menhaden      0    
      387 amo        1.28    2016     3     1 menhaden 0     menhaden      0    
      388 amo        1.25    2016     4     1 menhaden 0     menhaden      0    
      389 amo        1.61    2016     5     1 menhaden 0     menhaden      0    
      390 amo        1.75    2016     6     1 menhaden 0     menhaden      0    
      391 amo        1.80    2016     7     1 menhaden 0     menhaden      0    
      392 amo        1.85    2016     8     1 menhaden 0     menhaden      0    
      393 amo        1.85    2016     9     1 menhaden 0     menhaden      0    
      394 amo        1.69    2016    10     1 menhaden 0     menhaden      0    
      395 amo        1.71    2016    11     1 menhaden 0     menhaden      0    
      396 amo        1.59    2016    12     1 menhaden 0     menhaden      0    
      397 pdsi       0.385   1985     1     0 phytoplankton  phytoplankton <NA> 
      398 pdsi       0.390   1985     2     0 phytoplankton  phytoplankton <NA> 
      399 pdsi       0.285   1985     3     0 phytoplankton  phytoplankton <NA> 
      400 pdsi       0.0832  1985     4     0 phytoplankton  phytoplankton <NA> 
      401 pdsi       0.681   1985     5     0 phytoplankton  phytoplankton <NA> 
      402 pdsi       0.732   1985     6     0 phytoplankton  phytoplankton <NA> 
      403 pdsi       1.03    1985     7     0 phytoplankton  phytoplankton <NA> 
      404 pdsi       1.25    1985     8     0 phytoplankton  phytoplankton <NA> 
      405 pdsi       1.24    1985     9     0 phytoplankton  phytoplankton <NA> 
      406 pdsi       1.17    1985    10     0 phytoplankton  phytoplankton <NA> 
      407 pdsi       1.43    1985    11     0 phytoplankton  phytoplankton <NA> 
      408 pdsi       0.744   1985    12     0 phytoplankton  phytoplankton <NA> 
      409 pdsi       0.798   1986     1     0 phytoplankton  phytoplankton <NA> 
      410 pdsi       0.828   1986     2     0 phytoplankton  phytoplankton <NA> 
      411 pdsi       0.703   1986     3     0 phytoplankton  phytoplankton <NA> 
      412 pdsi       0.530   1986     4     0 phytoplankton  phytoplankton <NA> 
      413 pdsi       0.386   1986     5     0 phytoplankton  phytoplankton <NA> 
      414 pdsi       0.684   1986     6     0 phytoplankton  phytoplankton <NA> 
      415 pdsi       0.772   1986     7     0 phytoplankton  phytoplankton <NA> 
      416 pdsi       0.862   1986     8     0 phytoplankton  phytoplankton <NA> 
      417 pdsi       0.645   1986     9     0 phytoplankton  phytoplankton <NA> 
      418 pdsi       0.582   1986    10     0 phytoplankton  phytoplankton <NA> 
      419 pdsi       0.782   1986    11     0 phytoplankton  phytoplankton <NA> 
      420 pdsi       1.16    1986    12     0 phytoplankton  phytoplankton <NA> 
      421 pdsi       1.31    1987     1     0 phytoplankton  phytoplankton <NA> 
      422 pdsi       0.982   1987     2     0 phytoplankton  phytoplankton <NA> 
      423 pdsi       0.983   1987     3     0 phytoplankton  phytoplankton <NA> 
      424 pdsi       1.31    1987     4     0 phytoplankton  phytoplankton <NA> 
      425 pdsi       0.711   1987     5     0 phytoplankton  phytoplankton <NA> 
      426 pdsi       0.636   1987     6     0 phytoplankton  phytoplankton <NA> 
      427 pdsi       0.455   1987     7     0 phytoplankton  phytoplankton <NA> 
      428 pdsi       0.352   1987     8     0 phytoplankton  phytoplankton <NA> 
      429 pdsi       0.654   1987     9     0 phytoplankton  phytoplankton <NA> 
      430 pdsi       0.678   1987    10     0 phytoplankton  phytoplankton <NA> 
      431 pdsi       0.631   1987    11     0 phytoplankton  phytoplankton <NA> 
      432 pdsi       0.592   1987    12     0 phytoplankton  phytoplankton <NA> 
      433 pdsi       0.574   1988     1     0 phytoplankton  phytoplankton <NA> 
      434 pdsi       0.678   1988     2     0 phytoplankton  phytoplankton <NA> 
      435 pdsi       0.595   1988     3     0 phytoplankton  phytoplankton <NA> 
      436 pdsi       0.545   1988     4     0 phytoplankton  phytoplankton <NA> 
      437 pdsi       0.607   1988     5     0 phytoplankton  phytoplankton <NA> 
      438 pdsi       0.453   1988     6     0 phytoplankton  phytoplankton <NA> 
      439 pdsi       0.916   1988     7     0 phytoplankton  phytoplankton <NA> 
      440 pdsi       0.643   1988     8     0 phytoplankton  phytoplankton <NA> 
      441 pdsi       0.571   1988     9     0 phytoplankton  phytoplankton <NA> 
      442 pdsi       0.547   1988    10     0 phytoplankton  phytoplankton <NA> 
      443 pdsi       0.909   1988    11     0 phytoplankton  phytoplankton <NA> 
      444 pdsi       0.571   1988    12     0 phytoplankton  phytoplankton <NA> 
      445 pdsi       0.455   1989     1     0 phytoplankton  phytoplankton <NA> 
      446 pdsi       0.709   1989     2     0 phytoplankton  phytoplankton <NA> 
      447 pdsi       0.747   1989     3     0 phytoplankton  phytoplankton <NA> 
      448 pdsi       1.04    1989     4     0 phytoplankton  phytoplankton <NA> 
      449 pdsi       1.28    1989     5     0 phytoplankton  phytoplankton <NA> 
      450 pdsi       1.46    1989     6     0 phytoplankton  phytoplankton <NA> 
      451 pdsi       1.59    1989     7     0 phytoplankton  phytoplankton <NA> 
      452 pdsi       1.66    1989     8     0 phytoplankton  phytoplankton <NA> 
      453 pdsi       1.72    1989     9     0 phytoplankton  phytoplankton <NA> 
      454 pdsi       1.91    1989    10     0 phytoplankton  phytoplankton <NA> 
      455 pdsi       1.87    1989    11     0 phytoplankton  phytoplankton <NA> 
      456 pdsi       1.48    1989    12     0 phytoplankton  phytoplankton <NA> 
      457 pdsi       1.48    1990     1     0 phytoplankton  phytoplankton <NA> 
      458 pdsi       1.41    1990     2     0 phytoplankton  phytoplankton <NA> 
      459 pdsi       1.22    1990     3     0 phytoplankton  phytoplankton <NA> 
      460 pdsi       1.29    1990     4     0 phytoplankton  phytoplankton <NA> 
      461 pdsi       1.54    1990     5     0 phytoplankton  phytoplankton <NA> 
      462 pdsi       1.21    1990     6     0 phytoplankton  phytoplankton <NA> 
      463 pdsi       1.18    1990     7     0 phytoplankton  phytoplankton <NA> 
      464 pdsi       1.31    1990     8     0 phytoplankton  phytoplankton <NA> 
      465 pdsi       1.16    1990     9     0 phytoplankton  phytoplankton <NA> 
      466 pdsi       1.41    1990    10     0 phytoplankton  phytoplankton <NA> 
      467 pdsi       0.972   1990    11     0 phytoplankton  phytoplankton <NA> 
      468 pdsi       1.09    1990    12     0 phytoplankton  phytoplankton <NA> 
      469 pdsi       0.951   1991     1     0 phytoplankton  phytoplankton <NA> 
      470 pdsi       0.724   1991     2     0 phytoplankton  phytoplankton <NA> 
      471 pdsi       0.871   1991     3     0 phytoplankton  phytoplankton <NA> 
      472 pdsi       0.737   1991     4     0 phytoplankton  phytoplankton <NA> 
      473 pdsi       0.629   1991     5     0 phytoplankton  phytoplankton <NA> 
      474 pdsi       0.501   1991     6     0 phytoplankton  phytoplankton <NA> 
      475 pdsi       0.452   1991     7     0 phytoplankton  phytoplankton <NA> 
      476 pdsi       0.818   1991     8     0 phytoplankton  phytoplankton <NA> 
      477 pdsi       0.928   1991     9     0 phytoplankton  phytoplankton <NA> 
      478 pdsi       0.707   1991    10     0 phytoplankton  phytoplankton <NA> 
      479 pdsi       0.667   1991    11     0 phytoplankton  phytoplankton <NA> 
      480 pdsi       0.717   1991    12     0 phytoplankton  phytoplankton <NA> 
      481 pdsi       0.651   1992     1     0 phytoplankton  phytoplankton <NA> 
      482 pdsi       0.592   1992     2     0 phytoplankton  phytoplankton <NA> 
      483 pdsi       0.651   1992     3     0 phytoplankton  phytoplankton <NA> 
      484 pdsi       0.598   1992     4     0 phytoplankton  phytoplankton <NA> 
      485 pdsi       0.571   1992     5     0 phytoplankton  phytoplankton <NA> 
      486 pdsi       0.922   1992     6     0 phytoplankton  phytoplankton <NA> 
      487 pdsi       1.11    1992     7     0 phytoplankton  phytoplankton <NA> 
      488 pdsi       1.26    1992     8     0 phytoplankton  phytoplankton <NA> 
      489 pdsi       1.29    1992     9     0 phytoplankton  phytoplankton <NA> 
      490 pdsi       1.20    1992    10     0 phytoplankton  phytoplankton <NA> 
      491 pdsi       1.30    1992    11     0 phytoplankton  phytoplankton <NA> 
      492 pdsi       1.42    1992    12     0 phytoplankton  phytoplankton <NA> 
      493 pdsi       1.31    1993     1     0 phytoplankton  phytoplankton <NA> 
      494 pdsi       1.28    1993     2     0 phytoplankton  phytoplankton <NA> 
      495 pdsi       1.55    1993     3     0 phytoplankton  phytoplankton <NA> 
      496 pdsi       1.59    1993     4     0 phytoplankton  phytoplankton <NA> 
      497 pdsi       0.740   1993     5     0 phytoplankton  phytoplankton <NA> 
      498 pdsi       0.631   1993     6     0 phytoplankton  phytoplankton <NA> 
      499 pdsi       0.481   1993     7     0 phytoplankton  phytoplankton <NA> 
      500 pdsi       0.321   1993     8     0 phytoplankton  phytoplankton <NA> 
      501 pdsi       0.737   1993     9     0 phytoplankton  phytoplankton <NA> 
      502 pdsi       0.743   1993    10     0 phytoplankton  phytoplankton <NA> 
      503 pdsi       0.859   1993    11     0 phytoplankton  phytoplankton <NA> 
      504 pdsi       1.10    1993    12     0 phytoplankton  phytoplankton <NA> 
      505 pdsi       1.22    1994     1     0 phytoplankton  phytoplankton <NA> 
      506 pdsi       1.23    1994     2     0 phytoplankton  phytoplankton <NA> 
      507 pdsi       1.50    1994     3     0 phytoplankton  phytoplankton <NA> 
      508 pdsi       0.772   1994     4     0 phytoplankton  phytoplankton <NA> 
      509 pdsi       0.775   1994     5     0 phytoplankton  phytoplankton <NA> 
      510 pdsi       0.687   1994     6     0 phytoplankton  phytoplankton <NA> 
      511 pdsi       0.790   1994     7     0 phytoplankton  phytoplankton <NA> 
      512 pdsi       0.968   1994     8     0 phytoplankton  phytoplankton <NA> 
      513 pdsi       0.953   1994     9     0 phytoplankton  phytoplankton <NA> 
      514 pdsi       0.690   1994    10     0 phytoplankton  phytoplankton <NA> 
      515 pdsi       0.665   1994    11     0 phytoplankton  phytoplankton <NA> 
      516 pdsi       0.662   1994    12     0 phytoplankton  phytoplankton <NA> 
      517 pdsi       0.736   1995     1     0 phytoplankton  phytoplankton <NA> 
      518 pdsi       0.697   1995     2     0 phytoplankton  phytoplankton <NA> 
      519 pdsi       0.551   1995     3     0 phytoplankton  phytoplankton <NA> 
      520 pdsi       0.459   1995     4     0 phytoplankton  phytoplankton <NA> 
      521 pdsi       0.599   1995     5     0 phytoplankton  phytoplankton <NA> 
      522 pdsi       0.645   1995     6     0 phytoplankton  phytoplankton <NA> 
      523 pdsi       0.475   1995     7     0 phytoplankton  phytoplankton <NA> 
      524 pdsi       0.300   1995     8     0 phytoplankton  phytoplankton <NA> 
      525 pdsi       0.301   1995     9     0 phytoplankton  phytoplankton <NA> 
      526 pdsi       1.18    1995    10     0 phytoplankton  phytoplankton <NA> 
      527 pdsi       1.29    1995    11     0 phytoplankton  phytoplankton <NA> 
      528 pdsi       1.16    1995    12     0 phytoplankton  phytoplankton <NA> 
      529 pdsi       1.39    1996     1     0 phytoplankton  phytoplankton <NA> 
      530 pdsi       1.31    1996     2     0 phytoplankton  phytoplankton <NA> 
      531 pdsi       1.24    1996     3     0 phytoplankton  phytoplankton <NA> 
      532 pdsi       1.34    1996     4     0 phytoplankton  phytoplankton <NA> 
      533 pdsi       1.36    1996     5     0 phytoplankton  phytoplankton <NA> 
      534 pdsi       1.31    1996     6     0 phytoplankton  phytoplankton <NA> 
      535 pdsi       1.54    1996     7     0 phytoplankton  phytoplankton <NA> 
      536 pdsi       1.47    1996     8     0 phytoplankton  phytoplankton <NA> 
      537 pdsi       1.69    1996     9     0 phytoplankton  phytoplankton <NA> 
      538 pdsi       1.90    1996    10     0 phytoplankton  phytoplankton <NA> 
      539 pdsi       1.83    1996    11     0 phytoplankton  phytoplankton <NA> 
      540 pdsi       1.98    1996    12     0 phytoplankton  phytoplankton <NA> 
      541 pdsi       1.21    1997     1     0 phytoplankton  phytoplankton <NA> 
      542 pdsi       0.791   1997     2     0 phytoplankton  phytoplankton <NA> 
      543 pdsi       0.908   1997     3     0 phytoplankton  phytoplankton <NA> 
      544 pdsi       0.953   1997     4     0 phytoplankton  phytoplankton <NA> 
      545 pdsi       0.868   1997     5     0 phytoplankton  phytoplankton <NA> 
      546 pdsi       0.731   1997     6     0 phytoplankton  phytoplankton <NA> 
      547 pdsi       0.672   1997     7     0 phytoplankton  phytoplankton <NA> 
      548 pdsi       0.709   1997     8     0 phytoplankton  phytoplankton <NA> 
      549 pdsi       0.589   1997     9     0 phytoplankton  phytoplankton <NA> 
      550 pdsi       0.494   1997    10     0 phytoplankton  phytoplankton <NA> 
      551 pdsi       1.05    1997    11     0 phytoplankton  phytoplankton <NA> 
      552 pdsi       0.993   1997    12     0 phytoplankton  phytoplankton <NA> 
      553 pdsi       1.20    1998     1     0 phytoplankton  phytoplankton <NA> 
      554 pdsi       1.38    1998     2     0 phytoplankton  phytoplankton <NA> 
      555 pdsi       1.46    1998     3     0 phytoplankton  phytoplankton <NA> 
      556 pdsi       1.45    1998     4     0 phytoplankton  phytoplankton <NA> 
      557 pdsi       1.48    1998     5     0 phytoplankton  phytoplankton <NA> 
      558 pdsi       1.75    1998     6     0 phytoplankton  phytoplankton <NA> 
      559 pdsi       0.828   1998     7     0 phytoplankton  phytoplankton <NA> 
      560 pdsi       0.707   1998     8     0 phytoplankton  phytoplankton <NA> 
      561 pdsi       0.556   1998     9     0 phytoplankton  phytoplankton <NA> 
      562 pdsi       0.547   1998    10     0 phytoplankton  phytoplankton <NA> 
      563 pdsi       0.360   1998    11     0 phytoplankton  phytoplankton <NA> 
      564 pdsi       0.207   1998    12     0 phytoplankton  phytoplankton <NA> 
      565 pdsi       0.725   1999     1     0 phytoplankton  phytoplankton <NA> 
      566 pdsi       0.768   1999     2     0 phytoplankton  phytoplankton <NA> 
      567 pdsi       0.840   1999     3     0 phytoplankton  phytoplankton <NA> 
      568 pdsi       0.469   1999     4     0 phytoplankton  phytoplankton <NA> 
      569 pdsi       0.442   1999     5     0 phytoplankton  phytoplankton <NA> 
      570 pdsi       0.255   1999     6     0 phytoplankton  phytoplankton <NA> 
      571 pdsi       0.127   1999     7     0 phytoplankton  phytoplankton <NA> 
      572 pdsi       0.0670  1999     8     0 phytoplankton  phytoplankton <NA> 
      573 pdsi       1.33    1999     9     0 phytoplankton  phytoplankton <NA> 
      574 pdsi       1.40    1999    10     0 phytoplankton  phytoplankton <NA> 
      575 pdsi       1.08    1999    11     0 phytoplankton  phytoplankton <NA> 
      576 pdsi       0.974   1999    12     0 phytoplankton  phytoplankton <NA> 
      577 pdsi       0.967   2000     1     0 phytoplankton  phytoplankton <NA> 
      578 pdsi       0.937   2000     2     0 phytoplankton  phytoplankton <NA> 
      579 pdsi       0.925   2000     3     0 phytoplankton  phytoplankton <NA> 
      580 pdsi       1.12    2000     4     0 phytoplankton  phytoplankton <NA> 
      581 pdsi       1.07    2000     5     0 phytoplankton  phytoplankton <NA> 
      582 pdsi       1.22    2000     6     0 phytoplankton  phytoplankton <NA> 
      583 pdsi       1.40    2000     7     0 phytoplankton  phytoplankton <NA> 
      584 pdsi       1.39    2000     8     0 phytoplankton  phytoplankton <NA> 
      585 pdsi       1.44    2000     9     0 phytoplankton  phytoplankton <NA> 
      586 pdsi       0.756   2000    10     0 phytoplankton  phytoplankton <NA> 
      587 pdsi       0.716   2000    11     0 phytoplankton  phytoplankton <NA> 
      588 pdsi       0.747   2000    12     0 phytoplankton  phytoplankton <NA> 
      589 pdsi       0.676   2001     1     0 phytoplankton  phytoplankton <NA> 
      590 pdsi       0.614   2001     2     0 phytoplankton  phytoplankton <NA> 
      591 pdsi       1.07    2001     3     0 phytoplankton  phytoplankton <NA> 
      592 pdsi       0.707   2001     4     0 phytoplankton  phytoplankton <NA> 
      593 pdsi       0.887   2001     5     0 phytoplankton  phytoplankton <NA> 
      594 pdsi       1.09    2001     6     0 phytoplankton  phytoplankton <NA> 
      595 pdsi       1.12    2001     7     0 phytoplankton  phytoplankton <NA> 
      596 pdsi       1.00    2001     8     0 phytoplankton  phytoplankton <NA> 
      597 pdsi       0.839   2001     9     0 phytoplankton  phytoplankton <NA> 
      598 pdsi       0.636   2001    10     0 phytoplankton  phytoplankton <NA> 
      599 pdsi       0.339   2001    11     0 phytoplankton  phytoplankton <NA> 
      600 pdsi       0.184   2001    12     0 phytoplankton  phytoplankton <NA> 
      601 pdsi       0.122   2002     1     0 phytoplankton  phytoplankton <NA> 
      602 pdsi       0       2002     2     0 phytoplankton  phytoplankton <NA> 
      603 pdsi       0.110   2002     3     0 phytoplankton  phytoplankton <NA> 
      604 pdsi       0.136   2002     4     0 phytoplankton  phytoplankton <NA> 
      605 pdsi       0.447   2002     5     0 phytoplankton  phytoplankton <NA> 
      606 pdsi       0.5     2002     6     0 phytoplankton  phytoplankton <NA> 
      607 pdsi       0.406   2002     7     0 phytoplankton  phytoplankton <NA> 
      608 pdsi       0.304   2002     8     0 phytoplankton  phytoplankton <NA> 
      609 pdsi       0.455   2002     9     0 phytoplankton  phytoplankton <NA> 
      610 pdsi       1.05    2002    10     0 phytoplankton  phytoplankton <NA> 
      611 pdsi       1.19    2002    11     0 phytoplankton  phytoplankton <NA> 
      612 pdsi       1.28    2002    12     0 phytoplankton  phytoplankton <NA> 
      613 pdsi       1.10    2003     1     0 phytoplankton  phytoplankton <NA> 
      614 pdsi       1.29    2003     2     0 phytoplankton  phytoplankton <NA> 
      615 pdsi       1.29    2003     3     0 phytoplankton  phytoplankton <NA> 
      616 pdsi       1.29    2003     4     0 phytoplankton  phytoplankton <NA> 
      617 pdsi       1.40    2003     5     0 phytoplankton  phytoplankton <NA> 
      618 pdsi       1.63    2003     6     0 phytoplankton  phytoplankton <NA> 
      619 pdsi       1.61    2003     7     0 phytoplankton  phytoplankton <NA> 
      620 pdsi       1.64    2003     8     0 phytoplankton  phytoplankton <NA> 
      621 pdsi       1.82    2003     9     0 phytoplankton  phytoplankton <NA> 
      622 pdsi       1.92    2003    10     0 phytoplankton  phytoplankton <NA> 
      623 pdsi       1.54    2003    11     0 phytoplankton  phytoplankton <NA> 
      624 pdsi       1.63    2003    12     0 phytoplankton  phytoplankton <NA> 
      625 pdsi       1.28    2004     1     0 phytoplankton  phytoplankton <NA> 
      626 pdsi       1.14    2004     2     0 phytoplankton  phytoplankton <NA> 
      627 pdsi       0.977   2004     3     0 phytoplankton  phytoplankton <NA> 
      628 pdsi       1.40    2004     4     0 phytoplankton  phytoplankton <NA> 
      629 pdsi       1.13    2004     5     0 phytoplankton  phytoplankton <NA> 
      630 pdsi       1.09    2004     6     0 phytoplankton  phytoplankton <NA> 
      631 pdsi       1.21    2004     7     0 phytoplankton  phytoplankton <NA> 
      632 pdsi       1.37    2004     8     0 phytoplankton  phytoplankton <NA> 
      633 pdsi       1.61    2004     9     0 phytoplankton  phytoplankton <NA> 
      634 pdsi       1.39    2004    10     0 phytoplankton  phytoplankton <NA> 
      635 pdsi       1.41    2004    11     0 phytoplankton  phytoplankton <NA> 
      636 pdsi       1.21    2004    12     0 phytoplankton  phytoplankton <NA> 
      637 pdsi       1.25    2005     1     0 phytoplankton  phytoplankton <NA> 
      638 pdsi       1.00    2005     2     0 phytoplankton  phytoplankton <NA> 
      639 pdsi       1.09    2005     3     0 phytoplankton  phytoplankton <NA> 
      640 pdsi       1.11    2005     4     0 phytoplankton  phytoplankton <NA> 
      641 pdsi       1.09    2005     5     0 phytoplankton  phytoplankton <NA> 
      642 pdsi       0.756   2005     6     0 phytoplankton  phytoplankton <NA> 
      643 pdsi       0.746   2005     7     0 phytoplankton  phytoplankton <NA> 
      644 pdsi       0.607   2005     8     0 phytoplankton  phytoplankton <NA> 
      645 pdsi       0.427   2005     9     0 phytoplankton  phytoplankton <NA> 
      646 pdsi       1.45    2005    10     0 phytoplankton  phytoplankton <NA> 
      647 pdsi       1.45    2005    11     0 phytoplankton  phytoplankton <NA> 
      648 pdsi       1.43    2005    12     0 phytoplankton  phytoplankton <NA> 
      649 pdsi       1.40    2006     1     0 phytoplankton  phytoplankton <NA> 
      650 pdsi       1.24    2006     2     0 phytoplankton  phytoplankton <NA> 
      651 pdsi       0.897   2006     3     0 phytoplankton  phytoplankton <NA> 
      652 pdsi       0.830   2006     4     0 phytoplankton  phytoplankton <NA> 
      653 pdsi       0.981   2006     5     0 phytoplankton  phytoplankton <NA> 
      654 pdsi       1.69    2006     6     0 phytoplankton  phytoplankton <NA> 
      655 pdsi       1.48    2006     7     0 phytoplankton  phytoplankton <NA> 
      656 pdsi       1.38    2006     8     0 phytoplankton  phytoplankton <NA> 
      657 pdsi       1.47    2006     9     0 phytoplankton  phytoplankton <NA> 
      658 pdsi       1.68    2006    10     0 phytoplankton  phytoplankton <NA> 
      659 pdsi       1.78    2006    11     0 phytoplankton  phytoplankton <NA> 
      660 pdsi       1.26    2006    12     0 phytoplankton  phytoplankton <NA> 
      661 pdsi       1.20    2007     1     0 phytoplankton  phytoplankton <NA> 
      662 pdsi       1.08    2007     2     0 phytoplankton  phytoplankton <NA> 
      663 pdsi       1.12    2007     3     0 phytoplankton  phytoplankton <NA> 
      664 pdsi       1.36    2007     4     0 phytoplankton  phytoplankton <NA> 
      665 pdsi       0.712   2007     5     0 phytoplankton  phytoplankton <NA> 
      666 pdsi       0.672   2007     6     0 phytoplankton  phytoplankton <NA> 
      667 pdsi       0.658   2007     7     0 phytoplankton  phytoplankton <NA> 
      668 pdsi       0.504   2007     8     0 phytoplankton  phytoplankton <NA> 
      669 pdsi       0.337   2007     9     0 phytoplankton  phytoplankton <NA> 
      670 pdsi       0.321   2007    10     0 phytoplankton  phytoplankton <NA> 
      671 pdsi       0.212   2007    11     0 phytoplankton  phytoplankton <NA> 
      672 pdsi       0.397   2007    12     0 phytoplankton  phytoplankton <NA> 
      673 pdsi       0.323   2008     1     0 phytoplankton  phytoplankton <NA> 
      674 pdsi       0.886   2008     2     0 phytoplankton  phytoplankton <NA> 
      675 pdsi       0.932   2008     3     0 phytoplankton  phytoplankton <NA> 
      676 pdsi       0.966   2008     4     0 phytoplankton  phytoplankton <NA> 
      677 pdsi       0.970   2008     5     0 phytoplankton  phytoplankton <NA> 
      678 pdsi       0.955   2008     6     0 phytoplankton  phytoplankton <NA> 
      679 pdsi       1.06    2008     7     0 phytoplankton  phytoplankton <NA> 
      680 pdsi       1.01    2008     8     0 phytoplankton  phytoplankton <NA> 
      681 pdsi       1.25    2008     9     0 phytoplankton  phytoplankton <NA> 
      682 pdsi       1.14    2008    10     0 phytoplankton  phytoplankton <NA> 
      683 pdsi       1.09    2008    11     0 phytoplankton  phytoplankton <NA> 
      684 pdsi       1.27    2008    12     0 phytoplankton  phytoplankton <NA> 
      685 pdsi       1.07    2009     1     0 phytoplankton  phytoplankton <NA> 
      686 pdsi       0.852   2009     2     0 phytoplankton  phytoplankton <NA> 
      687 pdsi       0.879   2009     3     0 phytoplankton  phytoplankton <NA> 
      688 pdsi       1.00    2009     4     0 phytoplankton  phytoplankton <NA> 
      689 pdsi       1.06    2009     5     0 phytoplankton  phytoplankton <NA> 
      690 pdsi       1.30    2009     6     0 phytoplankton  phytoplankton <NA> 
      691 pdsi       1.60    2009     7     0 phytoplankton  phytoplankton <NA> 
      692 pdsi       1.57    2009     8     0 phytoplankton  phytoplankton <NA> 
      693 pdsi       1.46    2009     9     0 phytoplankton  phytoplankton <NA> 
      694 pdsi       1.61    2009    10     0 phytoplankton  phytoplankton <NA> 
      695 pdsi       1.59    2009    11     0 phytoplankton  phytoplankton <NA> 
      696 pdsi       1.76    2009    12     0 phytoplankton  phytoplankton <NA> 
      697 pdsi       1.67    2010     1     0 phytoplankton  phytoplankton <NA> 
      698 pdsi       1.69    2010     2     0 phytoplankton  phytoplankton <NA> 
      699 pdsi       2       2010     3     0 phytoplankton  phytoplankton <NA> 
      700 pdsi       0.942   2010     4     0 phytoplankton  phytoplankton <NA> 
      701 pdsi       0.828   2010     5     0 phytoplankton  phytoplankton <NA> 
      702 pdsi       0.716   2010     6     0 phytoplankton  phytoplankton <NA> 
      703 pdsi       0.599   2010     7     0 phytoplankton  phytoplankton <NA> 
      704 pdsi       0.547   2010     8     0 phytoplankton  phytoplankton <NA> 
      705 pdsi       0.524   2010     9     0 phytoplankton  phytoplankton <NA> 
      706 pdsi       0.880   2010    10     0 phytoplankton  phytoplankton <NA> 
      707 pdsi       0.792   2010    11     0 phytoplankton  phytoplankton <NA> 
      708 pdsi       0.828   2010    12     0 phytoplankton  phytoplankton <NA> 
      709 pdsi       0.787   2011     1     0 phytoplankton  phytoplankton <NA> 
      710 pdsi       0.811   2011     2     0 phytoplankton  phytoplankton <NA> 
      711 pdsi       0.983   2011     3     0 phytoplankton  phytoplankton <NA> 
      712 pdsi       1.07    2011     4     0 phytoplankton  phytoplankton <NA> 
      713 pdsi       1.03    2011     5     0 phytoplankton  phytoplankton <NA> 
      714 pdsi       1.06    2011     6     0 phytoplankton  phytoplankton <NA> 
      715 pdsi       0.927   2011     7     0 phytoplankton  phytoplankton <NA> 
      716 pdsi       1.37    2011     8     0 phytoplankton  phytoplankton <NA> 
      717 pdsi       1.61    2011     9     0 phytoplankton  phytoplankton <NA> 
      718 pdsi       1.76    2011    10     0 phytoplankton  phytoplankton <NA> 
      719 pdsi       1.13    2011    11     0 phytoplankton  phytoplankton <NA> 
      720 pdsi       1.13    2011    12     0 phytoplankton  phytoplankton <NA> 
      721 pdsi       0.833   2012     1     0 phytoplankton  phytoplankton <NA> 
      722 pdsi       0.651   2012     2     0 phytoplankton  phytoplankton <NA> 
      723 pdsi       0.424   2012     3     0 phytoplankton  phytoplankton <NA> 
      724 pdsi       0.365   2012     4     0 phytoplankton  phytoplankton <NA> 
      725 pdsi       0.514   2012     5     0 phytoplankton  phytoplankton <NA> 
      726 pdsi       0.554   2012     6     0 phytoplankton  phytoplankton <NA> 
      727 pdsi       0.543   2012     7     0 phytoplankton  phytoplankton <NA> 
      728 pdsi       0.585   2012     8     0 phytoplankton  phytoplankton <NA> 
      729 pdsi       0.673   2012     9     0 phytoplankton  phytoplankton <NA> 
      730 pdsi       0.932   2012    10     0 phytoplankton  phytoplankton <NA> 
      731 pdsi       0.617   2012    11     0 phytoplankton  phytoplankton <NA> 
      732 pdsi       0.713   2012    12     0 phytoplankton  phytoplankton <NA> 
      733 pdsi       0.756   2013     1     0 phytoplankton  phytoplankton <NA> 
      734 pdsi       0.805   2013     2     0 phytoplankton  phytoplankton <NA> 
      735 pdsi       0.747   2013     3     0 phytoplankton  phytoplankton <NA> 
      736 pdsi       0.627   2013     4     0 phytoplankton  phytoplankton <NA> 
      737 pdsi       0.868   2013     5     0 phytoplankton  phytoplankton <NA> 
      738 pdsi       1.44    2013     6     0 phytoplankton  phytoplankton <NA> 
      739 pdsi       1.12    2013     7     0 phytoplankton  phytoplankton <NA> 
      740 pdsi       1.11    2013     8     0 phytoplankton  phytoplankton <NA> 
      741 pdsi       1.01    2013     9     0 phytoplankton  phytoplankton <NA> 
      742 pdsi       0.896   2013    10     0 phytoplankton  phytoplankton <NA> 
      743 pdsi       0.860   2013    11     0 phytoplankton  phytoplankton <NA> 
      744 pdsi       1.01    2013    12     0 phytoplankton  phytoplankton <NA> 
      745 pdsi       0.960   2014     1     0 phytoplankton  phytoplankton <NA> 
      746 pdsi       1.11    2014     2     0 phytoplankton  phytoplankton <NA> 
      747 pdsi       1.15    2014     3     0 phytoplankton  phytoplankton <NA> 
      748 pdsi       1.19    2014     4     0 phytoplankton  phytoplankton <NA> 
      749 pdsi       1.13    2014     5     0 phytoplankton  phytoplankton <NA> 
      750 pdsi       0.835   2014     6     0 phytoplankton  phytoplankton <NA> 
      751 pdsi       0.921   2014     7     0 phytoplankton  phytoplankton <NA> 
      752 pdsi       0.840   2014     8     0 phytoplankton  phytoplankton <NA> 
      753 pdsi       0.690   2014     9     0 phytoplankton  phytoplankton <NA> 
      754 pdsi       0.868   2014    10     0 phytoplankton  phytoplankton <NA> 
      755 pdsi       0.896   2014    11     0 phytoplankton  phytoplankton <NA> 
      756 pdsi       0.976   2014    12     0 phytoplankton  phytoplankton <NA> 
      757 pdsi       0.942   2015     1     0 phytoplankton  phytoplankton <NA> 
      758 pdsi       0.811   2015     2     0 phytoplankton  phytoplankton <NA> 
      759 pdsi       0.816   2015     3     0 phytoplankton  phytoplankton <NA> 
      760 pdsi       0.767   2015     4     0 phytoplankton  phytoplankton <NA> 
      761 pdsi       0.505   2015     5     0 phytoplankton  phytoplankton <NA> 
      762 pdsi       0.886   2015     6     0 phytoplankton  phytoplankton <NA> 
      763 pdsi       0.785   2015     7     0 phytoplankton  phytoplankton <NA> 
      764 pdsi       0.664   2015     8     0 phytoplankton  phytoplankton <NA> 
      765 pdsi       0.682   2015     9     0 phytoplankton  phytoplankton <NA> 
      766 pdsi       0.733   2015    10     0 phytoplankton  phytoplankton <NA> 
      767 pdsi       0.632   2015    11     0 phytoplankton  phytoplankton <NA> 
      768 pdsi       0.723   2015    12     0 phytoplankton  phytoplankton <NA> 
      769 pdsi       0.681   2016     1     0 phytoplankton  phytoplankton <NA> 
      770 pdsi       0.874   2016     2     0 phytoplankton  phytoplankton <NA> 
      771 pdsi       0.645   2016     3     0 phytoplankton  phytoplankton <NA> 
      772 pdsi       0.578   2016     4     0 phytoplankton  phytoplankton <NA> 
      773 pdsi       0.744   2016     5     0 phytoplankton  phytoplankton <NA> 
      774 pdsi       0.691   2016     6     0 phytoplankton  phytoplankton <NA> 
      775 pdsi       0.642   2016     7     0 phytoplankton  phytoplankton <NA> 
      776 pdsi       0.432   2016     8     0 phytoplankton  phytoplankton <NA> 
      777 pdsi       0.428   2016     9     0 phytoplankton  phytoplankton <NA> 
      778 pdsi       0.571   2016    10     0 phytoplankton  phytoplankton <NA> 
      779 pdsi       0.409   2016    11     0 phytoplankton  phytoplankton <NA> 
      780 pdsi       0.425   2016    12     0 phytoplankton  phytoplankton <NA> 
      781 pdsi       0.500   2017     1     0 phytoplankton  phytoplankton <NA> 
      782 pdsi       0.365   2017     2     0 phytoplankton  phytoplankton <NA> 
      783 pdsi       0.386   2017     3     0 phytoplankton  phytoplankton <NA> 
      784 pdsi       0.711   2017     4     0 phytoplankton  phytoplankton <NA> 
      785 pdsi       1.04    2017     5     0 phytoplankton  phytoplankton <NA> 
      786 pdsi       0.895   2017     6     0 phytoplankton  phytoplankton <NA> 
      787 pdsi       0.993   2017     7     0 phytoplankton  phytoplankton <NA> 
      788 pdsi       0.888   2017     8     0 phytoplankton  phytoplankton <NA> 
      789 pdsi       0.708   2017     9     0 phytoplankton  phytoplankton <NA> 
      790 pdsi       0.922   2017    10     0 phytoplankton  phytoplankton <NA> 
      791 pdsi       0.723   2017    11     0 phytoplankton  phytoplankton <NA> 
      792 pdsi       0.601   2017    12     0 phytoplankton  phytoplankton <NA> 

