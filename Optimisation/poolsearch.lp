\ Model IAA Media
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  0.5 x[0] + 2 x[1] + 0.2 x[2] + x[3] + 0.8 x[4] + 0.7 x[5] + 0.6 x[6]
   + 0.75 x[7]
Subject To
 SpotLimit[0]: x[0] <= 10
 SpotLimit[1]: x[1] <= 10
 SpotLimit[2]: x[2] <= 10
 SpotLimit[3]: x[3] <= 10
 SpotLimit[4]: x[4] <= 10
 SpotLimit[5]: x[5] <= 10
 SpotLimit[6]: x[6] <= 10
 SpotLimit[7]: x[7] <= 10
 MaxCost: 300 x[0] + 200 x[1] + 500 x[2] + 600 x[3] + 200 x[4] + 210 x[5]
   + 140 x[6] + 250 x[7] <= 7000
Bounds
Generals
 x[0] x[1] x[2] x[3] x[4] x[5] x[6] x[7]
End
