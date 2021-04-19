;
; March 2021
; Wageningen UR, Laboratory of Geo-Information Science and Remote Sensing
; Judith Verstegen: judith.verstegen@wur.nl
; For demonstration and education purposes only
;;============================================


extensions [gis]

globals
[
  study-area-patches
]

;; declaring patch attributes
;; added the attribute utility, as the paper uses this as an attribute that that the agent owns (for a specific patch)
patches-own
[
  elevation
  distance-to-service-center
  distance-quality
  aesthetic-quality
  social-similarity
  utility
  percentage-built-up-surroundings
]

;;declaring agent types and their attibutes
breed [ service-centers service-center ]
breed [ residents resident ]


;; declaring what attributes the residents own
residents-own
[
  weight-distance
  weight-aesthetics
  weight-social-similarity
]



;;model setup
to setup
  ;; clear from previous run
  clear-all
  reset-ticks

  ;; load input data elevation
  loaddata "data/defaultdem_.asc"
  set study-area-patches patches with [ elevation > -9999 ]
  ;; use elevation as color for patches
  let minimum min [elevation] of study-area-patches
  let maximum max [elevation] of study-area-patches
  ask study-area-patches
  [
    set pcolor scale-color green elevation minimum maximum
  ]

  set-default-shape residents "house"

  ;;create the first service center
  set-default-shape service-centers "x"
  create-service-centers 1
  [
    setxy 310 -94
    set color white
    set size 3
  ]

  ;; compute distance to it for all patches
  compute-distance-quality

  ;; aesthetic quality
  compute-aesthetic-quality

end

;;model run
to go

  ;; add resident (nr according to slider in GUI)
  add-new-residents

  ;; check stop condition and export the result(s) when reached
  let developed (count patches with [count turtles-here > 0] / count study-area-patches)
  if developed > (stop-at-percent-developed / 100)
  [
    store-output-rasters
    stop
  ]
  ;; next time step
  tick
end


;; FUNCTIONS

;; OBSERVER

to loaddata [filename] ;;observer procedure
  clear-patches

  show "loading elevation from file..."
  ;; load data from file
  let ahn gis:load-dataset filename

  ;; put everything (elevation en roughness on the netlogo
  ;; world. You need to enter the right extent of the world
  ;; beforehand through the gui as netlogo does not allow
  ;; you to do by scripting
  ;;----------------------------------------------------------------
  gis:set-world-envelope (gis:raster-world-envelope ahn 0 0)
  gis:apply-raster ahn elevation
  show "    done loading data"

end


to store-output-rasters ;;observer procedure

  ;; compute the output variable: percent of neighborhood developed
  ask patches
  [
    set percentage-built-up-surroundings ((count residents-on neighbors + count residents-here) / 9) * 100
  ]
  ;; set output variable to NoData value outside the study area
  ask patches
  [
    if not member? self study-area-patches
    [
      set percentage-built-up-surroundings -9999
    ]
  ]

  ;; in NetLogo 6, there is a problem with the context of gis:patch-dataset
  ;; see https://github.com/NetLogo/GIS-Extension/issues/14
  ;; the "one-of" is a workaround proposed there
  let patches_out nobody
  ask one-of patches
  [
    set patches_out gis:patch-dataset percentage-built-up-surroundings
  ]
    gis:store-dataset patches_out word word "perc_built_up_" stop-at-percent-developed ".asc"

end


to compute-distance-quality
  ;; find nearest service center and define distance-to-service-center as this distance
  ask study-area-patches
  [
    set distance-to-service-center distance min-one-of service-centers [ distance myself ]
  ]

  ;; create local variable longest-distance to get the longest distance to a service center among all patches
  let longest-distance [ distance-to-service-center ] of max-one-of study-area-patches [ distance-to-service-center ]

  ;; scale distance-to-service-center according to longest-distance and inverse to get a range from 0-1 (0 is bad, 1 is good) and set as distance-quality
  ask study-area-patches
  [
    set distance-quality 1 - ( distance-to-service-center / longest-distance )
  ]

end


to compute-aesthetic-quality
  ;; create local variables highest-elevation and lowest-elevation to get the extreme elevation values among all patches
  let highest-elevation [ elevation ] of max-one-of study-area-patches [ elevation ]
  let lowest-elevation [ elevation ] of min-one-of study-area-patches [ elevation ]


  ;; scale elevation according to extreme elevation values to get a range from 0-1 (0 is bad, 1 is good) and set as aesthetic-quality
  ask study-area-patches
  [
    set aesthetic-quality ( elevation - lowest-elevation ) / ( highest-elevation - lowest-elevation )
  ]
end


to add-new-residents

  repeat new-residents-per-tick
  [
  create-residents 1
    [
      set color red

      set weight-distance mean-weight-distance
      set weight-aesthetics mean-weight-aesthetics
      set weight-social-similarity mean-weight-social-similarity

      ;; allocate
      let my-selection n-of 15 study-area-patches with [ count turtles-here = 0 ]

      ;; compute social similarity
      ask my-selection
      [
        ifelse (count residents-on neighbors + count residents-here) = 0
        [ set social-similarity 0.5 ]
        [
          let n count residents-on neighbors
          let friends residents in-radius 1.5
          let similarity-part (social-similarity-diff [weight-distance] of myself [weight-distance] of friends [weight-aesthetics] of myself [weight-aesthetics] of friends [weight-social-similarity] of myself [weight-social-similarity] of friends)
          ifelse similarity-part = 0
          [ set social-similarity 1 ]
          [ set social-similarity (n / similarity-part) ]

          ;; scale social similarity
          let highest-similarity [ social-similarity ] of max-one-of my-selection [ social-similarity ]
          let lowest-similarity [ social-similarity ] of min-one-of my-selection [ social-similarity ]

          ask my-selection
          [
            set social-similarity ( social-similarity - lowest-similarity ) / ( highest-similarity - lowest-similarity )
          ]
        ]
      ]

      ;; utility functie oproepen, beste utility berekenen en dan beste kiezen voor de selectie
      ;; utility function based on formule 1 (Brown and Robinson, 2006)
      ask my-selection [set utility (distance-quality ^ [weight-distance] of myself) + (aesthetic-quality ^ [weight-aesthetics] of myself) + (social-similarity ^ [weight-social-similarity] of myself)]
      let best-patch max-one-of my-selection [utility]

      setxy [pxcor] of best-patch [pycor] of best-patch

      ;; new service centre for every 1000th resident
      if (count residents) mod 1000 = 0 and ticks != 0
      [
        hatch-service-centers 1
        [
          let close-patch min-one-of study-area-patches with [ count turtles-here = 0 ] [ distance best-patch ]
          setxy [pxcor] of close-patch [pycor] of close-patch
          set color white
          set size 3
        ]

        compute-distance-quality
      ]
    ]
  ]

end

;; neighbourhood similarity function based on formule 2 (Brown and Robinson, 2006)
to-report social-similarity-diff [#dist-eval #dist-neighbors #aesth-eval #aesth-neighbors #social-eval #social-neighbors]
  report reduce + (map [ [dist-list aesth-list social-list] -> ( sqrt ( (dist-list - #dist-eval) ^ 2 + (aesth-list - #aesth-eval) ^ 2 + (social-list - #social-eval) ^ 2) ) ] #dist-neighbors #aesth-neighbors #social-neighbors)
end
@#$#@#$#@
GRAPHICS-WINDOW
234
10
1119
449
-1
-1
2.5
1
10
1
1
1
0
0
0
1
0
350
-171
0
0
0
1
ticks
30.0

SLIDER
16
65
200
98
new-residents-per-tick
new-residents-per-tick
50
200
200.0
10
1
NIL
HORIZONTAL

BUTTON
15
20
78
53
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
104
20
167
53
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
17
105
201
138
stop-at-percent-developed
stop-at-percent-developed
0
90
90.0
1
1
%
HORIZONTAL

SLIDER
16
156
223
189
mean-weight-distance
mean-weight-distance
0
1
0.76
0.01
1
NIL
HORIZONTAL

SLIDER
16
194
223
227
mean-weight-aesthetics
mean-weight-aesthetics
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
15
233
223
266
mean-weight-social-similarity
mean-weight-social-similarity
0
1
0.87
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

The urban sprawl model is representing the urban development over an area. The urban development involves the spread of new residents over the area based on resident's preferences. Two agents/turtles are involved in the model being the residents and the service centres. The preferences of the residents to live on a specific patch are dependent on the attributes of residents and attributes of the patches. The attribute of the resident is 'social similarity'. The atrributes of the patches include 'aesthetic quality', which is the elevation level, and 'distance to service centre'. These attributes are normalised, to make the variations within the attributes relatively important for all attributes.

## HOW IT WORKS

The outcome of the model depends on the behaviour of the two agents mentioned before, residents and service centres. 100 residents enter the area per iteration. The place where each of these residents will settle is dependent on three weights that link to the attributes of both the residents and the patches. The weights are: weight-distance, weight-aesthetics, weight-social-similarity. Weight-distance involves the distance of the resident to a service center, the larger the distance, the less convenient for residents. Which means the value will be closer to zero. Weight-aesthetics has a one-to-one relation with elevation. The higher the elevation, the higher the aesthetics value which makes it more likely that residents want to live there. Weight-social-similarity refers to the relation between residents. If the value for social similarity is higher, it means that residents like each other and thus are willing to live close to each other. The importance of the weights can be adjusted with use of the sliders in the interface. After each 1000th new resident, a new service centre will be built.

## HOW TO USE IT

The interface of the model consists of one main window that shows the output of the model. The single input needed for this model is a digital elevation model which in this case covers the area of Land van Maas en Waal. Furthermore, there are several sliders. The first slider can be used to set the amount of incoming residents per tick, which is set by default to 100 residents. The second slider indicates that the model should stop when a certain percentage of the area has been developed -in other words- area where residents have settled. The default is set to 15% of the area being developed. The following three sliders correspond to the weight-distance, weight-aesthetics and weight-social-similarity. Each of these three slides can be set to a value scaled between 0 and 1. Values set closer to 0 mean that the weight of the corresponding attribute is lower and thus less important. Moreover, there is a setup button that needs to be pushed when the sliders are set to the desired values, whereafter the go button can be pushed to run the model.  

## THINGS TO NOTICE

Things to notice while running the model are mainly the difference in urban sprawl of the residents when the varying wheights are adjusted. Patterns can generally be noticed when around 50 ticks have passed. Notice the elevation differences on the map. Residents settle near these areas when the weight for aesthetics is set to high. Also notice the random placement of a new service centre after every 1000th resident. 

## THINGS TO TRY

Main things to try, are moving sliders which results in different patterns for urban sprawl. For example when setting the weight for aesthetics high and the other weights lower, one can notice that the residents mainly settle in and around areas with higher elevation. Moreover when the weight for distance is set relatively high compared to the other weights, one can see that the residents settle mainly around the service centres and thus cluster more compared to the first situation described before. 

## EXTENDING THE MODEL

Suggestions to make the model more complicated could be to add more variables. The model as it is, is very simplistic while only three variables are taken into account whereas in real life resident's settlement depends on many more variables. For example, one could try to implement housing prices based on locations or distance from main infrastructure. Moreover, at this stage the model does not take into account any form of uncertainty. For example, it is assumed that residents base their decision to live somewhere on only three variables. While taking more variables into consideration, would improve the model in the first place, also an uncertainty factor is suggested to be built in. Lastly, the social similarity for every resident in this mode is the same. So the similarity is eather 0.5 when the resident has no neighbour or 1.0 when a neihbour is present. It would be realistic if people had different social simularities, as these would present various social prefferences in society.  

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
van Dijk, Luc; 't Hart, Jochem; De Ronde, Wouter
GRS30306 Spatial Modeling and Statistics, 
Wageningen University

References:
- Brown & Robinson.(2006).Effects of Heterogeneity in Residential Preferences on an Agent-Based Model of Urban Sprawl. Ecology and Society. http://www.ecologyandsociety.org/volXX/issYY/artZZ/ 
- Netlogo user manual 
https://ccl.northwestern.edu/netlogo/docs/
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="uncertain-uncertain" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="stop-at-percent-developed">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="chosen-random-seed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="residential-preferences">
      <value value="&quot;normal-distribution&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-residents-per-tick">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain">
      <value value="&quot;uncertain&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fixed-uncertain" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="stop-at-percent-developed">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="chosen-random-seed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="residential-preferences">
      <value value="&quot;normal-distribution&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-residents-per-tick">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain">
      <value value="&quot;fixed&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="uncertain-fixed" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="stop-at-percent-developed">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="chosen-random-seed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="residential-preferences">
      <value value="&quot;homogeneous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-residents-per-tick">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain">
      <value value="&quot;uncertain&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
