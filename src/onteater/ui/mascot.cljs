(ns onteater.ui.mascot
  "The full Onteater mascot artwork (the upright tamandua playing with an Earth
  globe) as an inline-SVG string. Kept as a string constant, generated from
  onteater_mascot.svg, so it inlines cleanly into the single-file build and can be
  dropped into the DOM via :dangerouslySetInnerHTML. Rendered in the About dialog.
  Regenerate from the source SVG if the artwork changes.")

(def full-svg
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 400 400' role='img' aria-label='Onteater mascot — a friendly southern tamandua anteater standing upright'>
  <title>Onteater Mascot</title>
  <desc>A stylized southern tamandua (collared anteater): cream head with a long black-tipped snout, small rounded ears, a black 'vest' over the back and shoulders, and large dark claws, standing upright and gripping a branch.</desc>

  <defs>
    <!-- Soft depth gradients -->
    <radialGradient id='creamGrad' cx='42%' cy='38%' r='75%'>
      <stop offset='0%' stop-color='#F7EDD8'/>
      <stop offset='100%' stop-color='#E6D3AE'/>
    </radialGradient>
    <linearGradient id='headGrad' x1='0' y1='0' x2='1' y2='1'>
      <stop offset='0%' stop-color='#F2E2C2'/>
      <stop offset='100%' stop-color='#E2CDA4'/>
    </linearGradient>
    <linearGradient id='vestGrad' x1='0' y1='0' x2='0.6' y2='1'>
      <stop offset='0%' stop-color='#332E29'/>
      <stop offset='100%' stop-color='#211D19'/>
    </linearGradient>
    <linearGradient id='snoutGrad' x1='0' y1='0' x2='1' y2='1'>
      <stop offset='0%' stop-color='#5A4E44'/>
      <stop offset='55%' stop-color='#2A2420'/>
      <stop offset='100%' stop-color='#171310'/>
    </linearGradient>
    <linearGradient id='branchGrad' x1='0' y1='0' x2='1' y2='0.4'>
      <stop offset='0%' stop-color='#B98A55'/>
      <stop offset='100%' stop-color='#8A6238'/>
    </linearGradient>
    <radialGradient id='shadow' cx='50%' cy='50%' r='50%'>
      <stop offset='0%' stop-color='#000000' stop-opacity='0.20'/>
      <stop offset='100%' stop-color='#000000' stop-opacity='0'/>
    </radialGradient>
    <!-- Earth globe -->
    <radialGradient id='ocean' cx='36%' cy='32%' r='72%'>
      <stop offset='0%' stop-color='#5AB0E8'/>
      <stop offset='55%' stop-color='#2C7FC0'/>
      <stop offset='100%' stop-color='#134C7C'/>
    </radialGradient>
    <linearGradient id='landGrad' x1='0' y1='0' x2='0.7' y2='1'>
      <stop offset='0%' stop-color='#68B85E'/>
      <stop offset='100%' stop-color='#3B8544'/>
    </linearGradient>
    <radialGradient id='gloss' cx='34%' cy='28%' r='40%'>
      <stop offset='0%' stop-color='#FFFFFF' stop-opacity='0.55'/>
      <stop offset='100%' stop-color='#FFFFFF' stop-opacity='0'/>
    </radialGradient>
    <clipPath id='globeClip'>
      <circle cx='295' cy='322' r='50'/>
    </clipPath>
  </defs>

  <!-- Ground shadow -->
  <ellipse cx='185' cy='372' rx='120' ry='20' fill='url(#shadow)'/>

  <!-- ===================== TAIL (behind body) ===================== -->
  <path d='M130 300
           C 70 300 45 250 55 205
           C 62 172 92 165 96 200
           C 99 232 96 268 150 280 Z'
        fill='#2A2521'/>
  <path d='M132 296
           C 88 296 70 258 78 218
           C 82 196 100 194 100 218
           C 100 248 108 272 150 278 Z'
        fill='#403A33'/>

  <!-- ===================== BODY (cream) ===================== -->
  <path d='M150 168
           C 118 178 96 214 98 262
           C 100 306 108 344 135 360
           C 165 378 205 372 224 350
           C 244 326 246 292 244 258
           C 242 214 232 182 205 168
           C 186 158 168 160 150 168 Z'
        fill='url(#creamGrad)'/>

  <!-- ===================== BLACK VEST (back + shoulder band) ===================== -->
  <path d='M150 168
           C 128 176 110 202 102 240
           C 122 236 140 232 156 236
           C 176 240 190 236 200 214
           C 214 232 232 240 244 250
           C 240 210 230 182 205 168
           C 186 158 168 160 150 168 Z'
        fill='url(#vestGrad)'/>
  <!-- lower rear black patch -->
  <path d='M112 300
           C 108 330 118 352 138 362
           C 158 372 182 370 200 356
           C 176 352 150 342 138 322
           C 130 310 120 302 112 300 Z'
        fill='url(#vestGrad)'/>

  <!-- ===================== HIND FEET ===================== -->
  <path d='M118 352 C 100 356 92 366 100 372 C 112 378 138 376 146 366 C 140 356 130 350 118 352 Z' fill='#3A342E'/>
  <path d='M170 358 C 158 362 154 372 164 376 C 178 380 198 374 200 364 C 192 356 182 354 170 358 Z' fill='#463F38'/>

  <!-- ===================== NECK + HEAD (tan) ===================== -->
  <path d='M196 176
           C 200 146 218 120 250 108
           C 276 98 300 108 308 132
           C 314 152 306 172 286 184
           C 300 190 312 202 316 216
           C 300 214 282 214 268 220
           C 250 228 226 224 210 210
           C 198 200 194 188 196 176 Z'
        fill='url(#headGrad)'/>

  <!-- Ear -->
  <path d='M258 106 C 250 88 260 76 276 80 C 290 84 294 100 286 114 C 278 110 266 108 258 106 Z' fill='#E2CDA4'/>
  <path d='M262 102 C 258 92 265 86 274 88 C 282 91 283 101 279 109 C 273 105 267 103 262 102 Z' fill='#C7987F'/>

  <!-- ===================== SNOUT (long, black-tipped) ===================== -->
  <path d='M292 142
           C 326 150 352 184 363 218
           C 369 238 361 251 344 250
           C 329 249 319 237 311 221
           C 300 199 282 182 266 176
           C 272 162 282 150 292 142 Z'
        fill='url(#snoutGrad)'/>
  <!-- nose highlight -->
  <ellipse cx='350' cy='234' rx='8' ry='6' fill='#0F0C0A'/>
  <ellipse cx='347' cy='231' rx='2.6' ry='2' fill='#5b524a'/>

  <!-- Eye -->
  <circle cx='286' cy='158' r='7.5' fill='#1A1512'/>
  <circle cx='283.5' cy='155.5' r='2.4' fill='#F4ECDD'/>

  <!-- Mouth line along snout -->
  <path d='M292 186 C 312 202 330 220 346 238' fill='none' stroke='#0F0C0A' stroke-width='1.6' stroke-linecap='round' opacity='0.4'/>

  <!-- ===================== EARTH GLOBE (the ball it plays with) ===================== -->
  <!-- contact shadow under the ball -->
  <ellipse cx='298' cy='374' rx='46' ry='10' fill='#000000' opacity='0.18'/>
  <!-- ocean sphere -->
  <circle cx='295' cy='322' r='50' fill='url(#ocean)'/>
  <!-- continents (stylized), clipped to the sphere -->
  <g clip-path='url(#globeClip)' fill='url(#landGrad)'>
    <path d='M256 292 C 272 284 292 288 300 300 C 306 310 298 318 286 320
             C 276 322 266 330 258 326 C 248 320 246 300 256 292 Z'/>
    <path d='M300 336 C 314 330 330 336 332 350 C 333 362 322 372 310 368
             C 298 364 292 346 300 336 Z'/>
    <path d='M262 344 C 272 340 282 346 280 356 C 278 366 266 368 258 362
             C 250 356 254 348 262 344 Z'/>
    <path d='M320 300 C 332 296 344 302 344 312 C 344 322 332 324 324 318
             C 318 314 314 304 320 300 Z'/>
    <circle cx='248' cy='318' r='7'/>
    <circle cx='330' cy='336' r='5'/>
  </g>
  <!-- rim shading for roundness -->
  <circle cx='295' cy='322' r='50' fill='none' stroke='#0C3860' stroke-width='3' opacity='0.35'/>
  <!-- glossy highlight -->
  <ellipse cx='278' cy='304' rx='26' ry='20' fill='url(#gloss)'/>

  <!-- ===================== FRONT ARM + BIG CLAWS ===================== -->
  <path d='M232 236
           C 250 240 266 254 268 274
           C 269 286 262 294 250 292
           C 238 290 230 278 226 264
           C 222 250 224 240 232 236 Z'
        fill='url(#creamGrad)'/>
  <path d='M236 250 C 248 253 257 263 260 274 C 246 268 238 260 236 250 Z' fill='#C9B48A' opacity='0.55'/>
  <!-- claws -->
  <g fill='#221E1A'>
    <path d='M250 288 C 258 300 270 308 282 306 C 276 296 266 288 258 284 Z'/>
    <path d='M244 292 C 250 306 260 316 272 316 C 268 304 258 294 250 289 Z'/>
    <path d='M237 293 C 240 308 248 320 260 322 C 258 308 250 297 243 291 Z'/>
  </g>

</svg>
")
