Edit Patterns
=============

You can find the details of our experiment here: http://arxiv.org/abs/1307.1719

This work was supported in part by the US Department of Energy Office of
Science, Advanced Scientic Computing Research contract no. DE-SC0004968.
Additional support was provided by Galois, Inc.

Overview
--------

This tool extract patterns of edits. These patterns allow you to better make
sense of changes in software history.

You will need:

  * A git repository containing Java source code
  * graphviz
  * Haskell (tested with GHC 7.6.3)
  * tree-utils from COMPOSE-HPC: https://sourceforge.net/projects/compose-hpc/files/ROTE/

How to use the tool
-------------------

The tool works in several phases:

  1. Build up a database of which files changed in which revisions
  2. Parse the files that have changed to generate ATerms
  3. Use Yang's structural diff algorithm on the ATerms to generate edit scripts in tree form
  4. Extraction of difference subtrees from edit scripts (including removing a lot of the surrounding tree)
  5. Grouping related subtrees using the similarity score and thresholding
  6. Using anti-unification over the groups of related trees
  7. Generating a pattern (in graphviz dot format) for a representative of each group


Command line options:

```
$ edit-patterns --help
edit-patterns
  -s DIRECTORY  --sandbox=DIRECTORY    Directory for storing or reading aterms
  -v            --verbose              Enable verbose output
  -m MODE       --mode=MODE            Mode of operation, MODE is one of: generate-aterms, antiunify-aterms, antiunify-group, graphviz, similarity, unparse, weave
  -t THRESHOLD  --threshold=THRESHOLD  Threshold for similarity, as a floating point value, in the range [0..1]
  -n NUM        --num=NUM              The number of changes to consume
  -h            --help                 Show help
```

To accomplish steps (1) and (2), change to the top level of the git repository
containing the java code and type something like: `edit-patterns -s
/tmp/sandbox -m generate-aterms`

This will create `version-pairs.zip` in the directory `/tmp/sandbox` that
contains the serialized ATerms. There is currently no way to tell the tool when
to stop. So you may need to type Ctrl-C. This should be safe as the tool
updates the zip file in a temporary location and then moves it on top of the
old zip file (eg., the write should be more or less atomic). Future commands
will need to reference this same zip file.

**Note:** If you'd like to change this behavior, look in `Main.hs` for
`generateTerms` and uncomment the `take` in the line that reads `let pairs = {-
take 10 -} (zip cs (drop 1 cs))`.

Once you have the ATerms, steps (3) and (4) are accomplished using the `weave` mode, but first you should change cdirectory to the sandbox or somewhere else where you want to continue the analysis. Something like this:
`cd /tmp/sandbox && edit-patterns -s /tmp/sandbox -m weave`

As a side effect, this will generate `treetypes.csv`,
`treesimilarity-before.csv`, `treesimilarity-after.csv`,
`treesimilarity-delete.csv`, `treesimilarity-add.csv` in the current direcotry.

Steps (5), (6), and (7) are accomplished using the the `similarity` mode with a threshold, like this:
`edit-patterns -s /tmp/sandbox -m similarity -t 0.5`

The side effect of this is to generate a log file and graphviz files. The log
file can be used to associate the output trees with the trees from earlier
steps. The `.gv` files can be viewed in any graphviz viewer.

Example `run.log`

```
Similarity threshold: 0.0

antiUnifyTerms: 525 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200
201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220
221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260
261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280
281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300
301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320
321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340
341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360
361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380
381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400
401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418 419 420
421 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440
441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 459 460
461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 480
481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 496 497 49 8 499
500 501 502 503 504 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519
520 521 522 523 524
...
```

This says that at a threshold of 0.0, that subtree 525 is similar to all the
other trees in the list. And in the output, we see `antiunify-add-525.gv`.

**Note:** The csv files from the previous step are processed in the order:
before, after, delete, and add.
