# Calendar


## Context

Originally this was not really a challenge
but a side-project for deuxfleurs.fr.
The more I worked on it, the more
I was thinking that it could be a great versatile programming challenge
as:
  - it is very easy to understand
  - it works with any programming language and their standard library
  - it is big enough to force you to think to your design
  - it requires multiple skills (handling data structures, dates, etc.) that you must combine together

## Goal

Given a month and a year, generate an ASCII art calendar as follow (note that I am french so for me weeks start on monday and not sunday):

```
.——————————————————————————————————————.
|       CALENDRIER DÉCEMBRE 2024       |
|——————————————————————————————————————|
|                                ————  |
|                               | 01 | |
|  ———— ———— ———— ———— ———— ———— ————  |
| | 02 | 03 | 04 | 05 | 06 | 07 | 08 | |
|  ———— ———— ———— ———— ———— ———— ————  |
| | 09 | 10 | 11 | 12 | 13 | 14 | 15 | |
|  ———— ———— ———— ———— ———— ———— ————  |
| | 16 | 17 | 18 | 19 | 20 | 21 | 22 | |
|  ———— ———— ———— ———— ———— ———— ————  |
| | 23 | 24 | 25 | 26 | 27 | 28 | 29 | |
|  ———— ———— ———— ———— ———— ———— ————  |
| | 30 | 31 |                          |
|  ———— ————                           |
|______________________________________|
```

## Constraints

The calendar must be as close as possible as the example one.
You must use only the standard library of your language.
Your code must run on an old computer in less than a second.

## Duration

It took me between 6 and 8 hours to get a working output.

## My answer

My answer is very ugly.
In the future I want to analyze why and write a second,
more elegant version.
