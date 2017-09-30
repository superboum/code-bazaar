def bashTest(name, exec, expected) {
  sh "bash -c 'echo Testing: ${name} && diff -i <(time ${exec}|md5sum|cut -c -32) <(echo ${expected})'"
}

node {
  stage('Cheatsheet') {
    docker.image('ubuntu:xenial').inside("-u root") {
      sh 'apt-get update'
      sh 'apt-get install --no-install-recommends -y texlive-latex-base texlive-latex-extra lmodern texlive-lang-french python-pygments'
      checkout scm
      sh 'pdflatex -shell-escape cheatsheet/python.tex'
      archiveArtifacts artifacts: 'python.pdf'
    }
  }

  stage('Common Lisp') {
    docker.image('ubuntu:xenial').inside("-u root --security-opt seccomp=unconfined") {
      sh 'apt-get update'
      sh 'apt-get install --no-install-recommends -y sbcl'
      checkout scm
      bashTest "Problem 01 - Multiples of 3 and 5", "./euler/01/main.lisp", "e1edf9d1967ca96767dcc2b2d6df69f4"
      bashTest "Problem 02 - Even Fibonacci numbers", "./euler/02/main.lisp", "4194eb91842c8e7e6df099ca73c38f28"
      bashTest "Problem 03 - Largest prime factor", "./euler/03/main-factors.lisp", "94c4dd41f9dddce696557d3717d98d82"
      bashTest "Problem 03 - Largest prime factor - Implementation with Pollard algorithm", "./euler/03/main-pollard.lisp", "94c4dd41f9dddce696557d3717d98d82"
      bashTest "Problem 06 - Sum square difference", "./euler/06/main.lisp", "867380888952c39a131fe1d832246ecc"
      bashTest "Problem 07 - 10001st prime", "./euler/07/main.lisp", "8c32ab09ec0210af60d392e9b2009560"
      bashTest "Problem 08 - Largest product in a series", "./euler/08/main.lisp < ./euler/08/in.txt", "0f53ea7949d32ef24f9186207600403c"
      bashTest "Problem 09 - Special Pythagorean triplet", "./euler/09/main.lisp", "24eaa9820350012ff678de47cb85b639"
      bashTest "Problem 10 - Summation of primes", "./euler/10/main.lisp", "d915b2a9ac8749a6b837404815f1ae25"
      bashTest "Problem 11 - Largest product in a grid", "./euler/11/main.lisp < ./euler/11/in.txt", "678f5d2e1eaa42f04fa53411b4f441ac"
      bashTest "Problem 12 - Highly divisible triangular number", "./euler/12/main.lisp", "8091de7d285989bbfa9a2f9f3bdcc7c0"
      bashTest "Problem 13 - Large sum", "./euler/13/main.lisp < ./euler/13/in.txt", "361113f19fd302adc31268f8283a4f2d"
      bashTest "Problem 14 - Longest Collatz sequence", "./euler/14/main.lisp", "5052c3765262bb2c6be537abd60b305e"
      bashTest "Problem 14 - Longest Collatz sequence - with Memoization", "./euler/14/main-memo.lisp", "5052c3765262bb2c6be537abd60b305e"
      bashTest "Problem 15 - Lattice paths", "./euler/15/main.lisp", "928f3957168ac592c4215dcd04e0b678"
      bashTest "Problem 16 - Power digit sum", "./euler/16/main.lisp", "6a5889bb0190d0211a991f47bb19a777"
      bashTest "Problem 17 - Number letter counts", "./euler/17/main.lisp", "6A979D4A9CF85135408529EDC8A133D0"
      bashTest "Problem 19 - Counting Sundays", "./euler/19/main.lisp", "A4A042CF4FD6BFB47701CBC8A1653ADA"
      bashTest "Problem 20 - Factorial digit sum", "./euler/20/main.lisp", "443CB001C138B2561A0D90720D6CE111"
      bashTest "Problem 27 - Quadratic primes", "./euler/27/main.lisp", "69D9E3218FD7ABB6FF453EA96505183D"
      bashTest "Problem 29 - Distinct powers", "./euler/29/main.lisp", "6F0CA67289D79EB35D19DECBC0A08453"
      bashTest "Problem 30 - Digit fifth powers", "./euler/30/main.lisp", "27A1779A8A8C323A307AC8A70BC4489D"
      bashTest "Problem 31 - Coin sums", "./euler/31/main.lisp", "142DFE4A33D624D2B830A9257E96726D"
      bashTest "Problem 32 - Pandigital products", "./euler/32/main.lisp", "100F6E37D0B0564490A2EE27EFF0660D"
    }
  }
}
