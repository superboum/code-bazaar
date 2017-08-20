def bashTest(name, exec, expected) {
  sh "bash -c 'echo Testing: ${name} && diff <(time ${exec}|md5sum|cut -c -32) <(echo ${expected})'"
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
      echo "Problem 10 - Summation of primes is too slow and too computing intensive for the CI"
      // bashTest "Problem 10 - Summation of primes", "./euler/10/main.lisp", "d915b2a9ac8749a6b837404815f1ae25"
      bashTest "Problem 11 - Largest product in a grid", "./euler/11/main.lisp < ./euler/11/in.txt", "678f5d2e1eaa42f04fa53411b4f441ac"
      bashTest "Problem 12 - Highly divisible triangular number", "./euler/12/main.lisp", "8091de7d285989bbfa9a2f9f3bdcc7c0"
      bashTest "Problem 13 - Large sum", "./euler/13/main.lisp < ./euler/13/in.txt", "361113f19fd302adc31268f8283a4f2d"
      bashTest "Problem 14 - Longest Collatz sequence", "./euler/14/main.lisp", "5052c3765262bb2c6be537abd60b305e"
      bashTest "Problem 14 - Longest Collatz sequence - with Memoization", "./euler/14/main-memo.lisp", "5052c3765262bb2c6be537abd60b305e"
    }
  }
}
