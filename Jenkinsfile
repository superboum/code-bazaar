def bashTest(name, exec, expected) {
  sh "echo Testing: ${name} && bash -c 'diff <(time ${exec}|tee /dev/stderr) <(echo -n ${expected})'"
}

node {
  stage('Cheatsheet') {
    docker.image('ubuntu:xenial').inside("-u root") {
      sh 'apt-get update'
      sh 'apt-get install --no-install-recommends -y git texlive-latex-base texlive-latex-extra lmodern texlive-lang-french python-pygments'
      checkout scm
      sh 'pdflatex -shell-escape cheatsheet/python.tex'
      archiveArtifacts artifacts: 'python.pdf'
    }
  }

  stage('Common Lisp') {
    docker.image('ubuntu:xenial').inside("-u root --security-opt seccomp=unconfined") {
      sh 'apt-get update'
      sh 'apt-get install --no-install-recommends -y git sbcl'
      checkout scm
      bashTest "Problem 01 - Multiples of 3 and 5", "./euler/01/main.lisp", "233168"
      bashTest "Problem 02 - Even Fibonacci numbers", "./euler/02/main.lisp", "4613732"
      bashTest "Problem 03 - Largest prime factor", "./euler/03/main-factors.lisp", "6857"
      bashTest "Problem 06 - Sum square difference", "./euler/06/main.lisp", "25164150"
      bashTest "Problem 07 - 10001st prime", "./eulear/07/main.lisp", "104743"
      bashTest "Problem 08 - Largest product in a series", "./eulear/08/main.lisp < ./euler/08/in.txt", "23514624000"
      bashTest "Problem 09 - Special Pythagorean triplet", "./eulear/09/main.lisp", "31875000"
      bashTest "Problem 10 - Summation of primes", "./eulear/10/main.lisp", "142913828922"
      bashTest "Problem 11 - Largest product in a grid", "./eulear/11/main.lisp < ./euler/11/in.txt", "70600674"
      bashTest "Problem 12 - Highly divisible triangular number", "./eulear/12/main.lisp", "76576500"

    }
  }
}
