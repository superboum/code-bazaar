def bashTest(name, exec, expected) {
  echo "Testing: ${name}"
  sh "bash -c 'diff <(time ${exec}|tee /dev/stderr) <(echo -n ${expected})'"
}

node {
  stage('Cheatsheet') {
    docker.image('debian').inside {
      sh 'apt update && apt install --no-install-recommends -y git texlive-latex-base texlive-latex-extra lmodern texlive-lang-french python-pigments'
      checkout scm
      sh 'pdflatex -shell-escape cheatsheet/python.tex'
      archiveArtifacts artifacts: './python.pdf'
    }
  }

  stage('Common Lisp') {
    docker.image('debian').inside {
      sh 'apt update && apt install --no-install-recommends -y git sbcl'
      checkout scm
      bashTest "Problem 1 - Multiples of 3 and 5", "./euler/01/main.lisp", "233168"
    }
  }
}
