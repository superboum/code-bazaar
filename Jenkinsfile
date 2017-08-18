node {
  stage('Cheatsheet') {
    docker.image('blang/latex').inside {
      checkout scm
      sh 'pdflatex -shell-escape cheatsheet/python.tex'
      archiveArtifacts artifacts: './python.pdf'
    }
  }
}
