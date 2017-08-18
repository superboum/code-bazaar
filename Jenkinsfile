pipeline {
  agent any

  stages {
    stage('Cheatsheets') {
      steps {
        docker.image('blang/latex').inside {
          checkout scm
          sh 'pdflatex -shell-escape cheatsheet/python.tex'
          archiveArtifacts artifacts: './python.pdf'
        }
      }
    }
  }
}
