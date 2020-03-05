const inquirer = require('inquirer')
const rp = require('request-promise-native')

inquirer.registerPrompt('autocomplete', require('inquirer-autocomplete-prompt'))
const pr = cmd => 
  inquirer
    .prompt(cmd.questions)
    .then(cmd.answer)
    .catch(console.error)

const state = {
  github_login: null,
  github_password: null,
  github_ok: false
}

const gh = req => new Object({
  method: 'GET',
  uri: 'https://api.github.com' + req,
  headers: {
    'User-Agent': 'superboum/code-bazaar'
  },
  auth: {
    user: state.github_login,
    pass: state.github_password
  },
  json: true
})

const subcommands = [
  {
    name: 'login',
    questions: [{
      'type': 'input',
      'name': 'login',
      'message': 'Github Login:',
    }, {
      'type': 'password',
      'name': 'pass',
      'message': 'Github Password:'
    }],
    answer: res => { 
      state.github_login = res.login
      state.github_password = res.pass

      return rp(gh('/'))
        .then(r => {
          state.github_ok = true
          console.log(r)
          console.log('success')
        })
        .catch(r => console.log(r.error.message))
    }
  },
  {
    name: 'orgs.index',
    exec: () => {
    }
  },
  {
    name: 'repo.index'
  }, 
  {
    name: 'repo.filter'
  }, 
  {
    name: 'repo.download'
  }, 
  {
    name: 'files.index'
  },
  {
    name: 'files.filter'
  },
  {
    name: 'analyze'
  },
  {
    name: 'exit',
    exec: () => Promise.reject('bye bye')
  }
]

const rootcommand = {
  questions: [{
    type: 'autocomplete',
    name: 'cmd',
    message: '>',
    source: (answers, input) => Promise.resolve(subcommands.filter(x => x.name.indexOf(input||'') != -1))
  }],
  answer: res => {
    const cmd = subcommands.find(s => s.name == res['cmd'])
    const p = 'exec' in cmd ? cmd.exec() : Promise.resolve()
    p
      .then(() => 'questions' in cmd && 'answer' in cmd ? pr(cmd) : Promise.resolve())
      .then(() => pr(rootcommand))
      .catch(console.error)
  }
}

pr(rootcommand)
