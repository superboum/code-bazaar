const inquirer = require('inquirer')
const rp = require('request-promise-native')

inquirer.registerPrompt('autocomplete', require('inquirer-autocomplete-prompt'))
const pr = cmd => 
  inquirer
    .prompt(cmd.questions)
    .then(cmd.answer)
    .catch(console.error)

const state = {
  github: {
    login: null,
     password: null,
     ok: false
  },
  orgs: {
    scan: [],
    filtered: [],
  }
}

const gh = req => new Object({
  method: 'GET',
  uri: 'https://api.github.com' + req,
  headers: {
    'User-Agent': 'superboum/code-bazaar'
  },
  auth: {
    user: state.github.login,
    pass: state.github.password
  },
  json: true
})

const subcommands = [
  {
    name: 'login',
    questions: [{
      'type': 'input',
      'name': 'login',
      'message': 'login.username>',
    }, {
      'type': 'password',
      'name': 'pass',
      'message': 'login.password>'
    }],
    answer: res => { 
      state.github.login = res.login
      state.github.password = res.pass

      return rp(gh('/'))
        .then(r => {
          state.github.ok = true
          console.log('success')
        })
        .catch(r => console.log(r.error.message))
    }
  },
  {
    name: 'orgs.scan',
    exec: () => {
      return rp(gh('/user/orgs'))
        .then(orgs => {
          state.orgs.scan = orgs.map(o => o.login)
          state.orgs.filtered = orgs.map(o => o.login)
          console.log('success')
        })
        .catch(r => console.log(r.error.message))
    }
  },
  {
    name: 'orgs.filter',
    questions: [{
      type: 'autocomplete',
      name: 'filter',
      message: 'orgs.filter>',
      suggestOnly: true,
      source: (answers, input) => Promise.resolve(state.orgs.scan.filter(o => o.match(input)))
    }],
    answer: res => {
      state.orgs.filter = res.filter
      state.orgs.filtered = state.orgs.scan.filter(o => o.match(state.orgs.filter))
      console.log(state.orgs.filtered)
      return Promise.resolve()
    }
  },
  {
    name: 'orgs.selected',
    questions: [{
      type: 'list',
      name: 'list',
      message: 'orgs.selected>',
      choices: () => state.orgs.filtered.length > 0 ? state.orgs.filtered : ['none']
    }],
    answer: res => Promise.resolve()
  },
  {
    name: 'repo.scan'
    exec: () => {
      return Promise.resolve()
    }
  }, 
  {
    name: 'repo.filter'
  }, 
  {
    name: 'repo.download'
  }, 
  {
    name: 'files.scan'
  },
  {
    name: 'files.filter'
  },
  {
    name: 'files.ngram'
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
    message: 'cmd>',
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
