const inquirer = require('inquirer')
const rp = require('request-promise-native')
const fs = require('fs').promises
const plimit = require('p-limit')

const ghq = plimit(5)
const fsq = plimit(50)

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
    filter: '',
    filtered: [],
  },
  repo: {
    scan: [],
    filter: '',
    filtered: [],
  }
}

const save = () => fs.writeFile(process.env.HOME + '/.ghtool.json', JSON.stringify(state), {encoding: 'utf-8', flag: 'w+'})
const load = () => 
  fs
    .readFile(process.env.HOME + '/.ghtool.json', 'utf-8')
    .then(f => JSON.parse(f))
    .then(j => Object.assign(state, j))
    .catch(e => console.error('unable to load save',e ))

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

const filt = (substate, input) => {
  substate.filter = input
  try {
    substate.filtered = substate.scan.filter(o => o.match(substate.filter))
  } catch(e) {}
  return Promise.resolve(substate.filtered)
}

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
      pageSize: 30,
      message: 'orgs.filter>',
      suggestOnly: true,
      source: (answers, input) => filt(state.orgs, input)
    }],
    answer: res => filt(state.orgs, res.filter)
  },
  {
    name: 'orgs.selected',
    questions: [{
      type: 'list',
      name: 'list',
      pageSize: 30,
      message: 'orgs.selected>',
      choices: () => state.orgs.filtered.length > 0 ? state.orgs.filtered : ['none']
    }],
    answer: res => Promise.resolve()
  },
  {
    name: 'repo.scan',
    exec: () => 
      Promise.all(
        state.orgs.filtered.map(o => 
          ghq(() => rp(gh(`/orgs/${o}/repos`)))))
        .then(res => res.length > 0 && Array.isArray(res[0]) ? res[0] : res) 
        .then(res => res.map(r => r.full_name))
        .then(res => { state.repo.scan = res })
        .catch(console.error)
  }, 
  {
    name: 'repo.filter',
    questions: [{
      type: 'autocomplete',
      name: 'filter',
      pageSize: 30,
      message: 'repo.filter>',
      suggestOnly: true,
      source: (answers, input) => filt(state.repo, input)
    }],
    answer: res => filt(state.repo, res.filter)

  }, 
  {
    name: 'repo.selected',
    questions: [{
      type: 'list',
      name: 'list',
      message: 'repo.selected>',
      pageSize: 30,
      choices: () => state.repo.filtered.length > 0 ? state.repo.filtered : ['none']
    }],
    answer: res => Promise.resolve()
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
      .then(() => save())
      .then(() => pr(rootcommand))
      .catch(console.error)
  }
}

load().then(() => pr(rootcommand))
