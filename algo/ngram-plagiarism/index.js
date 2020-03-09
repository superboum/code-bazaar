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

const gh_host = 'https://api.github.com'
const gh = req, opts => 
  req === null ? 
    [] :
    ghq(() => rp({
      method: 'GET',
      uri: req.match('https?:\/\/') ? req : gh_host + req,
      headers: {
        'User-Agent': 'superboum/code-bazaar'
      },
      auth: {
        user: state.github.login,
        pass: state.github.password
      },
      json: opts && opts.is_raw ? opts.is_raw : true,
      transform: (body, response, resolveFull) => {
        console.log(`done ${response.request.uri.href}`)
        body['__next'] = null
        if (response && response.headers && response.headers.link) {
          const next_match = response.headers.link.match('<([^<>]+)>; rel="next"')
          const next = next_match.length == 2 ? next_match[1] : null
          const last_match = response.headers.link.match('<([^<>]+)>; rel="last"')
          const last = last_match.length == 2 ? last_match[1] : null
          if (next && last && next != last) body['__next'] = next
        }
        return body
      }
    }))
    .then(b => Promise.all([b, gh(b['__next'])]))
    .then(([e1, e2]) => {
      if (Array.isArray(e1) && Array.isArray(e2)) return [...e1, ...e2]
      else if (e2.length === 0) return e1
      else return [e1, e2]
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

      return gh('/')
        .then(r => {
          state.github.ok = true
          console.log('success')
        })
        .catch(r => console.log(r.error.message))
    }
  },
  {
    name: 'orgs.scan',
    exec: () => 
      gh('/user/orgs')
        .then(orgs => {
          state.orgs.scan = orgs.map(o => o.login)
          state.orgs.filtered = orgs.map(o => o.login)
          console.log('success')
        })
        .catch(r => console.log(r.error.message))
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
      Promise.all(state.orgs.filtered.map(o => gh(`/orgs/${o}/repos`)))
        //.then(r => {console.log(r) ; return r})
        .then(orgs => orgs.map(repos => repos.map(repo => repo.full_name)).flat())
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
    exec: () => 
      state.repo.filtered.map(r => 
        fs.mkdir(`${process.env.HOME}/ghtool/${r}`, {recursive: true})
          .then(() => gh(`/repos/${r}/tarball/master`, { is_raw: true})))
          .then(c => fsq(() => fs.writeFile(`${process.env.HOME}/ghtool/${r}.tar.gz`, c)))
          .then()

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
