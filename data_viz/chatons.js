// API wrapper
const endpoint = "https://www.chatons.org/api/chatons/"
const apiCall = async (p) => {
    const res = await fetch(`${endpoint}${p}`)
    return await res.json()
}
const list = async () => { return await apiCall("all/json") }
const structure = async id => { return await apiCall(`${id}/structure-info/json`) }
const orga = async id => { return await apiCall(`${id}/organization-info/json`) }
const infra = async id => { return await apiCall(`${id}/infrastructure-info/json`) }
const loc = async id => { return await apiCall(`${id}/location-info/json`) }
const soft = async id => { return await apiCall(`${id}/software-info/json`) }

// Dumping API
const dump = async () => {
    const base = await list()
    //const base = (await list()).slice(0,5)
    
    console.log("fetching structures...")
    const withStruct = await Promise.all(base.map(c => structure(c.nid)))
    console.log("fetching organizations...")
    const withOrga = await Promise.all(base.map(c => orga(c.nid)))
    console.log("fetching infrastructures...")
    const withInfra = await Promise.all(base.map(c => infra(c.nid)))
    console.log("fetching locations...")
    const withLoc = await Promise.all(base.map(c => loc(c.nid)))
    console.log("fetching software...")
    const withSoftware = await Promise.all(base.map(c => soft(c.nid)))

    const world = base.map((c, idx) => ({
        ...c,
        structure: withStruct[idx],
        organisation: withOrga[idx],
        infrastructure: withInfra[idx],
        geoloc: withLoc[idx],
        software: withSoftware[idx],
    }))

    return world
}

// Main
(async () => {
    const data = await dump()
    console.log(data)

    const fs = require('fs');
    fs.writeFileSync('chatons.json', JSON.stringify(data));
    console.log(`${data.length} items written`)
})()
