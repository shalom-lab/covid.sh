const gcoord = require('gcoord');
const fs = require('fs');

const tranform = function(input) {
    return gcoord.transform(input, gcoord.BD09, gcoord.WGS84)
}

var Papa = require('papaparse');



let rawdata = fs.readFileSync('../data/database/map.2.all.json');

let map = JSON.parse(rawdata);

let newMap = map.map(item=>{
    var input = [item.lng,item.lat]
    var result =  gcoord.transform(input, gcoord.BD09, gcoord.WGS84)
    return {
        ...item,
        lng1:result[0],
        lat1:result[1],
    }
})

console.log(newMap.length)
fs.writeFileSync('../data/database/map.2.all.new.json', JSON.stringify(newMap));
