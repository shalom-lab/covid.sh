const gcoord = require('gcoord');
const fs = require('fs');

const tranform = function(input) {
    return gcoord.transform(input, gcoord.BD09, gcoord.WGS84)
}

var Papa = require('papaparse');
 

  
let rawdata = fs.readFileSync('../data/map.2.json');

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

console.log(newMap);
fs.writeFileSync('../data/map.2.new.json', JSON.stringify(newMap));
