/////////////////////////////////
// Arrays
//////////////////////////////////

// append a button that re-fetches data in case we forgot or refreshed
var allButtons = d3.selectAll(".method-example")
    .append('button')
    .attr("class","refetch-button")
    .text("Refetch Data");

allButtons.on("click", fetchData);


/////////////////////////////////
// 1. Inherent
//////////////////////////////////

// .constructor
var buttonLength = d3.select("#constructor")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log("flow.constructor", flow[0], "returns", flow[0].constructor)
    })


// .length
var buttonLength = d3.select("#length")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log("flow.length", flow[0], "returns", flow[0].length)
    })

// .prototype 
var buttonLength = d3.select("#prototype")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log("flow.prototype", flow[0], "returns", flow[0].prototype)
    })


/////////////////////////////////
// 2. Most Common Methods
//////////////////////////////////

// .forEach
var buttonLength = d3.select("#forEach")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log(
            "flow.forEach(function(d,i)) { return i }", 
            flow[0],
            "returns", 
            flow[0].forEach(function(d) { 
                console.log(d); })
            );
    })



/////////////////////////////////
// 3. Less Common Methods
//////////////////////////////////