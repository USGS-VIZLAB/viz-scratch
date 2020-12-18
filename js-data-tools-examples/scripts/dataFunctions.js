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
d3.select("#constructor")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log("flow.constructor", flow[0], "returns", flow[0].constructor)
    })


// .length
d3.select("#length")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log("flow.length", flow[0], "returns", flow[0].length)
    })

// .prototype 
d3.select("#prototype")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        console.log("flow.prototype", flow[0], "returns", flow[0].prototype)
    })


/////////////////////////////////
// 2. Most Common Methods
//////////////////////////////////

// .map
d3.select("#map")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })
        console.log(
            ".map() || Using .map on the array called 'flow', like this: flow.map(function(day) { return day.dateFull; }) returns an array of all the dates in the array.",
            arrayOfDates,
            "Basically, .map() is used when you want to transform elements in an array.  Or, when you want to pull an ARRAY out of an ARRAY OF OBJECTS. In other words, if you have multiple objects and you want to get a list of all the values of a particular property within those objects, use .map()."
            );
    })

// .forEach
d3.select("#forEach")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        flow[0].forEach(function(day,index) { 
            var this_day = day.dateFull.getDay(); // returns a number from 0 to 6 representing Sunday through Saturday
            if (this_day == 5) {
                day.isFriday = true;
               console.log(day.date, "was a Friday. TGIF!")
            } else {
                day.isFriday = false;
               console.log(day.date, "was not a Friday :(")
            }
        });
        console.log(".forEach() || This is basically a more readable for-loop. We just went item-by-item in the 'flow' array and checked to see whether the date was a Friday or not. While we were there, we also added a property to each object indicating whether it was a Friday or not. Dig down into the data below to find it!", flow[0])
    })

// .indexOf()
d3.select("#indexOf")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var allDateObjects = flow[0].map(function(day) { return day.dateFull; });
        console.log(
            ".indexOf() || 'flow' is an array of OBJECTS, and the dates are nested as properties in each object. So let's pull an array out of these objects, one with just all the dates by using the .map function.",
            "Then we'll check to see what the index of the user's 'birthday' date is. Since there are 31 dates in the array, the birthday should be right in the middle. And it is...",
            allDateObjects.indexOf(dates[0].birthday),
            "Hooray it worked!"
            );
    })

// .filter()
d3.select("#filter")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var filteredFlows = flow[0].filter(function(day) { 
            return day.dateFull.getDay() === 5
        });
        console.log(".filter() || We can return any item in the array that passes the test, in this case, the test is 'Is it Friday?'  If the day passed the test, it got included in the new array.  If not, the computer ignored it.  Check out our new filtered array of flow data, showing only Fridays!", filteredFlows)
    })



/////////////////////////////////
// 3. Less Common Methods
//////////////////////////////////