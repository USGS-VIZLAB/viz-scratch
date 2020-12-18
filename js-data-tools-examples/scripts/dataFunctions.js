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
            ".map() || Using .map on the array called 'flow ',", flow," like this: flow.map(function(day) { return day.dateFull; }) returns an array of all the dates in the array.",
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


// .reduce()
d3.select("#reduce")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfFlowInHUC1 = flow[0].map(function(day) { return day.huc01; })
        var totalFlowInHUC1 = arrayOfFlowInHUC1.reduce(function(accumulator, currentValue){
            return accumulator + currentValue;
        }, 0);
        console.log(
            ".map() || Using .map on the array called 'flow ',", flow," returns an array of all the discharge values in flow's array of objects.  Then, we can reduce that array into a single value with an accumulator and currentValue, which gives us a single sum of: ",
            arrayOfFlowInHUC1, 
            "Reduces to",
            totalFlowInHUC1
            );
    })

// .push()
d3.select("#push")
    .append('button')
    .text("Test in console")
    .on("click", function() {

        function getRandomInteger(max) {
            return Math.floor(Math.random() * Math.floor(max));
        }

        var arrayOfRandomFlowValues = [];
        for (var i = 0; i<=4; i++){
            var randoFlowValue = flow[0][getRandomInteger(21)].date
            arrayOfRandomFlowValues.push(randoFlowValue);
        }     
       
        console.log(
            ".push() || Here, we've made a loop that runs 5 times, and each time get a random date from the array of possible dates in the data set.",
            "All five random dates should be in this array, and since the LAST one in the loop was ", 
            randoFlowValue, 
            ", it should be the LAST item in the array.",
            arrayOfRandomFlowValues,
            "Basically, .pop() and .push() work on the end of an array, and shift() and .unshift() work at the front."
            );
    })

// .pop()
d3.select("#pop")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })
        var popped = arrayOfDates.pop()
        console.log(
            ".pop() || Using .map on the flow array, I can get another array of all the dates in the flow array.",
            arrayOfDates,
            "But because I used .pop(), you'll notice that array is only 30 items long, because I removed the last date! That date was: ",
            popped,
            "Basically, .pop() and .push() work on the end of an array, and shift() and .unshift() work at the front."
            );
       
    })

// .shift()
d3.select("#shift")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })
        var shifted = arrayOfDates.shift()
        console.log(
            ".shift() || Using .map on the flow array, I can get another array of all the dates in the flow array.",
            arrayOfDates,
            "But because I used .shift(), you'll notice that array is only 30 items long, because I removed the first date! That date was: ",
            shifted,
            "Basically, .shift() and .unshift() work at the front of an array, and .pop() and .push() work on the end."
            );
       
    })

// .unshift()
d3.select("#unshift")
.append('button')
.text("Test in console")
.on("click", function() {

    function getRandomInteger(max) {
        return Math.floor(Math.random() * Math.floor(max));
    }

    var arrayOfRandomFlowValues = [];
    for (var i = 0; i<=4; i++){
        var randoFlowValue = flow[0][getRandomInteger(21)].date
        arrayOfRandomFlowValues.unshift(randoFlowValue);
    }     
   
    console.log(
        ".push() || Here, we've made a loop that runs 5 times, and each time get a random date from the array of possible dates in the data set.",
        "All five random dates should be in this array, and since the LAST one in the loop was ", 
        randoFlowValue, 
        ", it should be the FIRST item in the array.",
        arrayOfRandomFlowValues,
        "Basically, .shift() and .unshift() work at the front of an array, and .pop() and .push() work on the end."
        );
})


// .reverse
d3.select("#reverse")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })
        var reversedDates = arrayOfDates.slice().reverse();  // Had to use .slice() so I didn't mutate the original array!
        console.log(
            ".reverse() || Using .map on the flow array returns an array of all the dates.",
            arrayOfDates,
            "But we can put my thang down, flipit and reverse it!",
            reversedDates,
            "Ti esrever dna ti pilf nwod gnaht ym tup i, Ti esrever dna ti pilf nwod gnaht ym tup i <3 <3 <3 luv u missy elliot"
            );
    })

// .slice
d3.select("#slice")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })
        var exactCopy = arrayOfDates.slice();
        var slicedArray = arrayOfDates.slice(1,4);
        console.log(
            ".slice() || Using .map on the flow array returns an array of all the dates.",
            arrayOfDates,
            "If we slice it with no arguments, we get an exact copy of the whole array. See?",
            exactCopy,
            "But we can also pass two arguments, which are index positions to start and end.  If we take a slice from the 2nd to the 5th items - ie, .slice(1,4), we get:",
            slicedArray
            );
    })

// .splice()
d3.select("#splice")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })
        var splicedArray = arrayOfDates.slice().splice(2,3);
        console.log(
            ".splice() || Using .map on the flow array returns an array of all the dates.",
            arrayOfDates,
            "You can splice values in and out, using arguments.  The first argument is the starting spot.  The second argument is the number of items to splice out.",
            "So .splice(2,3) starts at index 2 (the 3rd item) and pulls out three values, like so:",
            splicedArray
            );
    })


// .some()
d3.select("#some")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfFlows = [];
        flow[0].forEach(function(day){
            arrayOfFlows.push(day.huc01);
            arrayOfFlows.push(day.huc02);
            arrayOfFlows.push(day.huc03);
            arrayOfFlows.push(day.huc04);
            arrayOfFlows.push(day.huc05);
            arrayOfFlows.push(day.huc06);
            arrayOfFlows.push(day.huc07);
            arrayOfFlows.push(day.huc08);
            arrayOfFlows.push(day.huc09);
            arrayOfFlows.push(day.huc10);
            arrayOfFlows.push(day.huc11);
            arrayOfFlows.push(day.huc12);
            arrayOfFlows.push(day.huc13);
            arrayOfFlows.push(day.huc14);
            arrayOfFlows.push(day.huc15);
            arrayOfFlows.push(day.huc16);
            arrayOfFlows.push(day.huc17);
            arrayOfFlows.push(day.huc18);
            arrayOfFlows.push(day.huc19);
            arrayOfFlows.push(day.huc20);
            arrayOfFlows.push(day.huc21);
        });
        var isFlowZero = arrayOfFlows.some(function(discharge){
            return discharge == 0            
        })
        console.log(
            ".some() || Is a boolean test to see if any values in an array pass a test.",
            arrayOfFlows,
            "In this case, I made an array of every discharge value and am checking if any of them are zero.  So - are any of the values zero?",
            isFlowZero            
            );
    })

// .sort()
d3.select("#sort")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfFlows = [];
        flow[0].forEach(function(day){
            arrayOfFlows.push(day.huc01);
            arrayOfFlows.push(day.huc02);
            arrayOfFlows.push(day.huc03);
            arrayOfFlows.push(day.huc04);
            arrayOfFlows.push(day.huc05);
            arrayOfFlows.push(day.huc06);
            arrayOfFlows.push(day.huc07);
            arrayOfFlows.push(day.huc08);
            arrayOfFlows.push(day.huc09);
            arrayOfFlows.push(day.huc10);
            arrayOfFlows.push(day.huc11);
            arrayOfFlows.push(day.huc12);
            arrayOfFlows.push(day.huc13);
            arrayOfFlows.push(day.huc14);
            arrayOfFlows.push(day.huc15);
            arrayOfFlows.push(day.huc16);
            arrayOfFlows.push(day.huc17);
            arrayOfFlows.push(day.huc18);
            arrayOfFlows.push(day.huc19);
            arrayOfFlows.push(day.huc20);
            arrayOfFlows.push(day.huc21);
        });
       var sorted = arrayOfFlows.slice().sort()
        console.log(
            ".sort() || This method arranges values in array either alphabetically or numerically, and either ascending (up) or descending (down).",
            arrayOfFlows,
            "If no arguments are passed, that originall array gets sorted numerically in ascending order, into: ",
            sorted          
            );
    })

/////////////////////////////////
// 3. Less Common Methods
//////////////////////////////////


// .findIndex
d3.select("#findIndex")
    .append('button')
    .text("Test in console")
    .on("click", function() {
        var arrayOfDates = flow[0].map(function(day) { return day.date; })

        console.log(
            "This isn't working yet."
            // ".findIndex() || Using .map on the flow array returns an array of all the dates.",
            // arrayOfDates,
            // "Then I can see what the index position is of the first item that passes a test.  Specifically, what is the index of first date that is 2 days beyond the birthday?"
            );
    })