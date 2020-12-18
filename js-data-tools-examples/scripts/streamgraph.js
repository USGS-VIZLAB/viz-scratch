
/////////////////////////////////
// Declare any variables that need to be Global, ie, accessible outside of any particular functions.
//////////////////////////////////

var emptyAPI = "https://waterservices.usgs.gov/nwis/dv/?format=json";
var state = '';
var sites = []; 
var birthday = '1992-06-07'; //startDT=
var start = '';
var startDate = '';
var end = '';
var endDate = '';
var paramCode = '00060'; // discharge in cubic feet per second
var siteType = 'ST';
var siteStatus = "all";
var colorScheme = ["#4f0b56","#482a70","#41498a","#3287bd","#4da4b1","#67c2a5","#8acda4","#acd7a3","#c8e19e","#e4ea99","#f7eda9","#fcde89","#ffc28a","#e5ccf5", /*"#eeb4d1",*/"#f79cac","#ae3a7d","#890965","#760a60","#620a5b","#420f4e"];
var flow =[]; // empty array, but makes this variable globally accessible once we push values to it.
var rawAPIdata = []; // empty array, but makes this variable globally accessible once we push values to it.
var dates = []; // an empty array of dates that we'll push info to

// We also are probably going to need these data structures later, so I'm just gonna declare them here for funsies.

var HUCInfo = [  // this is an array of objects
    {no:"01",id:"huc01", name:"New England Region"},
    {no:"02",id:"huc02", name:"Mid Atlantic Region"},
    {no:"03",id:"huc03", name:"South Atlantic-Gulf Region"},
    {no:"04",id:"huc04", name:"Great Lakes Region"},
    {no:"05",id:"huc05", name:"Ohio Region"},
    {no:"06",id:"huc06", name:"Tennessee Region"},
    {no:"07",id:"huc07", name:"Upper Mississippi Region"},
    {no:"08",id:"huc08", name:"Lower Mississippi"},
    {no:"09",id:"huc09", name:"Souris-Red-Rainy Region"},
    {no:"10",id:"huc10", name:"Missouri Region"},
    {no:"11",id:"huc11", name:"Arkansas-White-Red Region"},
    {no:"12",id:"huc12", name:"Texas-Gulf Region"},
    {no:"13",id:"huc13", name:"Rio Grande Region"},
    {no:"14",id:"huc14", name:"Upper Colorado Region"},
    {no:"15",id:"huc15", name:"Lower Colorado Region"},
    {no:"16",id:"huc16", name:"Great Basin Region"},
    {no:"17",id:"huc17", name:"Pacific Northwest Region"},
    {no:"18",id:"huc18", name:"California Region"},
    {no:"19",id:"huc19", name:"Alaska Region"},
    {no:"20",id:"huc20", name:"Hawaii Region"},
    {no:"21",id:"huc21", name:"Carribbean-Puerto Rico Region"}
];

// For the SVG
var margin = {top: 20, right: 50, bottom: 0, left: 50}; // A wild OBJECT appears!  Notice the curly braces which gives you the clue.
var width = window.innerWidth;
var height = d3.min([window.innerHeight, 1000]) - margin.top - margin.bottom;

// Append an svg dynamically to the div, so it gets resized on each button push
var svg = d3.select("#streamgraph")
    .append("svg")
    .classed("streamgraph-svg","true")
    .attr("width", width)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(0," + margin.top + ")");

/////////////////////////////////
// Fetch and Use data
//////////////////////////////////

// make one function that gets called when the button gets pushed

function fetchData() { // no arguments because as part of the function, we'll select the value of whatever's in the input

    
    /////////////////////////////////
    // Here's the general structure of what we're about to do
    //
    // 1. Declare Static Variables
    // 2. Get Date from document and convert it to what the code needs
    // 3. Pre-make a data structure that will plug nicely into the streamgraph.
    // 4. Read a CSV to get a list of gage sites
    // 5. Compile all the chunks of the NWIS API url and call it
    // 6. Call the API and get the actual discharge data!
    // 7. Do some data wrangling - push out any bad data
    // 8. More data wrangling - sort all the timeseries values into arrays grouped by HUC2.
    // 9. More data wrangling - aggregate all the measurements so we get a single timeseries for the HUC
    //
    // Then the data wrangling is done! I'll put the streamgraph drawing in another function to keep things organized.
    //
    //////////////////////////////////


    /////////////////////////////////
    // 1. Clear any variables we need (that might have data stored from a previous click)
    //////////////////////////////////

    


    // Declare a bunch of variables so they exist, but don't give them any values. This overwrites any pre-existing data that was there from the last time this function ran.
    var birthday = ''; // will be in Javascript date format
    start = ''; // will be in Javascript date format. These have already been declared, but we should clear them in case this is the second time the data are being grabbed.
    end = ''; // will be in Javascript date format
    startDate = ''; // will be converted to Long String
    endDate = ''; // will be converted to Long String
    var allDates = []; // will be an ARRAY of the dates, 
    var sitey = ""; // the final, filled-in API query
    var birthdayFlow = []; // will be an ARRAY, where each 
    var info = [];
    var timeseries = [];
    var allSites = [];

    /////////////////////////////////
    // 2. Get Date from document and convert it to what the code needs
    //////////////////////////////////

    // Function to get a new date a certain number of days from another date
    function addDays (date, daysToAdd) {
        var _24HoursInMilliseconds = 86400000; // conversion count
        return new Date(date.getTime() + daysToAdd * _24HoursInMilliseconds); // new Date() is Javascript's date object!
    };
    
    // Grab the value input by the user and assign it to the variable "birthday", converting it to a date object in the process
    birthday = new Date(document.getElementById("birthdate").value); 
    console.log("1.",birthday, "is the user's birthday as a Javascript object"); // console log to see what this object looks like.  Notice it's missing the curly braces! Confusing. It's definitely still an object though.

    // Let's get a month worth of dates to pull! Make dates for 15 days before and after birthday
    birthday = addDays(birthday, 1); // need to adjust by 1 for some reason
    start = addDays(birthday, - 15);
    end = addDays(birthday, 15);       

    // Make a function that converts a date obejct YYYY-MM-DD format because that's what NWIS needs for the API call
    function getYYYYMMDD(d0){
        var d = new Date(d0)
        return new Date(d.getTime() - d.getTimezoneOffset() * 60 * 1000).toISOString().split('T')[0]
    }
    // Now apply that function to the start and end dates
    startDate = getYYYYMMDD(start);
    endDate = getYYYYMMDD(end);
    console.log("2. We converted that to YYYY-MM-DD and counted out to be able to call a month of data from ",startDate, " to ",endDate);
    

    /////////////////////////////////
    // 3. Pre-make a data structure that will plug nicely into the streamgraph.
    //////////////////////////////////

    // While we're at it, let's make an array of all the dates we're going to pull data for!  We won't need it for the actual API call, but we will need this array for plotting a streamgraph
    allDates = [
        getYYYYMMDD(start), 
        getYYYYMMDD(addDays(start,1)), 
        getYYYYMMDD(addDays(start,2)), 
        getYYYYMMDD(addDays(start,3)), 
        getYYYYMMDD(addDays(start,4)), 
        getYYYYMMDD(addDays(start,5)), 
        getYYYYMMDD(addDays(start,6)), 
        getYYYYMMDD(addDays(start,7)), 
        getYYYYMMDD(addDays(start,8)), 
        getYYYYMMDD(addDays(start,9)), 
        getYYYYMMDD(addDays(start,10)), 
        getYYYYMMDD(addDays(start,11)), 
        getYYYYMMDD(addDays(start,12)), 
        getYYYYMMDD(addDays(start,13)), 
        getYYYYMMDD(addDays(start,14)), 
        getYYYYMMDD(birthday), 
        getYYYYMMDD(addDays(end,-14)), 
        getYYYYMMDD(addDays(end,-13)), 
        getYYYYMMDD(addDays(end,-12)), 
        getYYYYMMDD(addDays(end,-11)), 
        getYYYYMMDD(addDays(end,-10)), 
        getYYYYMMDD(addDays(end,-9)), 
        getYYYYMMDD(addDays(end,-8)), 
        getYYYYMMDD(addDays(end,-7)), 
        getYYYYMMDD(addDays(end,-6)), 
        getYYYYMMDD(addDays(end,-5)), 
        getYYYYMMDD(addDays(end,-4)), 
        getYYYYMMDD(addDays(end,-3)), 
        getYYYYMMDD(addDays(end,-2)), 
        getYYYYMMDD(addDays(end,-1)), 
        getYYYYMMDD(end)
    ];
    console.log("3. Here are all the dates we'll pull streamgage data for:", allDates); // Should be an array of 31 dates because 15+15+1

    // We've already declared birthdayFlow above, and made it an empty array. This populates it as an array of objects, with properties that we can now call on later
    // I am not smart enough to come up with this on my own!  I just looked at a streamgraph example in d3, console.logged the data that fed it, and it was in this structure.
    // So this is just reverse engineering data that we have to fit into an existing thing that works. Maybe not the most elegant way to do it. 
    birthdayFlow = [
        {dateFull:start, date:allDates[0], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,1), date:allDates[1], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,2), date:allDates[2], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,3), date:allDates[3], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,4), date:allDates[4], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,5), date:allDates[5], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,6), date:allDates[6], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,7), date:allDates[7], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,8), date:allDates[8], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,9), date:allDates[9], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,10), date:allDates[10], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,11), date:allDates[11], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,12), date:allDates[12], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,13), date:allDates[13], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(start,14), date:allDates[14], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:birthday, date:allDates[15], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-14), date:allDates[16], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-13), date:allDates[17], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-12), date:allDates[18], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-11), date:allDates[19], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-10), date:allDates[20], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-9), date:allDates[21], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-8), date:allDates[22], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-7), date:allDates[23], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-6), date:allDates[24], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-5), date:allDates[25], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-4), date:allDates[26], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-3), date:allDates[27], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-2), date:allDates[28], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:addDays(end,-1), date:allDates[29], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0},
        {dateFull:end, date:allDates[30], huc01:0,huc02:0,huc03:0,huc04:0,huc05:0,huc06:0,huc07:0,huc08:0,huc09:0,huc10:0,huc11:0,huc12:0,huc13:0,huc14:0,huc15:0,huc16:0,huc17:0,huc18:0,huc19:0,huc20:0,huc21:0}
    ];


    /////////////////////////////////
    // 4. Read a CSV to get a list of gage sites
    //////////////////////////////////
    d3.csv("data/ref_gages.csv", function(gages) { 
        console.log("4. Read in the csv of HCDN gages", gages);  // Notice in the console that this csv is displayed as an ARRAY of OBJECTS

        // Get list of site numbers to add to the url.  We can use the .map() function!
        var gageSites = gages.map(function(g) { return g.site_no; });
        console.log("4b. Use .map() to pull out all values of 'site_no' in the gages object and make them into an array", gageSites);
        // This is exactly what we need for NWIS, which says it can pull data for multiple gages as long as you provide a list of sites...separated by a comma! Which is how an array is formatted!!!
    
        /////////////////////////////////
        // 5. Compile all the chunks of the NWIS API url
        //////////////////////////////////

        //var urlCounty = "&countyCd=" + countyCd;
        var urlSites = "&sites=" + gageSites; // this attaches the array of site numbers
        var urlStartDate = "&startDT=" + startDate; // this attaches the start date in YYYY-MM-DD
        var urlEndDate = "&endDT=" + endDate; // this attaches the end date in YYYY-MM-DD
        var urlStatCD = "&statCd=00003" // 00003 means "mean" values
        var urlParam = "&PARAMETERcD=" + paramCode; // we declared above to only want discharge
        //var urlSiteType = "&siteType=" + siteType;
        //var urlSiteStatus = "&siteStatus=" + siteStatus;

        // Compile the URL
        sitey = emptyAPI + urlSites + urlStartDate + urlEndDate + urlStatCD + urlParam;
        console.log("5. Now that we have all the components of the URL, we compile them into one long string for the API, in the order that NWIS needs", sitey);
    
        /////////////////////////////////
        // 6. Call the API and get the actual discharge data!
        //////////////////////////////////
        d3.json(sitey, function(error, apiData) { 
            
            console.log("6. This is the data that the API returns: ",apiData);
            // Notice the extremely nested nature of this dataset. Objects and Arrays all the way down. 
            // I'll push this data to a globally accesible variable so we can play with the raw data elsewhere on the page.
            rawAPIdata.push(apiData);

            // let's just grab the timeseries out of the data, rather than all the extra metadata
            //timeseries = apiData; // EXERCISE : let's go get just the timeseries. Use the console to explore the data structure and fill out this line. Answer is below. 
            timeseries = apiData.value.timeSeries;
            console.log("6b. Here's just the timeseries", timeseries);  // This is an ARRAY of OBJECTS


            /////////////////////////////////
            // 7. Do some data wrangling - push out any bad data
            //////////////////////////////////
            var badGages = []; // make empty array to populate with a list of gages with incomplete data
            
            console.log("7a. We're about to do a forEach() loop over an array! You can turn on the console.log inside there to see what's being dealt with in each iteration of the loop, ie array item - by - array item.")
            // forEach!!! This is an array method that is essentially a for-loop, but fancier.
            timeseries.forEach(function(gage, index) { // This translates to: "Take the array of objects called timeseries. Go item-by-item in the array.  Each item we'll call 'gage' and keep track of the index. "
                var array = gage.values[0].value;  // Each item in the array (ie, 'gage') is actually nested data.  As we go item-by-item, pull out just the array of timeseries values and assign that array to the variable 'array'.  Ignore the rest of the metadata.
                //console.log("7a. This is each array we're dealing with as we go gage-by-gage in the timeseries", array);
                if(array.length !== 31 ) { // Check to see if there aren't exactly 31 measurements.  If not...
                    badGages.push(gage.name); // ... take the gage name of this particular gage, and push it into the array of "bad gages"...
                    timeseries.splice(index, 1); // ... then go back to the original timeseries and remove 1 item from that array, specifically the item with this index. They will always match!
                }
                
                //console.log("7a. While we're here, let's grab the HUC code. Where is it in this nested dataset?", gage);
                // gage.huc02 = gage; // EXERCISE : let's go get the huc code from this gage we're dealing with. Use the console to explore the data structure and fill out this line. Answer is below.
                gage.huc02 = gage.sourceInfo.siteProperty[1].value.slice(0,2); // And while we're here, go ahead and add a property that lists the HUC02 for each gage by slicing the first two numbers from the huc_cd
            });
            console.log("7b. Here are all the gages that don't have all 31 daily discharge values found by the forEach() loop.  They have been deleted/spliced from the timeseries array as part of the loop.", badGages);
            console.log("7c. The timeseries array should be shorter, by the exact number of items in the badGages array.", timeseries);
        
            /////////////////////////////////
            // 8. More data wrangling - sort all the timeseries values into arrays grouped by HUC2.
            //////////////////////////////////
        
            var huc01 = []; // These are empty arrays
            var huc02 = [];
            var huc03 = [];
            var huc04 = [];
            var huc05 = [];
            var huc06 = [];
            var huc07 = [];
            var huc08 = [];
            var huc09 = [];
            var huc10 = [];
            var huc11 = [];
            var huc12 = [];
            var huc13 = [];
            var huc14 = [];
            var huc15 = [];
            var huc16 = [];
            var huc17 = [];
            var huc18 = [];
            var huc19 = [];
            var huc20 = [];
            var huc21 = [];
            console.log("8. We've got some empty HUC arrays.  Watch us populate them!", huc01, huc02, huc03, "etc");

            // Create a reusable function to collect all timeseries values and put them in their respective HUC array
            function getHUCArray(hucArray, huc_no) {
                timeseries.forEach(function(gage) {  // Another forEach loop, since timeseries is still an array
                    //console.log("8ish.This gage is", gage);
                    var this_huc = gage.huc02; // see what HUC 2 we're dealing with for each gage in the loop
                    //console.log("8ish. This gage's HUC2 is", this_huc);
                    var this_timeseries = gage.values[0].value; // grab the timeseries too
                    //console.log("8ish. This gage's timeseries is", this_timeseries);
                    var this_data = this_timeseries.map(function(v) {  // We use the map method to pull out 
                        //console.log("8ish. 'v' is each of the 31 measurements in this timeseries that we're dealing with in this iteration of the loop. 'v' is an object.",v);
                        if(this_huc == huc_no) { // if the HUC code matches the one I provded in the beginning, then it's good!
                            v.dateTime = v.dateTime.substring(0,10) // This takes the date of each measurement and just shortens it to a more readable format as YYYY-DD-MM without the time, which is meaningless
                            hucArray.push(v); // This takes each gage's timeseries (an object) and push it as another item in the array we've selected based on the HUC 2 code.
                        }                      
                    }); 
                });
                console.log("8ish. At the end of each loop, we've produced an array of objects, where each object is a discharge measurement within HUC", huc_no, " with a date, and here it is!", hucArray)
            };

            // Get huc arrays for all hucs
            getHUCArray(huc01,01);
            getHUCArray(huc02,02);
            getHUCArray(huc03,03);
            getHUCArray(huc04,04);
            getHUCArray(huc05,05);
            getHUCArray(huc06,06);
            getHUCArray(huc07,07);
            getHUCArray(huc08,08);
            getHUCArray(huc09,09);
            getHUCArray(huc10,10);
            getHUCArray(huc11,11);
            getHUCArray(huc12,12);
            getHUCArray(huc13,13);
            getHUCArray(huc14,14);
            getHUCArray(huc15,15);
            getHUCArray(huc16,16);
            getHUCArray(huc17,17);
            getHUCArray(huc18,18);
            getHUCArray(huc19,19);
            getHUCArray(huc20,20);
            getHUCArray(huc21,21);

            /////////////////////////////////
            // 9. More data wrangling - aggregate all the measurements so we get a single timeseries for the HUC
            //////////////////////////////////

            // Now we're going to sum everything and push those values into that empty birthdayFlow array of objects we made. 
            console.log("9. Remember that empty array of objects called 'birthdayFlow' we made? Time to populate it.", birthdayFlow)


            function getTotalFlow(hucArray, hucIDstring) {
                if (hucArray.length >= 1) { // This starts the loop only if there are objects in the array to sum.  If there were no measurements in this HUC 2, then it doesn't break, it just will keep the discharge measurement for that day and HUC in birthdayFlow array as 0.
                    console.log("9ish. For", hucIDstring, "we are gonna sum the ",hucArray.length, "discharge measurements (each of which is an object) in 'hucArray':", hucArray);
                    for (var i = 0; i <= allDates.length-1; i++) {
                        // in the first step, allDates[0] is YYYY-MM-DD of start date
                        
                        var todaysMeasurements = hucArray.filter(function(measurement) { // doing only HUC01 array
                            return measurement.dateTime === allDates[i];
                        })
                        var flows = todaysMeasurements.map(function(flows) {
                            return +flows.value;                        
                        })
                        
                        var totalFlow = Math.floor(flows.reduce(function(accumulator,flow) {
                            return accumulator + flow;
                        }))
                        
                        // console.log(birthdayFlow[i][property], "this")
                        birthdayFlow[i][hucIDstring] = totalFlow; // This assigns the aggregated flow to the particular day in the array (birthdayFlow[i]) which has a property with the same name as the hucArray.  There's probably a more elegant way to do this, but this [hucIDstring] bracket notation needs a string so whatever!
                    }
                } else {
                    console.log("9ish. For", hucIDstring, "there are ",hucArray.length, "discharge measurements 'hucArray'. See?", hucArray, "So we do nothing.");
                }
            };           
            getTotalFlow(huc01, "huc01"); 
            getTotalFlow(huc02, "huc02");  
            getTotalFlow(huc03,"huc03");
            getTotalFlow(huc04,"huc04");
            getTotalFlow(huc05,"huc05");
            getTotalFlow(huc06,"huc06");
            getTotalFlow(huc07,"huc07");
            getTotalFlow(huc08,"huc08");
            getTotalFlow(huc09,"huc09");
            getTotalFlow(huc10,"huc10");
            getTotalFlow(huc11,"huc11");
            getTotalFlow(huc12,"huc12");
            getTotalFlow(huc13,"huc13");
            getTotalFlow(huc14,"huc14");
            getTotalFlow(huc15,"huc15");
            getTotalFlow(huc16,"huc16");
            getTotalFlow(huc17,"huc17");
            getTotalFlow(huc18,"huc18");
            getTotalFlow(huc19,"huc19");
            getTotalFlow(huc20,"huc20");
            getTotalFlow(huc21,"huc21");

            console.log("9b. And now, 'birthdayFlow' should be populated with total discharge!!! YAAAAY WE HAVE OUR DATA!!!!!! ",birthdayFlow);
            flow.pop()
            flow.push(birthdayFlow); // Finally, I'll push it outside of this fetchData function into a globally scoped variable so I can grab it with other functions
            // ^ If I didn't push it, and tried to reference birthdayFlow after this, it would come back as undefined. Try it!


            // In fact, now let's just push a bunch of data we will need for drawing the graph in d3. 
            dates.pop()
            dates.push({'start': start, 'birthday': birthday,'end':end});

            // Do a tricky thing and make streamgraph drawing available only once the data are computed. You don't have to do this, I'm just having fun pacing you ;)
            d3.selectAll(".hidden-until-data").style("display","block");
            console.log("10. Now, because I'm tricky, I'm gonna make a button accessible to you that will let you draw the streamgraph with this data. MWahahahaahahaaa")

            
        // end the d3.json function
        });

    // end the d3.csv function
    });

// End fetchData() function
}

function drawStreamgraph() { // again, no arguments because we've pushed the data to a globally scoped variable, "flow"

    d3.select("#streamgraph").style("display","block"); // first make the block for the streamgraph appear in the DOM
    
    console.log(2.0, "Time to Draw streamgraph! Let's make sure the data is accessible: ", flow); 
    /////////////////////////////////
    // Here's the general structure of what we're about to do
    //
    // 1. Add a property called 'columns' upon which the streamgraph data will be stacked
    //
    //////////////////////////////////

    
    /////////////////////////////////
    // 1. Add a property called 'columns' upon which the streamgraph data will be stacked
    //////////////////////////////////

    // So if an array is...just a special kind of object...can we add properties to it? YES WE CAN!!! 
    flow.columns = [
        "huc01",
        "huc02",
        "huc03",
        "huc04",
        "huc05",
        "huc06",
        "huc07",
        "huc08",
        "huc09",
        "huc10",
        "huc11",
        "huc12",
        "huc13",
        "huc14",
        "huc15",
        "huc16",
        "huc17",
        "huc18",
        "huc19",
        "huc20",
        "huc21"
    ];
    console.log(2.1, "See? Now this is an array of objects, and the array itself as a property called 'columns' which lists all the columns in the data", flow)
    // I would never have figured out that I needed this on my own, but I noticed in the streamgraph d3 example that the data has a property called columns upon which the data is later stacked.
    // So this step ^ is just reverse engineering.

    /////////////////////////////////
    // 2. Draw Chart []
    //////////////////////////////////
    // Unfortunately I don't have enter-update-exit down pat, so I'm just gonna draw this once.

    // List of groups = header of the csv files
    var keys = flow.columns
    console.log(2.2, "Declare the 'keys' by which each band in the streamgraph will be grouped", keys)
    console.log(2.3, "Let's check and make sure our dates are accessible here. Should be an array with a single object", dates)
    
    // Add X axis
    var x = d3.scaleTime()
        .domain([dates[0].start, dates[0].end]) // set the beginning and end by accessing the values stored in the global 'dates' variable. It needs the javascript object format, not the YYYY-MM-DD format we made elsewhere
        .range([0,width ]);
    console.log(2.4, "Test the x scaling to make sure it works. Let's enter a date and see if it returns an X position that makes sense.",dates[0].birthday, "turns into", x(dates[0].birthday));


    // append a "group" to the svg to contain the ticks, and then draw them
    svg.append("g")
        .attr("class","tick-label")
        .attr("transform", "translate(0," + height*0.8 + ")")
        .call(d3.axisBottom(x)
            .tickSize(-height*.01)
            .tickFormat(d3.timeFormat("%B %d, %Y"))
            .tickValues([dates[0].birthday])) // this is wrong, we need a date
        .select(".domain").remove()

    // Add Y axis
    var y = d3.scaleLinear()
        .domain([-300000, 300000]) // we could definitely calculate this dynamically with d3.max, but I'm being lazy
        .range([height,0]);
    console.log(2.5, "Test the y scaling to make sure it works. Let's enter a flow from ",flow[0] ,"and see if it returns a Y position that makes sense.", flow[0][0].huc01, "turns to", y(flow[0][0].huc01));

    // Declare a color palette
    var color = d3.scaleOrdinal()
        .domain(keys)
        .range(colorScheme);
    console.log(2.6, "Test the color scaling to make sure it works. Let's enter a huc number and see if it returns a Y position that makes sense.", keys[0], "turns into", color(keys[0]));

    // Now D3 is doing something weird called Stacking that we need for streamgraphs. Don't ask me about it.
    var stackedData = d3.stack()
        .offset(d3.stackOffsetSilhouette)
        .keys(keys)
        (flow[0])
    console.log(2.7, "What the heck is stacked data? Let's make sure it's an array with exactly the same number of items as there are groups, or rather, HUCS", stackedData);
    
    // Reusable area generator - we are getting closer to drawing a thing! Don't know much about this part tho.
    var area = d3.area()
        .x(function(d) { 
            // console.log("What even is d?", d)
            return x(d.data.dateFull); 
        })
        .y0(function(d) { return y(d[0]); })
        .y1(function(d) { return y(d[1]); })
        .curve(d3.curveMonotoneX)


    /////////////////////////////////
    // 3. Make some variables that are actually just functions which can be called when we draw the svg
    //////////////////////////////////

    // create a lookup function
    function getHUCname(hucIDnum){
        var filtered = HUCInfo.filter(function(huc) { // take the HUCInfo array of objects, and filter items. We're using the argument 'huc' to represent the fact that each item in the array is a whole huc.  We'll store the one item we get in the variable 'filtered'
            return huc.no === hucIDnum; // return only the ONE object in the array where the number (.no) matches the provided hucIDnum exactly
        })
        if(filtered.length == 1) { // checking to make sure that there's ONE thing in the variable 'filtered' variable
            return filtered[0].name; // return the value in the ".name" property
        }
    }

    // create a tooltip
    var Tooltip = svg
        .append("text")
        .attr("x", 30)
        .attr("y", height*0.8 + 11)
        .attr("class", "tooltip")
        .style("opacity", 0)
        .style("z-index",100)

    // Three function that change the tooltip when user hover / move / leave a cell
    var mouseover = function(d) {
        Tooltip.style("opacity", 1)
        d3.selectAll(".flow").style("opacity", .2)
        d3.select(this)
            .style("opacity", 1)
    }
    var mousemove = function(d,i) {
        grp = keys[i]
        Tooltip.text(getHUCname(grp.slice(3,5)))
    }
    var mouseleave = function(d) {
        Tooltip.style("opacity", 0)
        d3.selectAll(".flow").style("opacity", 1).style("stroke", "none")
    }
    


    /////////////////////////////////
    // 4. Drumroll....let's draw the svg!
    //////////////////////////////////
        
    // Draw the areas
    svg
        .selectAll("path")
        .data(stackedData)
        .enter()
        .append("path")
            .attr("class", "flow")
            .style("fill", function(d) { return color(d.key); })
            .attr("d", area)
            .on("mouseover", mouseover)
            .on("mousemove", mousemove)
            .on("mouseleave", mouseleave);
   
    d3.selectAll("path")
        .transition()
        .duration(1000)
        .attr("d",area)
    d3.selectAll(".tick-label")
        .transition()
        .duration(1)
        .call(d3.axisBottom(x)
            .tickSize(-height*.01)
            .tickFormat(d3.timeFormat("%B %d, %Y"))
            .tickValues([dates[0].birthday]))
            .select(".domain").remove()
}