var d3 = Object.assign({}, require("d3-selection"), require("d3-geo"));

var svgWidth = 500;
var svgHeight = 300;

var svg = d3.select('#d3Module')
	.append('svg')
	.attr('width', svgWidth)
	.attr('height', svgHeight)
	.attr('class', 'barchart');

var dataset = [80, 100, 56, 120, 180, 30, 40, 120, 160];
var barPadding = 5;
var barWidth = (svgWidth / dataset.length);
var barChart = svg.selectAll("rect")
    .data(dataset)
    .enter()
    .append("rect")
    .attr("y", function(d) {
        return svgHeight - d
    })
    .attr("height", function(d) {
        return d;
    })
    .attr("width", barWidth - barPadding)
    .attr("transform", function (d, i) {
         var translate = [barWidth * i, 0];
         return "translate("+ translate +")";
    });