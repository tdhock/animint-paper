var plotDiv = d3.select("div#plot");
var plots = {};
var by = {};
var SIZE = 500;
d3.csv("WorldBank.csv", function (error, response) {
  var add_data = function(v){
    by[v] = d3.nest().key(function(d){
      return d[v];
    }).map(response);
  }
  add_data("year");
  add_data("country");
  var add_plot = function(name, x, y){
    var minmax = function(v){
      return [d3.min(response, function(d){return +d[v];}),
	      d3.max(response, function(d){return +d[v];})];
    }
    var make_scale = function(v){
      return d3.scale.linear()
	.domain(minmax(v))
	.range([0, SIZE]);
    }
    plots[name] = {
      "SVG":plotDiv.append("svg")
	.attr("width", SIZE)
	.attr("height", SIZE),
      "xvar":x,
      "yvar":y,
    }
    var toX = make_scale(x);
    var toY = make_scale(y);
      "x":function(d){return toX(d[x]);},
      "y":function(d){return toY(d[y]);},
    };
  }
  add_plot("timeSeries", "year", "life_expectancy");
  add_plot("scatterPlot", "fertility_rate", "life_expectancy");
  var selected_year = by.year["1996"];
  var points = plots.scatterPlot.SVG.selectAll("circle")
    .data(selected_year);
  points.enter()
    .append("circle")
    .attr("cx", plots.scatterPlot.x)
    .attr("cy", plots.scatterPlot.y)
    .attr("r", 10)
  ;
});
