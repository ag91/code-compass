var margin = 10,
    outerDiameter = 960,
    innerDiameter = outerDiameter - margin - margin;

var x = d3.scale.linear()
    .range([0, innerDiameter]);

var y = d3.scale.linear()
    .range([0, innerDiameter]);

var color = d3.scale.linear()
    .domain([-1, 5])
    .range(["hsl(185,60%,99%)", "hsl(187,40%,70%)"])
    .interpolate(d3.interpolateHcl);

var pack = d3.layout.pack()
    .padding(2)
    .size([innerDiameter, innerDiameter])
    .value(function(d) { return d.size; })

var mainDiv = d3
    .select("body")
    .append("div")
    .attr("id", "maindiv")
    .style("width", "100%")
    .style("float", "left")

var graphDiv = d3
    .select("#maindiv")
    .append("div")
    .attr("id", "graphdiv")
    .style("width", "600px")
    .style("float", "left")

var svg = d3
    .select("#graphdiv")
    .append("svg")
    .attr("width", outerDiameter)
    .attr("height", outerDiameter)
    .append("g")
    .attr("transform", "translate(" + margin + "," + margin + ")");

d3.json("knowledge.json", function(error, root) {
  var focus = root,
      nodes = pack.nodes(root);

  svg.append("g").selectAll("circle")
    .data(nodes)
    .enter().append("circle")
    .attr("class", function(d) { return d.parent ? d.children ? "node" : "node node--leaf" : "node node--root"; })
    .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; })
    .attr("r", function(d) { return d.r; })
    .style("fill", function(d) { return d.weight > 0.0 ? d.author_color :
                                 d.children ? color(d.depth) : "black"; })
    .style("fill-opacity", function(d) { return d.effort; })
    .on("click", function(d) { return  d.children ? zoom(focus == d ? root : d) : undefined; });

  svg.append("g").selectAll("text")
    .data(nodes)
    .enter().append("text")
    .attr("class", "label")
    .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; })
    .style("fill-opacity", function(d) { return d.parent === root ? 1 : 0; })
    .style("display", function(d) { return d.parent === root ? null : "none"; })
    .text(function(d) { return d.name; })
    .on("mouseover", function(d){this.innerHTML = d.author || d.name;})
    .on("mouseout", function(d){this.innerHTML = d.name;});

  d3.select(window)
    .on("click", function() { zoom(root); });

  function zoom(d, i) {
    var focus0 = focus;
    focus = d;

    var k = innerDiameter / d.r / 2;
    x.domain([d.x - d.r, d.x + d.r]);
    y.domain([d.y - d.r, d.y + d.r]);
    d3.event.stopPropagation();

    var transition = d3.selectAll("text,circle").transition()
        .duration(d3.event.altKey ? 7500 : 750)
        .attr("transform", function(d) { return "translate(" + x(d.x) + "," + y(d.y) + ")"; });

    transition.filter("circle")
      .attr("r", function(d) { return k * d.r; });

    transition.filter("text")
      .filter(function(d) { return d.parent === focus || d.parent === focus0; })
      .style("fill-opacity", function(d) { return d.parent === focus ? 1 : 0; })
      .each("start", function(d) { if (d.parent === focus) this.style.display = "inline"; })
      .each("end", function(d) { if (d.parent !== focus) this.style.display = "none"; });
  }}
       );

d3.select(self.frameElement).style("height", outerDiameter + "px");


var div = d3
    .select("#maindiv")
    .append("div")
    .attr("id", "legendiv")
    .attr("width", outerDiameter)
    .attr("height", outerDiameter)
    .style("margin-left", "1020px")
    .style("height", "800px")
    .style("overflow-y", "scroll");

var legendSVG = d3
    .select("#legendiv")
    .append("svg")
    .style("height", "3000")
    .style("width", "450");

var size = 20
d3.csv("authors.csv", function(authorsColors){
  // Add dots
  legendSVG.selectAll("mydots")
    .data(authorsColors)
    .enter()
    .append("rect")
    .attr("x", 100)
    .attr("y", function(d,i){ return 100 + i*(size+5)}) // 100 is where the first dot appears. 25 is the distance between dots
    .attr("width", size)
    .attr("height", size)
    .style("fill", function(d){ return d.color})

  // Add names.
  legendSVG.selectAll("mylabels")
    .data(authorsColors)
    .enter()
    .append("text")
    .attr("x", 100 + size*1.2)
    .attr("y", function(d,i){ return 100 + i*(size+5) + (size/2)})
    .style("fill", function(d){ return d.color})
    .text(function(d){ return d.author})
    .attr("text-anchor", "left")
    .style("alignment-baseline", "middle")
})
