:- module(an_dashboard_components_task_stats,
	  [ task_stats//1
	  ]).

:- use_module(library(gensym)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

task_stats(Task) -->
	{ http_link_to_id(http_api_dashboard_task,
			  [task(Task), filter(number)], DataSource),
	  gensym(chart, Class),
	  atom_concat('svg.', Class, Selector)
	},
	html([
	    \html_requires(task_stats),
	    svg([class(Class)],[]),
	    \js_script({|javascript(DataSource, Selector)||
			var cwidth = document.body.clientWidth;
			var margin = {top: 20, right: 40, bottom: 50, left: 50},
			width =  0.45*cwidth - margin.left - margin.right,
			height = 200 - margin.top - margin.bottom;


			var x = d3.scale.ordinal()
			.rangeRoundBands([5, width], .1);

			var y = d3.scale.linear()
			.range([height, 0]);

			var xAxis = d3.svg.axis()
			.scale(x)
			.tickSize(10,0)
			.orient("bottom");

			var yAxis = d3.svg.axis()
			.scale(y)
			.orient("left")
			.ticks(10, "");

			var svg = d3.select(Selector)
			.attr("width", width + margin.left + margin.right)
			.attr("height", height + margin.top + margin.bottom)
			.append("g")
			.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

			d3.json(DataSource, function(error, data) {
						data.sort(compare_value);

						x.domain(data.map(function(d) { return d.label; }));
						y.domain([0, d3.max(data, function(d) { return d['@value']; })]);

						var bar = svg.selectAll("g")
						.data(data)
						.enter()
						.append("g");

						bar.append("rect")
						.attr("class", function(d) { return "bar " + d.key; })
						.attr("x", function(d) { return x(d.label); })
						.attr("width", x.rangeBand())
						.attr("y", function(d) { return y(d['@value']); })
						.attr("height", function(d) { return height - y(d['@value']); });

						bar.append("text")
						.style("text-anchor", "middle")
						.attr("class", "count")
						.attr("x", function(d) { return x(d.label) + 0.5 * x.rangeBand(); })
						.attr("y", function(d) { return Math.min(y(0), y(d['@value'])); })
						.attr("dy", "-1ex")
						.text(function(d) { return d['@value']; });

						svg.append("g")
						.attr("class", "x axis")
						.attr("transform", "translate(0," + height + ")")
						.call(xAxis).selectAll("text")
						.style("text-anchor", "middle")
						.attr("dx", '.1em')
						.attr("dy", function(d,i) { return 0 + 14*(i%3); });

						d3.selectAll("g.x.axis g.tick line")
						.style("stroke-opacity", 0.2)
						.attr("y2", function(d,i){ return 15 + 14*(i%3); });

						svg.append("g")
						.attr("class", "y axis")
						.call(yAxis)
						.append("text")
						.attr("transform", "rotate(-90)")
						.attr("y", 6)
						.attr("dy", "0.7ex")
						.style("text-anchor", "end")
						.text("Count");
					    });

			function compare_value(a,b) {
				     if (a.value < b.value) return 1;
				     if (a.value > b.value) return -1;
				     return 0;
				 }


		       |})
	]).
