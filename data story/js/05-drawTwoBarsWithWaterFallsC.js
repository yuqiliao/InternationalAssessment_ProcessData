function drawTwoBarsWithWaterFallsC(data, response) {
    
    var data = data.sort( (a, b) => d3.descending(a["Coef_startToLogout_noClick_min"], b["Coef_startToLogout_noClick_min"]));
    console.log(data);
    //data = data.sort( (a, b) => d3.descending(a["PCT"], b["PCT"]));
    //console.log(data);
    /**********************
    ***** BASIC SETUP *****
    **********************/

    // dynamic dimension sizing code adapted from
    // https://github.com/d3/d3-selection/issues/128
    const bbox = d3.select("#chart").node().getBoundingClientRect()

    const width = bbox.width;
    const height = bbox.height;
    const margin = {top: 50, left: 130, right: 50, bottom: 50};

    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.bottom - margin.top;

    //const smallMultipleWidth = plotWidth/5

    const svg = d3.select("#chart").select("svg");

    const DURATION = 1000;


    /***********************
    ***** X & Y SCALES *****
    ***********************/

    let xMin = 0;
    let xMax = 80;
    let yGroup = "IDCNTRY";

    let smallMultiplePadding = 30

    let xScale = d3.scaleLinear()
            .domain([xMin, xMax])
            .range([smallMultiplePadding, plotWidth]);
   
    //using the same yScale order as in 03-drawLongBarChart.js
    let yScale = d3.scaleBand()
        .domain(data.map(d => d[yGroup]))
        .range([0, plotHeight])
        .padding(.5);
    
    /***************************************
    ***** X AXIS, AXIS LABEL, GRIDLINE *****
    ***************************************/

    // svg.selectAll(".xAxis1,.xAxis2,.xAxis3,.xAxis4")
    //     .remove()

    svg.selectAll(".xAxis0")
            .transition()
            .duration(DURATION)
            .attr("transform", `translate(${margin.left}, ${plotHeight + margin.top})`)
            .call(d3.axisBottom(xScale)
                .ticks(4)
                //.tickFormat(d3.format("d"))
            );

    // svg.selectAll(".xGrid1,.xGrid2,.xGrid3,.xGrid4")
    //     .remove()

    svg.selectAll(".xGrid0")
        .transition()
        .duration(DURATION)
        .attr("transform", `translate(${margin.left}, ${margin.top})`)
        .call(d3.axisBottom(xScale)
            .tickSize(plotHeight)
            .ticks(3)
            .tickFormat("")
        );

    svg.selectAll(".xLabel")
        .data([{"label": "Percent of students who clicked ads"}])
        .text(d => d.label);


    // svg.selectAll(".xTitle")
    //     .transition()
    //     .duration(DURATION * 0.5)
    //         .style("opacity", 0)
    //         .remove()



    /***************************************
    ***** Y AXIS, AXIS LABEL, GRIDLINE *****
    ***************************************/

    svg.select(".yAxis")
        .transition()
        .duration(DURATION)
            .attr("transform", `translate(${margin.left}, ${margin.top})`)
            .call(d3.axisLeft(yScale)
                .tickSize(3)
                .tickPadding(0))
            .style("text-anchor", "end")
            .style("alignment-baseline", "middle")
            //.style("font-weight", "bold")
            .style("font-family", "sans-serif")
            .style("font-size", 12);

    // svg.select(".yGrid")
    //     .attr("transform", `translate(${margin.left}, ${margin.top})`) //+ 1 * yScale.bandwidth()
    //     .call(d3.axisLeft(yScale)
    //         .tickSize(-(plotWidth))
    //         .tickFormat("")
    //     );

    /***************************************
    ***** LINES , CIRCLES, RECTS *****
    ***************************************/

    // Append g to hold lines
    var plot = svg.select("#plot")
        .attr("transform", `translate(${margin.left}, ${margin.top})`);

    // plot.selectAll(".rectLongNoClick, .rectLongYesClick, .rectLongGap")
    //     .transition("width0")
    //     .duration(DURATION * 0.5)
    //         .style("opacity", 0)
    //         //.remove();

    // plot.selectAll(".rectLongNoClick")
    //     .transition()
    //     .duration(DURATION)
    //         .remove()

    // plot.selectAll(".rectLongYesClick")
    //     .transition()
    //     .duration(DURATION)
    //         .remove()

    // plot.selectAll(".rectLongGap")
    //     .transition()
    //     .duration(DURATION)
    //         .remove()
    
    plot.selectAll(".rectLongNoClick2")
        .transition()
        .duration(DURATION)
            .style("opacity", 0.3)

    plot.selectAll(".rectLongYesClick2")
        .transition()
        .duration(DURATION)
            .style("opacity", 0.3)

    plot.selectAll(".rectLongGap2")
        .transition()
        .duration(DURATION)
            .style("opacity", 1)

    


    // plot.append("line")
    //     .attr("class", "lineAverageNoClick")
    //     .attr("x1", xScale(537.5))
    //     .attr("x2", xScale(537.5))
    //     .attr("y1", 0)
    //     .attr("y2", plotHeight)
    //     .attr("stroke", "green")
    //     .attr("stroke-width", "2px")
    //     .attr("stroke-opacity", 0)
    //     .lower()
    //     .transition()
    //     .delay(DURATION * 3.5)
    //     .duration(DURATION)
    //         .attr("stroke-opacity", 1)
            

    //  plot.append("line")
    //     .attr("class", "lineAverageYesClick")
    //     .attr("x1", xScale(502.1))
    //     .attr("x2", xScale(502.1))
    //     .attr("y1", 0)
    //     .attr("y2", plotHeight)
    //     .attr("stroke", "red")
    //     .attr("stroke-width", "2px")
    //     .attr("stroke-opacity", 0)
    //     .lower()
    //     .transition()
    //     .delay(DURATION * 3)
    //     .duration(DURATION)
    //         .attr("stroke-opacity", 1)


    // // Annotations for lines
    // const type = d3.annotationCalloutCurve

    // const annotationsNoClicks = [
    //   {
    //     note: {
    //       label: "Average score for students who did not click on ads"
    //       //bgPadding: 20,
    //       //title: "Annotation title"
    //     },
    //     color:"green",
    //     x: xScale(537.5),
    //     y: yScale("United Arab Emirates"),
    //     dy: -50,
    //     dx: 100,
    //     connector: {
    //       points: 1
    //     }
    //   }
    // ]

    // // Add annotation to the chart
    // const makeAnnotationsNoClicks = d3.annotation()
    //     //.editMode(true)
    //     //.notePadding(15)
    //     .type(type)
    //     .annotations(annotationsNoClicks)


    // d3.select("#plot")
    //   .append("g")
    //   .call(makeAnnotationsNoClicks)
    //   .attr("opacity", 0)
    //   .transition()
    //     .delay(DURATION * 3.5)
    //     .duration(DURATION)
    //         .attr("opacity", 1)





    // const annotationsYesClicks = [
    //   {
    //     note: {
    //       label: "Average score for students who clicked on ads"
    //       //bgPadding: 20,
    //       //title: "Annotation title"
    //     },
    //     color:"red",
    //     x: xScale(502.1),
    //     y: yScale("Abu Dhabi, UAE"),
    //     dy: -50,
    //     dx: 100 + (537.5 - 502.1),
    //     connector: {
    //       points: 1
    //     }
    //   }
    // ]

    // // Add annotation to the chart
    // const makeAnnotationsYesClicks = d3.annotation()
    //     //.editMode(true)
    //     //.notePadding(15)
    //     .type(type)
    //     .annotations(annotationsYesClicks)
    
    // d3.select("#plot")
    //   .append("g")
    //   .call(makeAnnotationsYesClicks)
    //   .attr("opacity", 0)
    //   .transition()
    //     .delay(DURATION * 3)
    //     .duration(DURATION)
    //         .attr("opacity", 1)
    

    
    /*************************
    ***** TITLE, CAPTION *****
    *************************/

    // Create header grouping
    const header = svg.select("#header");

    // chart title
    header.selectAll(".chartTitle")
        .data([{"label": "Pecent of students who clicked on ads by education system"}])
        .text(function(d) {return d.label;})


    // Create footer grouping
    const footer = svg.select("#footer");

    // Caption with data source
    footer.selectAll(".captionText")
        .data([{"label": "Data source: ePIRLS 2016 data"}])
        .text(function(d) {return d.label;})


    /*********************
    ***** HIDE BOX V3*****
    **********************/

    d3.selectAll(".highlightBoxV3")
        .transition()
        .duration(0.5 * DURATION)
        .style("stroke-opacity", 0)    


    /****************
    ***** LOOP  *****
    *****************/
    // var i;
    // for (i = 0; i < moduleVars.length; i ++){
    //     /***********************
    //     ***** X & Y SCALES *****
    //     ***********************/
    //     // console.log(i)
    //     // console.log([xMin, xMax])
    //     // console.log([smallMultipleWidth * i + smallMultiplePadding, smallMultipleWidth * (i+1)])
    //     let xScale = d3.scaleLinear()
    //         .domain([xMin, xMax])
    //         .range([smallMultipleWidth * i + smallMultiplePadding, smallMultipleWidth * (i+1)]);

        /***************************************
        ***** X AXIS, AXIS LABEL, GRIDLINE *****
        ***************************************/

        // svg.select(".xAxis".concat(i))
        //     .transition()
        //     .duration(DURATION)
        //     .attr("transform", `translate(${margin.left}, ${plotHeight + margin.top})`)
        //     .call(d3.axisBottom(xScale)
        //         .ticks(4)
        //         //.tickFormat(d3.format("d"))
        //     );

        // svg.select(".xGrid".concat(i))
        //     .transition()
        //     .duration(DURATION)
        //     .attr("transform", `translate(${margin.left}, ${margin.top})`)
        //     .call(d3.axisBottom(xScale)
        //         .tickSize(plotHeight)
        //         .ticks(3)
        //         .tickFormat("")
        //     );





        



    //     *******************************
    //     ***** LINES , CIRCLES, RECTS*****
    //     ********************************

        // // Append g to hold lines
        // var plot = svg.select("#plot")
        //     .attr("transform", `translate(${margin.left}, ${margin.top})`);


        // let filteredData = data.filter(d => d["Var"] === Object.keys(moduleVars[i])[0]);

        // console.log(filteredData)

        // plot.selectAll(".rect".concat(i))
        //     .data(filteredData)
        //     .enter() 
        //     .append("rect")
        //     .attr("class", "rect".concat(i))
        //     .attr("x", d => xScale(0))
        //     .attr("y", d => yScale(d[yGroup]))
        //     .attr("height", yScale.bandwidth())
        //     .style("fill", "blue")
        //     .attr("width", 0)
        //     .transition()
        //     .delay(DURATION)//wait for the removal to happen first
        //     .duration(DURATION)
        //         .attr("width", function(d){
        //             return xScale(d["PCT"]) - xScale(0)
        //         });

        // plot.selectAll(".line".concat(i))
        //     .transition()
        //     .duration(DURATION)
        //         .style("opacity", 0)
        //         .remove();

        // plot.selectAll(".circleLeft".concat(i))
        //     .transition()
        //     .duration(DURATION)
        //         .style("opacity", 0)
        //         .remove();

        // plot.selectAll(".circleRight".concat(i))
        //     .transition()
        //     .duration(DURATION)
        //         .style("opacity", 0)
        //         .remove();        

    //}

    


}
