function drawTwoBarsWithWaterFallsB(data, response) {
    
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

    /******************
    ***** TOOLTIP *****
    *******************/
    var div = d3.select(".tooltipBarChart")
    .style("opacity", 0)

    /***************************************
    ***** LINES , CIRCLES, RECTS *****
    ***************************************/

    // Append g to hold lines
    var plot = svg.select("#plot")
        .attr("transform", `translate(${margin.left}, ${margin.top})`);

    plot.selectAll(".rectLongNoClick, .rectLongYesClick, .rectLongGap")
        .transition("width0")
        .duration(DURATION * 0.5)
            .style("opacity", 0)
            .remove();

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
    if (response.direction === "down") {
        plot.selectAll(".rectLongNoClick2")
            .data(data)
            .enter() 
            .append("rect")
            .attr("class", "rectLongNoClick2")
            .attr("x", d => xScale(0))
            .attr("y", d => yScale(d[yGroup]) - yScale.bandwidth()*0.4)
            .attr("height", yScale.bandwidth()*0.8)
            .style("fill", "aqua")
            .style("opacity", 1)
            .attr("width", 0)
            .transition("width1")
            .delay(DURATION * 0.5)//wait for the removal to happen first
            .duration(DURATION)
                .attr("width", function(d){
                    return xScale(d["Coef_startToLogout_noClick_min"]) - xScale(0)
                });

        plot.selectAll(".rectLongNoClick2")
        .on("mouseenter", function(d) {
            d3.select(this)
                .style("fill", "#F24D29")
            
            div.style("opacity", 1)
                //.text([d["Min"]])
                .html(d3.format(".1f")(d["Coef_startToLogout_noClick_min"]))
                .style("left", (xScale(d["Coef_startToLogout_noClick_min"]) + 142) + "px")
                .style("top", (yScale(d[yGroup]) - yScale.bandwidth()*0.4 + 38) + "px")
            })              
        .on("mouseleave", function(d) { 
            d3.select(this)
                .style("fill", "aqua")
            div.style("opacity", 0)
            })  

        plot.selectAll(".rectLongYesClick2")
            .data(data)
            .enter() 
            .append("rect")
            .attr("class", "rectLongYesClick2")
            .attr("x", d => xScale(0))
            .attr("y", d => yScale(d[yGroup]) + yScale.bandwidth()*0.4)
            .attr("height", yScale.bandwidth()*0.8)
            .style("fill", "khaki")
            .style("opacity", 1)
            .attr("width", 0)
            .transition("width1")
            .delay(DURATION * 0.5)//wait for the removal to happen first
            .duration(DURATION)
                .attr("width", function(d){
                    return xScale(d["Coef_startToLogout_yesClick_min"]) - xScale(0)
                });

        plot.selectAll(".rectLongYesClick2")
        .on("mouseenter", function(d) {
            d3.select(this)
                .style("fill", "#F24D29")
            
            div.style("opacity", 1)
                //.text([d["Min"]])
                .html(d3.format(".1f")(d["Coef_startToLogout_yesClick_min"]))
                .style("left", (xScale(d["Coef_startToLogout_yesClick_min"]) + 142) + "px")
                .style("top", (yScale(d[yGroup]) + yScale.bandwidth()*0.4 + 38) + "px")
            })              
        .on("mouseleave", function(d) { 
            d3.select(this)
                .style("fill", "khaki")
            div.style("opacity", 0)
            }) 

        plot.selectAll(".rectLongGap2")
            .data(data)
            .enter() 
            .append("rect")
            .attr("class", "rectLongGap2")
            .attr("x", function(d){
                if(d["Coef_startToLogout_gap_min"] < 0){
                    return xScale(d["Coef_startToLogout_yesClick_min"])
                } else {
                    return xScale(d["Coef_startToLogout_noClick_min"])
                }
            })
            .attr("y", function(d){
                if(d["Coef_startToLogout_gap_min"] < 0){
                    return yScale(d[yGroup]) + yScale.bandwidth()*0.4
                } else {
                    return yScale(d[yGroup]) - yScale.bandwidth()*0.4
                }
            })
            .attr("height", yScale.bandwidth()*0.8)
            .style("fill", "indianred")
            .attr("width", function(d){
                return xScale(Math.abs(d["Coef_startToLogout_gap_min"])) - xScale(0)
            })
            .style("opacity", 0)


        plot.selectAll(".rectLongGap2")
        .on("mouseenter", function(d) {
            // d3.select(this)
            //     .style("fill", "#F24D29")
            
            div.style("opacity", 0)
                //.text([d["Min"]])
                .html(d3.format(".1f")(Math.abs(d["Coef_startToLogout_gap_min"])))
                .style("left", function(d){
                    if(d["Coef_startToLogout_gap_min"] < 0){
                        return (xScale(d["Coef_startToLogout_yesClick_min"]) + 142 + "px")
                    } else {
                        return (xScale(d["Coef_startToLogout_noClick_min"]) + 142 + "px")
                    }
                })
                .style("top", function(d){
                    if(d["Coef_startToLogout_gap_min"] < 0){
                        return ((yScale(d[yGroup]) + yScale.bandwidth()*0.4 + 38) + "px")
                    } else {
                        return ((yScale(d[yGroup]) - yScale.bandwidth()*0.4 + 38) + "px")
                    }
                })
            })              
        .on("mouseleave", function(d) { 
            d3.select(this)
                .style("fill", "indianred")
            div.style("opacity", 0)
            }) 
     } else {

            plot.selectAll(".rectLongNoClick2, .rectLongYesClick2")
                .transition()
                .duration(DURATION)
                    .style("opacity", 1)

             plot.selectAll(".rectLongGap2")
                .transition()
                .duration(DURATION)
                    .style("opacity", 0)
    }

    
    

    
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




    


}
