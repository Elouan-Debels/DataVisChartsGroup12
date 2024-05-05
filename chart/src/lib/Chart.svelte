<script>
  import { onMount } from "svelte";
  import { csv } from "d3-fetch";
  import { scaleLinear, scaleTime, scaleLog } from "d3-scale";
  import { min, max, extent, bisector } from "d3-array";
  import { select, selectAll } from "d3-selection";
  import { axisLeft, axisBottom } from "d3-axis";
  import _ from "lodash";
  import { format } from "d3-format";
  import {timeFormat} from "https://cdn.skypack.dev/d3-time-format@4";

  import TooltipLine from "./TooltipLine.svelte";
  import TooltipPoint from "./TooltipPoint.svelte";
  import Tooltip from "./Tooltip.svelte";

// Function to capitalize the first letter of a string
  function capitalize(s) {
    return s[0].toUpperCase() + s.slice(1);
  }

  $: console.log('timmee', timeFormat)

// Initializing variables
  let data = [];
  let colors = [
    "#DDA0DD",
    "#B0C4DE",
    "#20B2AA",
    "#168AD0",
    "#F1683C",
    "#DBDC25",
    "#8FBC8B",
    "#D2B48C",
  ];

// Initializing colors for boxes in the list  
  let boxColors = [
    "#dda0dd",
    "#b0c4de",
    "#ffc700",
    "#168ad0",
    "#f1683c",
    "#6D2932",
    "#4fcf43",
    "#c08b5c",
  ];

// Initializing formats for numbers and time  
  let formatNo = format(".1s");
  let formatTime = timeFormat("%Y")

// On mount, fetching data from a CSV file  
  onMount(async () => {
    let fetchedData = await csv("../../processed_weeks.csv", (data) => ({
      armsAndArmourQuantities: +data["ARMS & ARMOUR_quantities"],
      adventuringEquipmentQuantities: +data["ADVENTURING EQUIPMENT_quantities"],
      toolsAndKitsQuantities: +data["TOOLS & KITS_quantities"],
      PotionsAndScrollsQuantities: +data["POTIONS & SCROLLS_quantities"],
      animalsAndTransportationQuantities:
        +data["ANIMALS & TRANSPORTATION_quantities"],
      jewelryQuantities: +data["JEWELRY_quantities"],
      summoningDeviceQuantities: +data["SUMMONING DEVICE_quantities"],
      musicalInstrumentQuantities: +data["MUSICAL INSTRUMENT_quantities"],
      startDate: new Date(data["Start_Date"]),
    }));
    data = [...fetchedData];
  });


// Predefined data points for visualization
  const points = [
    { x: 1979, y: 7.19 },
    { x: 1980, y: 7.83 },
    { x: 1981, y: 7.24 },
    { x: 1982, y: 7.44 },
    { x: 1983, y: 7.51 },
    { x: 1984, y: 7.1 },
    { x: 1985, y: 6.91 },
    { x: 1986, y: 7.53 },
    { x: 1987, y: 7.47 },
    { x: 1988, y: 7.48 },
    { x: 1989, y: 7.03 },
    { x: 1990, y: 6.23 },
    { x: 1991, y: 6.54 },
    { x: 1992, y: 7.54 },
    { x: 1993, y: 6.5 },
    { x: 1994, y: 7.18 },
    { x: 1995, y: 6.12 },
    { x: 1996, y: 7.87 },
    { x: 1997, y: 6.73 },
    { x: 1998, y: 6.55 },
    { x: 1999, y: 6.23 },
    { x: 2000, y: 6.31 },
    { x: 2001, y: 6.74 },
    { x: 2002, y: 5.95 },
    { x: 2003, y: 6.13 },
    { x: 2004, y: 6.04 },
    { x: 2005, y: 5.56 },
    { x: 2006, y: 5.91 },
    { x: 2007, y: 4.29 },
    { x: 2008, y: 4.72 },
    { x: 2009, y: 5.38 },
    { x: 2010, y: 4.92 },
    { x: 2011, y: 4.61 },
    { x: 2012, y: 3.62 },
    { x: 2013, y: 5.35 },
    { x: 2014, y: 5.28 },
    { x: 2015, y: 4.63 },
    { x: 2016, y: 4.72 },
    { x: 2017, y: 4.82 },
    { x: 2018, y: 4.79 },
    { x: 2019, y: 4.36 },
    { x: 2020, y: 4 },
    { x: 2021, y: 4.92 },
  ];

  let attributes = [
    "armsAndArmourQuantities",
    "adventuringEquipmentQuantities",
    "toolsAndKitsQuantities",
    "PotionsAndScrollsQuantities",
    "animalsAndTransportationQuantities",
    "jewelryQuantities",
    "summoningDeviceQuantities",
    "musicalInstrumentQuantities",
  ];

// References for lines and areas in the chart  
  let ref1, ref2, ref3, ref4, ref5, ref6, ref7, ref8;
  let refLine1,
    refLine2,
    refLine3,
    refLine4,
    refLine5,
    refLine6,
    refLine7,
    refLine8;

  $: selected = "";
  let yValues = [];
// Collecting y values for each attribute
  $: for (let i = 0; i < data.length; i++) {
    yValues = [...yValues, ...Object.values(_.pick(data[i], attributes))];
    yValues = yValues;
  }

// Formatting data for visualization  
  let formattedData = [];

  $: for (let i = 0; i < data.length; i++) {
    let parent = data[i];
    let child = [];
    //console.log('parent', parent)
    //  for(let key in parent){
    let entryOne = {};
    entryOne.value = parent["armsAndArmourQuantities"];
    entryOne.item = "armsAndArmourQuantities";
    entryOne.date = parent["startDate"];

    let entryTwo = {};
    entryTwo.value = parent["adventuringEquipmentQuantities"];
    entryTwo.item = "adventuringEquipmentQuantities";
    entryTwo.date = parent["startDate"];

    let entryThree = {};
    entryThree.value = parent["toolsAndKitsQuantities"];
    entryThree.item = "toolsAndKitsQuantities";
    entryThree.date = parent["startDate"];

    let entryFour = {};
    entryFour.value = parent["PotionsAndScrollsQuantities"];
    entryFour.item = "PotionsAndScrollsQuantities";
    entryFour.date = parent["startDate"];

    let entryFive = {};
    entryFive.value = parent["animalsAndTransportationQuantities"];
    entryFive.item = "animalsAndTransportationQuantities";
    entryFive.date = parent["startDate"];

    let entrySix = {};
    entrySix.value = parent["jewelryQuantities"];
    entrySix.item = "jewelryQuantities";
    entrySix.date = parent["startDate"];

    let entrySeven = {};
    entrySeven.value = parent["summoningDeviceQuantities"];
    entrySeven.item = "summoningDeviceQuantities";
    entrySeven.date = parent["startDate"];

    let entryEight = {};
    entryEight.value = parent["musicalInstrumentQuantities"];
    entryEight.item = "musicalInstrumentQuantities";
    entryEight.date = parent["startDate"];

    child = [
      ...child,
      entryOne,
      entryTwo,
      entryThree,
      entryFour,
      entryFive,
      entrySix,
      entrySeven,
      entryEight,
    ];
    //}
    formattedData = [...formattedData, ...child];
  }

  let margin = { top: 25, right: 10, bottom: 75, left: 30 };

  $: xScale1 = scaleTime()
    .domain([min(data, (d) => d.startDate), max(data, (d) => d.startDate)])
    .range([margin.left, width - margin.right]);

  $: yScale1 = scaleLinear()
    .domain([0, max(data, (d) => d.armsAndArmourQuantities)])
    .range([height - margin.bottom, margin.top]);

  let xAxis;
  let yAxis;

  $: {
    select(yAxis)
      .call(axisLeft(yScale1).tickFormat(formatNo))
      .selectAll(".tick > text")
      .style("fill", "white")
      .style("font-size", "1.25em");

    select(yAxis).select("path").style("stroke", "white");
    select(yAxis).selectAll("line").style("stroke", "white");

    select(xAxis)
      .call(axisBottom(xScale1))
      .selectAll(".tick > text")
      .attr("y", 0)
      .attr("dy", "1.85em")
      .attr("text-anchor", "center")
      .style("fill", "white")
      .style("font-size", "1.25em");

    select(xAxis).select("path").style("stroke", "white");
    select(xAxis).selectAll("line").style("stroke", "white");

    select(yAxis)
      .select("text.yLabel").remove()

    select(yAxis)
      .append("text")
      .attr("class", "yLabel")
      .attr("text-anchor", "end")
      .attr("y", 16)
      .attr("dx", "-3.25em")
      .attr("dy", "-4.25em")
      .attr("transform", "rotate(-90)")
      .text("Sales Volume")
      .style("fill", "white")
      .style("font-size", "1.25em");

      select(xAxis)
      .select("text.xLabel").remove();

    select(xAxis)
      .append("text")
      .attr("class", "xLabel")
      .attr("text-anchor", "end")
      .attr("x", width)
      .attr("y", 20)
      .attr("dx", "-3.25em")
      .attr("dy", "1.5px")
      .text("Time")
      .style("fill", "white")
      .style("font-size", "1.25em");
  }

  let width = 800;
  let height = 400;

  $: maxValue = max(data, (d) => d.startDate);
  $: minValue = min(data, (d) => d.startDate);

  $: path1 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.armsAndArmourQuantities)}`).join("L")}`;
  $: area1 = `${path1}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path2 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.adventuringEquipmentQuantities)}`).join("L")}`;
  $: area2 = `${path2}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path3 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.toolsAndKitsQuantities)}`).join("L")}`;
  $: area3 = `${path3}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path4 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.PotionsAndScrollsQuantities)}`).join("L")}`;
  $: area4 = `${path4}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path5 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.animalsAndTransportationQuantities)}`).join("L")}`;
  $: area5 = `${path5}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path6 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.jewelryQuantities)}`).join("L")}`;
  $: area6 = `${path6}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path7 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.summoningDeviceQuantities)}`).join("L")}`;
  $: area7 = `${path7}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  $: path8 = `M${data.map((d) => `${xScale1(d.startDate)},${yScale1(d.musicalInstrumentQuantities)}`).join("L")}`;
  $: area8 = `${path8}L${xScale1(maxValue)},${yScale1(0)}L${xScale1(minValue)},${yScale1(0)}Z`;

  // d3's bisector function
  var bisect = bisector((d) => d.startDate).right;

  let m = { x: 0, y: 0 };
  let point = data[0];

  function handleMousemove(event) {
    m.x = event.offsetX;
    m.y = event.offsetY;

    // returns point to right of our current mouse position
    let i = bisect(data, xScale1.invert(m.x));

    if (i < data.length) {
      point = data[i]; // update point
    }
  }

  // coords for vertical tooltip line
  let vline = {};
  $: vline.y1 = 0;
  $: vline.y2 = height - margin.bottom;
  $: vline.x1 = (point && xScale1(point?.startDate)) ?? xScale1(data[0]?.startDate);
  $: vline.x2 = (point && xScale1(point?.startDate)) ?? xScale1(data[0]?.startDate);
</script>

<main bind:clientWidth={width}>
  <p class="title">Sales over time</p>
  <div class="list">
    <table>
      {#each attributes as attribute, i}
        <tr>
          <td style="padding-right: 0.42em;">
            <div style="background-color: {boxColors[i]};" class="box"></div>
          </td>
          <td>
            <p style="padding-right: 1.2em;">
              {capitalize(
                attribute
                  .split(/(?=[A-Z])/)
                  ?.join(" ")
                  .toLowerCase()
              )}
            </p></td
          >
          <td>
            <p>{(point && point[attribute]) ?? 0}</p>
          </td>
        </tr>
      {/each}
    </table>
  </div>
  <div class="chart">
    <svg role="article" on:mousemove={handleMousemove} {width} {height}>
      <path
        role="article"
        bind:this={ref1}
        on:mouseover={(e) => {
          [ref2, ref3, ref4, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "armsAndArmourQuantities";
        }}
        on:focus={(e) => (e) => {}}
        on:mouseleave={(e) => {
          [ref2, ref3, ref4, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        class="opacity path-area1"
        d={area1}
      />
      <path bind:this={refLine1} class="opacity line path-line1" d={path1} />

      <path
        role="article"
        bind:this={ref2}
        on:mouseover={(e) => {
          [ref1, ref3, ref4, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "adventuringEquipmentQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref3, ref4, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        class="opacity path-area2"
        d={area2}
      />
      <path bind:this={refLine2} class="line path-line2" d={path2} />

      <path
        bind:this={ref3}
        on:mouseover={(e) => {
          [ref1, ref2, ref4, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "toolsAndKitsQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref2, ref4, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        role="article"
        class="opacity path-area3"
        d={area3}
      />
      <path bind:this={refLine3} class="line path-line3" d={path3} />

      <path
        bind:this={ref4}
        on:mouseover={(e) => {
          [ref1, ref3, ref2, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "PotionsAndScrollsQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref3, ref2, ref5, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        role="article"
        class="opacity path-area4"
        d={area4}
      />
      <path bind:this={refLine4} class="line path-line4" d={path4} />

      <path
        bind:this={ref5}
        on:mouseover={(e) => {
          [ref1, ref3, ref2, ref4, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "animalsAndTransportationQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref3, ref2, ref4, ref6, ref7, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        role="article"
        class="opacity path-area5"
        d={area5}
      />
      <path bind:this={refLine5} class="line path-line5" d={path5} />

      <path
        bind:this={ref6}
        on:mouseover={(e) => {
          [ref1, ref3, ref2, ref5, ref4, ref7, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "jewelryQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref3, ref2, ref5, ref4, ref7, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        role="article"
        class="opacity path-area6"
        d={area6}
      />
      <path bind:this={refLine6} class="line path-line6" d={path6} />

      <path
        bind:this={ref7}
        on:mouseover={(e) => {
          [ref1, ref3, ref2, ref5, ref6, ref4, ref8].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "summoningDeviceQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref3, ref2, ref5, ref6, ref4, ref8].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        role="article"
        class="opacity path-area7"
        d={area7}
      />
      <path bind:this={refLine7} class="line path-line7" d={path7} />

      <path
        bind:this={ref8}
        on:mouseover={(e) => {
          [ref1, ref3, ref2, ref5, ref6, ref7, ref4].map(
            (ref) => (ref.style.opacity = "0.1")
          );
          selected = "musicalInstrumentQuantities";
        }}
        on:focus={(e) => {}}
        on:mouseleave={(e) => {
          [ref1, ref3, ref2, ref5, ref6, ref7, ref4].map(
            (ref) => (ref.style.opacity = "1")
          );
          selected = "";
        }}
        role="article"
        class="opacity path-area8"
        d={area8}
      />
      <path bind:this={refLine8} class="line path-line8" d={path8} />

      <g transform="translate(0, {height - margin.bottom})" bind:this={xAxis} />

      <g transform="translate({margin.left}, 0)" bind:this={yAxis} />

      <TooltipLine {vline} />
      {#each attributes as attribute, i}
        <TooltipPoint
          x={(point && xScale1(point?.startDate)) ?? xScale1(data[0]?.startDate)}
          y={yScale1(point && point[attribute])}
          selected={selected === attribute}
        />
      {/each}
    </svg>

    {#each attributes as attribute, i}
      <Tooltip
        x={xScale1(point?.startDate)}
        y={yScale1(point && point[attribute])}
        data={point && point[attribute]}
        key={attribute}
        selected={attribute === selected}
      />
    {/each}
  </div>
</main>

<style>
  main{
    position: relative;
    width: 100%;
    max-width: 800px;
  }
  .title {
    color: white;
    font-size: 1.25em;
    text-align: center;
  }
  .chart,
  h2,
  p {
    margin-left: auto;
    margin-right: auto;
    position: relative;
  }

  .line {
    stroke-linejoin: round;
    stroke-linecap: round;
    stroke-width: 2;
    fill: none;
  }

  .path-line1 {
    stroke: rgb(221, 160, 221, 0.2);
  }
  .path-area1 {
    fill: rgb(221, 160, 221, 0.6);
  }
   .path-line2 {
    stroke: rgb(176, 196, 222, 0.2);
  }
  .path-area2 {
    fill: rgb(176, 196, 222);
  }
 
  .path-line3 {
    stroke: rgb(255, 199, 0, 0.2);
  }
  .path-area3 {
    fill: rgb(255, 199, 0);
  }
  .path-line4 {
    stroke: rgb(109, 41, 50, 0.2);
  }
  .path-area4 {
    fill: rgb(109, 41, 50);
  }
  .path-line5 {
    stroke: rgb(241, 104, 60, 0.2);
  }
  .path-area5 {
    fill: rgb(241, 104, 60);
  }
  .path-line6 {
    stroke: rgb(22, 138, 208, 0.2);
  }
  .path-area6 {
    fill: rgb(22, 138, 208);
  }
  .path-line7 {
    stroke: rgb(79, 207, 67, 0.2);
  }
  .path-area7 {
    fill: rgb(79, 207, 67);
  }
  .path-line8 {
    stroke: rgb(192, 139, 92, 0.2);
  }
  .path-area8 {
    fill: rgb(192, 139, 92);
  }
  .opacity {
    opacity: 0.7;
  }
  .list {
    color: #f2f2f2;
    font-size: 0.675em;
    display: flex;
  }
  .box {
    width: 10px;
    height: 10px;
    margin-right: 2px;
  }
</style>
