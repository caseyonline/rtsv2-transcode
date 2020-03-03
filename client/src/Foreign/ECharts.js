var echarts = require("echarts");

(function () {

    require("echarts/lib/chart/bar");
    require("echarts/lib/chart/line");
    require("echarts/lib/chart/lines");
    require("echarts/lib/chart/map");
    require("echarts/lib/chart/scatter");
    require("echarts/lib/component/markLine");
    require("echarts/lib/component/markPoint");
    require("echarts/map/js/world");
    require("echarts/dist/extension/bmap.min.js");
    require("echarts/dist/extension/dataTool.min.js");

})();

exports.makeChart_ = function(node) {
  return function() {
      return echarts.init(node);
  };
};


exports.makeBlankMap_ = function(chart) {
  return function() {
      return chart.setOption(blankMap())
  };
};


exports.setOption_ = function(option) {
  return function(chart) {
    return function() {
        return chart.setOption(dashboardChart(option.scatterData))
    };
  };
};

exports.setOptionPoP_ = function(option) {
  return function(chart) {
    return function() {
        return chart.setOption(popChart(option))
    };
  };
};

exports.setClick_ = function(option) {
  return function(chart) {
    return function() {
        return chart.on('click', 'series.scatter', function (opts) {
           window.location.href = option.curHost + option.url + opts.name
        });
    };
  };
};


exports.ressizeObserver_ = function(chart) {
    var ro = new ResizeObserver(function() {
        chart.resize()
    });

    // Observe `.section` for a resize
    return function(){
      return ro.observe(document.querySelector('.section'));
    }
};


var geoCoordMap = {
    "Amsterdam": [4.895168,52.370216],
    "Athens": [-83.357567,33.951935],
    "Auckland": [174.763332,-36.84846],
    "Bangkok": [100.501762,13.756331],
    "Bangkok": [100.501765,13.756331],
    "Barcelona": [2.173404,41.385063],
    "Beijing": [116.407395,39.904211],
    "Berlin": [13.404954,52.520007],
    "Bogotá": [-74.072092,4.710989],
    "Bratislava": [17.107748,48.148596],
    "Brussels": [4.35171,50.85034],
    "Bucharest": [26.102538,44.426767],
    "Budapest": [19.040235,47.497912],
    "Buenos Aires": [-58.381559,-34.603684],
    "Cairo": [31.235712,30.04442],
    "Cape Town": [18.424055,-33.924870],
    "Caracas": [-66.903606,10.480594],
    "Chicago": [-87.629798,41.878114],
    "Copenhagen": [12.568337,55.676097],
    "Dalas": [-96.796989,32.776665],
    "Delhi": [77.209021,28.613939],
    "Doha": [51.53104,25.285447],
    "Dubai": [55.292679,25.267410],
    "Dublin": [-6.26031,53.349805],
    "Edinburgh": [-3.187550,55.950191],
    "Frankfurt": [8.682127,50.110922],
    "Geneva": [6.143158,46.204391],
    "Helsinki": [24.938379,60.169856],
    "Hong Kong": [114.109497,22.396428],
    "IAD Washington": [-77.039851, 38.877270],
    "Istanbul": [28.978359,41.008238],
    "Jakarta": [106.845599,-6.208763],
    "Johannesburg": [28.047305,-26.204103],
    "Kiev": [30.5234,50.4501],
    "Kuala Lumpur": [101.686855,3.139003],
    "Lima": [-77.042793,-12.046374],
    "Lisbon": [-9.139337,38.722252],
    "Ljubljana": [14.505751,46.056947],
    "London": [-0.127758,51.507351],
    "Los Angeles": [-118.243685,34.052234],
    "Luxembourg": [6.129583,49.815273],
    "Lyon": [4.835659,45.764043],
    "Madrid": [-3.70379,40.416775],
    "Manama": [50.58605,26.228516],
    "Manila": [120.984219,14.599512],
    "Mexico City": [-99.133208,19.432608],
    "Miami": [-80.19179,25.76168],
    "Milan": [9.185924,45.465422],
    "Montreal": [-73.567256,45.501689],
    "Moscow": [37.6173,55.755826],
    "Mumbai": [72.877656,19.075984],
    "Munich": [11.581981,48.135125],
    "Nairobi": [36.821946,-1.292066],
    "New York": [-74.005941,40.712784],
    "Nicosia": [33.382276,35.185566],
    "Oslo": [10.752245,59.913869],
    "Paris": [2.352222,48.856614],
    "Prague": [14.4378,50.075538],
    "Riga": [24.105186,56.949649],
    "Rio de Janeiro": [-43.172896,-22.906847],
    "Rome": [12.496366,41.902783],
    "San Diego": [-117.161087,32.715736],
    "Santiago de Chile": [-70.669265,-33.44889],
    "Seoul": [126.977969,37.566535],
    "Shanghai": [121.473701,31.230416],
    "Singapore": [103.819836,1.352083],
    "Sofia": [23.321868,42.697708],
    "Stockholm": [18.068581,59.329323],
    "Sydney": [151.209296,-33.86882],
    "São Paulo": [-46.633309,-23.55052],
    "Taipei": [121.565418,25.032969],
    "Tallinn": [24.753575,59.436961],
    "Tel Aviv": [34.781768,32.0853],
    "Tokyo": [139.691706,35.689487],
    "Toronto": [-79.383184,43.653226],
    "Vienna": [16.373819,48.208174],
    "Vilnius": [25.279651,54.687156],
    "Warsaw": [21.012229,52.229676],
    "DIA": [-77.0363700, 38.8951100],
    "Zurich": [8.541694,47.376887],
}

function blankMap() {
    return {
        legend: {
            orient: 'vertical',
            x:'left',
            y:'bottom',
            data:[],
            selectedMode: 'single',
            selected: {},
        textStyle : { color: '#fff' },
        },
        tooltip: {},
        geo: {
            map: 'world',
            silent: true,
            roam: true,
            scaleLimit: {min: 1.25},
            zoom: 1.25,
            label: {
                show: false,
                color: 'rgba(0,0,0,0.4)'
            },
            itemStyle: {
                borderColor:'#9ea1ae',
                borderWidth:1,
                areaStyle:{
                    color: '#1b1b1b'
                }
            },
        },
        series : []
    };
}



function dashboardChart(scatterData) {
    console.log(scatterData)
    var schema = [
        {name: 'location', index: 0, text: 'location'},
        {name: 'health', index: 1, text: 'Health'},
        {name: 'popNum', index: 2, text: 'Number pops'},
        {name: 'streams', index: 3, text: 'Number streams'},
    ];

    return {
    legend: {
        orient: 'vertical',
        x:'left',
        y:'bottom',
        data:['Journey Primary', 'Journey Secondary'],
        selectedMode: 'single',
        selected:{
            'Journey Primary' : false,
            'Journey Secondary' : false
        },
        textStyle : {
            color: '#fff'
        },
    },
    tooltip: {
        padding: 10,
        backgroundColor: '#222',
        borderColor: '#777',
        borderWidth: 1,
        formatter: function (obj) {
            var data = obj.data
            var value = obj.value;
            if (obj.seriesType == "lines") {
                return '<div style="border-bottom: 1px solid rgba(255,255,255,.3); font-size: 18px;padding-bottom: 7px;margin-bottom: 7px">'
                + data.fromName + " > " + data.toName
                + '</div>'
                    + 'Time' + '：' + data.data[0] + 'ms' + '<br>'
                    + 'Total' + '：' + data.data[1] + 'ms' + '<br>'
            } else {

            return '<div style="border-bottom: 1px solid rgba(255,255,255,.3); font-size: 18px;padding-bottom: 7px;margin-bottom: 7px">'
                + obj.name
                + '</div>'
                + schema[1].text + '：' + value[1] + '%' + '<br>'
                + schema[2].text + '：' + value[0] + '<br>'
                + schema[3].text + '：' + value[0] + '<br>'
            }
        }
    },
    geo: {
        map: 'world',
        silent: true,
        roam: true,
        scaleLimit: {min: 1.25},
        zoom: 1.25,
        label: {
            show: false,
            color: 'rgba(0,0,0,0.4)'
        },
        itemStyle: {
            borderColor:'#9ea1ae',
            borderWidth:0.5,
            areaStyle:{
                color: '#1b1b1b'
            }
        },
    },
    series : [
        { type: 'scatter',
          name: "locations",
          coordinateSystem: 'geo',
          data: scatterData,
          symbolSize: 15,
          animation: true,
          itemStyle: {
              color: '#269788'
          },
          emphasis: {
              label: {
                  show: false
              },
          },
          tooltip: { backgroundColor: '#269788'},
          zlevel: 5,
        },
    ]
    };
}

function popChart(scatterData) {
    console.log(scatterData[0])
    console.log(scatterData[1])
    return {
    legend: {
        orient: 'vertical',
        x:'left',
        y:'bottom',
        data:['Both','A', 'B'],
        selectedMode: 'single',
        selected:{
            'Both' : true
            
        },
        textStyle : {
            color: '#9ea1ae'
        },
    },
    tooltip: {
        padding: 10,
        backgroundColor: '#222',
        borderColor: '#777',
        borderWidth: 1,
        formatter: function (obj) {
            var data = obj.data
            var value = obj.value;
            if (obj.seriesType == "lines") {
                return '<div style="border-bottom: 1px solid rgba(255,255,255,.3); font-size: 18px;padding-bottom: 7px;margin-bottom: 7px">'
                + data.fromName + " > " + data.toName
                + '</div>'
                    + 'Time' + '：' + data.data[0] + 'ms' + '<br>'
                    + 'Total' + '：' + data.data[1] + 'ms' + '<br>'
            } else {

            return '<div style="border-bottom: 1px solid rgba(255,255,255,.3); font-size: 18px;padding-bottom: 7px;margin-bottom: 7px">'
                + obj.name
                + '</div>'
                + schema[1].text + '：' + value[1] + '%' + '<br>'
                + schema[2].text + '：' + value[0] + '<br>'
                + schema[3].text + '：' + value[0] + '<br>'
            }
        }
    },
    geo: {
        map: 'world',
        silent: true,
        roam: true,
        scaleLimit: {min: 1.25},
        zoom: 1.25,
        label: {
            show: false,
            color: 'rgba(0,0,0,0.4)'
        },
        itemStyle: {
            borderColor:'#9ea1ae',
            borderWidth:1,
            areaStyle:{
                color: '#1b1b1b'
            }
        },
    },
    series : [
         { type: 'scatter',
          name: "locations",
          coordinateSystem: 'geo',
          data: [],
          //  [
          //     {"name":"dal","value":[-96.796989,32.776665]},
          //     {"name":"iad","value":[-77.039851, 38.877270]},
          //     {"name":"lax","value":[-118.243685,34.052234]},
          //     {"name":"fra","value":[8.682127,50.110922]}
          // ],
          symbolSize: 15,
          animation: true,
          itemStyle: {
              color: '#269788'
          },
          emphasis: {
              label: {
                  show: false
              },
          },
          tooltip: { backgroundColor: '#269788'},
          zlevel: 5,
        },
        { name: "A",
          type: "lines",
          coordinateSystem: 'geo',
          label: {
              show: false
          },

          lineStyle: {
              color: '#6DBBBF',
              width: 2,
              type: "solid",
              shadowColor: "#717171",
              curveness: 0.3
          },
          effect : {
              show: true,
              scaleSize: 3,
              period: 4,
              color: '#1C3C54',
              shadowBlur: 6
          },
          zlevel: 10,

          data:scatterData[0],
          // [
          //     [ {name:'Fra', coord: [8.682127,50.110922]},
          //       {name:'Iad', coord: [-77.039851, 38.877270], data:[60, 120]},
          //     ],
          //     [ {name:'Iad', coord: [-77.039851, 38.877270]},
          //       {name:'Lax', coord: [-118.243685,34.052234], data: [60, 120]},
          //     ]
          // ],
        },
        { name: "B",
          type: "lines",
          coordinateSystem: 'geo',
          label: {
              show: false
          },

          lineStyle: {
              color: '#6DBBBF',
              width: 2,
              type: "solid",
              shadowColor: "#717171",
              curveness: 0.3
          },
          effect : {
              show: true,
              scaleSize: 3,
              period: 4,
              color: '#1C3C54',
              shadowBlur: 6
          },
           zlevel: 10,


          data: scatterData[1],
        },
        { name: "Both",
          type: "lines",
          coordinateSystem: 'geo',
          label: {
              show: false
          },

          lineStyle: {
              color: '#6DBBBF',
              width: 2,
              type: "solid",
              shadowColor: "#717171",
              curveness: 0.3
          },
          effect : {
              show: true,
              scaleSize: 3,
              period: 4,
              color: '#1C3C54',
              shadowBlur: 6
          },
          zlevel: 10,

          data: scatterData[0].concat(scatterData[1])
          // [
          //     [ {name:"Fra", coord:[8.682127,50.110922], data: [60, 120]},
          //       {name:'Dal', coord: [-96.796989,32.776665]}
          //     ],
          //     [ {name:'Dal', coord: [-96.796989,32.776665], data: [60, 120]},
          //       {name:'Lax', coord: [-118.243685,34.052234]}
          //     ],
          //     [ {name:'Fra', coord: [8.682127,50.110922]},
          //       {name:'Iad', coord: [-77.039851, 38.877270], data:[60, 120]},
          //     ],
          //     [ {name:'Iad', coord: [-77.039851, 38.877270]},
          //       {name:'Lax', coord: [-118.243685,34.052234], data: [60, 120]},
          //     ]
          // ]
        },
    ]
    };
}
