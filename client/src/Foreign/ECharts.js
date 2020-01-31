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

exports.setOption_ = function(option) {
  return function(chart) {
    return function() {
        return chart.setOption(optionz)
    };
  };
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
       
var data_good = [ {"name": "Amsterdam", "value": 4}
    , {"name": "Athens", "value": 83}
    , {"name": "Auckland", "value": 74}
    , {"name": "Bangkok", "value": 100}
    , {"name": "Barcelona", "value": 2}
    //, {"name": "Beijing", "value": 90}
    , {"name": "Berlin", "value": 12}
    , {"name": "Bogotá", "value": 74}
    , {"name": "Bratislava", "value": 17}
    , {"name": "Brussels", "value": 4}
    , {"name": "Budapest", "value": 19}
    , {"name": "Buenos Aires", "value": 8}
    , {"name": "Bucharest", "value": 26}
    , {"name": "Caracas", "value": 66}
    , {"name": "Chicago", "value": 87}
    , {"name": "Dalas", "value": 77}
    , {"name": "Delhi", "value": 77}
    , {"name": "Doha", "value": 51}
    , {"name": "Dubai", "value": 55}
    , {"name": "Dublin", "value": 6}
    , {"name": "Frankfurt", "value": 8}
    //, {"name": "Geneva", "value": 6}
    , {"name": "Helsinki", "value": 24}
    , {"name": "Hong Kong", "value": 100}
    , {"name": "Istanbul", "value": 28}
    , {"name": "Jakarta", "value": 6}
    , {"name": "Johannesburg", "value": 8}
    , {"name": "Cairo", "value": 31}
    , {"name": "Kiev", "value": 30}
    , {"name": "Copenhagen", "value": 12}
    , {"name": "Kuala Lumpur", "value": 101}
    , {"name": "Lima", "value": 7.,}
    , {"name": "Lisbon", "value": 9}
    , {"name": "Ljubljana", "value": 14}
    , {"name": "London", "value": 0}
    , {"name": "Los Angeles", "value": 118}
    , {"name": "Luxembourg", "value": 6}
    , {"name": "Lyon", "value": 4}
    , {"name": "Madrid", "value": 3}
    , {"name": "Milan", "value": 9}
    , {"name": "Manama", "value": 50}
    , {"name": "Manila", "value": 120}
    , {"name": "Mexico City", "value": 99}
    , {"name": "Miami", "value": 80}
    , {"name": "Montreal", "value": 73}
    , {"name": "Moscow", "value": 37}
    , {"name": "Mumbai", "value": 72}
    , {"name": "Munich", "value": 11}
    , {"name": "Nairobi", "value": 6}
    , {"name": "New York", "value": 74}
    , {"name": "Nicosia", "value": 33}
    , {"name": "Oslo", "value": 10}
    , {"name": "Paris", "value": 2}
    , {"name": "Prague", "value": 14}
    , {"name": "Riga", "value": 24}
    //, {"name": "Rio de Janeiro", "value": 3}
    , {"name": "Rome", "value": 12}
    , {"name": "Santiago de Chile", "value": 0}
    , {"name": "São Paulo", "value": 6}
    , {"name": "Seoul", "value": 126}
    , {"name": "Shanghai", "value": 121}
    , {"name": "Singapore", "value": 103}
    , {"name": "Sofia", "value": 23}
    , {"name": "Stockholm", "value": 18}
    , {"name": "Sydney", "value": 51}
    , {"name": "Taipei", "value": 121}
    , {"name": "Tallinn", "value": 24}
    , {"name": "Tel Aviv", "value": 34}
    , {"name": "Tokyo", "value": 139}
    , {"name": "Toronto", "value": 79}
    , {"name": "Vilnius", "value": 25}
    , {"name": "Warsaw", "value": 21}
    , {"name": "DIA", "value": 21}
    , {"name": "Vienna", "value": 16}
    , {"name": "Zurich", "value": 8}

]

var data_bad = [
    , {"name": "Rio de Janeiro", "value": 3}
    , {"name": "Geneva", "value": 6}
    , {"name": "Beijing", "value": 90}
]

var data_A = [
    [{name:'Los Angeles'}, {name:'Barcelona'}],
    [{name:'Barcelona'}, {name:'London'}],
    [{name:'London'}, {name:'Stockholm'}]

]

var data_B = [
    [{name:'Los Angeles'}, {name:'Milan'}],
    [{name:'Milan'}, {name:'London'}],
    [{name:'London'}, {name:'Stockholm'}]
]

var schema = [
    {name: 'location', index: 0, text: 'location'},
    {name: 'health', index: 1, text: 'Health'},
    {name: 'popNum', index: 2, text: 'Number pops'},
    {name: 'streams', index: 3, text: 'Number streams'},
];

var poop = [ {name:'Los Angeles', "coords": [], "value": 4}
           , {name:'Barcelona', "coords": [], "value": 4}
           ]

function convertData(data, geoCoordMap) {
   var res = [];
   for (var i = 0; i < data.length; i++) {
       var geoCoord = geoCoordMap[data[i].name];
       if (geoCoord) {
           res.push({
               name: data[i].name,
               value: geoCoord.concat(data[i].value)
           });
       }
   }
   return res;
};

var optionz = {
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
                console.log(data)


                return '<div style="border-bottom: 1px solid rgba(255,255,255,.3); font-size: 18px;padding-bottom: 7px;margin-bottom: 7px">'
                + data.fromName + " > " + data.toName
                + '</div>'
                    + 'Time' + '：' + data.data[0] + 'ms' + '<br>'
                    + 'Total' + '：' + data.data[1] + 'ms' + '<br>'
                    + 'Total' + '：' + data.data[1] + 'ms' + '<br>'
            } else {

            return '<div style="border-bottom: 1px solid rgba(255,255,255,.3); font-size: 18px;padding-bottom: 7px;margin-bottom: 7px">'
                + obj.name
                + '</div>'
                + schema[1].text + '：' + value[2] + '%' + '<br>'
                + schema[2].text + '：' + value[1] + '<br>'
                + schema[3].text + '：' + value[3] + '<br>'
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
            borderColor: '#667576',
            areaColor: '#3a454e'
        },
    },
    series : [
        { type: 'scatter',
          name: "bad",
          coordinateSystem: 'geo',
          data: [ {name: "Rio de Janeiro", value:[-43.172896,-22.906847, 4]}
                , {name: "Geneva", value:[6.143158,46.204391, 1]}
                , {name: "Beijing", value: [116.407395,39.904211, 5]}
                ],
          symbolSize: 15,
          animation: true,
          itemStyle: {
              color: '#a3535c'
          },
          emphasis: {
              label: {
                  show: false
              },
          },
          tooltip: { backgroundColor: '#a3535c'},
          zlevel: 5,
        },
        { type: 'scatter',
          name: "good",
          coordinateSystem: 'geo',
          data: convertData(data_good, geoCoordMap),
          symbolSize: 15,
          symbolRotate: 0,
          itemStyle: {
              color: '#269788'
          },
          emphasis: {
              label: {
                  show: false
              }
          },
          zlevel: 3,
        },
         { name: "Journey Primary",
          type: "lines",
          coordinateSystem: 'geo',
          label: {
              show: false
          },

          lineStyle: {
              color: '#afc343',
              width: 2,
              type: "solid",
              shadowColor: "red",
              curveness: 0.3
          },
          effect : {
              show: true,
              scaleSize: 3,
              period: 4,
              color: '#fff',
              shadowBlur: 6
          },
           zlevel: 10,

          data:  [
              [ {name:'Dalas - DAL', coord: [-96.796989,32.776665], data: [30, 120]}
              , {name:'London - LCY', coord: [-0.127758,51.507351]}
              ]
            , [ {name:'London - LCY', coord: [-0.127758,51.507351], data: [30, 120]}
              , {name:"Frankfurt - FRA", coord:[8.682127,50.110922]}
              ]
          ],
        },
        { name: "Journey Secondary",
          type: "lines",
          coordinateSystem: 'geo',
          label: {
              show: false
          },

          lineStyle: {
              color: '#afc343',
              width: 2,
              type: "solid",
              shadowColor: "red",
              curveness: 0.3
          },
          effect : {
              show: true,
              scaleSize: 3,
              period: 4,
              color: '#fff',
              shadowBlur: 6
          },
           zlevel: 10,


          data:  [
              [ {name:'Dalas - DAL', coord: [-96.796989,32.776665], data: [30, 120]}
                , {name:'Washington - DIA', coord: [-77.0363700, 38.8951100]}
              ]
              , [ {name:'Washington - DIA', coord: [-77.0363700, 38.8951100], data:[60, 120]}
                  , {name:'London - LCY', coord: [-0.127758,51.507351]}
              ]
            , [ {name:'London - LCY', coord: [-0.127758,51.507351], data: [30, 120]}
                , {name:"Frankfurt - FRA", coord:[8.682127,50.110922]}
              ]
          ],
        },

   ]
};
