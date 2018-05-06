/**
 * Convert the empty string to the string 'block', otherwise return
 * `display' unmodified.
 */
function normalizeDisplay(display) {
    return display === '' ? 'block' : display;
};
/** Toggle the display of the elements given by `id-a' and `id-b'. */
function toggleContent(idA, idB, idLink) {
    var link = document.getElementById(idLink);
    var elementA = document.getElementById(idA);
    var elementB = document.getElementById(idB);
    var aDisplay = normalizeDisplay(elementA.style.display);
    var bDisplay = normalizeDisplay(elementB.style.display);
    var linkText = 'Show ' + (aDisplay == 'block' ? idA : idB);
    var state = aDisplay + ',' + bDisplay;
    if (!(state == 'block,none' || state == 'none,block')) {
        throw 'Invalid state: ' + state;
    };
    var tmp127 = elementA.style.display;
    elementA.style.display = elementB.style.display;
    elementB.style.display = tmp127;
    return link.innerHTML = linkText;
};
document.getElementById('toggle-link').addEventListener('click', function (event) {
    event.preventDefault();
    return toggleContent('toggle-jelly', 'toggle-cat', 'toggle-link');
});
if ('undefined' === typeof fetch) {
    (function () {
        for (var el = null, _js_arrvar129 = document.getElementsByClassName('fetch-warning'), _js_idx128 = 0; _js_idx128 < _js_arrvar129.length; _js_idx128 += 1) {
            el = _js_arrvar129[_js_idx128];
            el.style.display = 'block';
            el.textContent = 'Your browser does not support the fetch API. This example won\'t work.';
        };
    })();
};
function mapcar(fun) {
    var arrs = Array.prototype.slice.call(arguments, 1);
    var resultArray = new Array();
    if (1 === arrs.length) {
        for (var element = null, _js_arrvar131 = arrs[0], _js_idx130 = 0; _js_idx130 < _js_arrvar131.length; _js_idx130 += 1) {
            element = _js_arrvar131[_js_idx130];
            resultArray.push(fun(element));
        };
    } else {
        for (var i = 0; i < arrs[0].length; i += 1) {
            with ({ i : i }) {
                var argsArray = mapcar(function (a) {
                    return a[i];
                }, arrs);
                resultArray.push(fun.apply(fun, argsArray));
            };
        };
    };
    return resultArray;
};
/** Call FN on each element in ARR, replace element with the return value. */
function mapInto(fn, arr) {
    var idx = 0;
    for (var el = null, _js_idx132 = 0; _js_idx132 < arr.length; _js_idx132 += 1) {
        el = arr[_js_idx132];
        arr[idx] = fn(el);
        idx += 1;
    };
    return arr;
};
/** Call FN on each element in ARR and return the returned values in a new array. */
function map(fn, arr) {
    var idx = 0;
    var result = [];
    for (var el = null, _js_idx133 = 0; _js_idx133 < arr.length; _js_idx133 += 1) {
        el = arr[_js_idx133];
        result[idx] = fn(el);
        idx += 1;
    };
    return result;
};
/** Check if ITEM is a member of ARR. */
function member(item, arr) {
    for (var el = null, _js_idx134 = 0; _js_idx134 < arr.length; _js_idx134 += 1) {
        el = arr[_js_idx134];
        if (el === item) {
            return true;
        };
    };
    return false;
};
/** Return a new array with only those elements in ARR that are not in ARR-TO-SUB. */
function setDifference(arr, arrToSub) {
    var idx = 0;
    var result = [];
    for (var el = null, _js_idx135 = 0; _js_idx135 < arr.length; _js_idx135 += 1) {
        el = arr[_js_idx135];
        if (!member(el, arrToSub)) {
            result[idx] = el;
            idx += 1;
        };
    };
    return result;
};
function reduce(func, list, init) {
    var acc = null;
    for (var i = init ? -1 : 0, acc = init ? init : list[0]; i < list.length - 1; i += 1, acc = func(acc, list[i])) {
    };
    return acc;
};
function nconc(arr) {
    var arrs = Array.prototype.slice.call(arguments, 1);
    if (arr && arr.length > 0) {
        var _js137 = arrs.length;
        for (var _js136 = 0; _js136 < _js137; _js136 += 1) {
            var other = arrs[_js136];
            if (other && other.length > 0) {
                arr['splice']['apply'](arr, [arr.length, other.length].concat(other));
            };
        };
    };
    return arr;
};;
/** Return the unmodified arguments array. */
function array() {
    var args = Array.prototype.slice.call(arguments, 0);
    return args;
};
/** Bind the first N `args' of `fn'. */
function curry(fn) {
    var curriedArgs = Array.prototype.slice.call(arguments, 1);
    return function () {
        var restArgs = Array.prototype.slice.call(arguments, 0);
        return fn.apply(this, curriedArgs.concat(restArgs));
    };
};
/** Reverse the order of arguments for 2-ary function `fn'. */
function flip(fn) {
    return function (a, b) {
        return fn(b, a);
    };
};
/** Numerically sort `table' on `key' in descending order. */
function numericSortBybang(key, table) {
    return table.sort(function (a, b) {
        return +b[key] - +a[key];
    });
};
/** Take the first `n' items from `array'. */
function take(n, array) {
    return array.slice(0, n);
};
function zip() {
    var args = Array.prototype.slice.call(arguments, 0);
    return mapcar.apply(this, [array].concat(args));
};
/**
 * Construct an Object from `key-value-pairs'.
 * 
 * Example:
 * 
 * (make-object-from ([] (a 1) (b 2) (c 3)))
 * -> {a: 1, b: 2, c: 3}
 */
function makeObjectFrom(keyValuePairs) {
    var o = {  };
    for (var kv = null, _js_idx138 = 0; _js_idx138 < keyValuePairs.length; _js_idx138 += 1) {
        kv = keyValuePairs[_js_idx138];
        o[kv[0]] = kv[1];
    };
    return o;
};
/** Fetch `url' and return the response body as text. */
function fetchText(url) {
    return fetch(url).then(function (response) {
        return response.text();
    })['catch'](function (error) {
        throw error;
    });
};
/**
 * Parse `csv-string' into an array of objects.
 * 
 * The first line of the `csv-string' is expected to contain the
 * header. Each line of the csv file is then parsed into an object with
 * properties taken from the header line.
 * 
 * Values for columns in `numeric-colums' are stored as numbers, rather
 * than strings.
 * 
 * Example:
 * (parse-csv "a,b,cn1,2,3n4,5,6" '("a" "b"))
 * -> [{a: 1, b: 2, c: "3"}, {a: 4, b: 5, c: "6"}]
 */
function parseCsv(csvString, numericColumns) {
    if (numericColumns === undefined) {
        numericColumns = [];
    };
    var convert = function (alist) {
        return map(function (kv) {
            var key = kv[0];
            var value = kv[1];
            return member(key, numericColumns) ? [key, +value] : kv;
        }, alist);
    };
    var lines = csvString.split('\n');
    var columns = lines.shift().split(',');
    return map(function (line) {
        return makeObjectFrom(convert(zip(columns, line.split(','))));
    }, lines);
};
var SVGNS = 'http://www.w3.org/2000/svg';
function makePlotTitle(title) {
    var _js140 = arguments.length;
    for (var n139 = 1; n139 < _js140; n139 += 2) {
        switch (arguments[n139]) {
        case 'fill':
            fill = arguments[n139 + 1];
            break;
        case 'font-size':
            fontSize = arguments[n139 + 1];
            break;
        case 'text-anchor':
            textAnchor = arguments[n139 + 1];
            break;
        case 'x':
            x = arguments[n139 + 1];
            break;
        case 'y':
            y = arguments[n139 + 1];
        };
    };
    var fill = 'undefined' === typeof fill ? 'grey' : fill;
    var fontSize = 'undefined' === typeof fontSize ? '10pt' : fontSize;
    var textAnchor = 'undefined' === typeof textAnchor ? 'middle' : textAnchor;
    var x = 'undefined' === typeof x ? 0 : x;
    var y = 'undefined' === typeof y ? 0 : y;
    var plotTitle = document.createElementNS(SVGNS, 'text');
    plotTitle.id = 'plot-title';
    plotTitle.textContent = title;
    plotTitle.style.fontSize = fontSize;
    plotTitle.setAttribute('x', x);
    plotTitle.setAttribute('y', y);
    plotTitle.setAttribute('fill', fill);
    plotTitle.setAttribute('text-anchor', textAnchor);
    return plotTitle;
};
function makeYLabel(label) {
    var _js142 = arguments.length;
    for (var n141 = 1; n141 < _js142; n141 += 2) {
        switch (arguments[n141]) {
        case 'fill':
            fill = arguments[n141 + 1];
            break;
        case 'font-size':
            fontSize = arguments[n141 + 1];
            break;
        case 'text-anchor':
            textAnchor = arguments[n141 + 1];
            break;
        case 'x':
            x = arguments[n141 + 1];
            break;
        case 'y':
            y = arguments[n141 + 1];
        };
    };
    var fill = 'undefined' === typeof fill ? 'grey' : fill;
    var fontSize = 'undefined' === typeof fontSize ? '8pt' : fontSize;
    var textAnchor = 'undefined' === typeof textAnchor ? 'middle' : textAnchor;
    var x = 'undefined' === typeof x ? 0 : x;
    var y = 'undefined' === typeof y ? 0 : y;
    var yLabel = document.createElementNS(SVGNS, 'text');
    yLabel.textContent = label;
    yLabel.style.fontSize = fontSize;
    yLabel.setAttribute('x', x);
    yLabel.setAttribute('y', y);
    yLabel.setAttribute('fill', fill);
    yLabel.setAttribute('text-anchor', textAnchor);
    yLabel.setAttribute('transform', 'rotate(-90)');
    return yLabel;
};
function makeYAxis() {
    var _js144 = arguments.length;
    for (var n143 = 0; n143 < _js144; n143 += 2) {
        switch (arguments[n143]) {
        case 'ax-offset':
            axOffset = arguments[n143 + 1];
            break;
        case 'ax-stroke':
            axStroke = arguments[n143 + 1];
            break;
        case 'ax-stroke-width':
            axStrokeWidth = arguments[n143 + 1];
            break;
        case 'grid-stroke':
            gridStroke = arguments[n143 + 1];
            break;
        case 'grid-stroke-width':
            gridStrokeWidth = arguments[n143 + 1];
            break;
        case 'label-fill':
            labelFill = arguments[n143 + 1];
            break;
        case 'label-font-size':
            labelFontSize = arguments[n143 + 1];
            break;
        case 'n-ticks':
            nTicks = arguments[n143 + 1];
            break;
        case 'tick-length':
            tickLength = arguments[n143 + 1];
            break;
        case 'tick-scale':
            tickScale = arguments[n143 + 1];
            break;
        case 'width':
            width = arguments[n143 + 1];
            break;
        case 'height':
            height = arguments[n143 + 1];
        };
    };
    var axOffset = 'undefined' === typeof axOffset ? 50 : axOffset;
    var axStroke = 'undefined' === typeof axStroke ? 'lightgrey' : axStroke;
    var axStrokeWidth = 'undefined' === typeof axStrokeWidth ? '1px' : axStrokeWidth;
    var gridStroke = 'undefined' === typeof gridStroke ? axStroke : gridStroke;
    var gridStrokeWidth = 'undefined' === typeof gridStrokeWidth ? axStrokeWidth / 4 : gridStrokeWidth;
    var labelFill = 'undefined' === typeof labelFill ? 'grey' : labelFill;
    var labelFontSize = 'undefined' === typeof labelFontSize ? '4pt' : labelFontSize;
    var nTicks = 'undefined' === typeof nTicks ? 4 : nTicks;
    var tickLength = 'undefined' === typeof tickLength ? 4 : tickLength;
    var tickScale = 'undefined' === typeof tickScale ? 1.0 : tickScale;
    var width = 'undefined' === typeof width ? 500 : width;
    var height = 'undefined' === typeof height ? 100 : height;
    var svgElement = document.createElementNS(SVGNS, 'svg');
    var axG = document.createElementNS(SVGNS, 'g');
    var labelsG = document.createElementNS(SVGNS, 'g');
    var gridG = document.createElementNS(SVGNS, 'g');
    var yAxis = document.createElementNS(SVGNS, 'line');
    var halfTickLength = tickLength / 2;
    var axX = axOffset - halfTickLength;
    svgElement.setAttribute('width', width);
    svgElement.setAttribute('height', height);
    axG.setAttribute('stroke', axStroke);
    axG.setAttribute('stroke-width', axStrokeWidth);
    gridG.setAttribute('stroke', axStroke);
    gridG.setAttribute('stroke-width', '0.25px');
    labelsG.setAttribute('fill', labelFill);
    labelsG.setAttribute('text-anchor', 'end');
    labelsG.style.fontSize = labelFontSize;
    yAxis.setAttribute('x1', axX);
    yAxis.setAttribute('x2', axX);
    yAxis.setAttribute('y1', 0);
    yAxis.setAttribute('y2', height);
    axG.appendChild(yAxis);
    for (var tickInterval = height / (nTicks + 1), tt = tickInterval; tt < height; tt += tickInterval) {
        var tick = document.createElementNS(SVGNS, 'line');
        var gridLine = document.createElementNS(SVGNS, 'line');
        var tickLabel = document.createElementNS(SVGNS, 'text');
        tick.setAttribute('x1', axX - halfTickLength);
        tick.setAttribute('x2', axX + halfTickLength);
        tick.setAttribute('y1', tt);
        tick.setAttribute('y2', tt);
        gridLine.setAttribute('x1', axX + halfTickLength);
        gridLine.setAttribute('x2', width);
        gridLine.setAttribute('y1', tt);
        gridLine.setAttribute('y2', tt);
        tickLabel.setAttribute('x', axX - (halfTickLength + 1));
        tickLabel.setAttribute('y', tt + 2);
        tickLabel.textContent = ((height - tt) * tickScale).toFixed().toString();
        axG.appendChild(tick);
        gridG.appendChild(gridLine);
        labelsG.appendChild(tickLabel);
    };
    svgElement.appendChild(axG);
    svgElement.appendChild(labelsG);
    svgElement.appendChild(gridG);
    return svgElement;
};
function makeBars(labelKey, titleKey, valueKey, table) {
    var _js146 = arguments.length;
    for (var n145 = 4; n145 < _js146; n145 += 2) {
        switch (arguments[n145]) {
        case 'fill':
            fill = arguments[n145 + 1];
            break;
        case 'label-font-size':
            labelFontSize = arguments[n145 + 1];
            break;
        case 'value-scale':
            valueScale = arguments[n145 + 1];
            break;
        case 'width':
            width = arguments[n145 + 1];
            break;
        case 'height':
            height = arguments[n145 + 1];
            break;
        case 'x':
            x = arguments[n145 + 1];
            break;
        case 'y':
            y = arguments[n145 + 1];
        };
    };
    var fill = 'undefined' === typeof fill ? 'steelblue' : fill;
    var labelFontSize = 'undefined' === typeof labelFontSize ? '6pt' : labelFontSize;
    var valueScale = 'undefined' === typeof valueScale ? 1.0 : valueScale;
    var width = 'undefined' === typeof width ? 500 : width;
    var height = 'undefined' === typeof height ? 100 : height;
    var x = 'undefined' === typeof x ? 0 : x;
    var y = 'undefined' === typeof y ? 0 : y;
    var svgElement = document.createElementNS(SVGNS, 'svg');
    var barWidth = width / table.length;
    svgElement.setAttribute('x', x);
    svgElement.setAttribute('y', y);
    svgElement.setAttribute('width', width);
    svgElement.setAttribute('height', height);
    svgElement.setAttribute('fill', fill);
    for (var i = 0; i < table.length; i += 1) {
        var value = table[i][valueKey];
        var h = value * valueScale;
        var x = barWidth * i;
        var y = height - h;
        var rect = document.createElementNS(SVGNS, 'rect');
        var label = document.createElementNS(SVGNS, 'text');
        var title = document.createElementNS(SVGNS, 'title');
        var labelText = table[i][labelKey];
        var titleText = [table[i][titleKey], ': ', value].join('');
        title.textContent = titleText;
        rect.appendChild(title);
        rect.setAttribute('x', x);
        rect.setAttribute('y', y);
        rect.setAttribute('width', barWidth);
        rect.setAttribute('height', h);
        svgElement.appendChild(rect);
        label.textContent = labelText;
        label.style.fontSize = labelFontSize;
        label.setAttribute('x', x + barWidth / 2);
        label.setAttribute('y', y - 2);
        label.setAttribute('text-anchor', 'middle');
        svgElement.appendChild(label);
    };
    return svgElement;
};
/**
 * Draw a bar chart from the data in `table' on the svg element given
 * by `svg-id'.
 */
function drawSvgBarChart(svgId, table) {
    var svgElement = document.getElementById(svgId);
    var svgHeight = 100;
    var svgWidth = 500;
    var viewBox = [0, 0, svgWidth, svgHeight].join(' ');
    var hOffset = 20;
    var wOffset = 50;
    var maxHeight = svgHeight - hOffset;
    var maxValue = reduce(function (acc, row) {
        return Math.max(acc, row['total exports']);
    }, table, -1);
    svgElement.setAttribute('viewBox', viewBox);
    var svgAppendAll = function () {
        var elements = Array.prototype.slice.call(arguments, 0);
        for (var element = null, _js_idx147 = 0; _js_idx147 < elements.length; _js_idx147 += 1) {
            element = elements[_js_idx147];
            svgElement.appendChild(element);
        };
    };
    return svgAppendAll(makeYAxis('w-offset', wOffset, 'width', svgWidth, 'height', svgHeight, 'tick-scale', maxValue / maxHeight), makePlotTitle('Top 25 US States by Agriculture Exports (2011)', 'x', wOffset + (svgWidth - wOffset) / 2, 'y', hOffset), makeYLabel('Millions USD', 'x', -(svgHeight / 2), 'y', hOffset), makeBars('code', 'state', 'total exports', table, 'width', svgWidth - wOffset, 'height', svgHeight, 'value-scale', maxHeight / maxValue, 'x', wOffset));
};
fetchText('2011_us_ag_exports.csv').then(curry(flip(parseCsv), ['total exports'])).then(curry(numericSortBybang, 'total exports')).then(curry(take, 25)).then(curry(drawSvgBarChart, 'svg-chart'));
/** Return an array of the values for `column' in `table'. */
function extractColumn(column, table) {
    return map(function (row) {
        return row[column];
    }, table);
};
function drawPlotlyBarChart(chartId, table) {
    var layout = { 'title' : 'Top 25 US States by Agriculture Exports (2011)', 'yaxis' : { 'title' : 'Millions USD' } };
    var data = [{ 'x' : extractColumn('code', table),
                  'y' : extractColumn('total exports', table),
                  'text' : extractColumn('state', table),
                  'type' : 'bar'
                }];
    return Plotly.plot(chartId, data, layout);
};
fetchText('2011_us_ag_exports.csv').then(curry(flip(parseCsv), ['total exports'])).then(curry(numericSortBybang, 'total exports')).then(curry(take, 25)).then(curry(drawPlotlyBarChart, 'plotly-bar'));
function drawPlotlyChoropleth(chartId, table) {
    var layout = { 'title' : 'US Agriculture Exports by State (2011)', 'geo' : { 'scope' : 'usa',
                                                                                 'showlakes' : true,
                                                                                 'lakecolor' : 'rgb(255,255,255)'
                                                                               } };
    var data = [{ 'type' : 'choropleth',
                  'locationmode' : 'USA-states',
                  'locations' : extractColumn('code', table),
                  'z' : extractColumn('total exports', table),
                  'text' : extractColumn('state', table),
                  'zmin' : 0,
                  'zmax' : 17000,
                  'colorbar' : { 'title' : 'Millions USD', 'thickness' : 0.2 },
                  'colorscale' : [[0, 'rgb(242,240,247)'], [0.2, 'rgb(218,218,235)'], [0.4, 'rgb(188,189,220)'], [0.6, 'rgb(158,154,200)'], [0.8, 'rgb(117,107,177)'], [1, 'rgb(84,39,143)']],
                  'marker' : { 'line' : { 'color' : 'rgb(255,255,255)', 'width' : 2 } }
                }];
    return Plotly.plot(chartId, data, layout, { 'show-link' : false });
};
fetchText('2011_us_ag_exports.csv').then(curry(flip(parseCsv), ['total exports'])).then(curry(drawPlotlyChoropleth, 'plotly-choropleth'));
