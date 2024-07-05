Infectech
================
Paula Christen
2024-07-05

*Introduction:* Package forecasts for policymakers. *Installation:* use
devtools::install_github()… *Test Coverage:* see test below

\<!DOCTYPE html\>
<html lang="en">
<head>
<meta charset="utf-8"/>
<style>body{background-color:white;}</style>
<link href="lib/htmltools-fill-0.5.8.1/fill.css" rel="stylesheet" />
<script src="lib/htmlwidgets-1.6.4/htmlwidgets.js"></script>
<link href="lib/datatables-css-0.0.0/datatables-crosstalk.css" rel="stylesheet" />
<script src="lib/datatables-binding-0.33/datatables.js"></script>
<script src="lib/jquery-3.6.0/jquery-3.6.0.min.js"></script>
<link href="lib/dt-core-1.13.6/css/jquery.dataTables.min.css" rel="stylesheet" />
<link href="lib/dt-core-1.13.6/css/jquery.dataTables.extra.css" rel="stylesheet" />
<script src="lib/dt-core-1.13.6/js/jquery.dataTables.min.js"></script>
<link href="lib/crosstalk-1.2.1/css/crosstalk.min.css" rel="stylesheet" />
<script src="lib/crosstalk-1.2.1/js/crosstalk.min.js"></script>
<link href="lib/highlight.js-6.2/rstudio.css" rel="stylesheet" />
<script src="lib/highlight.js-6.2/highlight.pack.js"></script>
<meta name="viewport" content="width=device-width, initial-scale=1" />
<link href="lib/bootstrap-3.3.5/css/bootstrap.min.css" rel="stylesheet" />
<link href="lib/bootstrap-3.3.5/css/bootstrap-theme.min.css" rel="stylesheet" />
<script src="lib/bootstrap-3.3.5/js/bootstrap.min.js"></script>
<script src="lib/bootstrap-3.3.5/shim/html5shiv.min.js"></script>
<script src="lib/bootstrap-3.3.5/shim/respond.min.js"></script>
</head>
<body>

<div class="container-fluid">

<style type="text/css">table tr:hover td {
  font-weight:bold;text-decoration:none
}
table tr.covered td{
  background-color:rgba(95,151,68,0.3)
}
&#10;table tr:hover.covered .num{
  background-color:rgba(95,151,68,0.7)
}
table tr.missed td{
  background-color:rgba(185,73,71,0.3)
}
table tr:hover.missed .num{
  background-color:rgba(185,73,71,0.7)
}
&#10;table tr.missed:hover td{
  -webkit-box-shadow:0 -2px 0 0 #b94947 inset;
  -moz-box-shadow:0 -2px 0 0 #b94947 inset;
  box-shadow:0 -2px 0 0 #b94947 inset
}
table tr.covered:hover td{
  -webkit-box-shadow:0 -2px 0 0 #5f9744 inset;
  -moz-box-shadow:0 -2px 0 0 #5f9744 inset;
  box-shadow:0 -2px 0 0 #5f9744 inset
}
&#10;table tr.never td{
  background-color:transparent
}
&#10;table tbody {
  border-style: solid;
  border: 1px solid rgba(0,0,0,0.1)
}
&#10;table .num {
  border-right: 1px solid rgba(0,0,0,0.1)
}
&#10;td.coverage em {
  opacity: 0.5;
}
&#10;table td.coverage {
  border-right: 1px solid rgba(0,0,0,0.1);
  font-weight: bold;
  text-align: center;
}
table.table-condensed pre {
  background-color: transparent;
  margin: 0;
  padding: 0;
  border: 0;
  font-size: 11px;
}
div#files td {
  padding: 0;
  padding-left: 5px;
}
&#10;div#files td.num {
  padding-right: 5px;
}
&#10;table.table-condensed {
  font-size: 11px;
}</style>

<div class="col-md-8 col-md-offset-2">

    <h2>infectech coverage - 0.00%</h2>
    <div class="tabbable">
      <ul class="nav nav-tabs" data-tabsetid="covr">
        <li class="active">
          <a href="#tab-covr-1" data-toggle="tab" data-value="Files">Files</a>
        </li>
        <li>
          <a href="#tab-covr-2" data-toggle="tab" data-value="Source">Source</a>
        </li>
      </ul>
      <div class="tab-content" data-tabsetid="covr">
        <div class="tab-pane active" title="Files" data-value="Files" id="tab-covr-1">
          <div class="datatables html-widget html-fill-item" id="htmlwidget-72f0ad15bdf6f46ac171" style="width:100%;height:500px;"></div>
          <script type="application/json" data-for="htmlwidget-72f0ad15bdf6f46ac171">{"x":{"filter":"none","vertical":false,"fillContainer":false,"data":[["<a href=\"#\">R/hello.R<\/a>","<a href=\"#\">R/dummy_function.R<\/a>"],[18,12],[1,1],[0,0],[1,1],["0","0"],["0.00%","0.00%"]],"container":"<table class=\"row-border\">\n  <thead>\n    <tr>\n      <th>File<\/th>\n      <th>Lines<\/th>\n      <th>Relevant<\/th>\n      <th>Covered<\/th>\n      <th>Missed<\/th>\n      <th>Hits / Line<\/th>\n      <th>Coverage<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"searching":false,"dom":"t","paging":false,"columnDefs":[{"targets":6,"createdCell":"function(td, cellData, rowData, row, col) {\n  var percent = cellData.replace(\"%\", \"\");\n  if (percent > 90) {\n    var grad = \"linear-gradient(90deg, #edfde7 \" + cellData + \", white \" + cellData + \")\";\n  } else if (percent > 75) {\n    var grad = \"linear-gradient(90deg, #f9ffe5 \" + cellData + \", white \" + cellData + \")\";\n  } else {\n    var grad = \"linear-gradient(90deg, #fcece9 \" + cellData + \", white \" + cellData + \")\";\n  }\n  $(td).css(\"background\", grad);\n}\n"},{"className":"dt-right","targets":[1,2,3,4]},{"name":"File","targets":0},{"name":"Lines","targets":1},{"name":"Relevant","targets":2},{"name":"Covered","targets":3},{"name":"Missed","targets":4},{"name":"Hits / Line","targets":5},{"name":"Coverage","targets":6}],"order":[],"autoWidth":false,"orderClasses":false},"callback":"function(table) {\ntable.on('click.dt', 'a', function() {\n  files = $('div#files div');\n  files.not('div.hidden').addClass('hidden');\n  id = $(this).text();\n  files.filter('div[id=\\'' + id + '\\']').removeClass('hidden');\n  $('ul.nav a[data-value=Source]').text(id).tab('show');\n});\n}"},"evals":["options.columnDefs.0.createdCell","callback"],"jsHooks":[]}</script>
        </div>
        <div class="tab-pane" title="Source" data-value="Source" id="tab-covr-2">
          <div id="files">
            <div id="R/hello.R" class="hidden">
              <table class="table-condensed">
                <tbody>
                  <tr class="never">
                    <td class="num">1</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r"># Hello, world!</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">2</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">3</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r"># This is an example function named 'hello' </pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">4</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r"># which prints 'Hello, world!'.</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">5</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">6</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r"># You can learn more about package authoring with RStudio at:</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">7</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">8</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#   https://r-pkgs.org</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">9</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">10</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r"># Some useful keyboard shortcuts for package authoring:</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">11</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">12</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#   Install Package:           'Cmd + Shift + B'</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">13</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#   Check Package:             'Cmd + Shift + E'</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">14</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#   Test Package:              'Cmd + Shift + T'</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">15</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r"></pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">16</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">hello &lt;- function() {</pre>
                    </td>
                  </tr>
                  <tr class="missed">
                    <td class="num">17</td>
                    <td class="coverage">!</td>
                    <td class="col-sm-12">
                      <pre class="language-r">  print("Hello, world!")</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">18</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">}</pre>
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
            <div id="R/dummy_function.R" class="hidden">
              <table class="table-condensed">
                <tbody>
                  <tr class="never">
                    <td class="num">1</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' Add Two Numbers</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">2</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#'</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">3</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' This function takes two numeric inputs and returns their sum.</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">4</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#'</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">5</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' @param x A numeric value.</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">6</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' @param y A numeric value.</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">7</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' @return The sum of x and y.</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">8</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' @examples</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">9</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">#' add_two_numbers(3, 5)  # Returns 8</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">10</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">add_two_numbers &lt;- function(x, y) {</pre>
                    </td>
                  </tr>
                  <tr class="missed">
                    <td class="num">11</td>
                    <td class="coverage">!</td>
                    <td class="col-sm-12">
                      <pre class="language-r">  x + y</pre>
                    </td>
                  </tr>
                  <tr class="never">
                    <td class="num">12</td>
                    <td class="coverage"></td>
                    <td class="col-sm-12">
                      <pre class="language-r">}</pre>
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
            <script>$('div#files pre').each(function(i, block) {
    hljs.highlightBlock(block);

});</script>

</div>

        </div>
      </div>
    </div>

</div>

</div>
</body>
</html>
