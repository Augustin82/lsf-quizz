// pull in desired CSS/SASS files

// inject bundled Elm app into div#main
var Elm = require("../elm/Main");
var app = Elm.Main.embed(document.getElementById("main"), window.options);
