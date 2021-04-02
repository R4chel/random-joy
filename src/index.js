import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app =Elm.Main.init({
  node: document.getElementById('root')
});

console.log(app);
app.ports.getSvg.subscribe(function(){
    var node = document.getElementById("output");
    var name = "elm-art.svg";

    node.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    var svgData = node.outerHTML;
    var preface = '<?xml version="1.0" standalone="no"?>\r\n';
    var svgBlob = new Blob([preface, svgData], {type:"image/svg+xml;charset=utf-8"});
    var svgUrl = URL.createObjectURL(svgBlob);
    var downloadLink = document.createElement("a");
    downloadLink.href = svgUrl;
    downloadLink.download = name;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
