//this code will generate pngs in 3 different ways when included as part of an html page


//wait until the page has been loaded
window.addEventListener('load', genpng, false);

function genpng() {
    var svg = document.querySelector("svg");
    var svgData = new XMLSerializer().serializeToString(svg);

    var canvas = document.createElement("canvas");
    var ctx = canvas.getContext("2d");
    var img = document.createElement("img");
    img.setAttribute("src", "data:image/svg+xml;base64," + btoa(svgData));

    img.onload = function () {

        //this one opens the png in a new window
        canvas.width = 1024;
        canvas.height = 512;
        ctx.drawImage(img, 0, 0);
        console.log("twitter");
        var d = canvas.toDataURL("image/png");
        var w = window.open('about:blank', 'image from canvas');
        w.document.write("<img src='" + d + "' alt='from canvas'/>");

        //this one puts the image as a data string in the console
        canvas.width = 1200;
        canvas.height = 628;
        ctx.drawImage(img, 0, 0);
        console.log("facebook");
        console.log(canvas.toDataURL("image/png"));

        //this one prompts a download with the image
        canvas.width = 1080;
        canvas.height = 1080;
        ctx.drawImage(img, 0, 0);
        console.log("default");
        var default_size = canvas.toDataURL("image/png").replace("image/png", "image/octet-stream");
        window.location.href = default_size;
    };
};
