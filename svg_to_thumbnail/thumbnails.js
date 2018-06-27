//this program takes an svg figure and creates appropriately sized thumbnails for various social media platforms

//to use it, first define filepaths to local assets, enter the path to the css file, and set up the figure generation
//in the headless browser. Then run the program using 'node thumbnails.js <optional flag for external styling:
// -css (path to css)>

var fs = require('fs');
var jsdom = require('jsdom/lib/old-api.js');
var svg2png = require('svg2png');

var style_ext = null;
var args = process.argv.splice(process.execArgv.length + 2);

var css_path;

if (args.length > 2) {
    console.log('\nUsage: node thumbnail.js ' +
        '\n\nOptional flag: -css path/to/css/file.css\n');
    process.exit();
} else {
    if (args[0]) {
        if (args[0] === '-css') {
            try {
                style_ext = fs.readFileSync(args[1], 'utf8');
            } catch(error) {
                console.log('\nError: external css file path not found. - Using only default style.\n\n' + error);
                style_ext = null;
            }
        } else {
            console.log('\nUnrecognized argument: ' + args[0]);
            console.log('\nUsage: node thumbnail.js ' +
                '\n\nOptional flag: -css path/to/css/file.css\n');
            process.exit();
        }
    }
}

jsdom.env(

    // create DOM hook
    "<html><body><div id='img'></div>" +
    "</body></html>",

    // load whatever local assets are needed into window environment
    [
        //examples:
        //'../bower_components/d3/d3.js',
        //'../js/hydrograph.js'
    ],

    //enter path to css
    //css_path = ...


    function (err, window) {

        //set up the figure as it is done in the viz
        var figure = null;
        // figure = window.visualization(
        //     {
        //         'div_id': '#img',
                    //specify other characteristics as needed
        //     }
        // );
        convert(figure, window, css_path);
}
);

function convert(figure, window, css_path) {


    var svg_string = Buffer.from(`<?xml version="1.0" encoding="utf-8"?>
           <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
           <svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 100 99.864" enable-background="new 0 0 100 99.864" xml:space="preserve">
              <rect x="0" y="0" width="100" height="100" fill="#808080"/>
              <path fill="#231F20" d="M83.713,63.42c-0.307-1.176-0.998-2.188-1.795-3.023c0.539-2.354,0.832-4.801,0.832-7.314      
              C77.318,54.467,77.18,55.815,76.98,57.143"></path>
              <path fill="#231F20" d="M44.366,48.122c0,2.231-1.808,4.039-4.039,4.039s-4.039-1.807-4.039-4.039c0-2.23,1.808-4.039,4.039-4.039
              S44.366,45.892,44.366,48.122"></path>
              <path fill="#231F20" d="M63.984,48.122c0,2.231-1.809,4.039-4.039,4.039c-2.231,0-4.039-1.807-4.039-4.039
              c0-2.23,1.808-4.039,4.039-4.039C62.176,44.083,63.984,45.892,63.984,48.122"></path>
            </svg>`);

    //if we have set up a figure, use it to generate thumbnails
    if (figure !== null) {
        var style_ext = null;
        svg_string = null;
        var svg = figure.get_svg_elem().node();
        var style_default = fs.readFileSync(css_path, 'utf8');
        figure.init();
        svg_string = inject_style(style_default, style_ext, svg, window);
    }

    //otherwise use default data


    svg2png(svg_string, {height: 512, width: 1024})
        .then(buffer => fs.writeFile('./twitter.png', buffer))
        .then(console.log('\nTwitter thumbnail created. \n'))
        .catch(e => console.error(e));

    svg2png(svg_string, {height: 628, width: 1200})
        .then(buffer => fs.writeFile('./facebook.png', buffer))
        .then(console.log('\nFacebook thumbnail created. \n'))
        .catch(e => console.error(e));

    svg2png(svg_string, {height: 1080, width: 1080})
        .then(buffer => fs.writeFile('./default.png', buffer))
        .then(console.log('\nDefault thhumbnail created. \n'))
        .catch(e => console.error(e));
}

function inject_style(style_string, ext_style, svgDomElement, window) {
    var s = window.document.createElement('style');
    s.setAttribute('type', 'text/css');
    s.innerHTML = "<![CDATA[\n" + style_string + ext_style + "\n]]>";
    var defs = window.document.createElement('defs');
    defs.appendChild(s);
    svgDomElement.insertBefore(defs, svgDomElement.firstChild);
    return svgDomElement.parentElement.innerHTML;
}