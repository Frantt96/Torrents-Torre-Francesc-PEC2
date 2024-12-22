// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, true, true, false, false, false ];
var arrayMetadata    = [ [ "1", "GSM944850.28", "GSM944850", "S. aureus USA300", "hour 24", "linezolid", "2010-11-10T19:22:48Z" ], [ "2", "GSM944836.26", "GSM944836", "S. aureus USA300", "hour 24", "linezolid", "2010-11-10T17:00:43Z" ], [ "3", "GSM944864.30", "GSM944864", "S. aureus USA300", "hour 24", "linezolid", "2010-11-10T21:40:07Z" ], [ "4", "GSM944857.29", "GSM944857", "S. aureus USA300", "hour 24", "linezolid", "2010-11-10T20:35:13Z" ], [ "5", "GSM944833.6", "GSM944833", "uninfected", "hour 0", "linezolid", "2010-11-10T16:32:50Z" ], [ "6", "GSM944861.10", "GSM944861", "uninfected", "hour 0", "linezolid", "2010-11-10T21:12:26Z" ], [ "7", "GSM944854.9", "GSM944854", "uninfected", "hour 0", "linezolid", "2010-11-10T20:00:00Z" ], [ "8", "GSM944847.8", "GSM944847", "uninfected", "hour 0", "linezolid", "2010-11-10T18:55:14Z" ], [ "9", "GSM944863.25", "GSM944863", "S. aureus USA300", "hour 24", "untreated", "2010-11-10T21:30:54Z" ], [ "10", "GSM944835.21", "GSM944835", "S. aureus USA300", "hour 24", "untreated", "2010-11-10T16:51:22Z" ], [ "11", "GSM944849.23", "GSM944849", "S. aureus USA300", "hour 24", "untreated", "2010-11-10T19:13:34Z" ], [ "12", "GSM944856.24", "GSM944856", "S. aureus USA300", "hour 24", "untreated", "2010-11-10T20:24:45Z" ], [ "13", "GSM944831.1", "GSM944831", "uninfected", "hour 0", "untreated", "2010-11-10T16:14:28Z" ], [ "14", "GSM944838.2", "GSM944838", "uninfected", "hour 0", "untreated", "2010-11-10T17:19:25Z" ], [ "15", "GSM944845.3", "GSM944845", "uninfected", "hour 0", "untreated", "2010-11-10T18:36:29Z" ], [ "16", "GSM944859.5", "GSM944859", "uninfected", "hour 0", "untreated", "2010-11-10T20:53:55Z" ], [ "17", "GSM944851.33", "GSM944851", "S. aureus USA300", "hour 24", "vancomycin", "2010-11-10T19:32:16Z" ], [ "18", "GSM944837.31", "GSM944837", "S. aureus USA300", "hour 24", "vancomycin", "2010-11-10T17:10:05Z" ], [ "19", "GSM944865.35", "GSM944865", "S. aureus USA300", "hour 24", "vancomycin", "2010-11-10T21:49:42Z" ], [ "20", "GSM944844.32", "GSM944844", "S. aureus USA300", "hour 24", "vancomycin", "2010-11-10T18:27:14Z" ], [ "21", "GSM944855.14", "GSM944855", "uninfected", "hour 0", "vancomycin", "2010-11-10T20:15:24Z" ], [ "22", "GSM944848.13", "GSM944848", "uninfected", "hour 0", "vancomycin", "2010-11-10T19:04:21Z" ], [ "23", "GSM944834.11", "GSM944834", "uninfected", "hour 0", "vancomycin", "2010-11-10T16:42:09Z" ], [ "24", "GSM944862.15", "GSM944862", "uninfected", "hour 0", "vancomycin", "2010-11-10T21:21:37Z" ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
    for(i=0; i<ssrules.length; i++) {
        if (ssrules[i].selectorText == (".aqm" + reportObjId)) {
		ssrules[i].style.cssText = cssText[0+status];
		break;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
