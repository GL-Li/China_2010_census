<!--
a = 1 // a=numbers of banners
var slump = Math.random();
var talet = Math.round(slump * (a-1))+1;
function create() {
        this.width = ''
        this.height = ''
        this.src = ''
        this.href = ''
        this.border = ''
        this.alt = ''
        this.under = ''
	this.isFlush= ''
	this.flushsrc= ''
}
b = new Array()
for(var i=1; i<=a; i++) { b[i] = new create() }

b[1].isFlush='T';
b[1].flushsrc='<OBJECT codeBase=http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0 height=60 width=468 classid=clsid:D27CDB6E-AE6D-11cf-96B8-444553540000 vspace="0" hspace="0"><PARAM NAME="movie" VALUE="http://imgs.xinhuanet.com/ad/20070305kyqc.swf"><PARAM NAME="quality" VALUE="high"><embed src="http://imgs.xinhuanet.com/ad/20070305kyqc.swf" quality=high pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" type="application/x-shockwave-flash" width="468" height="60" vspace="0" hspace="0"></embed></OBJECT>'



var visa = "";
if(b[talet].isFlush == "")
{
	visa += '<a href="'+b[talet].href+'" target="_blank">'
	visa += '<img src="'+b[talet].src+'" height='+b[talet].height
	visa += ' width='+b[talet].width+' border='+b[talet].border+' alt='+b[talet].alt+'>';
	visa += '</a>'
}
else	visa=b[talet].flushsrc;

function DisplayVisa()
{
	document.write(visa);
}

//fix domain issue
window.onload = function() {
    var ac = document.getElementsByTagName("A");
	for(var i=0;i<ac.length;i++)
	{
	    if(ac[i].href.indexOf("mms.xin")>=0)
		{
		    ac[i].href="http://www.xinhuanet.com";
		}

	    if(ac[i].href.indexOf("waps.xin")>=0)
		{
		    ac[i].href="http://www.xinhuanet.com";
		}

	}
}
//-->