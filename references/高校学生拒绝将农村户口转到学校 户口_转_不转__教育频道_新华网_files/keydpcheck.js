//utf8编码

function EncodeUtf8(cn)
{
	var cnc = escape(cn);
	var sa = cnc.split("%");
	var retV ="";
	if(sa[0] != "")
	{
		retV = sa[0];
	}
	for(var i = 1; i < sa.length; i ++)
	{
		if(sa[i].substring(0,1) == "u")
		{
			retV += Hex2Utf8(Str2Hex(sa[i].substring(1,5)))+sa[i].substring(5,sa[i].length);
		}
		else retV += "%" + sa[i];
	}
	return retV;
}


function Str2Hex(s)
{
	var c = "";
	var n;
	var ss = "0123456789ABCDEF";
	var digS = "";
	for(var i = 0; i < s.length; i ++)
	{
		c = s.charAt(i);
		n = ss.indexOf(c);
		digS += Dec2Dig(eval(n));
	}
	return digS;
}

function Dec2Dig(n1)
{
	var s = "";
	var n2 = 0;
	for(var i = 0; i < 4; i++)
	{
		n2 = Math.pow(2,3 - i);
		if(n1 >= n2)
		{
			s += '1';
			n1 = n1 - n2;
		}
		else s += '0';
	}
	return s;
}

function Dig2Dec(s)
{
	var retV = 0;
	if(s.length == 4)
	{
		for(var i = 0; i < 4; i ++)
		{
			retV += eval(s.charAt(i)) * Math.pow(2, 3 - i);
		}
		return retV;
	}
	return -1;
}

function Hex2Utf8(s)
{
	var retS = "";
	var tempS = "";
	var ss = "";
	if(s.length == 16)
	{
		tempS = "1110" + s.substring(0, 4);
		tempS += "10" + s.substring(4, 10);
		tempS += "10" + s.substring(10,16);
		var sss = "0123456789ABCDEF";
		for(var i = 0; i < 3; i ++)
		{
			retS += "%";
			ss = tempS.substring(i * 8, (eval(i)+1)*8);
			retS += sss.charAt(Dig2Dec(ss.substring(0,4)));
			retS += sss.charAt(Dig2Dec(ss.substring(4,8)));
		}
		return retS;
	}
	return "";
}
//细览搜索关键词

var elem =$("meta[name=keywords]").attr("content");
var ele_idL;
if (elem!="")
{
	ele_idL=elem.replace(/ /g,",");
	ele_idL=ele_idL.replace(/#/g,",");
	ele_idL1=ele_idL.split(",")[0];
	if (ele_idL.split(",").length>1)
	{
		ele_idL2=ele_idL.split(",")[1];
		document.getElementById("searchkeywords").innerHTML="<span class=\"lan14\">搜索更多 <\/span><a href=http:\/\/search.news.cn\/n\/s.jsp?n="+EncodeUtf8(ele_idL1)+" target=_blank class=qianlan_14>"+ele_idL1+"<\/a>&nbsp;<a href=http:\/\/search.news.cn\/n\/s.jsp?n="+EncodeUtf8(ele_idL2)+" target=_blank class=qianlan_14>"+ele_idL2+"<\/a><span class=\"lan14\"> 的新闻</span>";
	}
	else
	{
		document.getElementById("searchkeywords").innerHTML="<span class=\"lan14\">搜索更多 <\/span><a href=http:\/\/search.news.cn\/n\/s.jsp?n="+EncodeUtf8(ele_idL1)+" target=_blank class=qianlan_14>"+ele_idL1+"<\/a><span class=\"lan14\"> 的新闻</span>";
	}
}             




//打印
function doPrint()
{
	var Title = document.all.Title.innerHTML;
	var Time = document.all.Time.innerHTML;
	var Position = document.all.Position.innerHTML;
	var mContent = document.all.Content.innerHTML;
	var Images = document.all.Image.innerHTML;
var PartI = ' \
<style type="text/css"><!-- \
--></style><table width="600" border="0" cellspacing="0" cellpadding="0" align="center" bordercolor="#FFCC66" > \
 <tr>  <td bgcolor="#FFCC00" width="465"><b><font class="font-family: 宋体 ;font-size: 9pt">欢迎访问新华网</font>  <font face="Verdana, Arial, Helvetica, sans-serif" size="1"> - WWW.XINHUANET.COM</font></b> </td> \
    <td bgcolor="#FFDC71" width="23">&nbsp;</td>    <td bgcolor="#FFE8A2" width="13">&nbsp;</td>    <td bgcolor="#FFF1BB" width="12">&nbsp;</td>    <td bgcolor="#FFF8D9" width="15">&nbsp;</td> \
    <td bgcolor="#FFF9DD" width="34">&nbsp;</td>  </tr>  <tr bgcolor="#EAEAEA">     <td class="p9" colspan="6">       <div align="right"><font color="#000000"> ';

var PartII = Position+'<font size="1"><b>&gt;&gt;</b></font></font></div>    </td>  </tr>  <tr><td colspan="6">      <hr size="1" noshade>    </td></tr>  <tr valign="top" align="left"> \
    <td colspan="6">       <table width="100%" border="0" cellspacing="0" cellpadding="5" align="center" class="main">        <tr>           <td>  ';
	
var PartIII= '<div align="center"><b>'+Title+'</b></div>';
var Part4='</td></tr><tr><td> <div align="center" class="p9">'+Time+'</div>';
var Part5='</td></tr><tr><td align="center">'+Images+'</td></tr><tr> <td>'+mContent+'</td></tr></table> </td></tr> <tr> <td colspan="6">      <hr noshade size="1"></td>  </tr>  <tr>     <td bgcolor="#FFCC66" colspan="6">       <div align="right" class="p9"><b><font face="宋体" font-size="9pt">新华网版权所有</b></font></div> </td>  </tr></table>';
document.body.innerHTML = PartI+PartII+PartIII+Part4+Part5;
window.print();

}

//email
function searchalert(){
validate=true;
if(document.formsearch.searchword.value=="")
{
validate=false;
alert('请输入关键字!');
}
else
validate=true;
return validate;
}

function validate_form() {

  var str0,str1,str2,str3,str4,str;

  validity = true; // assume valid

  if (!check_email(document.Form2000.FriendEmail.value))

        { validity = false; alert(' 朋友的Email可能打错了或为空的！');}

  if (validity)
  {
	str0="◆◆◆新华网新闻◆◆◆"
	str1="您好!";
	str2="您的朋友向您推荐新华网新闻:";
	str3="“"+document.title+"”"+"\n链接网址是：";
	str4=this.location;
	str=str0+"\n"+str1+"\n"+str2+"\n"+str3+"\n"+str4+"\n";
	document.Form2000.Context.value=str;

	document.Form2000.action="mailto:"+document.Form2000.FriendEmail.value+"?\&amp;Subject=推荐新闻";
  }
  return validity;
}

function check_email(address) {

  if ((address == "")

    || (address.indexOf ('@') == -1)

    || (address.indexOf ('.') == -1))

      return false;

  return true;
}
