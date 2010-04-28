<?php
$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';
echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">
  
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title><?php echo $group_name; ?></title>
  <link href="css/main.css" rel="stylesheet" type="text/css" />
  </head>
  
  <body>

  <!-- includes table and top row and colum -->
  <?php include("begin.php"); ?>
  <table>
  <tr>
  <th>Screenshots</th>
  </tr>
  <?php 
  $examples=array("three_genes", "barto_seg1", "barto_multiseg", 
		  "chrY_subseg", "figureBioinfo");
foreach ($examples as $example){
  echo "<tr>
     <td><b>Example: $example</b></td>
    </tr>
     <td><a href=\"pdfs/$example.pdf\"><img border=\"0\" 
       src=\"img/$example.png\"/></a></td>
    </tr>
    <tr>
      <td><a href=\"code/$example.R\">Code</a> and 
        <a href=\"pdfs/$example.pdf\">pdf</a></td>
    </tr>";
  /* to plot the code directly in the window 
    <tr>
    <td><code style=\"white-space:nowrap\">";
  $str= file_get_contents("code/$example.R");
  $str = nl2br($str);
  echo $str; 
  echo "</code>
  */
   echo "</td>
	</tr>";
}
?>
</table>
<!-- includes end of table --> 
<?php include("end.php"); ?>
