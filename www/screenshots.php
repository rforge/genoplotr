<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
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
        <th>Screenshots
        </th>
      </tr>
      <tr>
        <td>Click on the image to download pdf.
        </td>
      </tr>
      <tr>
        <td><a href="pdfs/three_genes.pdf"><img border="0" src="img/three_genes.png"/></a>
        </td>
      </tr>
      <tr>
        <td>Code:</td>
      </tr>
      <tr>
        <td>
          <code style="white-space:nowrap">
            <?php
              $str= file_get_contents("code/three_genes.R");
              $str = nl2br($str);
              echo $str; 
            ?>
          </code>
        </td>
      </tr>
      <tr>
        <td><a href="pdfs/barto_seg1.pdf"><img border="0" src="img/barto_seg1.png"/></a>
        </td>
      </tr>
      <tr>
        <td>Code:</td>
      </tr>
      <tr>
        <td>
          <code style="white-space:nowrap">
            <?php
              $str= file_get_contents("code/barto_seg1.R");
              $str = nl2br($str);
              echo $str; 
            ?>
          </code>
        </td>
      </tr>
    </table>
  <!-- includes end of table --> 
  <?php include("end.php"); ?>