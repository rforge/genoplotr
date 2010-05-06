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
  <p>(This page is an automatic conversion of the <a href="pdfs/genoPlotR.pdf">
      pdf vignette<a> included in the package)</p>
  <!-- includes vignette html -->
  <?php include("vignette.html") ?>
  <!-- includes end of table --> 
  <?php include("end.php"); ?>
