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
  <h1>genoPlotR</h1>
  <p>
  <?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
  $contents = '';
  while (!feof($handle)) {
    $contents .= fread($handle, 8192);
  }
  fclose($handle);
  echo $contents; } ?>
  </p>
  <p>This project is hosted on <a href="http://r-forge.r-project.org/">R-Forge
  </a>.
  <p>The <strong>project summary page</strong> can be found
  <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">
  <strong>here</strong></a>, with all info on downloading, obtaining help, 
  reporting bugs and proposing features.
  </p>
  <p>There are some <a href="screenshots.php">screenshots</a> available, and 
  a <a href="pdfs/genoplot_all_examples.pdf">pdf<a> with all the graphics 
  from the examples.
  </p>
  <!-- includes end of table --> 
  <?php include("end.php"); ?>
  