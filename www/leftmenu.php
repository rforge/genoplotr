<?php
$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
?>
<!--left menu-->
<p>&nbsp;</p>
<p><a href="index.php">Home</a>
</p>
<p><a href="screenshots.php">Screenshots</a></br>
</p>
<p><a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">
  R-Forge summary page</a></br>
</p>
<p><a href="http://r-forge.r-project.org/R/?group_id=651">
  Downloads</a></br>
<!--/left menu-->
   