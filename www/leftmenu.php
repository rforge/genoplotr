<?php
$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
?>
<!--left menu-->
<p>&nbsp;</p>
<p><a href="index.php">Home</a>
</p>
<p><a href="screenshots.php">Examples</a></br>
</p>
  <p><a href="vignette.php">Getting started</a></br>
</p>
<p><a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">
  R-Forge summary page</a></br>
</p>
<p><a href="http://r-forge.r-project.org/R/?group_id=651">
  Development versions (R-Forge)</a></br>
</p>
<p><a href="http://cran.r-project.org/web/packages/genoPlotR/index.html">
  Stable version (CRAN)</a></br>
</p>
<!--/left menu-->
   