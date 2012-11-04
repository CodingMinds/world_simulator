<?php
/*
 * A simple demo of a UI to show interested users what's
 * going on. Uses the ctrl port to get the whole environment.
 * 04.11.12 M. Bittorf <info@coding-minds.com>
 */

// config
$domain = AF_INET;
$server = "localhost";
$port = 4567;
$ctl_port = 4568;

$timeout = 30;

?>
<html>
 <head>
  <title>Actual environment of 'world simulator'</title>
 </head>
 <body>
<?php

$fp = @fsockopen($server, $ctl_port, $errno, $errstr, $timeout);

if(!$fp) { // 1
  echo "  <h1>".$errstr." (".$errno.")</h1>\n";

}else { // if (1)
  $write = "map\r\n"
          ."quit\r\n";
  @fwrite($fp, $write);

  $buffer = "";
  while (!feof($fp)) { // 1
    $buffer .= @fgets($fp, 128);
  } // while (1)
  @fclose($fp);
?>
  <h1>Actual environment</h1>
  <pre>
<?php
  foreach(explode("\n", $buffer) as $row) { // 1
    if(strstr($row, "100 ")) { // 2
      $row = str_replace("100 ", "", $row);
      for($i = 0; $i < strlen($row); ++$i) { // 1
        echo $row[$i]." ";
      } // for (1)
      echo "\n";
    } // if (2)
  } // foreach (1)
?>
  </pre>

  <p>Wanna participate ?<br />
  Server: <?php echo $server; ?><br />
  Port: <?php echo $port; ?><br />
  <a href="https://github.com/CodingMinds/world_simulator/blob/master/Readme.md">Readme</a></p>
<?php
} // if (1)
?>
 </body>
</html>
