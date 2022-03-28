<div style="color: gray" ><?=htmlspecialchars(`date +"%b %d %H:%M:%S"`)?></div>

<h4><pre>free -h</pre></h4>
<pre>
<?=htmlspecialchars(`free -h`)?>
</pre>

<h4><pre>cpu-usage-get</pre></h4>
<pre>
<?php
          // $cpu_usage = `brishz.dash cpu-usage-get`;
          // $cpu_usage = `brishz2.dash cpu-usage-get`;
          // $cpu_usage = `which php jq zsh`;
          // $cpu_usage = `echo \$PATH`;
          $cpu_usage = `brishzq.zsh ec hi`;
          echo htmlspecialchars($cpu_usage);
?>
</pre>
