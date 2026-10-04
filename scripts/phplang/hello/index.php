<div style="color: gray" >
<?=htmlspecialchars(`date +"%b %d %H:%M:%S"`)?><br>

<?=htmlspecialchars(`hostname`)?>
</div>

<h4><pre>uptime</pre></h4>
<pre>
<?php
          echo htmlspecialchars(`uptime`);
?>
</pre>

<h4><pre>cpu-usage-get</pre></h4>
<pre>
<?php
          require_once __DIR__ . '/../brishzgo.php';
          $cpu_usage = shell_exec(brishzgo_command(['cpu-usage-get']));
          echo htmlspecialchars($cpu_usage);
?>
</pre>

<h4><pre>free -h</pre></h4>
<pre>
<?=htmlspecialchars(`free -h`)?>
</pre>

<div style="display: none">
<h4><pre>PATH</pre></h4>
<?=htmlspecialchars(`echo \$PATH`)?>
</div>
