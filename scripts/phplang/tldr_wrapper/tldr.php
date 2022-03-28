<html>
<body>

<pre><?php echo htmlspecialchars(shell_exec('tldr '. escapeshellarg($_POST["name"]))); ?></pre><br>

</body>
</html>
