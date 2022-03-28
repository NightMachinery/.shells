<?php if (!empty($_POST)): ?>

Result:<br>
<pre><?php echo htmlspecialchars(shell_exec('brishzq.zsh tldr '. escapeshellarg($_POST["name"]))); ?></pre><br>
<?php else: ?>
    <form method="post">
        <input type="text" name="name"><br>
        <button type="submit">TLDR</button>
    </form>
<?php endif; ?>
