<?php require_once __DIR__ . '/../brishzgo.php'; if (!empty($_POST)): ?>

Result:<br>
<pre><?php echo htmlspecialchars(shell_exec(brishzgo_command(['tldr', $_POST["name"]]))); ?></pre><br>
<?php else: ?>
    <form method="post">
        <input type="text" name="name"><br>
        <button type="submit">TLDR</button>
    </form>
<?php endif; ?>
