<?php 
// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 
?>

<div class="title-bar" data-responsive-toggle="topNav" data-hide-for="medium">
  <div class="title-bar-left">
    <div style="display: inline-block;"><img src="img/greysiteLogo25x25.png" <?php echo "alt='$siteTitle logo'"; ?>></div>
    <div class="title-bar-title "><?php echo $siteTitle; ?></div>
  </div>
  <div class="title-bar-right">
    <button class="menu-icon align-right" type="button" data-toggle="topNav"></button>
  </div>
</div>

<div class="top-bar" id="topNav">
  <div class="grid-x" style="width: 100%;">
    <div class="cell auto">
      <ul class='menu'>
        <li class="tn-siteLogo-container show-for-medium"><img class="tn-siteLogo" src="img/greysiteLogo38x38.png" <?php echo "alt='$siteTitle logo'"; ?>></li>
        <li class="menu-text show-for-medium"><?php echo $siteTitle; ?></li>
        <li><a class="tn-menu-item" href='home.php'>Home</a></li>
        <?php if (isset($user)) { echo "<li><a class='tn-menu-item' href='userProfile.php'>User Profile</a></li>"; } ?>
        <li><a class="tn-menu-item" href='createPalette.php'>Create Palette</a></li>
        <!-- 
        <li><a href='login.php'>Login</a></li>
        <li><a href='logout.php'>Logout</a></li>
        -->
      </ul>
    </div>

    <div class="cell shrink">
      <?php 
        if (isset($user)) { 
          echo "<ul class='menu align-right'>
                  <li><button type='button' class='button tn-menu-item' onclick='location.href=\"logout.php\"'>Logout</button></li>
                </ul>";
        } else {
          echo "<ul class='menu align-right'>
                  <li><button type='button' class='button tn-menu-item' onclick='location.href=\"login.php\"'>Login</button></li>
                </ul>";
        }
      ?>
    </div>
  </div>
</div>