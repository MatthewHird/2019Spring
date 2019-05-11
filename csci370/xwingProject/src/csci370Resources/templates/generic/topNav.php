<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
?>

<div class="title-bar" data-responsive-toggle="topNav" data-hide-for="medium">
    <div class="title-bar-left">
        <div style="display: inline-block;"><img <?php echo "src='$siteIcon25x25' alt='$siteTitle logo'"; ?>></div>
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
                <li class="tn-siteLogo-container show-for-medium"><img class="tn-siteLogo" <?php echo " src='$siteIcon38x38' alt='$siteTitle logo'"; ?>></li>
                <li class="menu-text show-for-medium"><?php echo $siteTitle; ?></li>
                <li><a class="tn-menu-item" href='searchPage.php'>Search Page</a></li>
                <li><a class="tn-menu-item" href='addProductPage.php'>Add Product</a></li>
            </ul>
        </div>

        <div class="cell shrink">
            <form action='searchPage.php' method='GET'>
                <ul class='menu align-right'>
                    <li>
                        <select name="resultType">
                            <option value="ships">Ships</option>
                            <option value="pilots">Pilots</option>
                        </select>
                    </li>
                    <li><input name='nameContains' type='search' placeholder='Search'></li>
                    <li><button type='submit' class='button tn-menu-item'>Search</button></li>
                </ul>
            </form>
        </div>
    </div>
</div>