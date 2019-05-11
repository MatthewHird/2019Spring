<?php 
// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 

  $jsImports[] = 'frameworks/w3color.js'; 
  $jsImports[] = 'createPaletteWidget.js'; 
?>

<form class="createPaletteWidget" method="POST" action="createPalette.php" data-abide novalidate>
  

  <div class="cpw-outer">
    
    <div class="cpw-inner" style="border: 2px solid black">  
      


      <!-- START OPTIONS BAR -->
      <div id="cpwPaletteOptionsBar" style="border-bottom: 2px solid black">
        <div class="">
          
          <div class="grid-container">
              <div class="grid-x grid-padding-x">

                <div class="auto cell"></div>

                <div class="large-4 medium-4 small-6 cell">
                  <label class="cpw-text" for="cpwPaletteName">Palette Name</label>
                  <input id="cpwPaletteName" class="cpw-text" type="text" placeholder="Name your palette" required="required">
                </div>
                
                <div class="auto cell"></div>

                <div class="small-3 cell">
                  <label class="cpw-text" for="cpwColourCount"># of Colours</label>
                  <input id="cpwColourCount" class="cpw-text" type="number" min="1" max="9" value="5" style="max-width: 100px; margin: 0 auto;" disabled="disabled">
                </div>

                <div class="auto cell"></div>

              </div>
            </div>

        </div>
      </div>
      <!-- END OPTIONS BAR -->

      
      <!-- START BUTTON BAR -->
      <div id="cpwButtonBar" style="border-bottom: 2px solid black; margin-bottom: 2px;">
        <div class="grid-container">
          <div class="grid-x">
            
            
            <div class="cell small-6">
              <div class="flex-center">    
                  
                <div id="cpwUndo" class="cpw-button-cell" tabindex="-1" style="opacity: 0.3;" aria-label="Undo" onclick="undo()">
                  <div class="cpw-icon-button-group">
                    <div class="cpw-icon"><i class="fi fi-arrow-left"></i></div>
                    <div class="cpw-icon-button-label cpw-text">Undo</div>
                  </div>
                </div>
                
                <div id="cpwRedo" class="cpw-button-cell" tabindex="-1" style="opacity: 0.3;" aria-label="Redo" onclick="redo()">
                  <div class="cpw-icon-button-group">
                    <div class="cpw-icon"><i class=" fi fi-arrow-right"></i></div>
                    <div class="cpw-icon-button-label cpw-text">Redo</div>
                  </div>
                </div>

                <div id="cpwRandomAll" class="cpw-button-cell" tabindex="0" aria-label="Random All" onclick="randomAll()">
                  <div class="cpw-icon-button-group">
                    <div class="cpw-icon"><i class="fi fi-shuffle"></i></div>
                    <div class="cpw-icon-button-label cpw-text">Random</div>
                  </div>
                </div>

              </div>
            </div>

                
            <div class="cell small-6">
              <div class="flex-center">
                
                <div id="cpwLockAll" class="cpw-button-cell" tabindex="0" aria-label="Lock All" onclick="lockAll()">
                  <div class="cpw-icon-button-group">
                    <div class="cpw-icon"><i class="fi fi-lock"></i></div>
                    <div class="cpw-icon-button-label cpw-text">Lock All</div>
                  </div>
                </div>
                
                <div id="cpwUnlockAll" class="cpw-button-cell" tabindex="0" aria-label="Unlock All" onclick="unlockAll()">
                  <div class="cpw-icon-button-group">
                    <div class="cpw-icon"><i class="fi fi-unlock"></i></div>
                    <div class="cpw-icon-button-label cpw-text">Unlock All</div>
                  </div>
                </div>
              </div>
            </div>
          

          </div>
        </div>
      </div>
      <!-- END BUTTON BAR -->


      <!-- START DISPLAY BAR -->
      <div id="cpwDiplayBar" class="flex-center">
          
        <div class="small-2 cpw-colour-cell" id="ccell1">
          <div class="grid-y">
            <div class="text-align-center cpw-text cell small-3">
                <div id="ccell1Text" class="cpw-ccell-text"></div></div>
            <div class="text-align-center cpw-icon cell small-9 middle">
                <div id="ccell1Lock" class="cpw-ccell-lock">
                <i class="fi-unlock"></i></div></div>
          </div>
        </div>
        <div class="small-2 cpw-colour-cell" id="ccell2">
          <div class="grid-y">
            <div class="text-align-center cpw-text cell small-3">
                <div id="ccell2Text" class="cpw-ccell-text"></div></div>
            <div class="text-align-center cpw-icon cell small-9">
                <div id="ccell2Lock" class="cpw-ccell-lock">
                <i class="fi-unlock"></i></div></div>
          </div>
        </div>
        <div class="small-2 cpw-colour-cell" id="ccell3">
          <div class="grid-y">
            <div class="text-align-center cpw-text cell small-3">
                <div id="ccell3Text" class="cpw-ccell-text"></div></div>
            <div class="text-align-center cpw-icon cell small-9">
                <div id="ccell3Lock" class="cpw-ccell-lock">
                <i class="fi-unlock"></i></div></div>
          </div>
        </div>
        <div class="small-2 cpw-colour-cell" id="ccell4">
          <div class="grid-y">
            <div class="text-align-center cpw-text cell small-3">
                <div id="ccell4Text" class="cpw-ccell-text"></div></div>
            <div class="text-align-center cpw-icon cell small-9">
                <div id="ccell4Lock" class="cpw-ccell-lock">
                <i class="fi-unlock"></i></div></div>
          </div>
        </div>
        <div class="small-2 cpw-colour-cell" id="ccell5">
          <div class="grid-y">
            <div class="text-align-center cpw-text cell small-3">
                <div id="ccell5Text" class="cpw-ccell-text"></div></div>
            <div class="text-align-center cpw-icon cell small-9">
                <div id="ccell5Lock" class="cpw-ccell-lock">
                <i class="fi-unlock"></i></div></div>
          </div>
        </div>
      </div>
      <!-- END DISPLAY BAR -->
      
      
      <!-- START CONTROL BAR -->
      <div id="cpwControlBar" style="border-top: 2px solid black; margin-top: 2px;"> 
        
        

        <div class="grid-x">
          

          <div class="small-6 cell">
            <div id="cpwColourInputs" class="grid-container">

              <div id="cpwHex" class="grid-x">
                <div class="small-4 cell">
                  <label for="cpwHexInput" class="text-right middle cpw-control-label cpw-text">HEX:</label>
                </div>
                  
                <div class="small-8 cell">
                  <input id="cpwHexInput" class="cpw-control-input cpw-text" type="text" spellcheck="false" value="#7F7F7F">
                </div>          
              </div>


              <div id="cpwRGB" class="grid-x">
                <div class="small-4 cell">
                  <label for="cpwRGBInput" class="text-right middle cpw-control-label cpw-text">RGB:</label>  
                </div>

                <div class="small-8 cell">
                  <input id="cpwRGBInput" class="cpw-control-input cpw-text" type="text" spellcheck="false" value="127,127,127">
                </div>
              </div>
                
            </div>    
          </div>

          <div class="small-6 cell">
            <div class="grid-y">
              <div class="auto cell">
                <span id="submitConfirmation" class="cpw-text"></span>
              </div>

              <div class="small-5 cell">
                <div class="grid-x">
                  <div class="small-6 cell">
                    
                  </div>
                  <div class="small-6 cell">
                    <?php if ($user) { echo "<input class='button cpw-text' type='button' 
                        onclick='savePalette()' value='Save Palette'>"; } ?>
                  </div>


                </div>


                
              </div>
            </div>


          </div>


        </div>


      </div> 
      <!-- END CONTROL BAR -->


    </div>
  </div>
</form>

