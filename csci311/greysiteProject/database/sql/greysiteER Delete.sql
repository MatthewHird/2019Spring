DELETE FROM AccountPermission 
  WHERE account_type_id = ? AND permission_id = ?;
DELETE FROM AccountType 
  WHERE account_type_id = ?;
DELETE FROM Colour 
  WHERE colour_id = ?;
DELETE FROM Login 
  WHERE user_id = ?;
DELETE FROM Palette 
  WHERE palette_id = ?;
DELETE FROM PaletteColour 
  WHERE palette_id = ? AND colour_id = ? AND position_index = ?;
DELETE FROM Permission 
  WHERE permission_id = ?;
DELETE FROM SavedPalette 
  WHERE palette_id = ? AND user_id = ?;
DELETE FROM UserAccount 
  WHERE user_id = ?;

