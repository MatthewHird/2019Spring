UPDATE AccountPermission SET 
   
WHERE
  account_type_id = ? AND permission_id = ?;
UPDATE AccountType SET 
  account_type_name = ? 
WHERE
  account_type_id = ?;
UPDATE Colour SET 
  red = ?, 
  green = ?, 
  blue = ?, 
  permission_id = ?, 
  name = ? 
WHERE
  colour_id = ?;
UPDATE Login SET 
  password_hash = ? 
WHERE
  user_id = ?;
UPDATE Palette SET 
  name = ?, 
  user_id = ?, 
  type = ?, 
  `size` = ?, 
  creation_date = ?, 
  modification_date = ? 
WHERE
  palette_id = ?;
UPDATE PaletteColour SET 
   
WHERE
  palette_id = ? AND colour_id = ? AND position_index = ?;
UPDATE Permission SET 
  permission_name = ? 
WHERE
  permission_id = ?;
UPDATE SavedPalette SET 
   
WHERE
  palette_id = ? AND user_id = ?;
UPDATE UserAccount SET 
  username = ?, 
  account_type_id = ? 
WHERE
  user_id = ?;

