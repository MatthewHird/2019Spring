SELECT account_type_id, permission_id 
  FROM AccountPermission;
SELECT account_type_id, account_type_name 
  FROM AccountType;
SELECT colour_id, red, green, blue, permission_id, name 
  FROM Colour;
SELECT user_id, password_hash 
  FROM Login;
SELECT palette_id, name, user_id, type, `size`, creation_date, modification_date 
  FROM Palette;
SELECT palette_id, colour_id, position_index 
  FROM PaletteColour;
SELECT permission_id, permission_name 
  FROM Permission;
SELECT palette_id, user_id 
  FROM SavedPalette;
SELECT user_id, username, account_type_id 
  FROM UserAccount;

