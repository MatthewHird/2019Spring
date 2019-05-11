INSERT INTO AccountPermission
  (account_type_id, 
  permission_id) 
VALUES 
  (?, 
  ?);
INSERT INTO AccountType
  (account_type_id, 
  account_type_name) 
VALUES 
  (?, 
  ?);
INSERT INTO Colour
  (colour_id, 
  red, 
  green, 
  blue, 
  permission_id, 
  name) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?);
INSERT INTO Login
  (user_id, 
  password_hash) 
VALUES 
  (?, 
  ?);
INSERT INTO Palette
  (palette_id, 
  name, 
  user_id, 
  type, 
  `size`, 
  creation_date, 
  modification_date) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?);
INSERT INTO PaletteColour
  (palette_id, 
  colour_id, 
  position_index) 
VALUES 
  (?, 
  ?, 
  ?);
INSERT INTO Permission
  (permission_id, 
  permission_name) 
VALUES 
  (?, 
  ?);
INSERT INTO SavedPalette
  (palette_id, 
  user_id) 
VALUES 
  (?, 
  ?);
INSERT INTO UserAccount
  (user_id, 
  username, 
  account_type_id) 
VALUES 
  (?, 
  ?, 
  ?);

