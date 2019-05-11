CREATE TABLE AccountPermission (
  account_type_id tinyint(1) NOT NULL, 
  permission_id   tinyint(1) NOT NULL, 
  PRIMARY KEY (account_type_id, 
  permission_id));
CREATE TABLE AccountType (
  account_type_id   tinyint(1) NOT NULL, 
  account_type_name tinytext NOT NULL, 
  PRIMARY KEY (account_type_id));
CREATE TABLE Colour (
  colour_id     int(10) NOT NULL, 
  red           smallint(6) NOT NULL, 
  green         smallint(6) NOT NULL, 
  blue          smallint(6) NOT NULL, 
  permission_id tinyint(1) NOT NULL, 
  name          tinytext NOT NULL, 
  PRIMARY KEY (colour_id));
CREATE TABLE Login (
  user_id       int(10) NOT NULL, 
  password_hash text NOT NULL, 
  PRIMARY KEY (user_id));
CREATE TABLE Palette (
  palette_id        int(10) NOT NULL, 
  name              tinytext NOT NULL, 
  user_id           int(10) NOT NULL, 
  type              tinytext NOT NULL, 
  `size`            tinyint(1) DEFAULT 5 NOT NULL, 
  creation_date     datetime NOT NULL, 
  modification_date datetime NOT NULL, 
  PRIMARY KEY (palette_id));
CREATE TABLE PaletteColour (
  palette_id     int(10) NOT NULL, 
  colour_id      int(10) NOT NULL, 
  position_index tinyint(1) NOT NULL, 
  PRIMARY KEY (palette_id, 
  colour_id, 
  position_index));
CREATE TABLE Permission (
  permission_id   tinyint(1) NOT NULL, 
  permission_name tinytext NOT NULL, 
  PRIMARY KEY (permission_id));
CREATE TABLE SavedPalette (
  palette_id int(10) NOT NULL, 
  user_id    int(10) NOT NULL, 
  PRIMARY KEY (palette_id, 
  user_id));
CREATE TABLE UserAccount (
  user_id         int(10) NOT NULL, 
  username        varchar(20) NOT NULL UNIQUE, 
  account_type_id tinyint(1) DEFAULT 0 NOT NULL, 
  PRIMARY KEY (user_id));
ALTER TABLE PaletteColour ADD CONSTRAINT FKPaletteCol542994 FOREIGN KEY (colour_id) REFERENCES Colour (colour_id);
ALTER TABLE PaletteColour ADD CONSTRAINT FKPaletteCol701537 FOREIGN KEY (palette_id) REFERENCES Palette (palette_id);
ALTER TABLE Palette ADD CONSTRAINT FKPalette160563 FOREIGN KEY (user_id) REFERENCES UserAccount (user_id);
ALTER TABLE Login ADD CONSTRAINT FKLogin985910 FOREIGN KEY (user_id) REFERENCES UserAccount (user_id);
ALTER TABLE SavedPalette ADD CONSTRAINT FKSavedPalet189893 FOREIGN KEY (user_id) REFERENCES UserAccount (user_id);
ALTER TABLE SavedPalette ADD CONSTRAINT FKSavedPalet423967 FOREIGN KEY (palette_id) REFERENCES Palette (palette_id);
ALTER TABLE UserAccount ADD CONSTRAINT FKUserAccoun454283 FOREIGN KEY (account_type_id) REFERENCES AccountType (account_type_id);
ALTER TABLE Colour ADD CONSTRAINT FKColour131693 FOREIGN KEY (permission_id) REFERENCES Permission (permission_id);
ALTER TABLE AccountPermission ADD CONSTRAINT FKAccountPer692690 FOREIGN KEY (permission_id) REFERENCES Permission (permission_id);
ALTER TABLE AccountPermission ADD CONSTRAINT FKAccountPer417135 FOREIGN KEY (account_type_id) REFERENCES AccountType (account_type_id);

