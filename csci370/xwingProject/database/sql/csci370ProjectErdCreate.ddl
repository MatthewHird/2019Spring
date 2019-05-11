CREATE TABLE action (
  id          tinyint(2) NOT NULL, 
  name        varchar(50) NOT NULL, 
  description varchar(1000) NOT NULL, 
  PRIMARY KEY (id));
CREATE TABLE action_bar (
  ship_id   tinyint(2) NOT NULL, 
  action_id tinyint(2) NOT NULL, 
  position  tinyint(1) NOT NULL, 
  PRIMARY KEY (ship_id, 
  action_id, 
  position));
CREATE TABLE firing_arc (
  id             tinyint(2) NOT NULL, 
  name           varchar(50) NOT NULL, 
  primary_attack BOOL NOT NULL, 
  description    varchar(1000) DEFAULT '' NOT NULL, 
  PRIMARY KEY (id));
CREATE TABLE huge_maneuver_dial (
  ship_id     tinyint(2) NOT NULL, 
  speed       tinyint(1) NOT NULL, 
  maneuver_id tinyint(2) NOT NULL, 
  energy      tinyint(1) NOT NULL, 
  PRIMARY KEY (ship_id, 
  speed, 
  maneuver_id));
CREATE TABLE huge_ship (
  ship_id           tinyint(2) NOT NULL, 
  energy            tinyint(1), 
  min_primary_range tinyint(1), 
  max_primary_range tinyint(1), 
  PRIMARY KEY (ship_id));
CREATE TABLE maneuver (
  id          tinyint(2) NOT NULL, 
  name        varchar(50) NOT NULL, 
  description varchar(1000) DEFAULT '' NOT NULL, 
  PRIMARY KEY (id));
CREATE TABLE maneuver_dial (
  ship_id     tinyint(2) NOT NULL, 
  speed       tinyint(1) NOT NULL, 
  maneuver_id tinyint(2) NOT NULL, 
  difficulty  ENUM('G','W','R') NOT NULL, 
  PRIMARY KEY (ship_id, 
  speed, 
  maneuver_id));
CREATE TABLE pilot (
  id          smallint(3) NOT NULL AUTO_INCREMENT, 
  name        varchar(50) NOT NULL, 
  variant     varchar(20) DEFAULT '' NOT NULL, 
  u_name_id   smallint(3) NOT NULL, 
  ship_id     tinyint(2) NOT NULL, 
  pilot_skill tinyint(1) NOT NULL, 
  elite       BOOL, 
  point_cost  tinyint(2), 
  text_box    varchar(100) DEFAULT '', 
  PRIMARY KEY (id), 
  CONSTRAINT uq_pilot 
    UNIQUE (name, variant));
CREATE TABLE ship (
  id      tinyint(2) NOT NULL, 
  name    varchar(50) NOT NULL, 
  variant varchar(20) DEFAULT '' NOT NULL, 
  faction ENUM('Rebel','Imperial','Scum') NOT NULL, 
  `size`  ENUM('Small','Large','Huge') NOT NULL, 
  attack  tinyint(1), 
  agility tinyint(1), 
  hull    tinyint(1), 
  shield  tinyint(1), 
  PRIMARY KEY (id), 
  CONSTRAINT uq_ship 
    UNIQUE (name, variant));
CREATE TABLE ship_firing_arc (
  ship_id       tinyint(2) NOT NULL, 
  firing_arc_id tinyint(2) NOT NULL, 
  PRIMARY KEY (ship_id, 
  firing_arc_id));
CREATE TABLE unique_name (
  id   smallint(3) NOT NULL AUTO_INCREMENT, 
  name varchar(50) NOT NULL UNIQUE, 
  PRIMARY KEY (id));
CREATE TABLE upgrade (
  id          tinyint(2) NOT NULL, 
  name        varchar(50) NOT NULL, 
  description varchar(1000) DEFAULT '' NOT NULL, 
  PRIMARY KEY (id));
CREATE TABLE upgrade_bar (
  ship_id    tinyint(2) NOT NULL, 
  upgrade_id tinyint(2) NOT NULL, 
  position   tinyint(1) NOT NULL, 
  PRIMARY KEY (ship_id, 
  upgrade_id, 
  position));
ALTER TABLE upgrade_bar ADD CONSTRAINT FKupgrade_ba385697 FOREIGN KEY (upgrade_id) REFERENCES upgrade (id);
ALTER TABLE upgrade_bar ADD CONSTRAINT FKupgrade_ba814753 FOREIGN KEY (ship_id) REFERENCES ship (id);
ALTER TABLE action_bar ADD CONSTRAINT FKaction_bar707930 FOREIGN KEY (ship_id) REFERENCES ship (id);
ALTER TABLE action_bar ADD CONSTRAINT FKaction_bar534866 FOREIGN KEY (action_id) REFERENCES action (id);
ALTER TABLE ship_firing_arc ADD CONSTRAINT FKship_firin407817 FOREIGN KEY (ship_id) REFERENCES ship (id);
ALTER TABLE ship_firing_arc ADD CONSTRAINT FKship_firin858355 FOREIGN KEY (firing_arc_id) REFERENCES firing_arc (id);
ALTER TABLE huge_maneuver_dial ADD CONSTRAINT FKhuge_maneu835924 FOREIGN KEY (maneuver_id) REFERENCES maneuver (id);
ALTER TABLE huge_maneuver_dial ADD CONSTRAINT FKhuge_maneu336777 FOREIGN KEY (ship_id) REFERENCES huge_ship (ship_id);
ALTER TABLE maneuver_dial ADD CONSTRAINT FKmaneuver_d5382 FOREIGN KEY (maneuver_id) REFERENCES maneuver (id);
ALTER TABLE maneuver_dial ADD CONSTRAINT FKmaneuver_d597695 FOREIGN KEY (ship_id) REFERENCES ship (id);
ALTER TABLE pilot ADD CONSTRAINT FKpilot831152 FOREIGN KEY (u_name_id) REFERENCES unique_name (id);
ALTER TABLE pilot ADD CONSTRAINT FKpilot226174 FOREIGN KEY (ship_id) REFERENCES ship (id);
ALTER TABLE huge_ship ADD CONSTRAINT FKhuge_ship156730 FOREIGN KEY (ship_id) REFERENCES ship (id);

